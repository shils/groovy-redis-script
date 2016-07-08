package me.shils.redis

import me.shils.redis.RedisScriptGenerator.TransformedASTManager
import org.codehaus.groovy.ast.ClassNode
import org.codehaus.groovy.ast.GroovyCodeVisitor
import org.codehaus.groovy.ast.expr.AttributeExpression
import org.codehaus.groovy.ast.expr.BinaryExpression
import org.codehaus.groovy.ast.expr.ConstantExpression
import org.codehaus.groovy.ast.expr.DeclarationExpression
import org.codehaus.groovy.ast.expr.ElvisOperatorExpression
import org.codehaus.groovy.ast.expr.EmptyExpression
import org.codehaus.groovy.ast.expr.Expression
import org.codehaus.groovy.ast.expr.ExpressionTransformer
import org.codehaus.groovy.ast.expr.FieldExpression
import org.codehaus.groovy.ast.expr.ListExpression
import org.codehaus.groovy.ast.expr.MapEntryExpression
import org.codehaus.groovy.ast.expr.MapExpression
import org.codehaus.groovy.ast.expr.PropertyExpression
import org.codehaus.groovy.ast.expr.TernaryExpression
import org.codehaus.groovy.ast.expr.VariableExpression
import org.codehaus.groovy.control.CompilerConfiguration
import org.codehaus.groovy.control.ErrorCollector
import org.codehaus.groovy.control.SourceUnit
import org.codehaus.groovy.syntax.Token
import org.codehaus.groovy.syntax.Types
import spock.lang.Specification
import spock.lang.Unroll

import static org.codehaus.groovy.ast.tools.GeneralUtils.*

class RedisScriptGeneratorSpec extends Specification {

  private static final Token GT = Token.newSymbol(Types.COMPARE_GREATER_THAN, -1 , -1)

  SourceUnit sourceUnit
  ErrorCollector errorCollector
  TransformedASTManager manager
  RedisScriptGenerator transformer

  def setup() {
    sourceUnit = Mock(SourceUnit)
    errorCollector = Mock(ErrorCollector)
    manager = Mock(TransformedASTManager)
    sourceUnit.getErrorCollector() >> errorCollector
    sourceUnit.getConfiguration() >> Stub(CompilerConfiguration)
    transformer = new RedisScriptGenerator(sourceUnit: sourceUnit, manager: manager)
  }

  def 'String literals'() {
    given:
    def expr = constX('foo')

    when:
    transformer.convertToLuaSource(expr)

    then:
    1 * manager.storeLuaSource(expr, "'foo'")
  }

  def 'Number literals'(Number number) {
    given:
    def expr = constX(number)

    when:
    transformer.convertToLuaSource(expr)

    then:
    1 * manager.storeLuaSource(expr, { it == number as String })

    where:
    number << [1, 1L, 1G, 1.0f, 1.0d, 1.0G, 0x1]
  }

  def 'variable expressions'(String name, String transformed) {
    given:
    def vexp = varX(name)

    when:
    transformer.convertToLuaSource(vexp)

    then:
    1 * manager.storeLuaSource(vexp, transformed)

    where:
    name   || transformed
    'foo'  || 'foo'
    'keys' || 'KEYS'
    'argv' || 'ARGV'
  }

  def 'Assignment expressions are only supported as statements'() {
    given:
    def target = Stub(Expression)
    def value = Stub(Expression)
    def statement = assignS(target, value)
    def expression = assignX(target, value)

    and:
    manager.getLuaSource(target) >> 'foo'
    manager.getLuaSource(value) >> 'bar'

    when: 'assignment that is a statement'
    transformer.convertToLuaSource(statement)

    then:
    1 * manager.storeLuaSource({ statement.expression }, 'foo = bar')

    when: 'assignment that is not a statement'
    transformer.convertToLuaSource(expression)

    then:
    1 * errorCollector.addErrorAndContinue({
      it.cause.message.contains('Assignments which are not statements are not supported in Redis scripts')
    })
  }

  def 'Declaration expressions are only supported as statements'() {
    given:
    def target = Stub(VariableExpression)
    def value = Stub(Expression)
    def expression = new DeclarationExpression(target, ASSIGN, value)
    def statement = declS(target, value)

    and:
    manager.getLuaSource(target) >> 'foo'
    manager.getLuaSource(value) >> 'bar'

    when: 'declaration that is a statement'
    transformer.convertToLuaSource(statement)

    then: 'variable is declared local'
    1 * manager.storeLuaSource({ statement.expression }, 'local foo = bar')

    when: 'declaration that is not a statement'
    transformer.convertToLuaSource(expression)

    then:
    1 * errorCollector.addErrorAndContinue({
      it.cause.message.contains('Assignments which are not statements are not supported in Redis scripts')
    })
  }

  def 'Empty declarations'() {
    given:
    def target = Stub(VariableExpression)
    def statement = declS(target, EmptyExpression.INSTANCE)

    and:
    manager.getLuaSource(target) >> 'foo'

    when:
    transformer.convertToLuaSource(statement)

    then:
    1 * manager.storeLuaSource({ statement.expression }, 'local foo')
  }

  def 'Comparison expressions'() {
    given:
    def left = Stub(Expression)
    def right = Stub(Expression)
    def expr

    and:
    with(manager) {
      getLuaSource(left) >> 'lhs'
      getLuaSource(right) >> 'rhs'
    }

    when:
    expr = ltX(left, right)
    transformer.convertToLuaSource(expr)

    then:
    1 * manager.storeLuaSource({ expr }, 'lhs < rhs')

    when:
    expr = gtX(left, right)
    transformer.convertToLuaSource(expr)

    then:
    1 * manager.storeLuaSource({ expr }, 'lhs > rhs')
  }

  def 'Elvis expressions are compiled to lua or expressions'() {
    given:
    def first = Stub(Expression)
    def second = Stub(Expression)
    def elvis = new ElvisOperatorExpression(first, second)

    and:
    with(manager) {
      getLuaSource(first) >> 'first'
      getLuaSource(second) >> 'second'
    }

    when:
    transformer.convertToLuaSource(elvis)

    then:
    1 * manager.storeLuaSource(elvis, 'first or second')
  }

  def "'keys' variable expression is compiled to lua 'KEYS' variable"() {
    given:
    def expr = varX('keys')

    when:
    transformer.convertToLuaSource(expr)

    then:
    1 * manager.storeLuaSource(expr, 'KEYS')
  }

  def "'argv' variable expression is compiled to lua 'ARGV' variable"() {
    given:
    def expr = varX('argv')

    when:
    transformer.convertToLuaSource(expr)

    then:
    1 * manager.storeLuaSource(expr, 'ARGV')
  }

  def 'array.length property expressions'(Expression receiver, String result) {
    given:
    def expr = propX(receiver, constX('length'))

    and:
    with(manager) {
      getLuaSource(_ as VariableExpression) >> { VariableExpression ve -> ve.name in ['keys', 'argv'] ? ve.name.toUpperCase() : ve.name }
      getLuaSource(_ as DummyExpression) >> 'foo'
    }

    when:
    transformer.convertToLuaSource(expr)

    then:
    1 * manager.storeLuaSource(expr, result)

    where:
    receiver                                                         || result
    new DummyExpression(type: Stub(ClassNode) { isArray() >> true }) || 'table.getn(foo)'
    varX('keys')                                                     || 'table.getn(KEYS)'
    varX('argv')                                                     || 'table.getn(ARGV)'
  }

  @Unroll
  def 'Expressions of type #{expr.class} result in UnsupportedExpressionException being added to error collector'(Expression expr) {
    when:
    def result = transformer.convertToLuaSource(expr)

    then: //TODO make the message condition more specific
    1 * errorCollector.addErrorAndContinue({
      it.cause.message.contains('are not supported in Redis scripts')
    })

    where:
    expr << [
            new FieldExpression(null),
            new PropertyExpression(Stub(Expression), ''),
            new AttributeExpression(null, null),
            new TernaryExpression(null, null, null)
    ]
  }

  def 'Lists are compiled to lua tables'() {
    given:
    def list
    def first = Stub(Expression)
    def second = Stub(Expression)

    and:
    with(manager) {
      getLuaSource(first) >> 'first'
      getLuaSource(second) >> 'second'
    }

    when: 'empty list'
    list = new ListExpression()
    transformer.convertToLuaSource(list)

    then:
    1 * manager.storeLuaSource({ list }, '{}')

    when: 'single element list'
    list = new ListExpression([first])
    transformer.convertToLuaSource(list)

    then:
    1 * manager.storeLuaSource({ list }, '{first}')

    when: 'multi element list'
    list = new ListExpression([first, second])
    transformer.convertToLuaSource(list)

    then:
    1 * manager.storeLuaSource({ list }, '{first, second}')
  }

  def 'Map entry expressions'() {
    given:
    def key = Stub(Expression)
    def value = Stub(Expression)
    def entry = new MapEntryExpression(key, value)

    and:
    with(manager) {
      getLuaSource(key) >> 'foo'
      getLuaSource(value) >> 'bar'
    }

    when:
    transformer.convertToLuaSource(entry)

    then:
    1 * manager.storeLuaSource(entry, '[foo]=bar')
  }

  def 'Maps are compiled to lua tables'() {
    given:
    def map
    def first = Stub(MapEntryExpression)
    def second = Stub(MapEntryExpression)

    and:
    with(manager) {
      getLuaSource(first) >> '[first]=foo'
      getLuaSource(second) >> '[second]=bar'
    }

    when: 'empty map'
    map = new MapExpression()
    transformer.convertToLuaSource(map)

    then:
    1 * manager.storeLuaSource({ map }, '{}')

    when: 'single entry map'
    map = new MapExpression([first])
    transformer.convertToLuaSource(map)

    then:
    1 * manager.storeLuaSource({ map }, '{[first]=foo}')

    when: 'multi entry map'
    map = new MapExpression([first, second])
    transformer.convertToLuaSource(map)

    then:
    1 * manager.storeLuaSource({ map }, '{[first]=foo, [second]=bar}')
  }

  private static BinaryExpression gtX(Expression lhv, Expression rhv) {
    new BinaryExpression(lhv, GT, rhv)
  }

  static class DummyExpression extends Expression {
    @Override
    Expression transformExpression(ExpressionTransformer transformer) {
      this
    }

    @Override
    void visit(GroovyCodeVisitor visitor) {}
  }
}
