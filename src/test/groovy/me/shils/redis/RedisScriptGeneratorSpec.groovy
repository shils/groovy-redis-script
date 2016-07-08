package me.shils.redis

import org.codehaus.groovy.ast.ClassNode
import org.codehaus.groovy.ast.expr.AttributeExpression
import org.codehaus.groovy.ast.expr.BinaryExpression
import org.codehaus.groovy.ast.expr.DeclarationExpression
import org.codehaus.groovy.ast.expr.ElvisOperatorExpression
import org.codehaus.groovy.ast.expr.EmptyExpression
import org.codehaus.groovy.ast.expr.Expression
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
import spock.lang.Shared
import spock.lang.Specification
import spock.lang.Unroll

import static org.codehaus.groovy.ast.tools.GeneralUtils.*

class RedisScriptGeneratorSpec extends Specification {

  private static final Token GT = Token.newSymbol(Types.COMPARE_GREATER_THAN, -1 , -1)

  SourceUnit sourceUnit
  ErrorCollector errorCollector
  RedisScriptGenerator transformer
  @Shared ClassNode arrayType = Stub(ClassNode) { isArray() >> true }

  def setup() {
    sourceUnit = Mock(SourceUnit)
    errorCollector = Mock(ErrorCollector)
    sourceUnit.getErrorCollector() >> errorCollector
    sourceUnit.getConfiguration() >> Stub(CompilerConfiguration)
    transformer = new RedisScriptGenerator(sourceUnit: sourceUnit)
  }

  def 'String literals'() {
    given:
    def result = transformer.convertToLuaSource(constX('foo'))

    expect:
    result == "'foo'"
  }

  def 'Number literals'(Number number) {
    given:
    def result = transformer.convertToLuaSource(constX(number))

    expect:
    result == number as String

    where:
    number << [1, 1L, 1G, 1.0f, 1.0d, 1.0G, 0x1]
  }

  def 'variable expressions'(String name, String transformed) {
    given:
    def vexp = varX(name)

    expect:
    transformer.convertToLuaSource(vexp) == transformed

    where:
    name   || transformed
    'foo'  || 'foo'
    'keys' || 'KEYS'
    'argv' || 'ARGV'
  }

  def 'Assignment expressions are only supported as statements'() {
    given:
    def target = Stub(Expression) { getNodeMetaData(RedisScriptGenerator) >> 'foo' }
    def value = Stub(Expression) { getNodeMetaData(RedisScriptGenerator) >> 'bar' }
    def statement = assignS(target, value)
    def expression = assignX(target, value)

    when: 'assignment that is a statement'
    transformer.convertToLuaSource(statement)

    and:
    def result = transformer.bufferValue

    then:
    result == 'foo = bar'

    when: 'assignment that is not a statement'
    transformer.convertToLuaSource(expression)

    then:
    1 * errorCollector.addErrorAndContinue({
      it.cause.message.contains('Assignments which are not statements are not supported in Redis scripts')
    })
  }

  def 'Declaration expressions are only supported as statements'() {
    def target = Stub(VariableExpression) { getNodeMetaData(RedisScriptGenerator) >> 'foo' }
    def value = Stub(Expression) { getNodeMetaData(RedisScriptGenerator) >> 'bar' }
    def statement = declS(target, value)
    def expression = new DeclarationExpression(target, ASSIGN, value)

    when: 'declaration that is a statement'
    transformer.convertToLuaSource(statement)

    and:
    def result = transformer.bufferValue

    then: 'variable is declared local'
    result == 'local foo = bar'

    when: 'declaration that is not a statement'
    transformer.convertToLuaSource(expression)

    then:
    1 * errorCollector.addErrorAndContinue({
      it.cause.message.contains('Assignments which are not statements are not supported in Redis scripts')
    })
  }

  def 'Empty declarations'() {
    given:
    def target = Stub(VariableExpression) { getNodeMetaData(RedisScriptGenerator) >> 'foo' }
    def statement = declS(target, EmptyExpression.INSTANCE)

    when:
    transformer.convertToLuaSource(statement)

    and:
    def result = transformer.bufferValue

    then:
    result == 'local foo'
  }

  def 'Comparison expressions'() {
    given:
    def left = Stub(Expression) { getNodeMetaData(RedisScriptGenerator) >> 'lhs' }
    def right = Stub(Expression) { getNodeMetaData(RedisScriptGenerator) >> 'rhs' }

    expect:
    transformer.convertToLuaSource(ltX(left, right)) == 'lhs < rhs'
    transformer.convertToLuaSource(gtX(left, right)) == 'lhs > rhs'
  }

  def 'Elvis expressions are compiled to lua or expressions'() {
    given:
    def elvis = new ElvisOperatorExpression(
            Stub(Expression) { getNodeMetaData(RedisScriptGenerator) >> 'first' },
            Stub(Expression) { getNodeMetaData(RedisScriptGenerator) >> 'second' }
    )

    expect:
    transformer.convertToLuaSource(elvis) == 'first or second'
  }

  def "'keys' variable expression is compiled to lua 'KEYS' variable"() {
    expect:
    transformer.convertToLuaSource(varX('keys')) == 'KEYS'
  }

  def "'argv' variable expression is compiled to lua 'ARGV' variable"() {
    expect:
    transformer.convertToLuaSource(varX('argv')) == 'ARGV'
  }

  def 'array.length property expressions'(Expression receiver, String result) {
    given:
    def expr = propX(receiver, constX('length'))

    expect:
    transformer.convertToLuaSource(expr) == result

    where:
    receiver                                                                                    || result
    Stub(Expression) { getType() >> arrayType; getNodeMetaData(RedisScriptGenerator) >> 'foo' } || 'table.getn(foo)'
    varX('keys')                                                                                || 'table.getn(KEYS)'
    varX('argv')                                                                                || 'table.getn(ARGV)'
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
    def first = Stub(Expression) { getNodeMetaData(RedisScriptGenerator) >> 'first' }
    def second = Stub(Expression) { getNodeMetaData(RedisScriptGenerator) >> 'second' }

    expect:
    transformer.convertToLuaSource(new ListExpression()) == '{}'
    transformer.convertToLuaSource(new ListExpression([first])) == '{first}'
    transformer.convertToLuaSource(new ListExpression([first, second])) == '{first, second}'
  }

  def 'Map entry expressions'() {
    given:
    def entry = new MapEntryExpression(
            Stub(Expression) { getNodeMetaData(RedisScriptGenerator) >> 'foo' },
            Stub(Expression) { getNodeMetaData(RedisScriptGenerator) >> 'bar' }
    )

    expect:
    transformer.convertToLuaSource(entry) == '[foo]=bar'
  }

  def 'Maps are compiled to lua tables'() {
    given:
    def first = Stub(MapEntryExpression) { getNodeMetaData(RedisScriptGenerator) >> '[first]=foo' }
    def second = Stub(MapEntryExpression) { getNodeMetaData(RedisScriptGenerator) >> '[second]=bar'}

    expect:
    transformer.convertToLuaSource(new MapExpression()) == '{}'
    transformer.convertToLuaSource(new MapExpression([first])) == '{[first]=foo}'
    transformer.convertToLuaSource(new MapExpression([first, second])) == '{[first]=foo, [second]=bar}'
  }

  private static BinaryExpression gtX(Expression lhv, Expression rhv) {
    new BinaryExpression(lhv, GT, rhv)
  }
}
