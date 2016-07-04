package me.shils.redis

import org.codehaus.groovy.ast.ClassNode
import org.codehaus.groovy.ast.expr.AttributeExpression
import org.codehaus.groovy.ast.expr.BinaryExpression
import org.codehaus.groovy.ast.expr.ElvisOperatorExpression
import org.codehaus.groovy.ast.expr.EmptyExpression
import org.codehaus.groovy.ast.expr.Expression
import org.codehaus.groovy.ast.expr.FieldExpression
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

  def sourceUnit
  def errorCollector
  RedisScriptGenerator transformer

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

  def 'Assignment expressions are only supported as statements'() {
    when: 'assignment that is a statement'
    def statement = assignS(varX('foo'), constX(1))
    transformer.visitExpressionStatement(statement)

    and:
    def result = transformer.bufferValue

    then:
    result == 'foo = 1'

    when: 'assignment that is not a statement'
    result = transformer.convertToLuaSource(assignX(varX('foo'), constX(1)))

    then:
    1 * errorCollector.addErrorAndContinue({
      it.cause.message.contains('Assignments which are not statements are not supported in Redis scripts')
    })
  }

  def 'Declaration expressions are only supported as statements'() {
    when: 'declaration that is a statement'
    def statement = declS(varX('foo'), constX(1))
    transformer.visitExpressionStatement(statement)

    and:
    def result = transformer.bufferValue

    then: 'variable is declared local'
    result == 'local foo = 1'

    when: 'declaration that is not a statement'
    result = transformer.convertToLuaSource(assignX(varX('foo'), constX(1)))

    then:
    1 * errorCollector.addErrorAndContinue({
      it.cause.message.contains('Assignments which are not statements are not supported in Redis scripts')
    })
  }

  def 'Empty declarations'() {
    given:
    def statement = declS(varX('foo'), EmptyExpression.INSTANCE)

    when:
    transformer.visitExpressionStatement(statement)

    and:
    def result = transformer.bufferValue

    then:
    result == 'local foo'
  }

  def 'Comparison expressions'() {
    when:
    def result = transformer.convertToLuaSource(ltX(varX('foo'), constX(3)))

    then:
    result == 'foo < 3'

    when:
    result = transformer.convertToLuaSource(gtX(varX('foo'), constX(3)))

    then:
    result == 'foo > 3'
  }

  def 'Elvis expressions are compiled to lua or expressions'() {
    when:
    def result = transformer.convertToLuaSource(new ElvisOperatorExpression(varX('foo'), varX('bar')))

    then:
    result == 'foo or bar'

    when: 'General case'
    def elvis = new ElvisOperatorExpression(
            Mock(Expression) { 1 * getNodeMetaData(_) >> 'first' },
            Mock(Expression) { 1 * getNodeMetaData(_) >> 'second' }
    )

    and:
    result = transformer.convertToLuaSource(elvis)

    then:
    result == 'first or second'
  }

  def "'keys' variable expression is compiled to lua 'KEYS' variable"() {
    expect:
    transformer.convertToLuaSource(varX('keys')) == 'KEYS'
  }

  def "'argv' variable expression is compiled to lua 'ARGV' variable"() {
    expect:
    transformer.convertToLuaSource(varX('argv')) == 'ARGV'
  }

  def 'array.length property expressions'() {
    given:
    def receiverType = Stub(ClassNode) { isArray() >> true }

    when: 'receiver has array type'
    def receiver = Mock(Expression) {
      getType() >> receiverType
      1 * getNodeMetaData(RedisScriptGenerator) >> 'foo'
    }

    then:
    transformer.convertToLuaSource(propX(receiver, constX('length'))) == 'table.getn(foo)'

    when: "receiver is 'keys' variable"
    receiver = varX('keys')

    then:
    transformer.convertToLuaSource(propX(receiver, constX('length'))) == 'table.getn(KEYS)'

    when: "receiver is 'argv' variable"
    receiver = varX('argv')

    then:
    transformer.convertToLuaSource(propX(receiver, constX('length'))) == 'table.getn(ARGV)'
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

  private static BinaryExpression gtX(Expression lhv, Expression rhv) {
    new BinaryExpression(lhv, GT, rhv)
  }
}
