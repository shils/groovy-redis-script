package me.shils.redis

import org.codehaus.groovy.ast.ClassHelper
import org.codehaus.groovy.ast.ClassNode
import org.codehaus.groovy.ast.expr.AttributeExpression
import org.codehaus.groovy.ast.expr.BinaryExpression
import org.codehaus.groovy.ast.expr.ConstantExpression
import org.codehaus.groovy.ast.expr.ElvisOperatorExpression
import org.codehaus.groovy.ast.expr.Expression
import org.codehaus.groovy.ast.expr.FieldExpression
import org.codehaus.groovy.ast.expr.PropertyExpression
import org.codehaus.groovy.ast.expr.TernaryExpression
import org.codehaus.groovy.control.CompilerConfiguration
import org.codehaus.groovy.control.ErrorCollector
import org.codehaus.groovy.control.SourceUnit
import org.codehaus.groovy.control.messages.ExceptionMessage
import org.codehaus.groovy.syntax.Token
import org.codehaus.groovy.syntax.Types
import spock.lang.Specification
import spock.lang.Unroll

import static org.codehaus.groovy.ast.tools.GeneralUtils.*
import static org.codehaus.groovy.ast.ClassHelper.*


class ASTTransformerSpec extends Specification {

  private static final Token GT = Token.newSymbol(Types.COMPARE_GREATER_THAN, -1 , -1)

  def sourceUnit = Mock(SourceUnit)
  def errorCollector = Mock(ErrorCollector)
  ASTTransformer transformer = new ASTTransformer(sourceUnit: sourceUnit)

  def setup() {
    sourceUnit.getErrorCollector() >> errorCollector
    sourceUnit.getConfiguration() >> Stub(CompilerConfiguration)
  }

  def 'String literals'() {
    given:
    def result = transformer.transform(constX('foo'))

    expect:
    result instanceof ConstantExpression
    result.type == STRING_TYPE
    result.value == 'foo'
  }

  def 'Number literals'(Number number) {
    given:
    def result = transformer.transform(constX(number))

    expect:
    result instanceof ConstantExpression
    result.type == STRING_TYPE
    result.value == number as String

    where:
    number << [1, 1L, 1G, 1.0f, 1.0d, 1.0G, 0x1]
  }

  def 'Assignment expressions'() {
    given:
    def result = transformer.transform(assignX(varX('foo'), constX(1)))

    expect:
    result instanceof ConstantExpression
    result.type == STRING_TYPE
    result.value == 'foo = 1'
  }

  def 'Declaration expressions'() {
    given:
    def result = transformer.transform(declS(varX('foo'), constX(1)).expression)

    expect: 'variable is declared local'
    result instanceof ConstantExpression
    result.type == STRING_TYPE
    result.value == 'local foo = 1'
  }

  def 'Comparison expressions'() {
    when:
    def result = transformer.transform(ltX(varX('foo'), constX(3)))

    then:
    result instanceof ConstantExpression
    result.type == STRING_TYPE
    result.value == 'foo < 3'

    when:
    result = transformer.transform(gtX(varX('foo'), constX(3)))

    then:
    result instanceof ConstantExpression
    result.type == STRING_TYPE
    result.value == 'foo > 3'
  }

  def 'Elvis expressions are compiled to lua or expressions'() {
    when:
    def result = transformer.transform(new ElvisOperatorExpression(varX('foo'), varX('bar')))

    then:
    result instanceof ConstantExpression
    result.type == STRING_TYPE
    result.value == 'foo or bar'
  }

  @Unroll
  def 'Expressions of type #exprType result in UnsupportedExpressionException being added to error collector'(Class<? extends Expression> exprType) {
    given:
    def expr = Stub(exprType)

    when:
    def result = transformer.transform(expr)

    then:
    1 * errorCollector.addException({ it instanceof UnsupportedExpressionException && it.expression == expr }, sourceUnit)

    where:
    exprType << [
            FieldExpression,
            PropertyExpression,
            AttributeExpression,
            TernaryExpression
    ]
  }

  private static BinaryExpression gtX(Expression lhv, Expression rhv) {
    new BinaryExpression(lhv, GT, rhv)
  }
}
