package me.shils.redis

import groovy.transform.CompileStatic
import org.codehaus.groovy.ast.ClassCodeExpressionTransformer
import org.codehaus.groovy.ast.expr.AttributeExpression
import org.codehaus.groovy.ast.expr.BinaryExpression
import org.codehaus.groovy.ast.expr.ConstantExpression
import org.codehaus.groovy.ast.expr.ElvisOperatorExpression
import org.codehaus.groovy.ast.expr.Expression
import org.codehaus.groovy.ast.expr.FieldExpression
import org.codehaus.groovy.ast.expr.PropertyExpression
import org.codehaus.groovy.ast.expr.TernaryExpression
import org.codehaus.groovy.ast.expr.VariableExpression
import org.codehaus.groovy.control.SourceUnit
import org.codehaus.groovy.control.messages.ExceptionMessage
import org.codehaus.groovy.syntax.Token
import org.codehaus.groovy.syntax.Types
import static org.codehaus.groovy.ast.tools.GeneralUtils.constX


@CompileStatic
class ASTTransformer extends ClassCodeExpressionTransformer {

  static final List<Class<? extends Expression>> UNSUPPORTED_EXPRESSION_TYPES = [
          FieldExpression,
          PropertyExpression,
          AttributeExpression
  ]

  static final List<Integer> SUPPORTED_BINARY_OPERATORS = [
          Types.COMPARE_EQUAL,
          Types.COMPARE_GREATER_THAN_EQUAL,
          Types.COMPARE_GREATER_THAN,
          Types.COMPARE_LESS_THAN,
          Types.COMPARE_LESS_THAN_EQUAL
  ]

  SourceUnit sourceUnit

  @Override
  protected SourceUnit getSourceUnit() {
    return sourceUnit
  }

  @Override
  Expression transform(Expression expr) {
    if (UNSUPPORTED_EXPRESSION_TYPES.any { it.isInstance(expr) }) {
      addUnsupportedExpressionException(expr)
    }
    switch (expr.class) {
      case ConstantExpression:
        return constX(expr.text)
      case BinaryExpression:
        return transformBinaryExpression((BinaryExpression) expr)
      case VariableExpression:
        return constX(((VariableExpression) expr).name)
      case ElvisOperatorExpression:
      case TernaryExpression:
        addUnsupportedExpressionException(expr)
    }
    null
  }

  private ConstantExpression transformBinaryExpression(BinaryExpression bin) {
    Expression left = bin.leftExpression
    Expression right = bin.rightExpression
    Token operation = bin.operation

    switch (operation.type) {
      case SUPPORTED_BINARY_OPERATORS:
        return constX(transform(left).text + ' ' + operation.text + ' ' + transform(right).text)
    }
  }

  void addUnsupportedExpressionException(Expression expression) {
    sourceUnit.errorCollector.addException(new UnsupportedExpressionException(expression), sourceUnit)
  }
}
