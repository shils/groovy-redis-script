package me.shils.redis

import groovy.transform.CompileStatic
import org.codehaus.groovy.ast.expr.Expression

@CompileStatic
class UnsupportedExpressionException extends Exception {
  Expression expression

  UnsupportedExpressionException(Expression expression) {
    super()
    this.expression = expression
  }
}
