package me.shils.redis

import groovy.transform.CompileStatic
import org.codehaus.groovy.ast.ASTNode
import org.codehaus.groovy.ast.ClassCodeExpressionTransformer
import org.codehaus.groovy.ast.ClassHelper
import org.codehaus.groovy.ast.ClassNode
import org.codehaus.groovy.ast.MethodNode
import org.codehaus.groovy.ast.expr.AttributeExpression
import org.codehaus.groovy.ast.expr.BinaryExpression
import org.codehaus.groovy.ast.expr.ConstantExpression
import org.codehaus.groovy.ast.expr.DeclarationExpression
import org.codehaus.groovy.ast.expr.ElvisOperatorExpression
import org.codehaus.groovy.ast.expr.EmptyExpression
import org.codehaus.groovy.ast.expr.Expression
import org.codehaus.groovy.ast.expr.FieldExpression
import org.codehaus.groovy.ast.expr.MethodCallExpression
import org.codehaus.groovy.ast.expr.PropertyExpression
import org.codehaus.groovy.ast.expr.StaticMethodCallExpression
import org.codehaus.groovy.ast.expr.TernaryExpression
import org.codehaus.groovy.ast.expr.TupleExpression
import org.codehaus.groovy.ast.expr.VariableExpression
import org.codehaus.groovy.ast.stmt.AssertStatement
import org.codehaus.groovy.ast.stmt.ExpressionStatement
import org.codehaus.groovy.ast.stmt.Statement
import org.codehaus.groovy.ast.stmt.SynchronizedStatement
import org.codehaus.groovy.control.MultipleCompilationErrorsException
import org.codehaus.groovy.control.SourceUnit
import org.codehaus.groovy.syntax.Token
import org.codehaus.groovy.syntax.Types
import redis.clients.jedis.JedisCommands

import static org.codehaus.groovy.ast.tools.GeneralUtils.constX

@CompileStatic
class ASTTransformer extends ClassCodeExpressionTransformer {

  static final ClassNode REDIS_COMMANDS_TYPE = ClassHelper.make(JedisCommands)

  static final List<Class<? extends Expression>> UNSUPPORTED_EXPRESSION_TYPES = [
          FieldExpression,
          PropertyExpression,
          AttributeExpression,
          StaticMethodCallExpression
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
      addError("${expr.class.simpleName}s are not supported in Redis scripts".toString(), expr)
    }
    switch (expr.class) {
      case ConstantExpression:
        return constX(expr.type == ClassHelper.STRING_TYPE ? "'${expr.text}'".toString() : expr.text)
      case DeclarationExpression:
        return transformDeclarationExpression((DeclarationExpression) expr)
      case BinaryExpression:
        return transformBinaryExpression((BinaryExpression) expr)
      case VariableExpression:
        return constX(((VariableExpression) expr).name)
      case ElvisOperatorExpression:
        return transformElvisExpression((ElvisOperatorExpression) expr)
      case MethodCallExpression:
        return transformMethodCallExpression((MethodCallExpression) expr)
      case TernaryExpression:
        addError('Ternary expressions are not supported in Redis scripts', expr)
    }
    null
  }

  @Override
  void visitExpressionStatement(ExpressionStatement es) {
    es.expression.putNodeMetaData(ExpressionStatement, true)
    super.visitExpressionStatement(es)
  }

  @Override
  void visitSynchronizedStatement(SynchronizedStatement sync) {
    addError("Synchronized blocks are not supported in Redis scripts", sync)
  }

  @Override
  void visitAssertStatement(AssertStatement assertStatement) {
    addError("Asserts are not supported in Redis scripts", assertStatement)
  }

  @SuppressWarnings("GroovyFallthrough")
  private ConstantExpression transformBinaryExpression(BinaryExpression bin) {
    Expression left = bin.leftExpression
    Expression right = bin.rightExpression
    Token operation = bin.operation

    switch (operation.type) {
      case Types.ASSIGN:
        if (!bin.getNodeMetaData(ExpressionStatement)) {
          addError('Assignments which are not statements are not supported in Redis scripts', bin)
          break
        }
      case SUPPORTED_BINARY_OPERATORS:
        return constX(transform(left).text + ' ' + operation.text + ' ' + transform(right).text)
      default:
        addError("Binary expressions using the $operation operator are not supported in Redis scripts", bin)
    }
    null
  }

  private ConstantExpression transformDeclarationExpression(DeclarationExpression decl) {
    if (decl.rightExpression instanceof EmptyExpression) {
      return constX('local ' + transform(decl.leftExpression).text)
    }
    return constX('local ' + transformBinaryExpression(decl).text)
  }

  private ConstantExpression transformElvisExpression(ElvisOperatorExpression elvis) {
    return constX(transform(elvis.trueExpression).text + ' or ' + elvis.falseExpression.text)
  }

  private ConstantExpression transformMethodCallExpression(MethodCallExpression mce) {
    List<Expression> args = ((TupleExpression) mce.arguments).expressions
    List<MethodNode> methods = REDIS_COMMANDS_TYPE.getDeclaredMethods(mce.getMethodAsString())
    MethodNode method = methods.find {
      int numParams = it.parameters.size()
      //TODO handle empty varargs case
      numParams == args.size() || (it.parameters[numParams - 1].type.isArray() && args.size() > numParams)
    }

    if (method) {
      StringBuilder sb = new StringBuilder("redis.call('${method.name}'".toString())
      args.each {
        sb.append(",${transform(it).text}".toString())
      }
      return constX(sb.append(')').toString())
    }
    addError("No such redis command: ${mce.methodAsString}".toString(), mce.method)
    null
  }

  @Override
  void addError(String msg, ASTNode expr) {
    super.addError(msg, expr)
    if (sourceUnit.errorCollector.hasErrors()) {
      throw new MultipleCompilationErrorsException(sourceUnit.errorCollector)
    }
  }
}
