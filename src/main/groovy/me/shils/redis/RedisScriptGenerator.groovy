package me.shils.redis

import groovy.transform.CompileStatic
import org.codehaus.groovy.ast.ASTNode
import org.codehaus.groovy.ast.ClassHelper
import org.codehaus.groovy.ast.ClassNode
import org.codehaus.groovy.ast.CodeVisitorSupport
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
import org.codehaus.groovy.ast.stmt.BlockStatement
import org.codehaus.groovy.ast.stmt.BreakStatement
import org.codehaus.groovy.ast.stmt.CaseStatement
import org.codehaus.groovy.ast.stmt.ContinueStatement
import org.codehaus.groovy.ast.stmt.DoWhileStatement
import org.codehaus.groovy.ast.stmt.ExpressionStatement
import org.codehaus.groovy.ast.stmt.ForStatement
import org.codehaus.groovy.ast.stmt.IfStatement
import org.codehaus.groovy.ast.stmt.ReturnStatement
import org.codehaus.groovy.ast.stmt.SwitchStatement
import org.codehaus.groovy.ast.stmt.SynchronizedStatement
import org.codehaus.groovy.ast.stmt.ThrowStatement
import org.codehaus.groovy.ast.stmt.TryCatchStatement
import org.codehaus.groovy.ast.stmt.WhileStatement
import org.codehaus.groovy.control.MultipleCompilationErrorsException
import org.codehaus.groovy.control.SourceUnit
import org.codehaus.groovy.control.messages.SyntaxErrorMessage
import org.codehaus.groovy.syntax.SyntaxException
import org.codehaus.groovy.syntax.Token
import org.codehaus.groovy.syntax.Types
import redis.clients.jedis.JedisCommands

import static org.codehaus.groovy.ast.tools.GeneralUtils.constX

@CompileStatic
class RedisScriptGenerator extends CodeVisitorSupport {

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
          Types.COMPARE_LESS_THAN_EQUAL,
          Types.PLUS,
          Types.LEFT_SQUARE_BRACKET
  ]

  SourceUnit sourceUnit

  private StringBuilder buffer = new StringBuilder()

  String getBufferValue() {
    buffer.toString()
  }

  String convertToLuaSource(Expression expr) {
    if (UNSUPPORTED_EXPRESSION_TYPES.any { it.isInstance(expr) }) {
      addError("${expr.class.simpleName}s are not supported in Redis scripts".toString(), expr)
    }
    switch (expr.class) {
      case ConstantExpression:
        return expr.type == ClassHelper.STRING_TYPE ? "'${expr.text}'".toString() : expr.text
      case DeclarationExpression:
        return transformDeclarationExpression((DeclarationExpression) expr)
      case BinaryExpression:
        return transformBinaryExpression((BinaryExpression) expr)
      case VariableExpression:
        return transformVariableExpression((VariableExpression) expr)
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
    buffer.append(convertToLuaSource(es.expression))
    buffer.append('\n')
  }

  @Override
  void visitForLoop(ForStatement forLoop) {
    super.visitForLoop(forLoop)
  }

  @Override
  void visitWhileLoop(WhileStatement loop) {
    super.visitWhileLoop(loop)
  }

  @Override
  void visitReturnStatement(ReturnStatement statement) {
    buffer.append('return ' + convertToLuaSource(statement.expression))
  }

  @Override
  void visitTryCatchFinally(TryCatchStatement statement) {
    super.visitTryCatchFinally(statement)
  }

  @Override
  void visitIfElse(IfStatement ifElse) {
    super.visitIfElse(ifElse)
  }

  @Override
  void visitSwitch(SwitchStatement statement) {
    addError('Switch statements are not supported in Redis scripts', statement)
  }

  @Override
  void visitCaseStatement(CaseStatement statement) {
    addError('Switch statements are not supported in Redis scripts', statement)
  }

  @Override
  void visitThrowStatement(ThrowStatement statement) {
    super.visitThrowStatement(statement)
  }

  @Override
  void visitBreakStatement(BreakStatement statement) {
    super.visitBreakStatement(statement)
  }

  @Override
  void visitContinueStatement(ContinueStatement statement) {
    super.visitContinueStatement(statement)
  }

  @Override
  void visitDoWhileLoop(DoWhileStatement loop) {
    super.visitDoWhileLoop(loop)
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
  private String transformBinaryExpression(BinaryExpression bin) {
    Expression left = bin.leftExpression
    Expression right = bin.rightExpression
    Token operation = bin.operation

    switch (operation.type) {
      case Types.LEFT_SQUARE_BRACKET:
        return convertToLuaSource(left) + "[${convertToLuaSource(right)}]".toString()
      case Types.ASSIGN:
        if (!bin.getNodeMetaData(ExpressionStatement)) {
          addError('Assignments which are not statements are not supported in Redis scripts', bin)
          break
        }
      case SUPPORTED_BINARY_OPERATORS:
        return convertToLuaSource(left) + ' ' + operation.text + ' ' + convertToLuaSource(right)
      default:
        addError("Binary expressions using the $operation operator are not supported in Redis scripts", bin)
    }
    null
  }

  private String transformDeclarationExpression(DeclarationExpression decl) {
    if (decl.rightExpression instanceof EmptyExpression) {
      return 'local ' + convertToLuaSource(decl.leftExpression)
    }
    return 'local ' + transformBinaryExpression(decl)
  }

  private String transformElvisExpression(ElvisOperatorExpression elvis) {
    return convertToLuaSource(elvis.trueExpression) + ' or ' + elvis.falseExpression.text
  }

  private String transformMethodCallExpression(MethodCallExpression mce) {
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
        sb.append(", ${convertToLuaSource(it)}".toString())
      }
      return sb.append(')').toString()
    }
    addError("No such redis command: ${mce.methodAsString}".toString(), mce.method)
    null
  }

  private String transformVariableExpression(VariableExpression vexp) {
    if (vexp.name == 'argv') {
      return 'ARGV'
    } else if (vexp.name == 'keys') {
      return 'KEYS'
    }
    vexp.name
  }

  private void addError(String msg, ASTNode expr) {
    sourceUnit.errorCollector.addErrorAndContinue(
            new SyntaxErrorMessage(
                    new SyntaxException(
                            msg + '\n',
                            expr.getLineNumber(),
                            expr.getColumnNumber(),
                            expr.getLastLineNumber(),
                            expr.getLastColumnNumber()
                    ),
                    sourceUnit
            )
    )
  }
}