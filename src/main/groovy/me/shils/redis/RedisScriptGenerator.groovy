package me.shils.redis

import groovy.transform.CompileStatic
import org.codehaus.groovy.ast.ASTNode
import org.codehaus.groovy.ast.ClassHelper
import org.codehaus.groovy.ast.ClassNode
import org.codehaus.groovy.ast.CodeVisitorSupport
import org.codehaus.groovy.ast.MethodNode
import org.codehaus.groovy.ast.expr.AttributeExpression
import org.codehaus.groovy.ast.expr.BinaryExpression
import org.codehaus.groovy.ast.expr.BooleanExpression
import org.codehaus.groovy.ast.expr.ClosureListExpression
import org.codehaus.groovy.ast.expr.ConstantExpression
import org.codehaus.groovy.ast.expr.DeclarationExpression
import org.codehaus.groovy.ast.expr.ElvisOperatorExpression
import org.codehaus.groovy.ast.expr.EmptyExpression
import org.codehaus.groovy.ast.expr.Expression
import org.codehaus.groovy.ast.expr.FieldExpression
import org.codehaus.groovy.ast.expr.ListExpression
import org.codehaus.groovy.ast.expr.MapEntryExpression
import org.codehaus.groovy.ast.expr.MapExpression
import org.codehaus.groovy.ast.expr.MethodCall
import org.codehaus.groovy.ast.expr.MethodCallExpression
import org.codehaus.groovy.ast.expr.PropertyExpression
import org.codehaus.groovy.ast.expr.RangeExpression
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
import org.codehaus.groovy.ast.stmt.EmptyStatement
import org.codehaus.groovy.ast.stmt.ExpressionStatement
import org.codehaus.groovy.ast.stmt.ForStatement
import org.codehaus.groovy.ast.stmt.IfStatement
import org.codehaus.groovy.ast.stmt.ReturnStatement
import org.codehaus.groovy.ast.stmt.SwitchStatement
import org.codehaus.groovy.ast.stmt.SynchronizedStatement
import org.codehaus.groovy.ast.stmt.ThrowStatement
import org.codehaus.groovy.ast.stmt.TryCatchStatement
import org.codehaus.groovy.ast.stmt.WhileStatement
import org.codehaus.groovy.control.SourceUnit
import org.codehaus.groovy.control.messages.SyntaxErrorMessage
import org.codehaus.groovy.syntax.SyntaxException
import org.codehaus.groovy.syntax.Token
import org.codehaus.groovy.syntax.Types
import redis.clients.jedis.JedisCommands

@CompileStatic
class RedisScriptGenerator extends CodeVisitorSupport {

  static final ClassNode REDIS_COMMANDS_TYPE = ClassHelper.make(JedisCommands)

  static final List<Integer> SUPPORTED_BINARY_OPERATORS = [
          Types.COMPARE_EQUAL,
          Types.COMPARE_GREATER_THAN_EQUAL,
          Types.COMPARE_GREATER_THAN,
          Types.COMPARE_LESS_THAN,
          Types.COMPARE_LESS_THAN_EQUAL,
          Types.PLUS,
          Types.LEFT_SQUARE_BRACKET
  ]

  private static final Map<String, ClassNode> UNQUALIFIED_TYPES = [
          String: ClassHelper.STRING_TYPE,
          List: ClassHelper.LIST_TYPE,
          Map: ClassHelper.MAP_TYPE
  ]

  SourceUnit sourceUnit

  private StringBuilder buffer = new StringBuilder()

  private boolean onNewLine = false

  String getBufferValue() {
    buffer.toString()
  }

  String convertToLuaSource(ASTNode node) {
    node.visit(this)
    getLuaSource(node)
  }

  @Override
  void visitExpressionStatement(ExpressionStatement es) {
    es.expression.putNodeMetaData(ExpressionStatement, true)
    write(convertToLuaSource(es.expression))
  }

  @Override
  void visitForLoop(ForStatement forLoop) {
    if (forLoop.collectionExpression instanceof ClosureListExpression) {
      addError('Only enhanced for loops are supported in Redis scripts', forLoop)
    }
    ClassNode collectionType = getType(forLoop.collectionExpression)
    if (collectionType == ClassHelper.MAP_TYPE) {
      addError('Enhanced for loops over maps are not supported', forLoop)
      return
    }

    if (collectionType == ClassHelper.LIST_TYPE) {
      write("for _, ${forLoop.variable.name} in ipairs(" + convertToLuaSource(forLoop.collectionExpression) + ') do')
    } else if (forLoop.collectionExpression instanceof RangeExpression) {
      RangeExpression range = (RangeExpression) forLoop.collectionExpression
      StringBuilder sb = new StringBuilder("for ${forLoop.variable.name} = ${convertToLuaSource(range.from)}, ${convertToLuaSource(range.to)}")
      if (!range.inclusive) {
        sb.append(' - 1')
      }
      write(sb.append(', 1 do').toString())
    }
    newLine()
    forLoop.loopBlock.visit(this)
    newLine()
    write('end')
    newLine()
  }

  @Override
  void visitWhileLoop(WhileStatement loop) {
    write('while ' + convertToLuaSource(loop.booleanExpression) + ' do')
    newLine()
    loop.loopBlock.visit(this)
    newLine()
    write('end')
    newLine()
  }

  @Override
  void visitReturnStatement(ReturnStatement statement) {
    write('return ' + convertToLuaSource(statement.expression))
  }

  @Override
  void visitTryCatchFinally(TryCatchStatement statement) {
    super.visitTryCatchFinally(statement)
  }

  @Override
  void visitIfElse(IfStatement ifElse) {
    write('if ' + convertToLuaSource(ifElse.booleanExpression) + ' then')
    newLine()
    ifElse.ifBlock.visit(this)
    newLine()
    if (ifElse.elseBlock && !(ifElse.elseBlock instanceof EmptyStatement)) {
      write('else')
      newLine()
      ifElse.elseBlock.visit(this)
      newLine()
    }
    write('end')
    newLine()
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
    write('break')
    newLine()
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
  void visitBlockStatement(BlockStatement block) {
    block.statements.each {
      it.visit(this)
      newLine()
    }
  }

  @Override
  void visitSynchronizedStatement(SynchronizedStatement sync) {
    addError("Synchronized blocks are not supported in Redis scripts", sync)
  }

  @Override
  void visitAssertStatement(AssertStatement assertStatement) {
    addError("Asserts are not supported in Redis scripts", assertStatement)
  }

  @Override
  void visitFieldExpression(FieldExpression expression) {
    addUnsupportedExpressionError(expression)
  }

  @Override
  void visitAttributeExpression(AttributeExpression expression) {
    addUnsupportedExpressionError(expression)
  }

  @Override
  void visitStaticMethodCallExpression(StaticMethodCallExpression call) {
    addUnsupportedExpressionError(call)
  }

  @Override
  void visitTernaryExpression(TernaryExpression expression) {
    addUnsupportedExpressionError(expression)
  }

  @Override
  void visitBooleanExpression(BooleanExpression expression) {
    storeLuaSource(expression, convertToLuaSource(expression.expression))
  }

  @Override
  void visitConstantExpression(ConstantExpression expression) {
    String result = expression.isNullExpression() ? 'nil'
            : expression.type == ClassHelper.STRING_TYPE ? "'${expression.text}'".toString() : expression.text
    storeLuaSource(expression, result)
  }

  @Override
  void visitBinaryExpression(BinaryExpression expression) {
    storeLuaSource(expression, transformBinaryExpression(expression))
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

  @Override
  void visitDeclarationExpression(DeclarationExpression expression) {
    String transformed = expression.rightExpression instanceof EmptyExpression ?
            convertToLuaSource(expression.leftExpression) : transformBinaryExpression(expression)
    storeLuaSource(expression, 'local ' + transformed)
  }

  @Override
  void visitShortTernaryExpression(ElvisOperatorExpression expression) {
    String result = convertToLuaSource(expression.trueExpression) + ' or ' + convertToLuaSource(expression.falseExpression)
    storeLuaSource(expression, result)
  }

  @Override
  void visitMethodCallExpression(MethodCallExpression call) {
    String result = call.implicitThis ? transformRedisCommand(call) : transformGroovyMethodCall(call)
    if (result) {
      storeLuaSource(call, result)
    } else if (call.implicitThis) {
      addError("No such redis command: ${call.methodAsString}".toString(), call)
    } else {
      addError("Method ${getMethodDescriptor(call)} is not supported in Redis scripts".toString(), call)
    }
  }

  private String transformRedisCommand(MethodCallExpression mce) {
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
    null
  }

  private String transformGroovyMethodCall(MethodCallExpression mce) {
    ClassNode receiverType = getType(mce.objectExpression)
    switch (mce.methodAsString) {
      case 'size':
        if (receiverType == ClassHelper.STRING_TYPE) {
          return 'string.len(' + convertToLuaSource(mce.objectExpression) + ')'
        } else if (receiverType == ClassHelper.LIST_TYPE) {
          return 'table.getn(' + convertToLuaSource(mce.objectExpression) + ')'
        }
        break
      case 'length':
        if (receiverType == ClassHelper.STRING_TYPE) {
          return 'string.len(' + convertToLuaSource(mce.objectExpression) + ')'
        }
        break
      default: return null
    }
    null
  }

  @Override
  void visitPropertyExpression(PropertyExpression expression) {
    if (isArray(expression.objectExpression) && expression.propertyAsString == 'length') {
      String result = 'table.getn(' + convertToLuaSource(expression.objectExpression) + ')'
      storeLuaSource(expression, result)
    } else {
      addError('Property expressions are not supported in Redis scripts', expression)
    }
  }

  @Override
  void visitVariableExpression(VariableExpression expression) {
    String name = expression.name
    String result = name == 'argv' ? 'ARGV' : name == 'keys' ? 'KEYS' : name
    storeLuaSource(expression, result)
  }

  @Override
  void visitListExpression(ListExpression expression) {
    String result = '{' + transformListOfExpressions(expression.expressions) + '}'
    storeLuaSource(expression, result)
  }

  @Override
  void visitMapExpression(MapExpression expression) {
    String result = '{' + transformListOfExpressions(expression.mapEntryExpressions) + '}'
    storeLuaSource(expression, result)
  }

  private String transformListOfExpressions(List<? extends Expression> expressions) {
    def sb = new StringBuilder()
    if (expressions.size() > 0) {
      sb.append(convertToLuaSource(expressions[0]))
    }
    expressions.size() > 1 && expressions.tail().each {
      sb.append(', ').append(convertToLuaSource(it))
    }
    sb.toString()
  }

  @Override
  void visitMapEntryExpression(MapEntryExpression expression) {
    String key = convertToLuaSource(expression.keyExpression)
    String value = convertToLuaSource(expression.valueExpression)
    storeLuaSource(expression, '[' + key + ']' + '=' + value)
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

  private void addUnsupportedExpressionError(Expression expr) {
    addError("${expr.class.simpleName}s are not supported in Redis scripts".toString(), expr)
  }

  private void write(String string) {
    buffer.append(string)
    onNewLine = false
  }

  private void newLine() {
    if (!onNewLine) {
      buffer.append('\n')
      onNewLine = true
    }
  }

  private static boolean isArray(Expression expression) {
    if (expression.type.isArray()) {
      return true
    } else if (expression instanceof VariableExpression) {
      String name = ((VariableExpression) expression).name
      return name == 'argv' || name == 'keys'
    }
    false
  }

  private static void storeLuaSource(ASTNode node, String luaSource) {
    node.putNodeMetaData(RedisScriptGenerator, luaSource)
  }

  private static String getLuaSource(ASTNode node) {
    (String) node.getNodeMetaData(RedisScriptGenerator)
  }

  private static ClassNode getType(Expression expr) {
    ClassNode type = expr.type
    ClassNode resolvedType = type.isResolved() ? type : UNQUALIFIED_TYPES[type.name]
    resolvedType ?: type
  }

  private static String getMethodDescriptor(MethodCallExpression call) {
    "${getType(call.objectExpression).name}#${call.methodAsString}".toString()
  }
}
