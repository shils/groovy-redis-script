package me.shils.redis

import groovy.transform.CompileStatic
import groovyjarjarasm.asm.Opcodes
import org.codehaus.groovy.ast.ASTNode
import org.codehaus.groovy.ast.ClassCodeExpressionTransformer
import org.codehaus.groovy.ast.ClassHelper
import org.codehaus.groovy.ast.ClassNode
import org.codehaus.groovy.ast.GroovyCodeVisitor
import org.codehaus.groovy.ast.MethodCallTransformation
import org.codehaus.groovy.ast.MethodInvocationTrap
import org.codehaus.groovy.ast.MethodNode
import org.codehaus.groovy.ast.ModuleNode
import org.codehaus.groovy.ast.Parameter
import org.codehaus.groovy.ast.expr.AttributeExpression
import org.codehaus.groovy.ast.expr.ClosureExpression
import org.codehaus.groovy.ast.expr.ConstantExpression
import org.codehaus.groovy.ast.expr.Expression
import org.codehaus.groovy.ast.expr.FieldExpression
import org.codehaus.groovy.ast.expr.MethodCallExpression
import org.codehaus.groovy.ast.expr.PropertyExpression
import org.codehaus.groovy.ast.expr.TupleExpression
import org.codehaus.groovy.classgen.ReturnAdder
import org.codehaus.groovy.control.CompilePhase
import org.codehaus.groovy.control.SourceUnit
import org.codehaus.groovy.control.io.ReaderSource
import org.codehaus.groovy.macro.transform.MacroInvocationTrap
import org.codehaus.groovy.transform.GroovyASTTransformation

import static org.codehaus.groovy.ast.expr.VariableExpression.THIS_EXPRESSION

@CompileStatic
@GroovyASTTransformation(phase = CompilePhase.CONVERSION)
class RedisScriptTransformation extends MethodCallTransformation {

  public static final String REDIS_SCRIPT_METHOD = 'redisScript'

  @Override
  protected GroovyCodeVisitor getTransformer(ASTNode[] nodes, SourceUnit sourceUnit) {
    ClassCodeExpressionTransformer transformer = new ASTTransformer(sourceUnit: sourceUnit)
    new RedisScriptInvocationTrap(transformer, sourceUnit)
  }

  static class RedisScriptInvocationTrap extends MethodInvocationTrap {

    ClassCodeExpressionTransformer transformer

    RedisScriptInvocationTrap(ClassCodeExpressionTransformer transformer, SourceUnit sourceUnit) {
      super(sourceUnit.source, sourceUnit)
      this.transformer = transformer
    }

    @Override
    protected boolean handleTargetMethodCallExpression(MethodCallExpression call) {
      TupleExpression args = (TupleExpression) call.arguments
      ClosureExpression closure = (ClosureExpression) args.getExpression(args.expressions.size() - 1)
      MethodNode dummy = new MethodNode(
              'dummy',
              Opcodes.ACC_PUBLIC,
              ClassHelper.DYNAMIC_TYPE,
              Parameter.EMPTY_ARRAY,
              ClassNode.EMPTY_ARRAY,
              closure.code
      )
      new ReturnAdder().visitMethod(dummy)
      dummy.code.visit(transformer)
      false
    }

    @Override
    protected boolean isBuildInvocation(MethodCallExpression call) {
      MacroInvocationTrap.isBuildInvocation(call, REDIS_SCRIPT_METHOD)
    }
  }
}