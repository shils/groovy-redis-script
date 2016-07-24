package me.shils.redis

import groovy.transform.CompileStatic
import groovyjarjarasm.asm.Opcodes
import org.codehaus.groovy.ast.ASTNode
import org.codehaus.groovy.ast.ClassHelper
import org.codehaus.groovy.ast.ClassNode
import org.codehaus.groovy.ast.GroovyCodeVisitor
import org.codehaus.groovy.ast.MethodCallTransformation
import org.codehaus.groovy.ast.MethodInvocationTrap
import org.codehaus.groovy.ast.MethodNode
import org.codehaus.groovy.ast.Parameter
import org.codehaus.groovy.ast.expr.ArgumentListExpression
import org.codehaus.groovy.ast.expr.ClosureExpression
import org.codehaus.groovy.ast.expr.MethodCallExpression
import org.codehaus.groovy.ast.expr.TupleExpression
import org.codehaus.groovy.classgen.ReturnAdder
import org.codehaus.groovy.classgen.VariableScopeVisitor
import org.codehaus.groovy.control.CompilePhase
import org.codehaus.groovy.control.SourceUnit
import org.codehaus.groovy.macro.transform.MacroInvocationTrap
import org.codehaus.groovy.transform.GroovyASTTransformation

import static org.codehaus.groovy.ast.tools.GeneralUtils.constX

@CompileStatic
@GroovyASTTransformation(phase = CompilePhase.CONVERSION)
class RedisScriptTransformation extends MethodCallTransformation {

  public static final String REDIS_SCRIPT_METHOD = 'redisScript'

  @Override
  protected GroovyCodeVisitor getTransformer(ASTNode[] nodes, SourceUnit sourceUnit) {
    new RedisScriptInvocationTrap(sourceUnit)
  }

  static class RedisScriptInvocationTrap extends MethodInvocationTrap {

    RedisScriptInvocationTrap(SourceUnit sourceUnit) {
      super(sourceUnit.source, sourceUnit)
    }

    @Override
    protected boolean handleTargetMethodCallExpression(MethodCallExpression call) {
      if (!call.getNodeMetaData(RedisScriptInvocationTrap)) {
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
        VariableScopeVisitor scopeVisitor = new VariableScopeVisitor(sourceUnit)
        //pick an arbitrary class in the source unit so that the variable scope visitor can run on the closure code
        ClassNode cls = sourceUnit.AST.classes[0]
        if (!cls.putNodeMetaData(RedisScriptTransformation, true)) {
          scopeVisitor.prepareVisit(cls)
        }
        scopeVisitor.visitMethod(dummy)
        new ReturnAdder().visitMethod(dummy)
        GroovyCodeVisitor transformer = new RedisScriptGenerator(sourceUnit: sourceUnit)
        dummy.code.visit(transformer)
        call.arguments = new ArgumentListExpression(constX(transformer.bufferValue))
        call.putNodeMetaData(RedisScriptInvocationTrap, true)
      }
      false
    }

    @Override
    protected boolean isBuildInvocation(MethodCallExpression call) {
      MacroInvocationTrap.isBuildInvocation(call, REDIS_SCRIPT_METHOD)
    }
  }
}
