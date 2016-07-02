package me.shils.redis

import groovy.transform.NotYetImplemented
import org.codehaus.groovy.control.CompilerConfiguration
import org.codehaus.groovy.control.customizers.ImportCustomizer
import org.intellij.lang.annotations.Language
import static me.shils.redis.RedisScriptHelper.redisScript


class RedisScriptTest extends GroovyShellTestCase {

  @Override
  protected GroovyShell createNewShell() {
    def icz = new ImportCustomizer().addStaticImport('me.shils.redis.RedisScriptHelper', 'redisScript')
    def config = new CompilerConfiguration().addCompilationCustomizers(icz)
    new GroovyShell(config)
  }

  void testCallRedis() {
    assertScriptResult '''
      redisScript {
        set(keys[1], argv[1])
      }
    ''', "return redis.call('set', KEYS[1], ARGV[1])"
  }

  @NotYetImplemented
  void testIfElseStatement() {
    assertScriptResult '''
      redisScript {
        if (argv.length > 0) {
          'non-zero'
        } else {
          'zero'
        }
      }
    ''', """
if table.getn(ARGV) > 0 then
  return 'non-zero'
else
  return 'zero'
    """
  }

  void testVariableDeclaration() {
    assertScriptResult '''
      redisScript {
        def i = 1
        i + argv[1]
      }
    ''', """
local i = 1
return i + ARGV[1]
    """
  }

  void assertScriptResult(@Language('Groovy') String script, String expected) {
    assert shell.evaluate(script) == expected.trim()
  }
}
