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
end
    """
  }

  void testIfStatementWithoutElse() {
    assertScriptResult '''
      redisScript {
        String result = 'zero'
        if (argv.length > 0) {
          result = 'non-zero'
        }
        result
      }
    ''', """
local result = 'zero'
if table.getn(ARGV) > 0 then
result = 'non-zero'
end
return result
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

  void testAssertNotSupported() {
    String error = shouldFail '''
      redisScript {
        assert 1 == 1
      }
    '''
    assert error.contains('Asserts are not supported')
  }

  void testSwitchNotSupported() {
    String error = shouldFail '''
      redisScript {
        switch(3) {
          case 4: return 5
          case 3: return 2
        }
      }
    '''
    assert error.contains('Switch statements are not supported')
  }

  void testSynchronizedNotSupported() {
    String error = shouldFail '''
      redisScript {
        synchronized (this) {
          return 5
        }
      }
    '''
    assert error.contains('Synchronized blocks are not supported')
  }

  void testStandardForLoopNotSupported() {
    String error = shouldFail '''
      redisScript {
        int total = 0
        for (int i = 0; i < 10; i++) {
          total = total + i
        }
        total
      }
    '''
    assert error.contains('Only enhanced for loops are supported')
  }

  void testWhileLoop() {
    assertScriptResult '''
      redisScript {
        int i = 0
        while (i < 10) {
          i = i + 1
        }
        i
      }
    ''', '''
local i = 0
while i < 10 do
i = i + 1
end
return i
    '''
  }

  void testBreakInLoop() {
    assertScriptResult '''
      redisScript {
        int i = 0
        while (true) {
          i = i + 1
          if (i == 10) {
            break
          }
        }
        i
      }
    ''', '''
local i = 0
while true do
i = i + 1
if i == 10 then
break
end
end
return i
    '''
  }

  void assertScriptResult(@Language('Groovy') String script, String expected) {
    assert shell.evaluate(script).trim() == expected.trim()
  }

  String shouldFail(@Language('Groovy') String script) {
    try {
      shell.evaluate(script)
    } catch (Exception e) {
      return e.message
    }
  }
}
