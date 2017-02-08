package me.shils.redis

import org.intellij.lang.annotations.Language
import redis.clients.jedis.Jedis
import redis.clients.jedis.JedisPool
import redis.clients.util.Pool
import spock.lang.Shared
import spock.lang.Specification

class RedisScriptIntegrationTest extends RedisScriptTest {

  Jedis jedis = new Jedis()
  private List<String> keys

  @Override
  protected void tearDown() {
    jedis.withCloseable { redis ->
      keys.each {
        redis.del(it)
      }
    }
    super.tearDown()
  }

  @Override
  void assertScriptResult(@Language('Groovy') String script, String expected, Object result, List<String> keys, List<String> args) {
    this.keys = keys
    assert result == jedis.withCloseable { redis -> redis.eval(expected, keys,  args) }
  }
}
