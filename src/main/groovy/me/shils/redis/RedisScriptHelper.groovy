package me.shils.redis

class RedisScriptHelper {
  static String redisScript(@DelegatesTo(RedisScriptContext) Closure closure) {
    null
  }

  static String redisScript(String script) {
    script
  }
}
