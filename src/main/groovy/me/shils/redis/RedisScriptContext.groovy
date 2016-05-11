package me.shils.redis

import redis.clients.jedis.JedisCommands


interface RedisScriptContext extends JedisCommands {

  String[] getKeys()

  String[] getArgv()
}
