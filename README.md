Erland Redis Node Discovery
===========================

[![Build Status](https://travis-ci.org/potatosalad/erlang_redis_node.png?branch=master)](https://travis-ci.org/potatosalad/erlang_redis_node)

Getting started
---------------

```erlang
{ok, _} = redis_node:start_group(mygroup, [{uri, "redis://127.0.0.1:6379/0"}]),
redis_node:call(mygroup, ping).

{ok,[{nonode@nohost,676,pong}]}
```

You can also configure groups using the application environment variables:

```erlang
{redis_node, [
    {groups, [
        {mygroup, [{uri, "redis://127.0.0.1:6379/0"}], redis_node_handler, []}
    ]}
]}.
```

License
-------

[CDDL 1.0](http://opensource.org/licenses/CDDL-1.0)
