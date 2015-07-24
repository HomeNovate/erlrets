# erlrets
Erlang Driver for libRETS

## **Building**

To build you need `rebar` and you must need install `libRETS`.

When building ensure that `ERL_ROOT` and `ERL_INTERFACE` on `Makefile` point to your
correct paths of your erlang installation.

When building is correct a `erl_rets.so` driver is placed under `priv` directory.


## **Using**

To use driver just start `erlang` with the correct paths:

```bash
erl -pa ebin/ deps/*/ebin
```

Now start the driver:

```erlang
1> application:start(erlrets).
2> application:start(erlrets).
20:26:59.022 [info] successfully loaded driver: erl_rets
ok
```
	
Use a search function, providing a query:

```erlang
6> erlrets:search("query").
20:27:14.506 [info] executing search query: query on port #Port<0.2875>
{ok,ok}
```

Enjoy!

## **Features**
* Native C/C++ driver with Erlang
* C/C++ from/to through terms instead binaries 
