# erlrets
Erlang Driver for libRETS

## **What is RETS?**

RETS is an acronym which stands for Real Estate Transaction Standard.
RETS is a framework used in Canada & the United States by the real estate 
industry to facilitate the exchange of data.

## **What is erlrets?**

`erlrets` is an erlang application that you can use to connect againts RETS server 
and use all the features like:

	* login
	* search
	* metadata
	* object
	* brokers
	* agents

## **Dependencies**

The project is based on `nif` so there are some dependencies to make it works:

	* libRETS
	* boost
	* libcurl
	* expat

In order to install `libRETS` follow the next steps:

	* Download libRETS project from github:
	
		$ git clone https://github.com/zgbjgg/libRETS
	
	* Move to cloned repo:

		$ cd libRETS

	* Generate configure with auto-tools:

		$ ./autogen.sh

	* Configure:

		$ ./configure

	* Compile:

		$ make

	* Install:

		$ make install

	* TROUBLESHOOTING:

		AUTO-TOOLS - You need auto-tools in order to start: `yum install autoconf automake`
		LIBCURL-DEVEL: You need the last version of libcurl-devel, so install with yum: `yum install libcurl-devel`
		BOOST-DEVEL: You need at least 1.44 or higher, if not then compile manually:
			Download the source from the official page [http://www.boost.org/]

			$ tar xvf boost_1_58_0.tar.gz

			$ cd boost_1_58_0

			$ ./bootstrap.sh

			$ ./b2 install

			WARNING: If after install the configure on the libRETS still fails, open vim /usr/include/boost/version.hpp and edit, change the version to 44 and done!!


## **Building**

To build you need `rebar` and you must need install `libRETS`:

	$ make

TIP: If build fails, open `rebar.config` and check the paths to libraries linked.
When building is correct a `erl_rets_nif.so` driver is placed under `priv` directory.


## **Using**

To use driver just start `erlang` with the correct paths:

```bash
erl -pa ebin/ deps/*/ebin
```

Now start the driver:

```erlang
2> application:start(erlrets).
ok
5> 22:52:12.903 [info] Application erlrets started on node nonode@nohost
```
	
Enjoy!

**[Session](#session)**

To start a `PID` to manage the session against RETS server:

```erlang
5> Options = [{user_agent, ""}, {user_agent_password, ""}, {use_http_get, "false"}, {rets_version, "1_8"}, {use_full_metadata, "false"}, {broker_code, ""}, {save_metadata_tm, ""}, {proxy_url, ""}, {proxy_pass, ""}, {disable_streaming, "false"}, {enable_caching, "true"}, {encoding, "ascii"}].
[{user_agent,[]},
 {user_agent_password,[]},
 {use_http_get,"false"},
 {rets_version,"1_8"},
 {use_full_metadata,"false"},
 {broker_code,[]},
 {save_metadata_tm,[]},
 {proxy_url,[]},
 {proxy_pass,[]},
 {disable_streaming,"false"},
 {enable_caching,"true"},
 {encoding,"ascii"}]
6> {ok, Pid} = erlrets:start_link("http://10.0.2.2:8080/rets/login", "zgbjgg", "password", Options).
21:54:38.476 [info] loading driver on "./priv/erl_rets_nif" 
21:54:38.500 [info] successfully loaded driver: erl_rets
{ok,<0.55.0>}
```
Now you can use the `PID` to perform actions over the opened session.

**[Checking session](#checking-session)**

To check if the session is valid:

```erlang
7> erlrets:check_session().
21:54:45.683 [info] checking session by resource <<131,97,19>>
{ok,active}
```

## **Comming soon more features ...**

## **Features**
* NIF between Erlang and C++
* Native C/C++ driver with Erlang
* C/C++ from/to through terms instead binaries 
