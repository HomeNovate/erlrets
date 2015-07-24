##
## Copyright (C) 2014
## This is a product for HomeNovate LLC 
## Authors: Jorge Garrido <zgbjgg@gmail.com>
## All rights reserved.
##
## This Source Code Form is subject to the terms of the Mozilla Public
## License, v. 2.0. If a copy of the MPL was not distributed with this
## file, You can obtain one at http://mozilla.org/MPL/2.0/.
##

##
## Rebar 
##
REBAR=./rebar

##
##
## Necessary includes for build a port driver
##
ERL_ROOT=/root/erlang_rb1602/lib/erlang

##
##
## Mandatory linked library for ei
##
ERL_INTERFACE=/root/erlang_rb1602/lib/erlang/usr/lib/

##
## Shared object name
##
SHARED_OBJ=erl_rets.so

##
## Other CFLAGS
##
CFLAGSO=-shared -fpic -lerl_interface -lei

.PHONY: c_src

all: compile

c_src:
	@mkdir -p priv
	$(CC) -o priv/$(SHARED_OBJ) c_src/*.c -L$(ERL_INTERFACE) -I$(ERL_ROOT)/usr/include/ -I$(PWD)/c_src/ $(CFLAGSO) 

deps: 
	@$(REBAR) get-deps

compile: c_src deps
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	@rm -rf priv/$(SHARED_OBJ)
