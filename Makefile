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

.PHONY: c_src

all: compile

deps: 
	@$(REBAR) get-deps

compile: deps
	@$(REBAR) compile -vv

clean:
	@rm -rf priv/*.o
	@$(REBAR) clean
	@rm -rf log
