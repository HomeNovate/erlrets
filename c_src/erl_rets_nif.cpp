/*
 *
 * Copyright (C) 2015
 * This is a product for HomeNovate LLC
 * Authors: Jorge Garrido <zgbjgg@gmail.com>
 * All rights reserved.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 */

#include <erl_nif.h>
#include <memory>
#include <iostream>
#include <set>
#include <string>
#include "login.h"

using std::map;

/* Login nif */
static ERL_NIF_TERM Login_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int len;
    if (!enif_get_list_length(env, argv[0], &len)) 
    {
        return enif_make_badarg(env);        
    }     

    /* map to store key value from a plist*/
    map<string, string> Options;
    
    /* If no plist then badarg */
    if(!parse_proplist(&Options, env, argv[0]))
        return enif_make_badarg(env);

    /* Logging in using map */
    if(!logging_in(Options.find("url_login")->second,
        Options.find("username")->second,
        Options.find("password")->second,
        Options.find("user_agent")->second,
        Options.find("user_agent_password")->second,
        Options.find("use_http_get")->second,
        Options.find("rets_version")->second,
        Options.find("use_full_metadata")->second,
        Options.find("broker_code")->second,
        Options.find("save_metadata_tm")->second,
        Options.find("proxy_url")->second,
        Options.find("proxy_pass")->second,
        Options.find("disable_streaming")->second,
        Options.find("enable_caching")->second,
        Options.find("encoding")->second)) 
    {
        return enif_make_atom(env, "true"); 
    } else {
        return enif_make_atom(env, "false");
    }
}

/* All nif functions */
static ErlNifFunc nif_funcs[] = {
    {"login", 1, Login_nif}
};

/* INIT Nif */
ERL_NIF_INIT(erlrets_driver_nif, nif_funcs, NULL, NULL, NULL, NULL)
