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
#include <stdio.h>
#include <stdlib.h>
#include <time.h> 
#include <string>
#include "librets.h"
#include "Login.h"
#include "erl_rets_util.h"

using namespace librets;
using std::map;

/* Global storage for sessions */
static map<int, RetsSessionPtr> sessions;

/* Check session nif */
static ERL_NIF_TERM CheckSession_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (!enif_is_number(env, argv[0])) 
        return enif_make_badarg(env); 

    // RetsSessionPtr session = sessions.begin()->second;
    int iKey;
    if (!enif_get_int(env, argv[0], &iKey))
        return enif_make_badarg(env); 

    RetsSessionPtr session = sessions.find(iKey)->second;
   
    if(!session)
        return enif_make_atom(env, "expired");    

    return enif_make_atom(env, "active");
}

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
    Login login;
    
    RetsSessionPtr session = login.OpenSession(Options.find("url_login")->second,
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
        Options.find("encoding")->second);

    if(!session)
        return enif_make_atom(env, "false");

    /* Generate random key */
    int iKey;
   
    srand (time(NULL));

    iKey = rand() % 100;  

    ERL_NIF_TERM atom_ok = enif_make_atom(env, "ok");
    ERL_NIF_TERM ref = enif_make_int(env, iKey);
    
    /* Store session into map sessions ! */
    sessions[iKey] = session;

    return enif_make_tuple(env, 2, atom_ok, ref);
}


/* All nif functions */
static ErlNifFunc nif_funcs[] = {
    {"login", 1, Login_nif},
    {"check_session", 1, CheckSession_nif}
};

/* INIT Nif */
ERL_NIF_INIT(erlrets_driver_nif, nif_funcs, NULL, NULL, NULL, NULL)

