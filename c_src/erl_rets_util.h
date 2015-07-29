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

// Includes
#include <string>
#include <fstream>
#include <iostream>
#include <erl_nif.h>

/* Max length for buffer (alloc) */
#define MAX_BUFF_LEN 1024

using std::string;
using std::map;

/* Protos */
bool parse_proplist(map<string,string> *Options, ErlNifEnv* env, ERL_NIF_TERM plist);
bool string_to_boolean(string elem);

/* Parse an Erlang Proplist into a map (key, value) */
bool parse_proplist(map<string,string> *Options, ErlNifEnv* env, ERL_NIF_TERM plist)
{
    ERL_NIF_TERM head, tail;
    int arity;
    const ERL_NIF_TERM *tuple;

    char value[MAX_BUFF_LEN];
    (void)memset(&value, '\0', sizeof(value));
    char key[MAX_BUFF_LEN];
    (void)memset(&key, '\0', sizeof(key));

    map<string,string>& myOptions = *Options;

    while(enif_get_list_cell(env, plist, &head, &tail)) {

        if(!enif_get_tuple(env, head, &arity, &tuple))
            return false;

        if(arity!=2)
            return false;

        if(!enif_get_atom(env, tuple[0], key, sizeof(key), ERL_NIF_LATIN1))
            return false;

        if(!enif_get_string(env, tuple[1], value, sizeof(value), ERL_NIF_LATIN1))
            return false;

        string str_key(key);
        string str_value(value);
        myOptions.insert(std::pair<string,string>(str_key,str_value));

        plist = tail;
    }

    return true;
}

/* String to boolean */
bool string_to_boolean(string elem)
{
    if ( elem == "true" )
    {
        return true;
    } else {
        return false;
    }
}

