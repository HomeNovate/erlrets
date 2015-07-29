#include <erl_nif.h>
#include <memory>
#include <iostream>
#include <set>
#include <string>
#include "login.h"

using namespace std;
using std::map;

static ERL_NIF_TERM Login_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Parse length of list from Erlang
    unsigned int len;
    if (!enif_get_list_length(env, argv[0], &len)) 
    {
        return enif_make_badarg(env);        
    }     

    ERL_NIF_TERM list = argv[0];
    ERL_NIF_TERM head, tail;
    int arity;
    const ERL_NIF_TERM *tuple;

    char value[1024];
    (void)memset(&value, '\0', sizeof(value));
    char key[1024];
    (void)memset(&key, '\0', sizeof(key));

    map<string, string> Options;
    
    // Iterate cell x cell of our list
    while(enif_get_list_cell(env, list, &head, &tail)) {

        // our list contains tuples of 2 elements each one, then parse
        if(!enif_get_tuple(env, head, &arity, &tuple))
            return enif_make_badarg(env);

        // if tuples elements are more than 2 throw an error
        if(arity!=2) 
            return enif_make_badarg(env);

        // get atom from 1st position in the tuple
        if(!enif_get_atom(env, tuple[0], key, sizeof(key), ERL_NIF_LATIN1))
            return enif_make_badarg(env);

        // get string from 2nd position in the tuple
        if(!enif_get_string(env, tuple[1], value, sizeof(value), ERL_NIF_LATIN1)) 
            return enif_make_badarg(env);

        string str_key(key);
        string str_value(value);        
        Options.insert(std::pair<string,string>(str_key,str_value)); 
        
        list = tail;
    }

    bool is_loggued = logging_in(Options);

    if (is_loggued) {
        return enif_make_atom(env, "true"); 
    } else {
       return enif_make_atom(env, "false");
    }
}

static ErlNifFunc nif_funcs[] = {
    {"login", 1, Login_nif}
};

ERL_NIF_INIT(erlrets_driver_nif, nif_funcs, NULL, NULL, NULL, NULL)
