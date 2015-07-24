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

/*
** Include this file for erlang struct definitions, used for define
** the struct ErlDrvPort. To use it, add the include path to compiler.
*/
#include "erl_driver.h"
#include <string.h>
#include "ei.h"

/*
** Struct for erlang port, send/receive data from/to erlang
*/
typedef struct {
    ErlDrvPort port;
} stct_port;

/* Protos */
static char* decode_query(const char* buf, int len);
static int encode_ok_result(ei_x_buff* x);

/*
 * Encode a new binary to send data back to Erlang driver.
 *
 */
static ErlDrvBinary* new_binary(ei_x_buff* x)
{
    ErlDrvBinary* bin = driver_alloc_binary(x->index);
    if (bin != NULL)
        memcpy(&bin->orig_bytes[0], x->buff, x->index);
    return bin;
}

/*
 * Decode query 
 *
 */
static char* decode_query(const char* buf, int len)
{
    char* result;
    if (len < 1 || len > 10000) return NULL;

    /* Alloc len+1 as Erlang Docs says! */
    result = driver_alloc(len+1);

    memcpy(result, buf, len);
    result[len] = '\0';
    return result;
}

/*
 * Encode an ok result as an atom  
 */
static int encode_ok_result(ei_x_buff* x)
{
    const char* ok = "ok";
    ei_x_encode_atom(x, ok);
    return 0;
}

