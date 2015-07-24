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

#include <stdio.h>
#include <stdlib.h>
#include "erl_rets.h" // Our Lib
#include "erl_driver.h"
#include "ei.h"
#include "erl_interface.h"
#include <string.h>

/*
 * Start the driver
*/
static ErlDrvData erl_rets_driver_start(ErlDrvPort port, char *buff)
{
    stct_port* to_erl_port = (stct_port*)driver_alloc(sizeof(port));
    to_erl_port->port = port;
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    return (ErlDrvData)to_erl_port;
}

/*
 * Stop the driver
*/
static void erl_rets_driver_stop(ErlDrvData handle)
{
    driver_free((char*)handle);
}

/*
 * Control the data in/out for the driver
 *
*/
ErlDrvSSizeT erl_rets_driver_control(ErlDrvData drv_data, unsigned int command, char *buf, ErlDrvSizeT len, char **rbuf, 
			ErlDrvSizeT rlen) {
    int r;
    /* Creates a new buffer to store result */
    ei_x_buff x;

    /* Decodes an incoming query */
    char* s = decode_query(buf, len);

    /* Versioning buffer */
    ei_x_new_with_version(&x);

    /* At this moment the command dictates how treats the query */
    switch (command) {
        case 1:  
		r = encode_ok_result(&x);
		break;
        default:
        	r = -1;	
		break;
    }

    *rbuf = (char*)new_binary(&x);
    ei_x_free(&x);
    driver_free(s);
    return r;
}

/*
 * Driver Entry
*/
ErlDrvEntry erl_rets_driver_entry = {
        NULL,                           /* F_PTR init, N/A */
        erl_rets_driver_start,            /* L_PTR start, called when port is opened */
        erl_rets_driver_stop,             /* F_PTR stop, called when port is closed */
        NULL,           /* F_PTR output, called when erlang has sent */
        NULL,                           /* F_PTR ready_input, called when input descriptor ready */
        NULL,                           /* F_PTR ready_output, called when output descriptor ready */
        "erl_rets",   	                /* char *driver_name, the argument to open_port */
        NULL,                           /* F_PTR finish, called when unloaded */
        NULL,
        erl_rets_driver_control,                           /* F_PTR control, port_command callback */
        NULL,                           /* F_PTR timeout, reserved */
        NULL,                           /* F_PTR outputv, reserved*/
        NULL,                           /* F_PTR ready_async, only for async drivers */
        NULL,                           /* F_PTR flush, called when port is about
                                           to be closed, but there is data in driver
                                           queue */
        NULL,                           /* F_PTR call, much like control, sync call
                                           to driver */
        NULL,                           /* F_PTR event, called when an event selected
                                           by driver_event() occurs. */
        ERL_DRV_EXTENDED_MARKER,        /* int extended marker, Should always be
                                           set to indicate driver versioning */
        ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be
                                           set to this value */
        ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be
                                           set to this value */
        0,                              /* int driver_flags, see documentation */
        NULL,                           /* void *handle2, reserved for VM use */
        NULL,                           /* F_PTR process_exit, called when a
                                           monitored process dies */
        NULL                            /* F_PTR stop_select, called to close an
                                           event object */
};

/*
 * Driver INITIALIZATION
*/
DRIVER_INIT(erl_rets) /* must match name in driver_entry */
{
    return &erl_rets_driver_entry;
}
