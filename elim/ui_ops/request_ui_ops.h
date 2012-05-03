/*
Copyright © 2009, 2010 Vivek Dasmohapatra 

email : vivek@etla.org
irc   : fledermaus on freenode, oftc
jabber: fledermaus@jabber.earth.li

This file is part of elim.

elim is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

elim is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with elim.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef _ELIM_REQUEST_UI_OPS_H_
#define _ELIM_REQUEST_UI_OPS_H_

#include <purple.h>
#include "../elim-rpc.h"
#include "../elim-client-queue.h"

extern PurpleRequestUiOps elim_request_ui_ops;

#define VA_LIST_TO_ALIST( alist, a_type, c_ktype, c_vtype, va ) \
    if( va )                                                    \
    {                                                           \
        c_ktype key = NULL;                                     \
        while( (key = va_arg( va, c_ktype )) )                  \
        {                                                       \
            c_vtype val = va_arg( va, c_vtype );                \
            AL_ ## a_type( alist, key, val );                   \
        }                                                       \
    }

#endif
