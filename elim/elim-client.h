/*
Copyright © 2009,2010 Vivek Dasmohapatra

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
#ifndef _IM_CLIENT_H_
#define _IM_CLIENT_H_

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <purple.h>

#include "elim-client-queue.h"
#include "elim-glibcompat.h"
#include "elim-func-handlers.h"
#include "xnode/xnode.h"
#include "sexp/sexp-xml.h"
#include "sexp/sexp-util.h"

typedef enum _xmlrpc_type
{
    XRPC_UNKNOWN ,
    XRPC_INTEGER ,
    XRPC_BOOLEAN ,
    XRPC_FLOAT   ,
    XRPC_STRING  ,
    XRPC_BASE64  ,
    XRPC_ALIST   ,
    XRPC_LIST    ,
    XRPC_DATE    ,
    XRPC_NIL         
} xmlrpc_type;

#endif
