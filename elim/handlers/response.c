/*
Copyright © 2009-2011 Vivek Dasmohapatra 

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
#include "response.h"
#include "../elim-rpc.h"

xmlnode * _h_elim_response ( const char *name , 
                             const char *id   , 
                             SEXP_VALUE *args , 
                             gpointer    data )
{
    // this action frees the key in the hash, but not the value:
    // the callback handler function we extract here is in charge of freeing
    // the data pointer if this is required;
    // we are NOT in charge of freeing the callback data pointer cbh:
    // that is done by the close_request callback in request_ui_ops.c
    // the callback handler is also in charge of freeing the SEXP args:
    CB_HANDLER *cbh = fetch_cb_data( id );
    if( cbh )
    {
        CB_FUNC  func   = cbh->func;
        xmlnode *rval   = NULL;
        if( func ) rval = func( cbh, args );
        else       sexp_val_free( args );

        // BAD: do not do this here!
        // g_free( cbh );

        return rval;
    }
    else { sexp_val_free( args ); }

    return NULL;
}
