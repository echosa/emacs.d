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
#include "set_icon.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_set_icon ( const char *name , 
                             const char *id   ,
                             SEXP_VALUE *args , 
                             gpointer    data )
{
    ASSERT_ALISTP( args, id, name );

    elim_ping();
    
    const char *aname = ALIST_VAL_STR ( args, "account-name" );
    const char *proto = ALIST_VAL_STR ( args, "im-protocol"  );
    gpointer    auid  = ALIST_VAL_PTR ( args, "account-uid"  );
    const char *file  = ALIST_VAL_STR ( args, "icon-file"    );
    GString    *img   = ALIST_VAL_DATA( args, "icon-data"    );
    gchar      *bytes = NULL;
    gsize       len   = 0;
    gpointer    set   = NULL;
    PurpleAccount *acct =
      auid ? find_acct_by_uid( auid ) : purple_accounts_find( aname, proto );

    if( !acct )
    {
        sexp_val_free( args );
        return response_error( ENXIO, id, name, "unknown account" );
    }

    if( !img && file && *file )
    {
        g_file_get_contents( file, &bytes, &len, NULL );
    }
    else if( img )
    {
        bytes = g_memdup( img->str, img->len );
        len   = img->len;
    }

    // the imgstore owns `bytes' after this, don't free it:
    set = purple_buddy_icons_set_account_icon( acct, (guchar *)bytes, len );

    xmlnode *rval = xnode_new( "alist" );
    AL_PTR ( rval, "account-uid" , acct );
    AL_STR ( rval, "account-name", purple_account_get_username   ( acct ) );
    AL_STR ( rval, "im-protocol" , purple_account_get_protocol_id( acct ) );
    AL_BOOL( rval, "has-icon"    , set ? TRUE : FALSE );

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
