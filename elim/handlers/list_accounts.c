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
#include "list_accounts.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_list_accounts ( const char *name ,
                                  const char *id   ,
                                  SEXP_VALUE *args ,
                                  gpointer data    )
{
    xmlnode *rval  = xnode_new( "alist" );
    GList   *acl   = NULL;
    xmlnode *alist = NULL;
    GString *akey  = g_string_new( "..........." );

    elim_ping();

    for( acl = purple_accounts_get_all(); acl; acl = acl->next )
    {
        PurpleAccount *a = acl->data;
        if( !a ) continue;

        g_string_printf( akey, "%ld", (long)a );
        alist = xmlnode_new( "alist" );
        AL_STR ( alist, "account-name", purple_account_get_username   ( a ) );
        AL_STR ( alist, "im-protocol" , purple_account_get_protocol_id( a ) );
        AL_BOOL( alist, "connected"   , purple_account_is_connected   ( a ) );
        AL_PTR ( alist, "account-uid" , a      );
        AL_NODE( rval , akey->str     , alist  );
    }

    g_string_free( akey , TRUE );
    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
