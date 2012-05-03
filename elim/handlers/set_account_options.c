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
#include "set_account_options.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

xmlnode * _h_elim_set_account_options ( const char *name , 
                                        const char *id   ,
                                        SEXP_VALUE *args , 
                                        gpointer    data )
{
    PurplePlugin  *prpl = NULL;
    PurpleAccount *acct = NULL;
    ASSERT_ALISTP( args, id, name );

    const char *proto  = ALIST_VAL_STR( args, "im-protocol"  );
    char       *aname  = ALIST_VAL_STR( args, "account-name" );
    gpointer    auid   = ALIST_VAL_PTR( args, "account-uid"  );
    SEXP_VALUE *fields = ALIST_VAL    ( args, "fields"       );

    elim_ping();

    if     ( auid           ) acct = find_acct_by_uid( auid ); 
    else if( aname && proto ) acct = purple_accounts_find( aname, proto );

    if( acct ) proto = purple_account_get_protocol_id( acct );

    if( !acct )
    {
        sexp_val_free( args );
        return response_error( ENXIO, id, name, "invalid account" );
    }

    prpl = find_plugin_by_protocol( proto );

    GList      *opts  = NULL;
    xmlnode    *rval  = xnode_new( "alist" );
    xmlnode    *items = xnode_new( "alist" );
    const char *pname = purple_plugin_get_name( prpl );
    PurplePluginProtocolInfo *info = PURPLE_PLUGIN_PROTOCOL_INFO( prpl );

    const char *pass  = ALIST_VAL_STR( args, "password"      );
    const char *alias = ALIST_VAL_STR( args, "account-alias" );

    if( pass  ) purple_account_set_password( acct, pass  );
    if( alias ) purple_account_set_alias   ( acct, alias );

    if( ALIST_VAL( args, "save-password" ) ) 
    {
        gboolean save = ALIST_VAL_BOOL( args, "save-password" );
        purple_account_set_remember_password( acct, save );
    }

    for( opts = info->protocol_options; opts; opts = opts->next )
    {
        PurpleAccountOption *opt = opts->data;
        if( !opt ) continue;

        xmlnode       *option = NULL;
        PurplePrefType type   = purple_account_option_get_type   ( opt );
        const char    *key    = purple_account_option_get_setting( opt );
        const char    *str    = NULL;

#define PAG( type, def ) purple_account_get_ ## type ( acct, key, def )
#define ODV( o )         (o->default_value)

        switch( type )
        {
          case PURPLE_PREF_BOOLEAN:
            option = xnode_new( "alist" );
            if( fields && ALIST_VAL( fields , key ) )
                purple_account_set_bool(acct, key, ALIST_VAL_BOOL(fields,key));
            AL_BOOL( option, "value", PAG(bool, ODV(opt).boolean) );
            break;

          case PURPLE_PREF_INT:
            option = xnode_new( "alist" );
            if( fields && ALIST_VAL( fields, key ) )
                purple_account_set_int( acct, key, ALIST_VAL_INT(fields,key) );
            AL_INT( option, "value", PAG(int, ODV(opt).integer));
            break;

          case PURPLE_PREF_STRING:
            option = xnode_new( "alist" );
            if( fields && (str = ALIST_VAL_STR( fields, key )) )
                purple_account_set_string( acct, key, str );
            AL_STR( option, "value", PAG(string, ODV(opt).string ) );
            break;

          default:
            break;
        }

        if( option )
        {
            AL_ENUM( option, "type"  , type, ":pref-type" );
            AL_STR ( option, "label" , opt->text    );
            AL_BOOL( option, "masked", opt->masked  );
            AL_NODE( items ,  key    , option );
        }
    }

    AL_STR ( rval, "protocol"    , pname );
    AL_PTR ( rval, "account-uid" , acct  );
    AL_STR ( rval, "im-protocol" , purple_account_get_protocol_id(acct) );
    AL_STR ( rval, "account-name", purple_account_get_username   (acct) );
    AL_NODE( rval, "fields"      , items );

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
