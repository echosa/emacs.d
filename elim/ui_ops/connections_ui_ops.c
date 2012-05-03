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
#include "connections_ui_ops.h"

static void _elim_connect_progress        ( PurpleConnection *gc ,
                                            const char     *text ,
                                            size_t          step ,
                                            size_t    step_count );
static void _elim_connected               ( PurpleConnection *gc );
static void _elim_disconnected            ( PurpleConnection *gc );
static void _elim_notice                  ( PurpleConnection *gc , 
                                            const char     *text );
static void _elim_network_connected       ( void );
static void _elim_network_disconnected    ( void );
static void _elim_report_disconnect_reason( PurpleConnection     *gc     ,
                                            PurpleConnectionError reason ,
                                            const char           *text   );

PurpleConnectionUiOps elim_connections_ui_ops = 
{
    _elim_connect_progress        ,
    _elim_connected               ,
    _elim_disconnected            ,
    _elim_notice                  ,
    _elim_notice                  ,
    _elim_network_connected       ,
    _elim_network_disconnected    ,
    _elim_report_disconnect_reason,
    NULL ,
    NULL ,
    NULL
};

static void _elim_notice ( PurpleConnection *conn, const char *msg )
{
    PurpleAccount *acct = purple_connection_get_account( conn );
    if( acct )
    {
        char       *ID    = new_elim_id();
        xmlnode    *alist = xnode_new( "alist" );
        xmlnode    *mcall = func_call( "elim-connection-state", ID, alist );
        const char *aname = purple_account_get_username   ( acct );
        const char *proto = purple_account_get_protocol_id( acct );
        int         state = purple_connection_get_state   ( conn );
        g_free( ID );

        AL_PTR ( alist, "account-uid" , acct  );
        AL_STR ( alist, "account-name", aname );
        AL_STR ( alist, "im-protocol" , proto );
        AL_STR ( alist, "message"     , msg   );
        AL_ENUM( alist, "state"       , state , ":connection-state" );

        add_outbound_sexp( mcall );
    }
}

static void _elim_connect_progress        ( PurpleConnection *gc ,
                                            const char     *text ,
                                            size_t          step ,
                                            size_t    step_count )
{
    PurpleAccount *acct = purple_connection_get_account( gc );
    fprintf( stderr, "(_elim_connect_progress)\n" );

    if( acct )
    {
        char       *ID    = new_elim_id();
        xmlnode    *alist = xnode_new( "alist" );
        xmlnode    *mcall = func_call( "elim-connection-progress", ID, alist );
        const char *aname = purple_account_get_username   ( acct );
        const char *proto = purple_account_get_protocol_id( acct );
        int         state = purple_connection_get_state   ( gc   );
        g_free( ID );

        AL_PTR ( alist, "account-uid" , acct       );
        AL_STR ( alist, "account-name", aname      );
        AL_STR ( alist, "im-protocol" , proto      );
        AL_INT ( alist, "step"        , step       );
        AL_INT ( alist, "step-count"  , step_count );        
        AL_STR ( alist, "message"     , text       );
        AL_ENUM( alist, "state"       , state      , ":connection-state" );

        add_outbound_sexp( mcall );
    }

}

static void _elim_connected ( PurpleConnection *gc )
{
    fprintf( stderr, "(_elim_connected)\n" );
    _elim_notice( gc, "connected" );
}

static void _elim_disconnected ( PurpleConnection *gc )
{
    fprintf( stderr, "(_elim_disconnected)\n" );
    _elim_notice( gc, "disconnected" );
}

static void _elim_network    ( const char *call )
{
    char       *ID    = new_elim_id();
    xmlnode    *mcall = func_call( call , ID, NULL );
    fprintf( stderr, "(_elim_network)\n" );

    g_free( ID );
    add_outbound_sexp( mcall );
}

static void _elim_network_connected   () { _elim_network("elim-network-up"  ); }
static void _elim_network_disconnected() { _elim_network("elim-network-down"); }

static void _elim_report_disconnect_reason( PurpleConnection     *conn   ,
                                            PurpleConnectionError reason ,
                                            const char           *text   )
{
    PurpleAccount *acct = purple_connection_get_account( conn );
    if( acct )
    {
        char       *ID    = new_elim_id();
        xmlnode    *alist = xnode_new( "alist" );
        fprintf( stderr, "(_elim_report_disconnect_reason)\n" );

        xmlnode    *mcall = func_call( "elim-disconnect-reason", ID, alist );
        const char *aname = purple_account_get_username   ( acct );
        const char *proto = purple_account_get_protocol_id( acct );
        int         state = purple_connection_get_state   ( conn );
        g_free( ID );

        AL_PTR ( alist, "account-uid" , acct  );
        AL_STR ( alist, "account-name", aname );
        AL_STR ( alist, "im-protocol" , proto );
        AL_STR ( alist, "message"     , text  );
        AL_ENUM( alist, "reason-code" , reason, ":connection-error" );
        AL_ENUM( alist, "state"       , state , ":connection-state" );

        add_outbound_sexp( mcall );
    }
}
