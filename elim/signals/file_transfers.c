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
#include "file_transfers.h"

#define PXG( what, xfer ) purple_xfer_get_    ## what( xfer )
#define PAG( what, acct ) purple_account_get_ ## what( acct )

#define XSIG( instance, io, ev ) \
     purple_signal_connect( instance                         , \
                            "file-" #io "-" #ev              , \
                            &handle                          , \
                            PURPLE_CALLBACK(_elim_signal_ft) , \
                            "" #io "." #ev                   );
static int  handle;
static long xfer_id = 0;

static void _elim_signal_ft        ( PurpleXfer *x , gpointer ignored );

static void _elim_ft_ui_op_create  ( PurpleXfer *x );
static void _elim_ft_ui_op_destroy ( PurpleXfer *x );
static void _elim_ft_ui_op_add     ( PurpleXfer *x );
static void _elim_ft_ui_op_cancel  ( PurpleXfer *x );
static void _elim_ft_ui_op_update  ( PurpleXfer *x , double p );

PurpleXferUiOps _elim_xfer_ui_ops =
{
    _elim_ft_ui_op_create  ,
    _elim_ft_ui_op_destroy ,
    _elim_ft_ui_op_add     ,
    _elim_ft_ui_op_update  ,
    _elim_ft_ui_op_cancel  ,
    _elim_ft_ui_op_cancel  ,
    NULL, NULL, NULL, NULL
};

void elim_ft_signals_init (void)
{
    gpointer instance = purple_xfers_get_handle();
    XSIG( instance, recv, accept   );
    XSIG( instance, recv, cancel   );
    XSIG( instance, recv, complete );
    XSIG( instance, recv, start    );
    XSIG( instance, send, accept   );
    XSIG( instance, send, cancel   );
    XSIG( instance, send, complete );
    XSIG( instance, send, start    );
    purple_xfers_set_ui_ops( &_elim_xfer_ui_ops );
}

// these four can remain stubroutines for now, I think: we maintain no
// ui data structures since that all happens in the client, which is 
// in a different process to us:
static void _elim_ft_ui_op_create  ( PurpleXfer *x )
{
    x->ui_data = (gpointer)(++xfer_id);
    return;
}
static void _elim_ft_ui_op_destroy( PurpleXfer *x ){ _elim_signal_ft(x, NULL); }
static void _elim_ft_ui_op_add    ( PurpleXfer *x ){ _elim_signal_ft(x, NULL); }
static void _elim_ft_ui_op_cancel ( PurpleXfer *x ){ _elim_signal_ft(x, NULL); }

// here we should prod the client with some update info:
static void _elim_ft_ui_op_update  ( PurpleXfer *x , double p )
{
    xmlnode *alist = xnode_new( "alist" );
    char    *ID    = new_elim_id();
    
    AL_PTR( alist, "xfer-uid"    , x->ui_data );
    AL_NUM( alist, "xfer-percent", p * 100    );
    
    xmlnode *mcall = func_call( "elim-file-transfer-percent", ID, alist );
    g_free( ID );
    add_outbound_sexp( mcall );    
}

static void _elim_signal_ft (PurpleXfer *x, gpointer thing )
{
    xmlnode       *alist = xnode_new( "alist" );
    char          *ID    = new_elim_id();
    PurpleAccount *acct  = PXG( account, x );
    const char    *what  = (const char *)thing;

    AL_PTR ( alist, "xfer-uid"        , x->ui_data );
    AL_STR ( alist, "event"           , what       );
    AL_PTR ( alist, "account-uid"     , acct       );
    AL_STR ( alist, "account-name"    , PAG( username   , acct ) );
    AL_STR ( alist, "im-protocol"     , PAG( protocol_id, acct ) );
    AL_ENUM( alist, "xfer-type"       , PXG( type  , x ), ":xfer-type"        );
    AL_ENUM( alist, "xfer-status"     , PXG( status, x ), ":xfer-status-type" );
    AL_INT ( alist, "xfer-size"       , PXG( size            , x ) );
    AL_INT ( alist, "xfer-start"      , PXG( start_time      , x ) );
    AL_INT ( alist, "xfer-bytes-left" , PXG( bytes_remaining , x ) );
    AL_INT ( alist, "xfer-bytes-sent" , PXG( bytes_sent      , x ) );
    AL_INT ( alist, "xfer-end"        , PXG( end_time        , x ) );
    AL_INT ( alist, "xfer-local-port" , PXG( local_port      , x ) );
    AL_STR ( alist, "xfer-remote-ip"  , PXG( remote_ip       , x ) );
    AL_INT ( alist, "xfer-remote-port", PXG( remote_port     , x ) );
    AL_STR ( alist, "xfer-remote-user", PXG( remote_user     , x ) );
    AL_STR ( alist, "xfer-filename"   , PXG( filename        , x ) );
    AL_STR ( alist, "xfer-local-file" , PXG( local_filename  , x ) );
    AL_NUM ( alist, "xfer-progress"   , PXG( progress , x ) * 100  );

    xmlnode *mcall = func_call( "elim-file-transfer-status", ID, alist );
    g_free( ID );
    add_outbound_sexp( mcall );
}
