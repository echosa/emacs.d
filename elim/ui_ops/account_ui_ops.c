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
#include "account_ui_ops.h"

// truly godawful type name. let's not type it any more than we have to:
#define PARA_CB PurpleAccountRequestAuthorizationCb

static void _elim_notify_added ( PurpleAccount *account    ,
                                 const char    *remote_user,
                                 const char    *id         ,
                                 const char    *alias      ,
                                 const char    *message    );

static void _elim_status_changed ( PurpleAccount *account ,
                                   PurpleStatus *status  );

static void _elim_request_add ( PurpleAccount *account ,
                                const char *remote_user,
                                const char *id         ,
                                const char *alias      ,
                                const char *message    );

static void *_elim_request_authorise ( PurpleAccount *account      ,
                                       const char    *remote_user  ,
                                       const char    *id           ,
                                       const char    *alias        ,
                                       const char    *message      ,
                                       gboolean       on_list      ,
                                       PARA_CB        authorize_cb ,
                                       PARA_CB        deny_cb      ,
                                       void          *user_data    );

static void _elim_close_account_request ( void *ui_handle );


PurpleAccountUiOps elim_account_ui_ops =
{
    _elim_notify_added         ,
    _elim_status_changed       ,
    _elim_request_add          ,
    _elim_request_authorise    ,
    _elim_close_account_request,
    NULL ,
    NULL ,
    NULL ,
    NULL
};


static void _elim_notify_added ( PurpleAccount *account    ,
                                 const char    *remote_user,
                                 const char    *id         ,
                                 const char    *alias      ,
                                 const char    *message    )
{
    xmlnode *alist = xnode_new( "alist" );
    char    *ID    = new_elim_id();
    fprintf( stderr, "(_elim_notify_added)\n" );

    AL_STR( alist, "user"         , remote_user  );
    AL_STR( alist, "alias"        , alias        );
    AL_STR( alist, "message"      , message      );
    AL_PTR( alist, "account-uid"  , account      );
    AL_STR( alist, "account-name" , purple_account_get_username   ( account ) );
    AL_STR( alist, "im-protocol"  , purple_account_get_protocol_id( account ) );
    xmlnode *mcall = func_call( "elim-account-notify-added", ID, alist );
    g_free( ID );
    add_outbound_sexp( mcall );
}

static void _elim_status_changed ( PurpleAccount *account ,
                                   PurpleStatus  *status  )
{
    xmlnode              *alist = xnode_new( "alist" );
    char                 *ID    = new_elim_id();
    fprintf( stderr, "(_elim_status_changed)\n" );

    PurpleStatusType     *type  = purple_status_get_type( status );
    PurpleStatusPrimitive statp = purple_status_type_get_primitive( type ); 

    AL_PTR ( alist, "account-uid" , account );
    AL_STR ( alist, "account-name", purple_account_get_username   ( account ) );
    AL_STR ( alist, "im-protocol" , purple_account_get_protocol_id( account ) );
    AL_STR ( alist, "status-name" , purple_status_get_name        ( status  ) );
    AL_ENUM( alist, "status-type" , statp, ":status-primitive" );
    AL_BOOL( alist, "connected"   , purple_account_is_connected   ( account ) );

    xmlnode *mcall = func_call( "elim-account-status-changed", ID, alist );

    g_free( ID );
    add_outbound_sexp( mcall );
}

static void _elim_request_add ( PurpleAccount *account ,
                                const char *remote_user,
                                const char *id         ,
                                const char *alias      ,
                                const char *message    )
{
    xmlnode *alist = xnode_new( "alist" );
    char    *ID    = new_elim_id();
    fprintf( stderr, "(_elim_request_add)\n" );

    AL_STR ( alist, "user"        , remote_user  );
    AL_STR ( alist, "alias"       , alias        );
    AL_STR ( alist, "message"     , message      );
    AL_PTR ( alist, "account-uid" , account      );
    AL_STR ( alist, "account-name", purple_account_get_username   ( account ) );
    AL_STR ( alist, "im-protocol" , purple_account_get_protocol_id( account ) );
    xmlnode *mcall = func_call( "elim-account-request-add", ID, alist );
    g_free( ID );
    add_outbound_sexp( mcall );
}

typedef struct _AUI_RESP AUI_RESP;
struct _AUI_RESP
{
    char     *id   ;
    gpointer  data ;
    PARA_CB   ok   ;
    PARA_CB   nok  ;
};

static xmlnode * _elim_request_authorise_cb( gpointer ptr, SEXP_VALUE *args )
{
    CB_HANDLER  *cbh = ptr;
    AUI_RESP *handle = cbh->data;
    if( handle && args && (args->type == SEXP_ALIST) )
    {
        gpointer data   = handle->data;
        int      status = ALIST_VAL_INT( args, "status" );
        if( status == 0 )
        {
            gboolean ok = ALIST_VAL_BOOL( args, "value" );
            ( ok ? handle->ok : handle->nok )( data );
        }
    }

    purple_account_request_close( cbh );
    if( args ) sexp_val_free( args );

    return NULL;
}

static void *_elim_request_authorise ( PurpleAccount *account      ,
                                       const char    *remote_user  ,
                                       const char    *id           ,
                                       const char    *alias        ,
                                       const char    *message      ,
                                       gboolean       on_list      ,
                                       PARA_CB        authorize_cb ,
                                       PARA_CB        deny_cb      ,
                                       void          *user_data    )
{
    CB_HANDLER *cbh   = g_new0( CB_HANDLER, 1 );
    AUI_RESP   *resp  = g_new0( AUI_RESP  , 1 );
    xmlnode    *alist = xnode_new( "alist" );
    char       *ID    = new_elim_id();
    fprintf( stderr, "(_elim_request_authorise)\n" );

    AL_STR ( alist, "user"        , remote_user  );
    AL_STR ( alist, "id"          , id           );
    AL_STR ( alist, "alias"       , alias        );
    AL_BOOL( alist, "on-list"     , on_list      );
    AL_STR ( alist, "message"     , message      );
    AL_PTR ( alist, "account-uid" , account      );
    AL_STR ( alist, "account-name", purple_account_get_username   ( account ) );
    AL_STR ( alist, "im-protocol" , purple_account_get_protocol_id( account ) );
    resp->ok   = authorize_cb;
    resp->nok  = deny_cb;
    resp->id   = ID;
    resp->data = user_data;
    cbh ->func = _elim_request_authorise_cb;
    cbh ->type = CB_TYPE_GENERIC;
    cbh ->data = resp;
    store_cb_data( ID, cbh );
    xmlnode *mcall = func_call( "elim-account-request-auth", ID, alist );
    add_outbound_sexp( mcall );
    return cbh;
}

static void _elim_close_account_request ( gpointer ui_handle )
{
    CB_HANDLER *cbh  = ui_handle;
    AUI_RESP   *resp = cbh->data;
    fprintf( stderr, "(_elim_close_account_request)\n" );

    if( !fetch_cb_data( resp->id ) ) // side effect: frees the key data
        g_free( resp->id );
    g_free( resp );
    g_free( cbh  );
}
