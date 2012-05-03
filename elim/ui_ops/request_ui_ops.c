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
#include "request_ui_ops.h"

#define PRF_SET( p, t, v ) purple_request_field_ ## t ## _set_value( (p), (v) )

static void *_elim_request_input ( const char         *title         ,
                                   const char         *primary       ,
                                   const char         *secondary     ,
                                   const char         *default_value ,
                                   gboolean            multiline     ,
                                   gboolean            masked        ,
                                   gchar              *hint          ,
                                   const char         *ok_text       ,
                                   GCallback           ok_cb         ,
                                   const char         *cancel_text   ,
                                   GCallback           cancel_cb     ,
                                   PurpleAccount      *account       ,
                                   const char         *who           ,
                                   PurpleConversation *conv          ,
                                   void               *user_data     );

static void *_elim_request_choice( const char         *title         ,
                                   const char         *primary       ,
                                   const char         *secondary     ,
                                   int                 default_value ,
                                   const char         *ok_text       ,
                                   GCallback           ok_cb         ,
                                   const char         *cancel_text   ,
                                   GCallback           cancel_cb     ,
                                   PurpleAccount      *account       ,
                                   const char         *who           ,
                                   PurpleConversation *conv          ,
                                   void               *user_data     ,
                                   va_list             choices       );

static void *_elim_request_action( const char          *title        ,
                                   const char          *primary      ,
                                   const char          *secondary    ,
                                   int                  default_act  ,
                                   PurpleAccount       *account      ,
                                   const char          *who          ,
                                   PurpleConversation  *conv         ,
                                   void                *user_data    ,
                                   size_t               action_count ,
                                   va_list              actions      );

static void *_elim_request_fields( const char          *title        ,
                                   const char          *primary      ,
                                   const char          *secondary    ,
                                   PurpleRequestFields *fields       ,
                                   const char          *ok_text      ,
                                   GCallback            ok_cb        ,
                                   const char          *cancel_text  ,
                                   GCallback            cancel_cb    ,
                                   PurpleAccount       *account      ,
                                   const char          *who          ,
                                   PurpleConversation  *conv         ,
                                   void                *user_data    );

static void *_elim_request_file  ( const char            *title      ,
                                   const char            *filename   ,
                                   gboolean               savedialog ,
                                   GCallback              ok_cb      ,
                                   GCallback              cancel_cb  ,
                                   PurpleAccount         *account    ,
                                   const char            *who        ,
                                   PurpleConversation    *conv       ,
                                   void                  *user_data  );

static void _elim_close_request  ( PurpleRequestType type, void *ui_handle );

static void *_elim_request_folder( const char            *title      ,
                                   const char            *dirname    ,
                                   GCallback              ok_cb      ,
                                   GCallback              cancel_cb  ,
                                   PurpleAccount         *account    ,
                                   const char            *who        ,
                                   PurpleConversation    *conv       ,
                                   void                  *user_data  );

PurpleRequestUiOps elim_request_ui_ops =
{
    _elim_request_input  ,
    _elim_request_choice ,
    _elim_request_action ,
    _elim_request_fields ,
    _elim_request_file   ,
    _elim_close_request  ,
    _elim_request_folder ,
    NULL ,
    NULL ,
    NULL ,
    NULL
};

typedef void (*action_func) (gpointer data);
typedef struct _REQ_RESP REQ_RESP;
struct _REQ_RESP
{
    char             *id   ;
    gpointer          data ;
    PurpleRequestType type ;
    union
    {
        struct { PurpleRequestInputCb  ok; PurpleRequestInputCb  nok; } input ;
        struct { PurpleRequestChoiceCb ok; PurpleRequestChoiceCb nok; } choice;
        struct { action_func        *func; int                 count; } action;
        struct { PurpleRequestFileCb   ok; PurpleRequestFileCb   nok; } path  ;
        struct { PurpleRequestFields  *fields;
                 PurpleRequestFieldsCb ok    ;
                 PurpleRequestFieldsCb nok   ; } fields;
    } req;
};

static xmlnode * _elim_request_input_cb ( gpointer ptr, SEXP_VALUE *args )
{
    fprintf( stderr, "(_elim_request_input_cb)\n" );

    CB_HANDLER  *cbh = ptr;
    REQ_RESP *handle = cbh->data;
    if( handle )
    {
        gpointer data = handle->data;

        if( args && (args->type == SEXP_ALIST) )
        {
            PurpleRequestInputCb cb = NULL;
            int status = ALIST_VAL_INT( args, "status" );
            if( status == 0 )
            {
                char *input = ALIST_VAL_STR( args, "value" );
                cb = ( input ? handle->req.input.ok : handle->req.input.nok );
                if( cb ) (cb)( data, input );
            }
            else
            {
                cb = (handle->req.input.nok);
                if( cb ) (cb)( data, "" );
            }
        }
        else { handle->req.input.nok( data, "" ); }
    }

    purple_request_close( handle->type, cbh );
    if( args   ) sexp_val_free( args );

    return NULL;
}

static void *_elim_request_input ( const char         *title         ,
                                   const char         *primary       ,
                                   const char         *secondary     ,
                                   const char         *default_value ,
                                   gboolean            multiline     ,
                                   gboolean            masked        ,
                                   gchar              *hint          ,
                                   const char         *ok_text       ,
                                   GCallback           ok_cb         ,
                                   const char         *cancel_text   ,
                                   GCallback           cancel_cb     ,
                                   PurpleAccount      *account       ,
                                   const char         *who           ,
                                   PurpleConversation *conv          ,
                                   void               *user_data     )
{
    fprintf( stderr, "(_elim_request_input)\n" );

    CB_HANDLER *cbh   = g_new0( CB_HANDLER, 1 );
    REQ_RESP   *resp  = g_new0( REQ_RESP  , 1 );
    xmlnode    *alist = xnode_new( "alist" );
    char       *ID    = new_elim_id();

    AL_STR ( alist, "title"     , title         );
    AL_STR ( alist, "primary"   , primary       );
    AL_STR ( alist, "secondary" , secondary     );
    AL_STR ( alist, "default"   , default_value );
    AL_STR ( alist, "hint"      , hint          );
    AL_STR ( alist, "ok-label"  , ok_text       );
    AL_STR ( alist, "nok-label" , cancel_text   );
    AL_BOOL( alist, "multi-line", multiline     );
    AL_BOOL( alist, "secret"    , masked        );

    if( account )
    {
        const char *aname = purple_account_get_username   ( account );
        const char *proto = purple_account_get_protocol_id( account );
        AL_PTR ( alist, "account-uid" , account );
        AL_STR ( alist, "account-name", aname   );
        AL_STR ( alist, "im-protocol" , proto   );
    }

    if( conv )
    {
        const char            *title = purple_conversation_get_title   ( conv );
        const char            *cname = purple_conversation_get_name    ( conv );
        PurpleConnectionFlags  cflag = purple_conversation_get_features( conv );
        PurpleConversationType ctype = purple_conversation_get_type    ( conv );

        AL_PTR ( alist, "conv-uid"     , conv  );
        AL_STR ( alist, "conv-name"    , cname );
        AL_STR ( alist, "conv-title"   , title );
        AL_ENUM( alist, "conv-type"    , ctype , ":conversation-type" );
        AL_ENUM( alist, "conv-features", cflag , ":connection-flags"  );
    }

    resp->req.input.ok   = (PurpleRequestInputCb)ok_cb;
    resp->req.input.nok  = (PurpleRequestInputCb)cancel_cb;

    resp->data = user_data;
    resp->type = PURPLE_REQUEST_INPUT;
    resp->id   = ID;
    cbh ->func = _elim_request_input_cb;
    cbh ->data = resp;

    store_cb_data( ID, cbh );
    xmlnode *mcall = func_call( "elim-request-input", ID, alist );
    add_outbound_sexp( mcall );
    fprintf(stderr, "(_elim_request_input HANDLE: %p . %p)", cbh, resp );
    return cbh;
}

static xmlnode * _elim_request_choice_cb ( gpointer ptr, SEXP_VALUE *args )
{
    fprintf( stderr, "(_elim_request_choice_cb)\n" );

    CB_HANDLER *cbh  = ptr;
    REQ_RESP *handle = cbh->data;
    if( handle )
    {
        gpointer data = handle->data;

        if( args && (args->type == SEXP_ALIST) )
        {
            PurpleRequestChoiceCb cb = NULL;
            int status = ALIST_VAL_INT( args, "status" );
            if( status == 0 )
            {
                int choice = ALIST_VAL_INT( args, "value" );
                cb =  ( (choice != -1) ?
                        handle->req.choice.ok : handle->req.choice.nok );
                if( cb ) (cb)( data, choice );
            }
            else
            {
                cb = handle->req.choice.nok;
                if( cb ) (cb)( data, 0 );
            }
        }
        else { handle->req.choice.nok( data, 0 ); }
    }

  //if( handle ) g_free( handle )
    purple_request_close( handle->type, cbh );
    if( args   ) sexp_val_free( args );

    return NULL;
}

static void *_elim_request_choice( const char         *title         ,
                                   const char         *primary       ,
                                   const char         *secondary     ,
                                   int                 default_value ,
                                   const char         *ok_text       ,
                                   GCallback           ok_cb         ,
                                   const char         *cancel_text   ,
                                   GCallback           cancel_cb     ,
                                   PurpleAccount      *account       ,
                                   const char         *who           ,
                                   PurpleConversation *conv          ,
                                   void               *user_data     ,
                                   va_list             choices       )
{
    fprintf( stderr, "(_elim_request_choice)\n" );

    CB_HANDLER *cbh    = g_new0( CB_HANDLER, 1 );
    REQ_RESP   *resp   = g_new0( REQ_RESP  , 1 );
    xmlnode    *alist  = xnode_new( "alist" );
    char       *ID     = new_elim_id();
    xmlnode    *choice = xnode_new( "alist" );

    VA_LIST_TO_ALIST( choice, INT, const char *, int, choices );

    AL_STR ( alist, "title"     , title         );
    AL_STR ( alist, "primary"   , primary       );
    AL_STR ( alist, "secondary" , secondary     );
    AL_STR ( alist, "ok-label"  , ok_text       );
    AL_STR ( alist, "nok-label" , cancel_text   );
    AL_STR ( alist, "who"       , who           );
    AL_INT ( alist, "default"   , default_value );
    AL_NODE( alist, "choices"   , choice        );

    if( account )
    {
        const char *aname = purple_account_get_username   ( account );
        const char *proto = purple_account_get_protocol_id( account );
        AL_PTR ( alist, "account-uid" , account );
        AL_STR ( alist, "account-name", aname   );
        AL_STR ( alist, "im-protocol" , proto   );
    }

    if( conv )
    {
        const char            *title = purple_conversation_get_title   ( conv );
        const char            *cname = purple_conversation_get_name    ( conv );
        PurpleConnectionFlags  cflag = purple_conversation_get_features( conv );
        PurpleConversationType ctype = purple_conversation_get_type    ( conv );

        AL_PTR ( alist, "conv-uid"     , conv  );
        AL_STR ( alist, "conv-name"    , cname );
        AL_STR ( alist, "conv-title"   , title );
        AL_ENUM( alist, "conv-type"    , ctype , ":conversation-type" );
        AL_ENUM( alist, "conv-features", cflag , ":connection-flags"  );
    }

    resp->req.choice.ok   = (PurpleRequestChoiceCb)ok_cb;
    resp->req.choice.nok  = (PurpleRequestChoiceCb)cancel_cb;
    resp->data = user_data;
    resp->type = PURPLE_REQUEST_CHOICE;
    resp->id   = ID;
    cbh ->func = _elim_request_choice_cb;
    cbh ->data = resp;
    store_cb_data( ID, cbh );
    xmlnode *mcall = func_call( "elim-request-choice", ID, alist );
    add_outbound_sexp( mcall );
    fprintf(stderr, "(_elim_request_choice HANDLE: %p . %p)", cbh, resp );
    return cbh;
}

static xmlnode * _elim_request_action_cb( gpointer ptr, SEXP_VALUE *args )
{
    fprintf( stderr, "(_elim_request_action_cb)\n" );

    CB_HANDLER  *cbh = ptr;
    REQ_RESP *handle = cbh->data;
    if( handle )
    {
        gpointer data = handle->data;

        if( args && (args->type == SEXP_ALIST) )
        {
            int status = ALIST_VAL_INT( args, "status" );
            int max    = handle->req.action.count;
            if( status == 0 )
            {
                int x = 0;
                gpointer     f = (gpointer)ALIST_VAL_INT( args, "value" );
                action_func *F = handle->req.action.func;
                fprintf( stderr, "( chosen action    : %p\n", f );
                for( x = 0; x < max; x++ )
                    if( f == *(F + x) )
                    {
                        fprintf( stderr, "     matched action: %p\n", *(F+x) );
                        (*(F + x))( data ); break;
                    }
                    else
                    {
                        fprintf( stderr, "   unmatched action: %p\n", *(F+x) );
                    }
            }
        }
    }

    purple_request_close( handle->type, cbh );
    if( args ) sexp_val_free( args );

    return NULL;
}

static void *_elim_request_action( const char          *title        ,
                                   const char          *primary      ,
                                   const char          *secondary    ,
                                   int                  default_act  ,
                                   PurpleAccount       *account      ,
                                   const char          *who          ,
                                   PurpleConversation  *conv         ,
                                   void                *user_data    ,
                                   size_t               action_count ,
                                   va_list              actions      )
{
    fprintf( stderr, "(_elim_request_action)\n" );

    CB_HANDLER *cbh   = g_new0( CB_HANDLER, 1 );
    REQ_RESP   *resp  = g_new0( REQ_RESP  , 1 );
    xmlnode    *alist = xnode_new( "alist" );
    char       *ID    = new_elim_id();
    xmlnode    *acts  = xnode_new( "alist" );
    resp->req.action.func  = g_new0( action_func, action_count );
    resp->req.action.count = action_count;

    if( actions )
    {
        int offs = 0;
        const char *label = NULL;
        while( ( offs  < action_count ) &&
               ( label = va_arg( actions, const char *) ) )
        {
            action_func func = va_arg( actions, action_func );
            AL_PTR( acts, label, func );
            fprintf( stderr, "storing action func: %s = %p\n", label, func );
            if( offs < action_count )
                *(resp->req.action.func + offs++) = func;
        }
    }

    AL_STR ( alist, "title"     , title       );
    AL_STR ( alist, "primary"   , primary     );
    AL_STR ( alist, "secondary" , secondary   );
    AL_STR ( alist, "who"       , who         );
    AL_INT ( alist, "default"   , default_act );
    AL_NODE( alist, "actions"   , acts        );

    if( account )
    {
        const char *aname = purple_account_get_username   ( account );
        const char *proto = purple_account_get_protocol_id( account );
        AL_PTR ( alist, "account-uid" , account );
        AL_STR ( alist, "account-name", aname   );
        AL_STR ( alist, "im-protocol" , proto   );
    }

    if( conv )
    {
        const char            *title = purple_conversation_get_title   ( conv );
        const char            *cname = purple_conversation_get_name    ( conv );
        PurpleConnectionFlags  cflag = purple_conversation_get_features( conv );
        PurpleConversationType ctype = purple_conversation_get_type    ( conv );

        AL_PTR ( alist, "conv-uid"     , conv  );
        AL_STR ( alist, "conv-name"    , cname );
        AL_STR ( alist, "conv-title"   , title );
        AL_ENUM( alist, "conv-type"    , ctype , ":conversation-type" );
        AL_ENUM( alist, "conv-features", cflag , ":connection-flags"  );
    }

    resp->data = user_data;
    resp->type = PURPLE_REQUEST_ACTION;
    resp->id   = ID;
    cbh ->func = _elim_request_action_cb;
    cbh ->data = resp;

    store_cb_data( ID, cbh );
    xmlnode *mcall = func_call( "elim-request-action", ID, alist );
    add_outbound_sexp( mcall );

    fprintf(stderr, "(_elim_request_action HANDLE: %p . %p)", cbh, resp );
    return cbh;
}

static void _elim_merge_request_fields( PurpleRequestFields *fields ,
                                        SEXP_VALUE          *value  )
{
    GList *gl; // group list (group of input widgets)

    for( gl = purple_request_fields_get_groups(fields); gl; gl = gl->next )
    {
        GList                   *fl    = NULL; // field list (within a group)
        PurpleRequestFieldGroup *group = gl->data;

        if( !group ) continue;

        fl = purple_request_field_group_get_fields( group );

        for( ; fl; fl = fl->next )
        {
            PurpleRequestField *prf    = (PurpleRequestField *)fl->data;
            const char         *fid    = purple_request_field_get_id   ( prf );
            PurpleRequestFieldType t   = purple_request_field_get_type ( prf );
            const char         *vstr   = NULL;

            switch( t )
            {
              case PURPLE_REQUEST_FIELD_STRING  :
                vstr = ALIST_VAL_STR( value, fid );
                PRF_SET( prf, string, vstr ? vstr : "" );
                break;
              case PURPLE_REQUEST_FIELD_INTEGER :
                PRF_SET( prf, int   , ALIST_VAL_INT (value, fid) );
                break;
              case PURPLE_REQUEST_FIELD_BOOLEAN :
                PRF_SET( prf, bool  , ALIST_VAL_BOOL(value, fid) );
                break;
              case PURPLE_REQUEST_FIELD_CHOICE  :
                PRF_SET( prf, choice, ALIST_VAL_INT (value, fid) );
                break;
              case PURPLE_REQUEST_FIELD_LABEL   : // noop
                break;
              case PURPLE_REQUEST_FIELD_LIST    :
              case PURPLE_REQUEST_FIELD_IMAGE   :
              case PURPLE_REQUEST_FIELD_ACCOUNT :
              case PURPLE_REQUEST_FIELD_NONE    :
              default:
                fprintf( stderr, "Unsupported field type merge: %d\n", t );
            }
        }
    }
}

static xmlnode * _elim_request_fields_cb ( gpointer ptr, SEXP_VALUE *args )
{
    fprintf( stderr, "(_elim_request_fields_cb)\n" );

    CB_HANDLER  *cbh = ptr;
    REQ_RESP *handle = cbh->data;
    if( handle )
    {
        PurpleRequestFields *F = handle->req.fields.fields;
        gpointer data = handle->data;

        if( args && (args->type == SEXP_ALIST) )
        {
            int         status = ALIST_VAL_INT  ( args, "status" );
            GHashTable *value  = ALIST_VAL_ALIST( args, "value"  );

            if( (status == 0) && value )
            {
                // copy the result data back:
                _elim_merge_request_fields( F, ALIST_VAL(args, "value") );
                if( handle->req.fields.ok )
                    handle->req.fields.ok( data, F );
            }
            else
            {
                if( handle->req.fields.nok )
                    handle->req.fields.nok( data, F );
            }
        }
        else
        {
            if( handle->req.fields.nok )
                handle->req.fields.nok( data, F );
        }
    }

    purple_request_close( handle->type, cbh );
    if( args   ) sexp_val_free( args );

    return NULL;
}

static void *_elim_request_fields( const char          *title        ,
                                   const char          *primary      ,
                                   const char          *secondary    ,
                                   PurpleRequestFields *fields       ,
                                   const char          *ok_text      ,
                                   GCallback            ok_cb        ,
                                   const char          *cancel_text  ,
                                   GCallback            cancel_cb    ,
                                   PurpleAccount       *account      ,
                                   const char          *who          ,
                                   PurpleConversation  *conv         ,
                                   void                *user_data    )
{
    fprintf( stderr, "(_elim_request_fields\n" );

    CB_HANDLER *cbh    = g_new0( CB_HANDLER, 1 );
    REQ_RESP   *resp   = g_new0( REQ_RESP  , 1 );
    xmlnode    *alist  = xnode_new( "alist" );
    xmlnode    *xflds  = xnode_new( "alist" );
    char       *ID     = new_elim_id();

    AL_STR ( alist, "title"     , title       );
    AL_STR ( alist, "primary"   , primary     );
    AL_STR ( alist, "secondary" , secondary   );
    AL_STR ( alist, "ok-label"  , ok_text     );
    AL_STR ( alist, "nok-label" , cancel_text );
    AL_STR ( alist, "who"       , who         );
    AL_NODE( alist, "fields"    , xflds       );

    if( account )
    {
        const char *aname = purple_account_get_username   ( account );
        const char *proto = purple_account_get_protocol_id( account );
        AL_PTR ( alist, "account-uid" , account );
        AL_STR ( alist, "account-name", aname   );
        AL_STR ( alist, "im-protocol" , proto   );
    }

    if( conv )
    {
        const char            *title = purple_conversation_get_title   ( conv );
        const char            *cname = purple_conversation_get_name    ( conv );
        PurpleConnectionFlags  cflag = purple_conversation_get_features( conv );
        PurpleConversationType ctype = purple_conversation_get_type    ( conv );

        AL_PTR ( alist, "conv-uid"     , conv  );
        AL_STR ( alist, "conv-name"    , cname );
        AL_STR ( alist, "conv-title"   , title );
        AL_ENUM( alist, "conv-type"    , ctype , ":conversation-type" );
        AL_ENUM( alist, "conv-features", cflag , ":connection-flags"  );
    }

    GList *gl; // group list (group of input widgets)

    for( gl = purple_request_fields_get_groups(fields); gl; gl = gl->next )
    {
        const char *gtitle             = NULL;
        GList                   *fl    = NULL; // field list (within a group)
        PurpleRequestFieldGroup *group = gl->data;
        xmlnode *prfg                  = xnode_new( "alist" );

        if( !group ) continue;

        fl     = purple_request_field_group_get_fields( group );
        gtitle = purple_request_field_group_get_title ( group );
        AL_NODE( xflds, gtitle ? gtitle : "Fields", prfg );

        for( ; fl; fl = fl->next )
        {
            PurpleRequestField *prf    = (PurpleRequestField *)fl->data;
            const char         *flabel = purple_request_field_get_label( prf );
            const char         *fid    = purple_request_field_get_id   ( prf );
            PurpleRequestFieldType t   = purple_request_field_get_type ( prf );
            xmlnode            *field  = xnode_new( "alist" );
            const char         *dlabel = "Unknown";
            gboolean       implemented = FALSE;

            AL_ENUM( field, "type" , t, ":request-field-type" );
            AL_BOOL( field, "required", purple_request_field_is_required(prf) );
            AL_NODE( prfg , fid, field );

            switch( t )
            {
              case PURPLE_REQUEST_FIELD_STRING  :
                AL_BOOL( field, "multiline", prf->u.string.multiline );
                AL_BOOL( field, "masked"   , prf->u.string.masked    );
                AL_BOOL( field, "editable" , prf->u.string.editable  );
                AL_STR ( field, "value"    , prf->u.string.value     );
                AL_STR ( field, "default"  , prf->u.string.default_value );
                implemented = TRUE;
                dlabel      = "Text";
                break;
              case PURPLE_REQUEST_FIELD_INTEGER :
                AL_INT ( field, "value"    , prf->u.integer.value         );
                AL_INT ( field, "default"  , prf->u.integer.default_value );
                implemented = TRUE;
                dlabel      = "Number";
                break;
              case PURPLE_REQUEST_FIELD_BOOLEAN :
                AL_BOOL( field, "value"    , prf->u.boolean.value         );
                AL_BOOL( field, "default"  , prf->u.boolean.default_value );
                implemented = TRUE;
                dlabel      = "On/off";
                break;
              case PURPLE_REQUEST_FIELD_CHOICE  :
                {
                    xmlnode *choices = xnode_new( "list" );
                    GList   *cl      = NULL;

                    AL_BOOL( field, "value"  , prf->u.choice.value         );
                    AL_BOOL( field, "default", prf->u.choice.default_value );
                    AL_NODE( field, "choices", choices );

                    for( cl = prf->u.choice.labels; cl; cl = cl->next )
                    {
                        const char *l = cl->data;
                        xmlnode    *c = xnode_new( "string" );
                        xnode_insert_data ( c, l, -1 );
                        xnode_insert_child( choices, c );
                    }
                    implemented = TRUE;
                    dlabel      = "Choice";
                };
                break;
              case PURPLE_REQUEST_FIELD_LIST    :
                dlabel = "List";
                break;
              case PURPLE_REQUEST_FIELD_LABEL   :
                dlabel = "Label";
                break;
              case PURPLE_REQUEST_FIELD_IMAGE   :
                dlabel = "Image";
                break;
              case PURPLE_REQUEST_FIELD_ACCOUNT :
                dlabel = "Account";
                break;
              case PURPLE_REQUEST_FIELD_NONE    :
                dlabel = "None";
              default:
                fprintf( stderr, "Unknown field type: %d\n", t );
            }

            AL_BOOL( field, "implemented", implemented );
            AL_STR ( field, "label", flabel ? flabel : dlabel );
        }
    }

    resp->req.fields.fields = fields;
    resp->req.fields.ok     = (PurpleRequestFieldsCb)ok_cb;
    resp->req.fields.nok    = (PurpleRequestFieldsCb)cancel_cb;
    resp->data              = user_data;
    resp->type              = PURPLE_REQUEST_FIELDS;
    resp->id                = ID;

    cbh ->func = _elim_request_fields_cb;
    cbh ->data = resp;

    store_cb_data( ID, cbh );
    xmlnode *mcall = func_call( "elim-request-fields", ID, alist );
    add_outbound_sexp( mcall );
    fprintf(stderr, "(_elim_request_fields HANDLE: %p . %p)", cbh, resp );
    return cbh;
}

static xmlnode * _elim_request_path_cb( gpointer ptr, SEXP_VALUE *args )
{
    fprintf( stderr, "(_elim_request_path_cb)\n" );

    CB_HANDLER  *cbh = ptr;
    REQ_RESP *handle = cbh->data;
    if( handle )
    {
        gpointer data = handle->data;
        PurpleRequestFileCb cb = NULL;

        if( args && (args->type == SEXP_ALIST) )
        {
            int status = ALIST_VAL_INT( args, "status" );
            if( status == 0 )
            {
                char *path = ALIST_VAL_STR( args, "value" );
                cb = ( path ?  handle->req.path.ok : handle->req.path.nok );
                if( cb ) (cb)( data, path );
            }
            else
            {
                cb = handle->req.path.nok;
                if( cb ) (cb)( data, "" );
            }
        }
        else
        {
            cb = handle->req.path.nok;
            if( cb ) (cb)( data, "" );
        }
    }

    purple_request_close( handle->type, cbh );
    //if( handle ) g_free( handle );

    if( args ) sexp_val_free( args );

    return NULL;
}

static void *_elim_request_file  ( const char            *title      ,
                                   const char            *filename   ,
                                   gboolean               savedialog ,
                                   GCallback              ok_cb      ,
                                   GCallback              cancel_cb  ,
                                   PurpleAccount         *account    ,
                                   const char            *who        ,
                                   PurpleConversation    *conv       ,
                                   void                  *user_data  )
{
    fprintf( stderr, "(_elim_request_file)\n" );

    CB_HANDLER *cbh   = g_new0( CB_HANDLER, 1 );
    REQ_RESP   *resp  = g_new0( REQ_RESP  , 1 );
    xmlnode    *alist = xnode_new( "alist" );
    char       *ID    = new_elim_id();

    AL_STR ( alist, "title"  , title      );
    AL_STR ( alist, "default", filename   );
    AL_STR ( alist, "who"    , who        );
    AL_BOOL( alist, "savep"  , savedialog );

    if( account )
    {
        const char *aname = purple_account_get_username   ( account );
        const char *proto = purple_account_get_protocol_id( account );
        AL_PTR ( alist, "account-uid" , account );
        AL_STR ( alist, "account-name", aname   );
        AL_STR ( alist, "im-protocol" , proto   );
    }

    if( conv )
    {
        const char            *title = purple_conversation_get_title   ( conv );
        const char            *cname = purple_conversation_get_name    ( conv );
        PurpleConnectionFlags  cflag = purple_conversation_get_features( conv );
        PurpleConversationType ctype = purple_conversation_get_type    ( conv );

        AL_PTR ( alist, "conv-uid"     , conv  );
        AL_STR ( alist, "conv-name"    , cname );
        AL_STR ( alist, "conv-title"   , title );
        AL_ENUM( alist, "conv-type"    , ctype , ":conversation-type" );
        AL_ENUM( alist, "conv-features", cflag , ":connection-flags"  );
    }

    resp->req.path.ok  = (PurpleRequestFileCb)ok_cb;
    resp->req.path.nok = (PurpleRequestFileCb)cancel_cb;
    resp->data         = user_data;
    resp->type         = PURPLE_REQUEST_FILE;
    resp->id           = ID;

    cbh ->func = _elim_request_path_cb;
    cbh ->data = resp;

    store_cb_data( ID, cbh );
    xmlnode *mcall = func_call( "elim-request-file", ID, alist );
    add_outbound_sexp( mcall );

    fprintf(stderr, "(_elim_request_file HANDLE: %p . %p)", cbh, resp );
    return cbh;
}

static void _elim_close_request  ( PurpleRequestType type, void *ui_handle )
{
    fprintf( stderr, "(_elim_close_request HANDLE %p)\n", ui_handle );

    CB_HANDLER *cbh  = ui_handle;
    REQ_RESP   *resp = cbh ? cbh->data : NULL;

    if( resp && (resp->type == PURPLE_REQUEST_ACTION) )
    {
        fprintf( stderr, "about to free action list: %p\n",
                 resp->req.action.func );
        g_free( resp->req.action.func );
    }
    fprintf( stderr, "about to free REQ_RESP : %p\n", resp );
    g_free( resp );
    fprintf( stderr, "about to free CB_HANDLE: %p\n", cbh  );
    g_free( cbh  );
}

static void *_elim_request_folder( const char            *title      ,
                                   const char            *dirname    ,
                                   GCallback              ok_cb      ,
                                   GCallback              cancel_cb  ,
                                   PurpleAccount         *account    ,
                                   const char            *who        ,
                                   PurpleConversation    *conv       ,
                                   void                  *user_data  )
{
    fprintf( stderr, "(_elim_request_folder)\n" );

    CB_HANDLER *cbh   = g_new0( CB_HANDLER, 1 );
    REQ_RESP   *resp  = g_new0( REQ_RESP  , 1 );
    xmlnode    *alist = xnode_new( "alist" );
    char       *ID    = new_elim_id();

    AL_STR ( alist, "title"  , title   );
    AL_STR ( alist, "default", dirname );
    AL_STR ( alist, "who"    , who     );

    if( account )
    {
        const char *aname = purple_account_get_username   ( account );
        const char *proto = purple_account_get_protocol_id( account );
        AL_PTR ( alist, "account-uid" , account );
        AL_STR ( alist, "account-name", aname   );
        AL_STR ( alist, "im-protocol" , proto   );
    }

    if( conv )
    {
        const char            *title = purple_conversation_get_title   ( conv );
        const char            *cname = purple_conversation_get_name    ( conv );
        PurpleConnectionFlags  cflag = purple_conversation_get_features( conv );
        PurpleConversationType ctype = purple_conversation_get_type    ( conv );
        AL_PTR ( alist, "conv-uid"     , conv  );
        AL_STR ( alist, "conv-name"    , cname );
        AL_STR ( alist, "conv-title"   , title );
        AL_ENUM( alist, "conv-type"    , ctype , ":conversation-type" );
        AL_ENUM( alist, "conv-features", cflag , ":connection-flags"  );
    }

    resp->req.path.ok  = (PurpleRequestFileCb)ok_cb;
    resp->req.path.nok = (PurpleRequestFileCb)cancel_cb;
    resp->data         = user_data;
    resp->type         = PURPLE_REQUEST_FOLDER;
    resp->id           = ID;

    cbh ->func = _elim_request_path_cb;
    cbh ->data = resp;

    store_cb_data( ID, cbh );
    xmlnode *mcall = func_call( "elim-request-directory", ID, alist );
    add_outbound_sexp( mcall );
    fprintf(stderr, "(_elim_request_folder HANDLE: %p . %p)", cbh, resp );
    return cbh;
}
