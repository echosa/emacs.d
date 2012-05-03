/*
Copyright © 2010 Savio Sena
Copyright © 2009 Vivek Dasmohapatra

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

#include "roomlist_ui_ops.h"
#include <assert.h>

static void _elim_roomlist_show_with_account ( PurpleAccount      *account  );

static void _elim_roomlist_create            ( PurpleRoomlist     *list     );

static void _elim_roomlist_set_field         ( PurpleRoomlist     *list     ,
                                               GList              *fields   );

static void _elim_roomlist_add               ( PurpleRoomlist     *list     ,
                                               PurpleRoomlistRoom *room     );

static void _elim_roomlist_in_progress       ( PurpleRoomlist     *list     ,
                                               gboolean flag                );

static void _elim_roomlist_destroy           ( PurpleRoomlist     *list     );

PurpleRoomlistUiOps elim_roomlist_ui_ops =
{
    _elim_roomlist_show_with_account ,
    _elim_roomlist_create            ,
    _elim_roomlist_set_field         ,
    _elim_roomlist_add               ,
    _elim_roomlist_in_progress       ,
    _elim_roomlist_destroy           ,
    NULL ,
    NULL ,
    NULL ,
    NULL
};

void __roomlist_insert_account( PurpleAccount *account, xmlnode *node )
{
    g_return_if_fail( account && node );

    const char *aname = purple_account_get_username    ( account );
    const char *proto = purple_account_get_protocol_id ( account );

    AL_PTR ( node, "account-uid"  , account );
    AL_STR ( node, "account-name" , aname   );
    AL_STR ( node, "im-protocol"  , proto   );
}

void __roomlist_add_list_field( gpointer _field, gpointer _parent )
{
    g_return_if_fail( _field && _parent );

    PurpleRoomlistField *field  = _field;
    xmlnode             *parent = _parent;
    xmlnode             *node   = xnode_new( "alist" );

    AL_ENUM( node , "field-type"  , field->type   , ":roomlist-field-type" );
    AL_STR ( node , "field-label" , field->label  );
    AL_STR ( node , "field-name"  , field->name   );
    AL_BOOL( node , "field-hidden", field->hidden );

    xnode_insert_child( parent, node );
}


static void _elim_roomlist_show_with_account ( PurpleAccount *account ) {}

static void _elim_roomlist_create ( PurpleRoomlist *list )
{
    g_return_if_fail( list );

    xmlnode    *alist = xnode_new( "alist" );
    char       *ID    = new_elim_id();    
    
    __roomlist_insert_account( list->account, alist );
    AL_PTR ( alist, "roomlist-id", list );

    xmlnode *flist = xnode_new_child( alist, "alist" );
    xnode_set_attrib( flist, "name", "fields" );
    g_list_foreach( list->fields, __roomlist_add_list_field, flist );

    xmlnode *mcall = func_call( "elim-roomlist-create", ID, alist );
    g_free( ID );
    add_outbound_sexp( mcall );
}

static void _elim_roomlist_set_field ( PurpleRoomlist  *list   ,
                                       GList           *fields )
{
    g_return_if_fail( list && fields );

    xmlnode *alist = xnode_new( "alist" );
    char    *ID    = new_elim_id();

    __roomlist_insert_account( list->account, alist );
    AL_PTR ( alist, "roomlist-id", list );

    xmlnode *flist = xnode_new_child( alist, "alist" );
    xnode_set_attrib( flist, "name", "fields" );
    g_list_foreach( fields, __roomlist_add_list_field, flist );

    xmlnode *mcall = func_call( "elim-roomlist-set-field", ID, alist );
    g_free( ID );
    add_outbound_sexp( mcall );
}

static void _elim_roomlist_add ( PurpleRoomlist     *list ,
                                 PurpleRoomlistRoom *room )
{
    g_return_if_fail( list && room );

    xmlnode *alist  = xnode_new( "alist" );
    char    *ID     = new_elim_id();

    __roomlist_insert_account( list->account, alist );
    AL_PTR ( alist , "roomlist-id" , list          );
    AL_STR ( alist , "room-name"   , room->name    );
    AL_ENUM( alist , "room-type"   , room->type    , ":roomlist-room-type" );
    AL_PTR ( alist , "room-parent" , room->parent  );
    AL_BOOL( alist , "room-expanded-once", room->expanded_once );

    xmlnode *fields = xnode_new_child( alist, "alist" );
    xnode_set_attrib( fields, "name", "fields" );

    GList *listf = g_list_first( list->fields );
    GList *roomf = g_list_first( room->fields );

#define NNEXTT( a, b ) a = g_list_next( a ), b = g_list_next( b )
#define PTR_TO_BOOL(_p) (_p != NULL)

    for( ; listf && roomf ; NNEXTT( listf, roomf ) )
    {
        PurpleRoomlistField *f = (PurpleRoomlistField*) listf->data;
        switch( f->type )
        {
          case PURPLE_ROOMLIST_FIELD_BOOL:
            AL_BOOL( fields, f->name, PTR_TO_BOOL( roomf->data ) );
            break;

          case PURPLE_ROOMLIST_FIELD_INT:
            AL_INT ( fields, f->name, roomf->data );
            break;

          case PURPLE_ROOMLIST_FIELD_STRING:
            AL_STR ( fields, f->name, roomf->data );
            break;

          default:
            fprintf( stderr, "unsupported room list field type.\n" );
            break;
        }
    }

    xmlnode *mcall = func_call( "elim-roomlist-add", ID, alist );
    g_free( ID );
    add_outbound_sexp( mcall );
}

static void _elim_roomlist_in_progress ( PurpleRoomlist *list ,
                                         gboolean        flag )
{
    g_return_if_fail( list );

    xmlnode *alist = xnode_new( "alist" );
    char    *ID    = new_elim_id();

    __roomlist_insert_account( list->account, alist );
    AL_PTR ( alist , "roomlist-id", list );
    AL_BOOL( alist , "in-progress", flag );

    xmlnode *mcall = func_call( "elim-roomlist-in-progress", ID, alist );
    g_free( ID );
    add_outbound_sexp( mcall );
}

static void _elim_roomlist_destroy ( PurpleRoomlist *list )
{
    g_return_if_fail( list );

    xmlnode *alist = xnode_new( "alist" );
    char     *ID   = new_elim_id();

    __roomlist_insert_account( list->account, alist );
    AL_PTR ( alist , "roomlist-id", list );

    xmlnode *mcall = func_call( "elim-roomlist-destroy", ID, alist );
    g_free( ID );
    add_outbound_sexp( mcall );
}
