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
#include "enumerations.h"
#include "../prpl/util.h"
#include "../ui_ops/ops.h"

static xmlnode *enums = NULL;

#define EG( name ) \
    group = xnode_new_child( enums, "alist" ); \
    xnode_set_attrib ( group, "name", ":" #name )

#define IV( stub, name ) \
    AL_INT( group, #name, PURPLE_ ## stub ## name )

static void _h_elim_cache_enums (void)
{
    if( enums ) return;

    enums = xnode_new( "alist" );
    xmlnode *group = NULL;

    EG( account-request-type );
    IV( ACCOUNT_REQUEST_, AUTHORIZATION  );
// =============================================
    EG( blist-node-type );
    IV( BLIST_ , GROUP_NODE   );
    IV( BLIST_ , CONTACT_NODE );
    IV( BLIST_ , BUDDY_NODE   );
    IV( BLIST_ , CHAT_NODE    );
    IV( BLIST_ , OTHER_NODE   );
// =============================================
    EG( blist-node-flags );
    IV( BLIST_NODE_FLAG_ , NO_SAVE );
// =============================================
    EG( certificate-verification-status );
    IV( CERTIFICATE_ , INVALID  );
    IV( CERTIFICATE_ , VALID    );
// =============================================
    EG( connection-flags );
    IV( CONNECTION_ , HTML            );
    IV( CONNECTION_ , NO_BGCOLOR      );
    IV( CONNECTION_ , AUTO_RESP       );
    IV( CONNECTION_ , FORMATTING_WBFO );
    IV( CONNECTION_ , NO_NEWLINES     );
    IV( CONNECTION_ , NO_FONTSIZE     );
    IV( CONNECTION_ , NO_URLDESC      );
    IV( CONNECTION_ , NO_IMAGES       );
// =============================================
    EG( connection-state );
    IV( , DISCONNECTED   );
    IV( , CONNECTED      );
    IV( , CONNECTING     );
// =============================================
    EG( connection-error );
    IV( CONNECTION_ERROR_ , NETWORK_ERROR              );
    IV( CONNECTION_ERROR_ , INVALID_USERNAME           );
    IV( CONNECTION_ERROR_ , AUTHENTICATION_FAILED      );
    IV( CONNECTION_ERROR_ , AUTHENTICATION_IMPOSSIBLE  );
    IV( CONNECTION_ERROR_ , NO_SSL_SUPPORT             );
    IV( CONNECTION_ERROR_ , ENCRYPTION_ERROR           );
    IV( CONNECTION_ERROR_ , NAME_IN_USE                );
    IV( CONNECTION_ERROR_ , INVALID_SETTINGS           );
    IV( CONNECTION_ERROR_ , CERT_NOT_PROVIDED          );
    IV( CONNECTION_ERROR_ , CERT_UNTRUSTED             );
    IV( CONNECTION_ERROR_ , CERT_EXPIRED               );
    IV( CONNECTION_ERROR_ , CERT_NOT_ACTIVATED         );
    IV( CONNECTION_ERROR_ , CERT_HOSTNAME_MISMATCH     );
    IV( CONNECTION_ERROR_ , CERT_FINGERPRINT_MISMATCH  );
    IV( CONNECTION_ERROR_ , CERT_SELF_SIGNED           );
    IV( CONNECTION_ERROR_ , CERT_OTHER_ERROR           );
    IV( CONNECTION_ERROR_ , OTHER_ERROR                );
// =============================================
    EG( conversation-type );
    IV( CONV_TYPE_ , UNKNOWN    );
    IV( CONV_TYPE_ , IM         );
    IV( CONV_TYPE_ , CHAT       );
    IV( CONV_TYPE_ , MISC       );
    IV( CONV_TYPE_ , ANY        );
// =============================================
    EG( conv-update-type );
    IV( CONV_UPDATE_  , ADD      );
    IV( CONV_UPDATE_  , REMOVE   );
    IV( CONV_UPDATE_  , ACCOUNT  );
    IV( CONV_UPDATE_  , TYPING   );
    IV( CONV_UPDATE_  , UNSEEN   );
    IV( CONV_UPDATE_  , LOGGING  );
    IV( CONV_UPDATE_  , TOPIC    );
    IV( CONV_ACCOUNT_ , ONLINE   );
    IV( CONV_ACCOUNT_ , OFFLINE  );
    IV( CONV_UPDATE_  , AWAY     );
    IV( CONV_UPDATE_  , ICON     );
    IV( CONV_UPDATE_  , TITLE    );
    IV( CONV_UPDATE_  , CHATLEFT );
    IV( CONV_UPDATE_  , FEATURES );
// =============================================
    EG( typing-state );
    IV( , NOT_TYPING );
    IV( , TYPING     );
    IV( , TYPED      );
// =============================================
    EG( message-flags );
    IV( MESSAGE_ , SEND        );
    IV( MESSAGE_ , RECV        );
    IV( MESSAGE_ , SYSTEM      );
    IV( MESSAGE_ , AUTO_RESP   );
    IV( MESSAGE_ , ACTIVE_ONLY );
    IV( MESSAGE_ , NICK        );
    IV( MESSAGE_ , NO_LOG      );
    IV( MESSAGE_ , WHISPER     );
    IV( MESSAGE_ , ERROR       );
    IV( MESSAGE_ , DELAYED     );
    IV( MESSAGE_ , RAW         );
    IV( MESSAGE_ , IMAGES      );
    IV( MESSAGE_ , NOTIFY      );
    IV( MESSAGE_ , NO_LINKIFY  );
    IV( MESSAGE_ , INVISIBLE   );
// =============================================
    EG( conv-chat-buddy-flags );
    IV( CBFLAGS_ , NONE        );
    IV( CBFLAGS_ , VOICE       );
    IV( CBFLAGS_ , HALFOP      );
    IV( CBFLAGS_ , OP          );
    IV( CBFLAGS_ , FOUNDER     );
    IV( CBFLAGS_ , TYPING      );
// =============================================
    EG( debug-level );
    IV( DEBUG_ , ALL           );
    IV( DEBUG_ , MISC          );
    IV( DEBUG_ , INFO          );
    IV( DEBUG_ , WARNING       );
    IV( DEBUG_ , ERROR         );
    IV( DEBUG_ , FATAL         );
// =============================================
    EG( input-condition );
    IV( INPUT_ , READ          );
    IV( INPUT_ , WRITE         );
// =============================================
    EG( xfer-type );
    IV( XFER_ , UNKNOWN        );
    IV( XFER_ , SEND           );
    IV( XFER_ , RECEIVE        );
// =============================================
    EG( xfer-status-type );
    IV( XFER_STATUS_ , UNKNOWN        );
    IV( XFER_STATUS_ , NOT_STARTED    );
    IV( XFER_STATUS_ , ACCEPTED       );
    IV( XFER_STATUS_ , STARTED        );
    IV( XFER_STATUS_ , DONE           );
    IV( XFER_STATUS_ , CANCEL_LOCAL   );
    IV( XFER_STATUS_ , CANCEL_REMOTE  );
// =============================================
    EG( log-type );
    IV( LOG_ , IM                     );
    IV( LOG_ , CHAT                   );
    IV( LOG_ , SYSTEM                 );
// =============================================
    EG( log-read-flags );
    IV( LOG_READ_ , NO_NEWLINE        );
// =============================================
    EG( notify-type );
    IV( NOTIFY_ , MESSAGE             );
    IV( NOTIFY_ , EMAIL               );
    IV( NOTIFY_ , EMAILS              );
    IV( NOTIFY_ , FORMATTED           );
    IV( NOTIFY_ , SEARCHRESULTS       );
    IV( NOTIFY_ , USERINFO            );
    IV( NOTIFY_ , URI                 );
// =============================================
    EG( notify-msg-type );
    IV( NOTIFY_MSG_ , ERROR           );
    IV( NOTIFY_MSG_ , WARNING         );
    IV( NOTIFY_MSG_ , INFO            );
// =============================================
    EG( notify-search-button-type );
    IV( NOTIFY_BUTTON_ , LABELED      );
    IV( NOTIFY_BUTTON_ , CONTINUE     );
    IV( NOTIFY_BUTTON_ , ADD          );
    IV( NOTIFY_BUTTON_ , INFO         );
    IV( NOTIFY_BUTTON_ , IM           );
    IV( NOTIFY_BUTTON_ , JOIN         );
    IV( NOTIFY_BUTTON_ , INVITE       );
// =============================================
    EG( notify-user-info-entry-type );
    IV( NOTIFY_USER_INFO_ENTRY_ , PAIR            );
    IV( NOTIFY_USER_INFO_ENTRY_ , SECTION_BREAK   );
    IV( NOTIFY_USER_INFO_ENTRY_ , SECTION_HEADER  );
// =============================================
    EG( plugin-type );
    IV( PLUGIN_ , UNKNOWN           );
    IV( PLUGIN_ , STANDARD          );
    IV( PLUGIN_ , LOADER            );
    IV( PLUGIN_ , PROTOCOL          );
// =============================================
    EG( string-format-type );
    IV( STRING_FORMAT_TYPE_ , NONE      );
    IV( STRING_FORMAT_TYPE_ , MULTILINE );
    IV( STRING_FORMAT_TYPE_ , HTML      );
// =============================================
    EG( plugin-pref-type );
    IV( PLUGIN_PREF_ , NONE          );
    IV( PLUGIN_PREF_ , CHOICE        );
    IV( PLUGIN_PREF_ , INFO          );
    IV( PLUGIN_PREF_ , STRING_FORMAT );
// =============================================
    EG( pounce-event );
    IV( POUNCE_ , NONE              );
    IV( POUNCE_ , SIGNON            );
    IV( POUNCE_ , SIGNOFF           );
    IV( POUNCE_ , AWAY              );
    IV( POUNCE_ , AWAY_RETURN       );
    IV( POUNCE_ , IDLE              );
    IV( POUNCE_ , IDLE_RETURN       );
    IV( POUNCE_ , TYPING            );
    IV( POUNCE_ , TYPED             );
    IV( POUNCE_ , TYPING_STOPPED    );
    IV( POUNCE_ , MESSAGE_RECEIVED  );
// =============================================
    EG( pounce-option );
    IV( POUNCE_OPTION_ , NONE       );
    IV( POUNCE_OPTION_ , AWAY       );
// =============================================
    EG( pref-type );
    IV( PREF_ , NONE                );
    IV( PREF_ , BOOLEAN             );
    IV( PREF_ , INT                 );
    IV( PREF_ , STRING              );
    IV( PREF_ , STRING_LIST         );
    IV( PREF_ , PATH                );
    IV( PREF_ , PATH_LIST           );
// =============================================
    EG( privacy-type );
    IV( PRIVACY_ , ALLOW_ALL        );
    IV( PRIVACY_ , DENY_ALL         );
    IV( PRIVACY_ , ALLOW_USERS      );
    IV( PRIVACY_ , DENY_USERS       );
    IV( PRIVACY_ , ALLOW_BUDDYLIST  );
// =============================================
    EG( proxy-type );
    IV( PROXY_ , USE_GLOBAL         );
    IV( PROXY_ , NONE               );
    IV( PROXY_ , HTTP               );
    IV( PROXY_ , SOCKS4             );
    IV( PROXY_ , SOCKS5             );
    IV( PROXY_ , USE_ENVVAR         );
// =============================================
    EG( request-type );
    IV( REQUEST_ , INPUT            );
    IV( REQUEST_ , CHOICE           );
    IV( REQUEST_ , ACTION           );
    IV( REQUEST_ , FIELDS           );
    IV( REQUEST_ , FILE             );
    IV( REQUEST_ , FOLDER           );
// =============================================
    EG( request-field-type );
    IV( REQUEST_FIELD_ , NONE       );
    IV( REQUEST_FIELD_ , STRING     );
    IV( REQUEST_FIELD_ , INTEGER    );
    IV( REQUEST_FIELD_ , BOOLEAN    );
    IV( REQUEST_FIELD_ , CHOICE     );
    IV( REQUEST_FIELD_ , LIST       );
    IV( REQUEST_FIELD_ , LABEL      );
    IV( REQUEST_FIELD_ , IMAGE      );
    IV( REQUEST_FIELD_ , ACCOUNT    );
// =============================================
    EG( roomlist-room-type );
    IV( ROOMLIST_ROOMTYPE_ , CATEGORY );
    IV( ROOMLIST_ROOMTYPE_ , ROOM     );
// =============================================
    EG( roomlist-field-type );
    IV( ROOMLIST_FIELD_ , BOOL      );
    IV( ROOMLIST_FIELD_ , INT       );
    IV( ROOMLIST_FIELD_ , STRING    );
// =============================================
    EG( ssl-error-type );
    IV( SSL_ , HANDSHAKE_FAILED     );
    IV( SSL_ , CONNECT_FAILED       );
    IV( SSL_ , CERTIFICATE_INVALID  );
// =============================================
    EG( presence-context );
    IV( PRESENCE_CONTEXT_ , UNSET   );
    IV( PRESENCE_CONTEXT_ , ACCOUNT );
    IV( PRESENCE_CONTEXT_ , CONV    );
    IV( PRESENCE_CONTEXT_ , BUDDY   );
// =============================================
    EG( status-primitive );
    IV( STATUS_ , UNSET             );
    IV( STATUS_ , OFFLINE           );
    IV( STATUS_ , AVAILABLE         );
    IV( STATUS_ , UNAVAILABLE       );
    IV( STATUS_ , INVISIBLE         );
    IV( STATUS_ , AWAY              );
    IV( STATUS_ , EXTENDED_AWAY     );
    IV( STATUS_ , MOBILE            );
    IV( STATUS_ , TUNE              );
    IV( STATUS_ , NUM_PRIMITIVES    );
// =============================================
    EG( cmd-status );
    IV( CMD_STATUS_ , OK         );
    IV( CMD_STATUS_ , FAILED     );
    IV( CMD_STATUS_ , NOT_FOUND  );
    IV( CMD_STATUS_ , WRONG_ARGS );
    IV( CMD_STATUS_ , WRONG_PRPL );
    IV( CMD_STATUS_ , WRONG_TYPE );
}

xmlnode * _h_elim_enumerations ( const char *name ,
                                 const char *id   ,
                                 SEXP_VALUE *args ,
                                 gpointer data    )
{
    ASSERT_ALISTP( args, id, name );
    elim_ping();
    _h_elim_cache_enums();

    xmlnode    *rval = NULL;
    const char *type = ALIST_VAL_STR( args, "enum-type" );

    if( type && *type )
    {
        rval = xnode_new( "alist" );
        xmlnode *what = xnode_first_child_tag( enums );
        for( ; what; what = xnode_get_next_sibling(what) )
        {
            if( strcmp( type, xnode_get_attrib( what, "name" ) ) ) continue;
            xnode_insert_child( rval, xnode_copy( what ) );
            break;
        }
    }
    else { rval = xnode_copy( enums ); }

    if( !rval ) rval = xnode_new( "alist" );

    sexp_val_free( args );
    return response_value( 0, id, name, rval );
}
