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
#ifndef _EMACSIM_HANDLER_UTIL_H_
#define _EMACSIM_HANDLER_UTIL_H_

#include <purple.h>

PurpleConversation *find_conv_by_acct_uid  ( PurpleAccount *acct, gpointer id );
PurpleAccount      *find_acct_by_uid       ( gpointer uid );
PurpleConversation *find_conv_by_uid       ( gpointer uid );
PurplePlugin       *find_plugin_by_protocol( const char *name );
PurpleBlistNode    *find_blist_node_by_uid ( gpointer uid , gboolean offline );
PurpleBlistNode    *find_blist_node_clone  ( gpointer uid );
#endif
