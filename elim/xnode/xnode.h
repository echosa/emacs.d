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
#ifndef _EMACSIM_XMLNODE_H_
#define _EMACSIM_XMLNODE_H_

#include <glib.h>
#include <xmlnode.h>
// ***************************************************************************
// macros to deal with C <-> xmlrpc type mappings
#define INT_32BIT_MAX   2147483647
// don't change this to -2147483648: that triggers a warning. don't ask.
#define INT_32BIT_MIN  (-2147483647 - 1)

char    * xnode_unescape_html     ( const char *html );
xmlnode * xnode_alist_item_string ( const char *name, const char *value );
xmlnode * xnode_alist_item_integer( const char *name, long        value );
xmlnode * xnode_alist_item_number ( const char *name, double      value );
xmlnode * xnode_alist_item_boolean( const char *name, gboolean    value );
xmlnode * xnode_alist_item_xnode  ( const char *name, xmlnode    *value );
xmlnode * xnode_alist_item_enum   ( const char *name, int val, const char *t );
xmlnode * xnode_alist_item_data   ( const char *name, const char *v, size_t l );

xmlnode * xnode_list_item_string  (const char *value);

/**
 * Gets the next tag at the same level as this one.
 *
 * @param node The node whose next sibling tag we want.
 *
 * @return The first tag sibling of node or NULL.
 */
xmlnode * xnode_get_next_sibling ( xmlnode *node );

/**
 * Gets the first child of node which is a tag (ie not data or an attribute)
 *
 * @param node The node whose first child tag we want.
 *
 * @return The first tag child of node or NULL.
 */
xmlnode *xnode_first_child_tag ( xmlnode *node );

// this code lifted from libpurple so we can use it the way we want without
// libpurple spewing dbus errors all over the place (GPL'd):

/**
 * Creates a new xmlnode.
 *
 * @param name The name of the node.
 *
 * @return The new node.
 */
xmlnode *xnode_new(const char *name);

/**
 * Creates a new xmlnode child.
 *
 * @param parent The parent node.
 * @param name   The name of the child node.
 *
 * @return The new child node.
 */
xmlnode *xnode_new_child(xmlnode *parent, const char *name);

/**
 * Inserts a node into a node as a child.
 *
 * @param parent The parent node to insert child into.
 * @param child  The child node to insert into parent.
 */
void xnode_insert_child(xmlnode *parent, xmlnode *child);

/**
 * Gets a child node named name.
 *
 * @param parent The parent node.
 * @param name   The child's name.
 *
 * @return The child or NULL.
 */
xmlnode *xnode_get_child(const xmlnode *parent, const char *name);

/**
 * Gets a child node named name in a namespace.
 *
 * @param parent The parent node.
 * @param name   The child's name.
 * @param xmlns  The namespace.
 *
 * @return The child or NULL.
 */
xmlnode *xnode_get_child_with_namespace(const xmlnode *parent, const char *name, const char *xmlns);

/**
 * Gets the next node with the same name as node.
 *
 * @param node The node of a twin to find.
 *
 * @return The twin of node or NULL.
 */
xmlnode *xnode_get_next_twin(xmlnode *node);

/**
 * Inserts data into a node.
 *
 * @param node   The node to insert data into.
 * @param data   The data to insert.
 * @param size   The size of the data to insert.  If data is
 *               null-terminated you can pass in -1.
 */
void xnode_insert_data(xmlnode *node, const char *data, gssize size);

/**
 * Gets (escaped) data from a node.
 *
 * @param node The node to get data from.
 *
 * @return The data from the node or NULL. This data is in raw escaped format.
 *         You must g_free this string when finished using it.
 */
char *xnode_get_data(xmlnode *node);

/**
 * Gets unescaped data from a node.
 *
 * @param node The node to get data from.
 *
 * @return The data from the node, in unescaped form.   You must g_free
 *         this string when finished using it.
 */
char *xnode_get_data_unescaped(xmlnode *node);

/**
 * Sets an attribute for a node.
 *
 * @param node  The node to set an attribute for.
 * @param attr  The name of the attribute.
 * @param value The value of the attribute.
 */
void xnode_set_attrib(xmlnode *node, const char *attr, const char *value);

/**
 * Sets a prefixed attribute for a node
 *
 * @param node   The node to set an attribute for.
 * @param attr   The name of the attribute to set
 * @param prefix The prefix of the attribute to ste
 * @param value  The value of the attribute
 */
void xnode_set_attrib_with_prefix(xmlnode *node, const char *attr, const char *prefix, const char *value);

/**
 * Sets a namespaced attribute for a node
 *
 * @param node  The node to set an attribute for.
 * @param attr  The name of the attribute to set
 * @param xmlns The namespace of the attribute to set
 * @param value The value of the attribute
 */
void xnode_set_attrib_with_namespace(xmlnode *node, const char *attr, const char *xmlns, const char *value);

/**
 * Gets an attribute from a node.
 *
 * @param node The node to get an attribute from.
 * @param attr The attribute to get.
 *
 * @return The value of the attribute.
 */
const char *xnode_get_attrib(xmlnode *node, const char *attr);

/**
 * Gets a namespaced attribute from a node
 *
 * @param node  The node to get an attribute from.
 * @param attr  The attribute to get
 * @param xmlns The namespace of the attribute to get
 *
 * @return The value of the attribute.
 */
const char *xnode_get_attrib_with_namespace(xmlnode *node, const char *attr, const char *xmlns);

/**
 * Removes an attribute from a node.
 *
 * @param node The node to remove an attribute from.
 * @param attr The attribute to remove.
 */
void xnode_remove_attrib(xmlnode *node, const char *attr);

/**
 * Removes a namespaced attribute from a node
 *
 * @param node  The node to remove an attribute from
 * @param attr  The attribute to remove
 * @param xmlns The namespace of the attribute to remove
 */
void xnode_remove_attrib_with_namespace(xmlnode *node, const char *attr, const char *xmlns);

/**
 * Sets the namespace of a node
 *
 * @param node The node to qualify
 * @param xmlns The namespace of the node
 */
void xnode_set_namespace(xmlnode *node, const char *xmlns);

/**
 * Returns the namespace of a node
 *
 * @param node The node to get the namespace from
 * @return The namespace of this node
 */
const char *xnode_get_namespace(xmlnode *node);

/**
 * Sets the prefix of a node
 *
 * @param node   The node to qualify
 * @param prefix The prefix of the node
 */
void xnode_set_prefix(xmlnode *node, const char *prefix);

/**
 * Returns the prefix of a node
 *
 * @param node The node to get the prefix from
 * @return The prefix of this node
 */
const char *xnode_get_prefix(xmlnode *node);

/**
 * Returns the node in a string of xml.
 *
 * @param node The starting node to output.
 * @param len  Address for the size of the string.
 *
 * @return The node represented as a string.  You must
 *         g_free this string when finished using it.
 */
char *xnode_to_str(xmlnode *node, int *len);

/**
 * Returns the node in a string of human readable xml.
 *
 * @param node The starting node to output.
 * @param len  Address for the size of the string.
 *
 * @return The node as human readable string including
 *         tab and new line characters.  You must
 *         g_free this string when finished using it.
 */
char *xnode_to_formatted_str(xmlnode *node, int *len);

/**
 * Creates a node from a string of XML.  Calling this on the
 * root node of an XML document will parse the entire document
 * into a tree of nodes, and return the xmlnode of the root.
 *
 * @param str  The string of xml.
 * @param size The size of the string, or -1 if @a str is
 *             NUL-terminated.
 *
 * @return The new node.
 */
xmlnode *xnode_from_str(const char *str, gssize size);

/**
 * Creates a new node from the source node.
 *
 * @param src The node to copy.
 *
 * @return A new copy of the src node.
 */
xmlnode *xnode_copy(const xmlnode *src);

/**
 * Frees a node and all of its children.
 *
 * @param node The node to free.
 */
void xnode_free(xmlnode *node);


#endif// _EMACSIM_XMLNODE_H_
