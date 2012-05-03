#include <glib.h>

#ifndef _ELIM_GLIBCOMPAT_H_
#define _ELIM_GLIBCOMPAT_H_

// ***************************************************************************
// support glib < 2.14
#ifdef  HAS_QUEUE_INIT
#define ELIM_G_QUEUE_INIT(x) g_queue_init( x )
#else   
#define ELIM_G_QUEUE_INIT(x) elim_g_queue_init( x )
void elim_g_queue_init( GQueue *queue );
#endif

#ifdef HAS_GET_HASH_KEYS
#define ELIM_G_HASH_TABLE_GET_KEYS(x) g_hash_table_get_keys( x )
#else
#define ELIM_G_HASH_TABLE_GET_KEYS(x) elim_g_hash_table_get_keys( x )
GList * elim_g_hash_table_get_keys (GHashTable *hash_table);
#endif

#endif // _ELIM_GLIBCOMPAT_H_
