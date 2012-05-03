#include <glib.h>

// ***************************************************************************
// support glib < 2.14
#ifndef HAS_QUEUE_INIT
void elim_g_queue_init( GQueue *queue )
{
    queue->head = queue->tail = NULL;
    queue->length = 0;
}
#endif

#ifndef HAS_GET_HASH_KEYS
static void elim_g_hash_collect_keys(gpointer k, gpointer v, gpointer x)
{
    GList **list = x;
    *list = g_list_prepend( *list, k );
}

GList * elim_g_hash_table_get_keys( GHashTable *hash )
{

    GList *retval = NULL;
    
    g_return_val_if_fail( hash != NULL, NULL );

    g_hash_table_foreach( hash, elim_g_hash_collect_keys, (gpointer)&retval );

    return retval;
}
#endif
