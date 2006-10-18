#include <stdlib.h>
#include <assert.h>
#include "dynarray.h"

/* Set the i:th value of 'da' to 'val'.  If 'da' is NULL, a new
   dynarray_t will be created and returned. */

dynarray_t *dynarray_set(dynarray_t *da, int i, da_numeric_t val)
{
    assert(i >= 0);

    if (!da) {
        da = malloc(sizeof(dynarray_t));
        da->next = NULL;
	da->n = 0;
    }

    if (i+1 > da->n)  da->n = i+1;

    if (i >= DYNARRAY_ALEN)
        da->next = dynarray_set(da->next, i-DYNARRAY_ALEN, val);
    else
        da->a[i] = val;

    return da;
}


/* Get the value of the i:th element of 'da'.  If that element hasn't been
 * assigned to (using da_seti), it's value will be undefined.  */

da_numeric_t dynarray_get(dynarray_t *da, int i)
{
    assert(i >= 0);

    if (!da) {
        da_numeric_t v;
        return v;
    }

    if (i >= DYNARRAY_ALEN)
        return dynarray_get(da->next, i-DYNARRAY_ALEN);
    else
        return da->a[i];
}


void dynarray_kill(dynarray_t *da)
{
    if (!da) return;

    da_kill(da->next);
    free(da);
}
