/*
 * Copyright (c) 1999-2001
 *      The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the
 *   distribution.
 * - Neither the name of the University of California nor the names of
 *   its contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL
 * THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */
#ifndef DHASH_H
#define DHASH_H

#include "regions.h"

typedef struct dhash_table *dhash_table;

dhash_table new_dhash_table(region r, unsigned long initial_size,
			    int (*compare)(void *entry1, void *entry2),
			    unsigned long (*hash)(void *entry));
/* Returns: new hash table created in region r, with specified initial size,
     comparison and hashing functions
*/

void *dhlookup(dhash_table h, void *entry);
/* Returns: An entry x in hash table h such that compare(x, entry) is true,
     or NULL if no entry found.
*/

void dhadd(dhash_table h, void *entry);
/* Effects: Unconditionally adds entry to hash table h (may create
     duplicates)
   Modifies: h
*/

void *dhaddif(dhash_table h, void *entry);
/* Effects: Adds entry to hash table h if it's not already there
     (as determined by dhlookup)
   Returns: dhlookup's result if entry not added, NULL otherwise
   Modifies: h
*/

unsigned long dhash_used(dhash_table h);
/* Returns: number of elements in hash table h
 */

typedef struct
{
  dhash_table h;
  int index;
} dhash_scan;

dhash_scan dhscan(dhash_table h);
/* Returns: new iterator for hash table h
 */
void *dhnext(dhash_scan *iterator);
/* Requires: no changes to hash table have been made since dhscan returned
    *iterator
   Effects: Returns next element of hash table iterated by *iterator, or
     NULL if no elements remain
   Modifies: iterator
*/

/* Some predefined hash tables */

dhash_table new_dhash_ptr_table(region r, unsigned long initial_size);
/* Returns: A new hash table which hashes pointers, with specified initial size
 */

#endif
