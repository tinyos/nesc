#ifndef DEPUTY_STAGE1_INCLUDED
#define DEPUTY_STAGE1_INCLUDED

#include <stddef.h>

#define __DEPUTY_UNUSED__                      __attribute__((unused))

struct @nonnull @deputy_scope() { int dummy; }; 
struct @bnd @deputy_scope() { void *lo, *hi; }; 
struct @bnd_nok @deputy_scope() { void *lo, *hi; }; 
struct @count @deputy_scope() { int n; }; 
struct @count_nok @deputy_scope() { int n; }; 
struct @one @deputy_scope() { int dummy; }; 
struct @one_nok @deputy_scope() { int dummy; };
struct @dmemset @deputy_scope() { int a1, a2, a3; };
struct @dmemcpy @deputy_scope() { int a1, a2, a3; };
struct @nts @deputy_scope() { int dummy; }; 

#define NONNULL                                @nonnull()
#define BND(x,y)                               @bnd(x,y)
#define BND_NOK(x,y)                           @bnd_nok(x,y)
#define COUNT(x)                               @count(x)
#define COUNT_NOK(x)                           @count_nok(x)
#define ONE                                    @one()
#define ONE_NOK                                @one_nok()
#define DMEMSET(x,y,z)                         @dmemset(x,y,z)
#define DMEMCPY(x,y,z)                         @dmemcpy(x,y,z)
#define TRUSTEDBLOCK                           @unsafe()
#define NTS                                    @nts()

#define TCAST(__type,__expr)                   ((__type)(__expr))                

#endif
