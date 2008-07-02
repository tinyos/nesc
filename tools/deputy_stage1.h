#ifndef DEPUTY_STAGE1_INCLUDED
#define DEPUTY_STAGE1_INCLUDED

#include <stddef.h>

#define __DEPUTY_UNUSED__                      __attribute__((unused))

struct @nonnull @deputy_scope() @macro("__DEPUTY_NONNULL") { }; 
struct @bnd @deputy_scope() @macro("__DEPUTY_BND") { void *lo, *hi; }; 
struct @bnd_nok @deputy_scope() @macro("__DEPUTY_BND_NOK") { void *lo, *hi; }; 
struct @count @deputy_scope() @macro("__DEPUTY_COUNT") { int n; }; 
struct @count_nok @deputy_scope() @macro("__DEPUTY_COUNT_NOK") { int n; }; 
struct @one @deputy_scope() @macro("__DEPUTY_ONE") { }; 
struct @one_nok @deputy_scope() @macro("__DEPUTY_ONE_NOK") { };
struct @dmemset @deputy_scope() @macro("__DEPUTY_DMEMSET") { int a1, a2, a3; };
struct @dmemcpy @deputy_scope() @macro("__DEPUTY_DMEMCPY") { int a1, a2, a3; };
struct @nts @deputy_scope() @macro("__DEPUTY_NTS") { }; 

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

#define __DEPUTY_TRUSTED                       __attribute__((trusted))
#define __DEPUTY_COPYTYPE                      __attribute__((copytype))

#define TCAST(__type,__expr)                   ((__type)((void * __DEPUTY_TRUSTED __DEPUTY_COPYTYPE)(__expr)))

void * (DMEMSET(1, 2, 3) memset)(void*, int, size_t);
void * (DMEMCPY(1, 2, 3) memcpy)(void*, const void*, size_t);
void * (DMEMCPY(1, 2, 3) memmove)(void*, const void*, size_t);

#endif
