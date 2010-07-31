AUTOMAKE_OPTIONS = foreign

SUBDIRS = libcpp libiberty src doc tools

# gcc directories aren't automake-friendly
DIST_SUBDIRS = src doc tools
gcc_SUBDIRS = include libcpp libiberty

src: libcpp libiberty

EXTRA_DIST =					\
	COPYING					\
	COPYRIGHT				\
	INSTALL					\
	README					\
	config-aux/acx.m4			\
	config-aux/codeset.m4			\
	config-aux/gettext-sister.m4		\
	config-aux/move-if-change		\
	config-aux/no-executables.m4		\
	config-aux/warnings.m4

dist-hook:
	for i in $(gcc_SUBDIRS); do \
	  (cd $$i; make distclean); \
	  tar cf - -X $$i/.distignore $$i | tar xf - -C $(distdir); \
	done
