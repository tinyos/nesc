dnl CPARSE_TRY_CFLAGS(FLAGS)
dnl
dnl   Try running the C compiler with FLAGS appended to $CFLAGS.  If
dnl   it completes without error, retain the additional flags.
dnl
AC_DEFUN([CPARSE_TRY_CFLAGS],[
AC_REQUIRE([AC_PROG_CC])
AC_MSG_CHECKING(whether ${CC-cc} accepts $1)
save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS $1"
AC_LANG_SAVE
AC_LANG_C
AC_TRY_COMPILE([], [],
  AC_MSG_RESULT(yes),
  AC_MSG_RESULT(no)
  CFLAGS="$save_CFLAGS")])


dnl CPARSE_GCCDIR
dnl
dnl   If the C compiler is gcc, set $GCCDIR to the directory
dnl   containing gcc's suite of supporting files and also define
dnl   GCCDIR macro to a string representation of the same.
dnl
AC_DEFUN([CPARSE_GCCDIR],[
AC_REQUIRE([AC_PROG_CC])
AC_MSG_CHECKING(for gcc support directory)
if test "$GCC" = yes; then
  GCCDIR="`$CC -v 2>&1 | sed -n 's:^Reading specs from \(.*\)/specs$:\1:p'`"
  if test -z "$GCCDIR"; then
    AC_MSG_ERROR(cannot find gcc support directory)
  else
    AC_MSG_RESULT($GCCDIR)
  fi
  AC_DEFINE_UNQUOTED(GCCDIR, "$GCCDIR", directory containing gcc support files)
else
  AC_MSG_RESULT(no)
fi
])

dnl RC_TRY_CPPFLAGS(FLAGS)
dnl
dnl   Try running the C preprocessor with FLAGS appended to $CPPFLAGS.
dnl   If it completes without error, retain the additional flags.
dnl
AC_DEFUN([RC_TRY_CPPFLAGS],[
AC_REQUIRE([AC_PROG_CPP])
AC_MSG_CHECKING(whether $CPP accepts $1)
save_CPPFLAGS="$CPPFLAGS"
CPPFLAGS="$CPPFLAGS $1"
AC_TRY_CPP([],
  AC_MSG_RESULT(yes)
,
  CPPFLAGS="$save_CPPFLAGS"
  AC_MSG_RESULT(no))])


dnl RC_CHECK_DECL(FUNCTION, HEADER-FILE)
dnl
dnl   Search for a declaration of FUNCTION in HEADER-FILE, and define
dnl   HAVE_FUNCTION_DECLARED if found.
dnl
AC_DEFUN([RC_CHECK_DECL],[
AC_CACHE_CHECK(for $1 declaration in <$2>, rc_cv_$1_declared, [
  AC_EGREP_HEADER(\<$1\>, $2, rc_cv_$1_declared=yes, rc_cv_$1_declared=no)])
if test "$rc_cv_$1_declared" = yes; then
  AC_DEFINE(HAVE_[]translit($1, a-z, A-Z)_DECLARED, 1, $1 is declared in <$2>)
fi])


dnl RC_FIX_EXEC(FILE)
dnl
dnl   If FILE is on the list of generated config files, turn on its
dnl   user execute bit.  Intended for use in the EXTRA-CMDS argument
dnl   to AC_OUTPUT or AC_OUTPUT_COMMANDS.
dnl
AC_DEFUN([RC_FIX_EXEC],[[
case "$CONFIG_FILES" in
  *$1*) chmod +x $1 ;;
esac]])
