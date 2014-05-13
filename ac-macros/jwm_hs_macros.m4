# Haskell support macros by John Meacham.
# Depends on David Roundy's GHC macros.
AC_DEFUN([HS_NEEDS_PACKAGE],[GHC_CHECK_MODULE($1,$2,,,[MPACK="$MPACK module '$1' from package '$2'
"])])

AC_DEFUN([HS_HAS_INSTANCE],[
AC_SUBST([HAS_$1])
AC_MSG_CHECKING([for instance $2])
TRY_COMPILE_GHC(
AC_FOREACH(imp,$3,[
import imp
])
instance $2
main = putStrLn "hello"
,HAS_$1=0
AC_MSG_RESULT(no),HAS_$1=1
AC_MSG_RESULT(yes))
])
