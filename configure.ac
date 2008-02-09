AC_INIT([jhc],[0.3.20080208])
AC_CONFIG_SRCDIR(Main.hs)
AC_CONFIG_MACRO_DIR(ac-macros)
AC_CONFIG_AUX_DIR(ac-macros)
AM_INIT_AUTOMAKE([foreign no-dependencies])


AC_CANONICAL_HOST

AC_PROG_LN_S


AC_PROGRAM_REQUIRE(hc,ghc,     [  --with-hc=<haskell compiler>    Specify location of ghc.])
AC_ARG_WITH(hcflags,
            [  --with-hcflags=HCFLAGS  specify flags for Haskell compiler],
	    [HCFLAGS=$withval])

AC_PATH_PROG([DRIFTGHC],[drift-ghc], [])
if test -z "$DRIFTGHC"; then
    AC_MSG_ERROR([ DrIFT not found get it from http://repetae.net/computer/haskell/DrIFT/])
fi

AC_PROGRAM_REQUIRE(hsc2hs,hsc2hs,     [  --with-hsc2hs=<hsc2hs location>    Specify location of hsc2hs.])

jlibpath=${libdir}/$PACKAGE-$VERSION

AC_DEFINE_DIR(LIBDIR,libdir)
AC_DEFINE_DIR(DATADIR,datadir)
AC_DEFINE_DIR(JLIBPATH,jlibpath)


AC_SUBST(HC)
AC_SUBST(HCFLAGS)
AC_SUBST(HSC2HS)
AC_SUBST(JLIBPATH)
AC_CONFIG_FILES([Makefile jhc.spec Version/Config.hs])
AC_OUTPUT
