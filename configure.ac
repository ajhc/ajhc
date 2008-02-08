AC_INIT([jhc],[0.3])
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

AC_PROGRAM_REQUIRE(drift-ghc,drift-ghc,     [  --with-drift-ghc=<drift-ghc>    Specify location of drift-ghc binary.])

AC_PROGRAM_REQUIRE(hsc2hs,hsc2hs,     [  --with-hsc2hs=<hsc2hs location>    Specify location of hsc2hs.])


AC_SUBST(HC)
AC_SUBST(HCFLAGS)
AC_SUBST(HSC2HS)
AC_CONFIG_FILES([Makefile jhc.spec])
AC_OUTPUT
