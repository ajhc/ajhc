AC_INIT([jhc],[0.7.2])
AC_CONFIG_SRCDIR(src/Main.hs)
AC_CONFIG_MACRO_DIR(ac-macros)
AC_CONFIG_AUX_DIR(ac-macros)
AM_INIT_AUTOMAKE([foreign no-dependencies std-options])

SHORTVERSION=0.7


AC_CANONICAL_HOST

AC_PROG_LN_S

AC_C_BIGENDIAN([BYTE_ORDER=BIG_ENDIAN],[BYTE_ORDER=LITTLE_ENDIAN],[BYTE_ORDER=PDP_ENDIAN])

AC_ARG_WITH(rpmrelease,
            [  --with-rpmrelease=RPMRELEASE  specify rpm release number],
	    [RPMRELEASE=$withval], [RPMRELEASE=1])

AC_PROGRAM_REQUIRE(hc,ghc,     [  --with-hc=<haskell compiler>    Specify location of ghc.])
if test -z "$HC"; then
    echo "The ghc compiler was not found, please specify a location for it with the --with-hc flag"
    exit 1
fi
AC_ARG_WITH(hcflags,
            [  --with-hcflags=HCFLAGS  specify flags for Haskell compiler],
	    [HCFLAGS=$withval])

AC_PATH_PROG([DRIFTGHC],[DrIFT], [])
if test -z "$DRIFTGHC"; then
    echo [DrIFT not found: using preprocessed files]
fi

GHC=$HC
GHCFLAGS="-hide-all-packages -package base"
GHC_CHECK_MODULE(Data.Generics.Instances,syb,,,[])

AC_PROGRAM_REQUIRE(hsc2hs,hsc2hs,     [  --with-hsc2hs=<hsc2hs location>    Specify location of hsc2hs.])
if test -z "$HSC2HS"; then
    echo "The hsc2hs program not found, please specify a location for it with the --with-hsc2hs flag"
    exit 1
fi

jlibpath=${datadir}/$PACKAGE-$SHORTVERSION
jetcpath=${sysconfdir}/$PACKAGE-$SHORTVERSION

AC_DEFINE_DIR(LIBDIR,libdir)
AC_DEFINE_DIR(DATADIR,datadir)
AC_DEFINE_DIR(JLIBPATH,jlibpath)
AC_DEFINE_DIR(JETCPATH,jetcpath)

dnl AX_CREATE_STDINT_H([cbits/_stdint.h])


AC_SUBST(HC)
AC_SUBST(HCFLAGS)
AC_SUBST(HSC2HS)
AC_SUBST(GHCFLAGS)
AC_SUBST(EXTRA_PACKAGES)
AC_SUBST(RPMRELEASE)
AC_SUBST(SHORTVERSION)
AC_SUBST(BYTE_ORDER)
AC_SUBST(JLIBPATH)
AC_SUBST(JETCPATH)
AC_CONFIG_FILES([Makefile jhc.spec src/Version/Config.hs lib/base/System/Info.hs docs/building.mkd src/cbits/config.h])
AC_OUTPUT
