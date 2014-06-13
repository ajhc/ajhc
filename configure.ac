AC_INIT([jhc],[0.8.2])
AC_CONFIG_SRCDIR(src/Main.hs)
AC_CONFIG_MACRO_DIR(ac-macros)
AC_CONFIG_AUX_DIR(ac-macros)
AM_INIT_AUTOMAKE([foreign no-dependencies std-options])

VERSION_MAJOR=$(echo $VERSION | cut -d'.' -f1)
VERSION_MINOR=$(echo $VERSION | cut -d'.' -f2)
VERSION_PATCH=$(echo $VERSION | cut -d'.' -f3)

SHORTVERSION="$VERSION_MAJOR.$VERSION_MINOR"

# tracks changes in binary format, should be incremented when the format of .ho
# or .hl files changes in an incompatible way.
HO_VERSION=17

AS_TMPDIR([jcfg])
AC_CANONICAL_HOST
AC_PROG_LN_S
AC_C_BIGENDIAN([BYTE_ORDER=BIG_ENDIAN],[BYTE_ORDER=LITTLE_ENDIAN],[BYTE_ORDER=PDP_ENDIAN])

AC_ARG_WITH(rpmrelease,
            [  --with-rpmrelease=RPMRELEASE  specify rpm release number],
	    [RPMRELEASE=$withval], [RPMRELEASE=1])

AC_PROGRAM_REQUIRE(hc,ghc,[  --with-hc=<haskell compiler>    Specify location of ghc.])
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
AC_PATH_PROG([CPPHS],[cpphs], [])
if test -z "$CPPHS"; then
    echo [cpphs not found, some libraries may not be able to be built.]
fi

AC_CHECK_PROGS([MINGW],[mingw-gcc mingw32-gcc i386-mingw32-gcc i586-mingw32msvc-gcc],
               [i386-mingw32-gcc])
AC_CHECK_PROGS([MINGW64],[mingw64-gcc x86_64-w64-mingw32-gcc],
               [x86_64-w64-mingw32-gcc])

GHC=$HC
GHCFLAGS="-hide-all-packages -package base"

MPACK=""
HS_NEEDS_PACKAGE([Codec.Compression.Zlib],zlib)
HS_NEEDS_PACKAGE([Control.Monad.Reader],[mtl])
HS_NEEDS_PACKAGE([Data.Array],[array])
HS_NEEDS_PACKAGE([Data.Binary],[binary])
HS_NEEDS_PACKAGE([Data.ByteString.UTF8],[utf8-string])
HS_NEEDS_PACKAGE([Data.ByteString.Unsafe],[bytestring])
HS_NEEDS_PACKAGE([Data.IntSet],[containers])
HS_NEEDS_PACKAGE([Data.Yaml.Syck],HsSyck)
HS_NEEDS_PACKAGE([System.Cmd],[process])
HS_NEEDS_PACKAGE([System.Directory],[directory])
HS_NEEDS_PACKAGE([System.FilePath],[filepath])
HS_NEEDS_PACKAGE([System.Posix],[unix])
HS_NEEDS_PACKAGE([System.Random],[random])
HS_NEEDS_PACKAGE([System.Time],old-time)
HS_NEEDS_PACKAGE([Text.PrettyPrint.HughesPJ],pretty)
HS_NEEDS_PACKAGE([Text.Regex],regex-compat)
if test -n "$MPACK"; then
    echo "Some packages were not found:"
    echo "$MPACK"
    exit 1
fi

USE_READLINE=0
USE_EDITLINE=0
AC_ARG_ENABLE([line],[  --disable-line do not depend on readline or editline],[],[
    GHC_CHECK_MODULE(System.Console.Readline,readline,,[USE_READLINE=1],[
        GHC_CHECK_MODULE(System.Console.Editline.Readline,editline,,[USE_EDITLINE=1] ,[])
    ])
])

GHC_CHECK_MODULE(System,base,,[] ,[ GHCINC="$GHCINC -i$srcdir/compat/haskell98"])

AC_PROGRAM_REQUIRE(hsc2hs,hsc2hs,     [  --with-hsc2hs=<hsc2hs location>    Specify location of hsc2hs.])
if test -z "$HSC2HS"; then
    echo "The hsc2hs program not found, please specify a location for it with the --with-hsc2hs flag"
    exit 1
fi

HS_HAS_INSTANCE(MONOID_DOC,Monoid Doc,[Text.PrettyPrint.HughesPJ Data.Monoid])
HS_HAS_INSTANCE(SHOW_IDENTITY,Show (Identity a),[Control.Monad.Identity])
HS_HAS_INSTANCE(TRAVERSABLE_INTMAP,Traversable Data.IntMap.IntMap,[Data.Traversable Data.IntMap])
HS_HAS_INSTANCE(FUNCTOR_TUPLE,Functor ((,) a), [Control.Monad.Instances])
HS_HAS_INSTANCE(FOLDABLE_TUPLE,Foldable ((,) a), [Data.Foldable])
HS_HAS_INSTANCE(FUNCTOR_TUPLE3,Functor ((,,) a b), [Control.Monad.Instances])
HS_HAS_INSTANCE(FOLDABLE_TUPLE3,Foldable ((,,) a b), [Data.Foldable])

jlibpath=${datadir}/$PACKAGE-$SHORTVERSION
jetcpath=${sysconfdir}/$PACKAGE-$SHORTVERSION

AC_DEFINE_DIR(LIBDIR,libdir)
AC_DEFINE_DIR(DATADIR,datadir)
AC_DEFINE_DIR(JLIBPATH,jlibpath)
AC_DEFINE_DIR(JETCPATH,jetcpath)

dnl AX_CREATE_STDINT_H([cbits/_stdint.h])

[echo "Deleting tmpdir $tmp"
 rmdir $tmp]

AC_SUBST(BYTE_ORDER)
AC_SUBST(EXTRA_PACKAGES)
AC_SUBST(GHCFLAGS)
AC_SUBST(GHCINC)
AC_SUBST(HAS_MONOID_DOC)
AC_SUBST(USE_READLINE)
AC_SUBST(USE_EDITLINE)
AC_SUBST(HC)
AC_SUBST(HCFLAGS)
AC_SUBST(HSC2HS)
AC_SUBST(JETCPATH)
AC_SUBST(JLIBPATH)
AC_SUBST(MINGW)
AC_SUBST(MINGW64)
AC_SUBST(RPMRELEASE)
AC_SUBST(SHORTVERSION)
AC_SUBST(VERSION)
AC_SUBST(VERSION_MAJOR)
AC_SUBST(VERSION_MINOR)
AC_SUBST(VERSION_PATCH)
AC_SUBST(HO_VERSION)
AC_CONFIG_FILES([Makefile jhc.spec src/hs_src_config.h src/Version/Config.hs docs/building.mkd src/cbits/config.h src/data/targets.ini])
AC_OUTPUT
