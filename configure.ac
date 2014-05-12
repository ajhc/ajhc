AC_INIT([jhc],[0.8.1])
AC_CONFIG_SRCDIR(src/Main.hs)
AC_CONFIG_MACRO_DIR(ac-macros)
AC_CONFIG_AUX_DIR(ac-macros)
AM_INIT_AUTOMAKE([foreign no-dependencies std-options])

VERSION_MAJOR=$(echo $VERSION | cut -d'.' -f1)
VERSION_MINOR=$(echo $VERSION | cut -d'.' -f2)
VERSION_PATCH=$(echo $VERSION | cut -d'.' -f3)

SHORTVERSION="$VERSION_MAJOR.$VERSION_MINOR"

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

USE_READLINE=0
USE_EDITLINE=0
GHC=$HC
GHCFLAGS="-hide-all-packages -package base"
MPACK=""

AC_DEFUN([NEEDS_PACKAGE],[GHC_CHECK_MODULE($1,$2,,,[MPACK="$MPACK module '$1' from package '$2'
"])])
NEEDS_PACKAGE([Codec.Compression.Zlib],zlib)
NEEDS_PACKAGE([Control.Monad.Reader],[mtl])
NEEDS_PACKAGE([Data.Array],[array])
NEEDS_PACKAGE([Data.Binary],[binary])
NEEDS_PACKAGE([Data.ByteString.UTF8],[utf8-string])
NEEDS_PACKAGE([Data.ByteString.Unsafe],[bytestring])
NEEDS_PACKAGE([Data.Generics.Instances],syb)
NEEDS_PACKAGE([Data.Graph.Inductive.Graph],fgl)
NEEDS_PACKAGE([Data.IntSet],[containers])
NEEDS_PACKAGE([Data.Yaml.Syck],HsSyck)
NEEDS_PACKAGE([System.Cmd],[process])
NEEDS_PACKAGE([System.Directory],[directory])
NEEDS_PACKAGE([System.FilePath],[filepath])
NEEDS_PACKAGE([System.Posix],[unix])
NEEDS_PACKAGE([System.Random],[random])
NEEDS_PACKAGE([System.Time],old-time)
NEEDS_PACKAGE([Text.PrettyPrint.HughesPJ],pretty)
NEEDS_PACKAGE([Text.Regex],regex-compat)
if test -n "$MPACK"; then
    echo "Some packages were not found:"
    echo "$MPACK"
    exit 1
fi

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

HAS_MONOID_DOC=1
TRY_COMPILE_GHC([
import Text.PrettyPrint.HughesPJ
import Data.Monoid
instance Monoid Doc
main = putStrLn "hello"
],[HAS_MONOID_DOC=0],[HAS_MONOID_DOC=1])

jlibpath=${datadir}/$PACKAGE-$SHORTVERSION
jetcpath=${sysconfdir}/$PACKAGE-$SHORTVERSION

AC_DEFINE_DIR(LIBDIR,libdir)
AC_DEFINE_DIR(DATADIR,datadir)
AC_DEFINE_DIR(JLIBPATH,jlibpath)
AC_DEFINE_DIR(JETCPATH,jetcpath)

dnl AX_CREATE_STDINT_H([cbits/_stdint.h])

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
AC_SUBST(RPMRELEASE)
AC_SUBST(SHORTVERSION)
AC_SUBST(VERSION)
AC_SUBST(VERSION_MAJOR)
AC_SUBST(VERSION_MINOR)
AC_SUBST(VERSION_PATCH)
AC_CONFIG_FILES([Makefile jhc.spec src/hs_src_config.h src/Version/Config.hs docs/building.mkd src/cbits/config.h src/data/targets.ini])
AC_OUTPUT
