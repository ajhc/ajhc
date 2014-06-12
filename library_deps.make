applicative-1.0.hl: lib/applicative/applicative.yaml haskell-extras-0.8.2.hl jhc-1.0.hl jhc-prim-1.0.hl lib/applicative/Control/Applicative.hs \
    lib/applicative/Control/Arrow.hs lib/applicative/Control/Category.hs lib/applicative/Data/Foldable.hs lib/applicative/Data/Traversable.hs
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
flat-foreign-1.0.hl: lib/flat-foreign/flat-foreign.yaml haskell-extras-0.8.2.hl haskell2010-0.8.2.hl jhc-1.0.hl jhc-prim-1.0.hl \
    lib/flat-foreign/Bits.hs lib/flat-foreign/CError.hs lib/flat-foreign/CForeign.hs lib/flat-foreign/CString.hs lib/flat-foreign/CTypes.hs \
    lib/flat-foreign/ForeignPtr.hs lib/flat-foreign/Int.hs lib/flat-foreign/MarshalAlloc.hs lib/flat-foreign/MarshalArray.hs lib/flat-foreign/MarshalError.hs \
    lib/flat-foreign/MarshalUtils.hs lib/flat-foreign/Ptr.hs lib/flat-foreign/StablePtr.hs lib/flat-foreign/Storable.hs lib/flat-foreign/Word.hs
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
haskell-extras-0.8.2.hl: lib/haskell-extras/haskell-extras.yaml.m4 jhc-1.0.hl jhc-prim-1.0.hl lib/haskell-extras/Control/Exception.hs lib/haskell-extras/Control/Monad.hs \
    lib/haskell-extras/Control/Monad/Fix.hs lib/haskell-extras/Control/Monad/Instances.hs lib/haskell-extras/Data/Array.hs lib/haskell-extras/Data/Array/IO.hs lib/haskell-extras/Data/Array/Unboxed.hs \
    lib/haskell-extras/Data/Bits.hs lib/haskell-extras/Data/Char.hs lib/haskell-extras/Data/Complex.hs lib/haskell-extras/Data/Function.hs lib/haskell-extras/Data/Functor.hs \
    lib/haskell-extras/Data/IORef.hs lib/haskell-extras/Data/Int.hs lib/haskell-extras/Data/Ix.hs lib/haskell-extras/Data/List.hs lib/haskell-extras/Data/Maybe.hs \
    lib/haskell-extras/Data/Monoid.hs lib/haskell-extras/Data/Typeable.hs lib/haskell-extras/Data/Unicode.hs lib/haskell-extras/Data/Version.hs lib/haskell-extras/Data/Word.hs \
    lib/haskell-extras/Debug/Trace.hs lib/haskell-extras/Foreign.hs lib/haskell-extras/Foreign/C.hs lib/haskell-extras/Foreign/ForeignPtr.hs lib/haskell-extras/Foreign/Marshal.hs \
    lib/haskell-extras/Foreign/Marshal/Error.hs lib/haskell-extras/Foreign/Marshal/Pool.hs lib/haskell-extras/Foreign/StablePtr.hs lib/haskell-extras/Prelude.hs lib/haskell-extras/System/CPUTime.hs \
    lib/haskell-extras/System/Cmd.hs lib/haskell-extras/System/Console/GetOpt.hs lib/haskell-extras/System/Directory.hs lib/haskell-extras/System/Environment.hs lib/haskell-extras/System/Exit.hs \
    lib/haskell-extras/System/IO.hs lib/haskell-extras/System/IO/Binary.hs lib/haskell-extras/System/IO/Error.hs lib/haskell-extras/System/IO/Pipe.hs lib/haskell-extras/System/Info.hs \
    lib/haskell-extras/System/Locale.hs lib/haskell-extras/System/Random.hs lib/haskell-extras/System/Time.hs lib/haskell-extras/Text/Printf.hs lib/haskell-extras/Text/Show/Functions.hs
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
haskell2010-0.8.2.hl: lib/haskell2010/haskell2010.yaml.m4 haskell-extras-0.8.2.hl jhc-1.0.hl jhc-prim-1.0.hl
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
haskell98-1.0.hl: lib/haskell98/haskell98.yaml haskell-extras-0.8.2.hl haskell2010-0.8.2.hl jhc-1.0.hl jhc-prim-1.0.hl \
    lib/haskell98/Array.hs lib/haskell98/CPUTime.hs lib/haskell98/Char.hs lib/haskell98/Complex.hs lib/haskell98/Directory.hs \
    lib/haskell98/IO.hs lib/haskell98/Ix.hs lib/haskell98/List.hs lib/haskell98/Locale.hs lib/haskell98/Maybe.hs \
    lib/haskell98/Monad.hs lib/haskell98/Random.hs lib/haskell98/Ratio.hs lib/haskell98/System.hs lib/haskell98/Time.hs
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
jhc-1.0.hl: lib/jhc/jhc.yaml jhc-prim-1.0.hl lib/jhc/Data/Ratio.hs lib/jhc/Data/String.hs lib/jhc/Foreign/C/Error.hs \
    lib/jhc/Foreign/C/String.hs lib/jhc/Foreign/C/Types.hs lib/jhc/Foreign/Marshal/Alloc.hs lib/jhc/Foreign/Marshal/Array.hs lib/jhc/Foreign/Marshal/Utils.hs \
    lib/jhc/Foreign/Ptr.hs lib/jhc/Foreign/Storable.hs lib/jhc/Jhc/Addr.hs lib/jhc/Jhc/Basics.hs lib/jhc/Jhc/Class/Num.hs \
    lib/jhc/Jhc/Class/Ord.hs lib/jhc/Jhc/Class/Real.hs lib/jhc/Jhc/Enum.hs lib/jhc/Jhc/Float.hs lib/jhc/Jhc/ForeignPtr.hs \
    lib/jhc/Jhc/Handle.hs lib/jhc/Jhc/IO.hs lib/jhc/Jhc/Inst/Enum.hs lib/jhc/Jhc/Inst/Num.hs lib/jhc/Jhc/Inst/Order.hs \
    lib/jhc/Jhc/Inst/PrimEnum.hs lib/jhc/Jhc/Inst/Read.hs lib/jhc/Jhc/Inst/Show.hs lib/jhc/Jhc/Inst/Storable.hs lib/jhc/Jhc/Int.hs \
    lib/jhc/Jhc/List.hs lib/jhc/Jhc/Maybe.hs lib/jhc/Jhc/Monad.hs lib/jhc/Jhc/Num.hs lib/jhc/Jhc/Numeric.hs \
    lib/jhc/Jhc/Options.hs lib/jhc/Jhc/Order.hs lib/jhc/Jhc/Prim.hs lib/jhc/Jhc/Show.hs lib/jhc/Jhc/String.hs \
    lib/jhc/Jhc/Text/Read.hs lib/jhc/Jhc/Tuples.hs lib/jhc/Jhc/Type/Basic.hs lib/jhc/Jhc/Type/C.hs lib/jhc/Jhc/Type/Float.hs \
    lib/jhc/Jhc/Type/Handle.hs lib/jhc/Jhc/Type/Ptr.hs lib/jhc/Jhc/Type/Word.hs lib/jhc/Numeric.hs lib/jhc/Prelude/CType.hs \
    lib/jhc/Prelude/Float.hs lib/jhc/Prelude/IO.hs lib/jhc/Prelude/Text.hs lib/jhc/System/C/Stdio.hs lib/jhc/System/IO/Unsafe.hs \
    lib/jhc/System/Mem.hs lib/jhc/System/Mem/StableName.hs
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
jhc-prim-1.0.hl: lib/jhc-prim/jhc-prim.yaml lib/jhc-prim/Jhc/Prim/Array.hs lib/jhc-prim/Jhc/Prim/Basics.hs lib/jhc-prim/Jhc/Prim/Bits.hs lib/jhc-prim/Jhc/Prim/IO.hs \
    lib/jhc-prim/Jhc/Prim/List.hs lib/jhc-prim/Jhc/Prim/Options.hs lib/jhc-prim/Jhc/Prim/Prim.hs lib/jhc-prim/Jhc/Prim/Rts.hs lib/jhc-prim/Jhc/Prim/Wrapper.hs
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
JHC_LIBS = applicative-1.0.hl flat-foreign-1.0.hl haskell-extras-0.8.2.hl haskell2010-0.8.2.hl haskell98-1.0.hl \
    jhc-1.0.hl jhc-prim-1.0.hl
Diff-0.3.0.hl: lib/ext/Diff.yaml applicative-1.0.hl haskell-extras-0.8.2.hl haskell2010-0.8.2.hl haskell98-1.0.hl \
    jhc-1.0.hl jhc-prim-1.0.hl pretty-1.1.1.1.hl
	perl utils/build_extlibs.prl $<
HUnit-1.2.5.2.hl: lib/ext/HUnit.yaml applicative-1.0.hl deepseq-1.2.0.1.hl haskell-extras-0.8.2.hl haskell2010-0.8.2.hl \
    haskell98-1.0.hl jhc-1.0.hl jhc-prim-1.0.hl
	perl utils/build_extlibs.prl $<
QuickCheck-1.2.0.1.hl: lib/ext/QuickCheck.yaml applicative-1.0.hl haskell-extras-0.8.2.hl haskell2010-0.8.2.hl haskell98-1.0.hl \
    jhc-1.0.hl jhc-prim-1.0.hl
	perl utils/build_extlibs.prl $<
bytestring-0.9.2.0.hl: lib/ext/bytestring.yaml applicative-1.0.hl haskell-extras-0.8.2.hl haskell2010-0.8.2.hl haskell98-1.0.hl \
    jhc-1.0.hl jhc-prim-1.0.hl lib/ext/bytestring.patch
	perl utils/build_extlibs.prl $<
containers-0.4.2.1.hl: lib/ext/containers.yaml applicative-1.0.hl deepseq-1.2.0.1.hl haskell-extras-0.8.2.hl haskell2010-0.8.2.hl \
    haskell98-1.0.hl jhc-1.0.hl jhc-prim-1.0.hl
	perl utils/build_extlibs.prl $<
deepseq-1.2.0.1.hl: lib/ext/deepseq.yaml applicative-1.0.hl haskell-extras-0.8.2.hl haskell2010-0.8.2.hl haskell98-1.0.hl \
    jhc-1.0.hl jhc-prim-1.0.hl
	perl utils/build_extlibs.prl $<
filepath-1.3.0.1.hl: lib/ext/filepath.yaml applicative-1.0.hl haskell-extras-0.8.2.hl haskell2010-0.8.2.hl haskell98-1.0.hl \
    jhc-1.0.hl jhc-prim-1.0.hl
	perl utils/build_extlibs.prl $<
html-1.0.1.2.hl: lib/ext/html.yaml applicative-1.0.hl haskell-extras-0.8.2.hl haskell2010-0.8.2.hl haskell98-1.0.hl \
    jhc-1.0.hl jhc-prim-1.0.hl
	perl utils/build_extlibs.prl $<
parsec-2.1.0.1.hl: lib/ext/parsec.yaml applicative-1.0.hl haskell-extras-0.8.2.hl haskell2010-0.8.2.hl haskell98-1.0.hl \
    jhc-1.0.hl jhc-prim-1.0.hl
	perl utils/build_extlibs.prl $<
pretty-1.1.1.1.hl: lib/ext/pretty.yaml applicative-1.0.hl haskell-extras-0.8.2.hl haskell2010-0.8.2.hl haskell98-1.0.hl \
    jhc-1.0.hl jhc-prim-1.0.hl
	perl utils/build_extlibs.prl $<
safe-0.3.4.hl: lib/ext/safe.yaml applicative-1.0.hl haskell-extras-0.8.2.hl haskell2010-0.8.2.hl haskell98-1.0.hl \
    jhc-1.0.hl jhc-prim-1.0.hl
	perl utils/build_extlibs.prl $<
smallcheck-0.6.1.hl: lib/ext/smallcheck.yaml applicative-1.0.hl haskell-extras-0.8.2.hl haskell2010-0.8.2.hl haskell98-1.0.hl \
    jhc-1.0.hl jhc-prim-1.0.hl
	perl utils/build_extlibs.prl $<
transformers-0.2.1.0.hl: lib/ext/transformers.yaml applicative-1.0.hl haskell-extras-0.8.2.hl haskell2010-0.8.2.hl haskell98-1.0.hl \
    jhc-1.0.hl jhc-prim-1.0.hl
	perl utils/build_extlibs.prl $<
wl-pprint-1.1.hl: lib/ext/wl-pprint.yaml applicative-1.0.hl haskell-extras-0.8.2.hl haskell2010-0.8.2.hl haskell98-1.0.hl \
    jhc-1.0.hl jhc-prim-1.0.hl
	perl utils/build_extlibs.prl $<
xhtml-3000.2.1.hl: lib/ext/xhtml.yaml applicative-1.0.hl haskell-extras-0.8.2.hl haskell2010-0.8.2.hl haskell98-1.0.hl \
    jhc-1.0.hl jhc-prim-1.0.hl
	perl utils/build_extlibs.prl $<
JHC_EXT_LIBS = Diff-0.3.0.hl HUnit-1.2.5.2.hl QuickCheck-1.2.0.1.hl bytestring-0.9.2.0.hl containers-0.4.2.1.hl \
    deepseq-1.2.0.1.hl filepath-1.3.0.1.hl html-1.0.1.2.hl parsec-2.1.0.1.hl pretty-1.1.1.1.hl \
    safe-0.3.4.hl smallcheck-0.6.1.hl transformers-0.2.1.0.hl wl-pprint-1.1.hl xhtml-3000.2.1.hl
