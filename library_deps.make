base-1.0.hl: lib/base/base.yaml lib/base/System/Info.hs lib/base/Data/Int.hs lib/base/Debug/Trace.hs lib/base/Data/Array.hs \
    lib/base/Data/Typeable.hs lib/base/Text/Printf.hs lib/base/System/Directory.hs lib/base/Data/Word.hs lib/base/Data/Maybe.hs \
    lib/base/System/IO/Error.hs lib/base/System/IO/Binary.hs lib/base/System/CPUTime.hs lib/base/System/IO/Pipe.hs lib/base/Control/Monad/Instances.hs \
    lib/base/Foreign/Marshal/Error.hs lib/base/System/Exit.hs lib/base/System/Environment.hs lib/base/Data/Array/IO.hs lib/base/Data/Char.hs \
    lib/base/Data/Version.hs lib/base/Text/Show/Functions.hs lib/base/Control/Monad.hs lib/base/Data/Functor.hs lib/base/Data/Monoid.hs \
    lib/base/System/Locale.hs lib/base/System/IO.hs lib/base/Foreign/Marshal/Pool.hs lib/base/Prelude.hs lib/base/Control/Monad/Fix.hs \
    lib/base/Data/Complex.hs lib/base/Data/IORef.hs lib/base/System/Console/GetOpt.hs lib/base/Data/List.hs lib/base/System/Cmd.hs \
    lib/base/System/Random.hs lib/base/Data/Unicode.hs lib/base/Foreign/ForeignPtr.hs lib/base/System/Time.hs lib/base/Control/Exception.hs \
    lib/base/Foreign/StablePtr.hs lib/base/Foreign/C.hs lib/base/Data/Bits.hs lib/base/Foreign/Marshal.hs lib/base/Foreign.hs \
    lib/base/Data/Array/Unboxed.hs lib/base/Data/Function.hs lib/base/Data/Ix.hs jhc-prim-1.0.hl jhc-1.0.hl
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
applicative-1.0.hl: lib/applicative/applicative.yaml lib/applicative/Control/Arrow.hs lib/applicative/Control/Applicative.hs lib/applicative/Data/Foldable.hs lib/applicative/Control/Category.hs \
    lib/applicative/Data/Traversable.hs jhc-prim-1.0.hl base-1.0.hl jhc-1.0.hl
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
flat-foreign-1.0.hl: lib/flat-foreign/flat-foreign.yaml lib/flat-foreign/StablePtr.hs lib/flat-foreign/MarshalAlloc.hs lib/flat-foreign/CForeign.hs lib/flat-foreign/Storable.hs \
    lib/flat-foreign/Ptr.hs lib/flat-foreign/MarshalArray.hs lib/flat-foreign/Int.hs lib/flat-foreign/CString.hs lib/flat-foreign/Word.hs \
    lib/flat-foreign/CError.hs lib/flat-foreign/CTypes.hs lib/flat-foreign/MarshalError.hs lib/flat-foreign/MarshalUtils.hs lib/flat-foreign/Bits.hs \
    lib/flat-foreign/ForeignPtr.hs jhc-prim-1.0.hl base-1.0.hl jhc-1.0.hl
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
haskell98-1.0.hl: lib/haskell98/haskell98.yaml lib/haskell98/System.hs lib/haskell98/List.hs lib/haskell98/Time.hs lib/haskell98/Array.hs \
    lib/haskell98/Random.hs lib/haskell98/Complex.hs lib/haskell98/Locale.hs lib/haskell98/CPUTime.hs lib/haskell98/Ratio.hs \
    lib/haskell98/Monad.hs lib/haskell98/Directory.hs lib/haskell98/IO.hs lib/haskell98/Ix.hs lib/haskell98/Char.hs \
    lib/haskell98/Maybe.hs jhc-prim-1.0.hl base-1.0.hl jhc-1.0.hl
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
jhc-1.0.hl: lib/jhc/jhc.yaml lib/jhc/Jhc/Prim.hs lib/jhc/Jhc/IO.hs lib/jhc/Jhc/Int.hs lib/jhc/Jhc/Inst/Enum.hs \
    lib/jhc/Prelude/IO.hs lib/jhc/Jhc/Order.hs lib/jhc/System/C/Stdio.hs lib/jhc/Foreign/C/String.hs lib/jhc/Jhc/Show.hs \
    lib/jhc/Jhc/Handle.hs lib/jhc/Jhc/Addr.hs lib/jhc/Numeric.hs lib/jhc/Jhc/Float.hs lib/jhc/Jhc/Numeric.hs \
    lib/jhc/Jhc/Inst/Storable.hs lib/jhc/Jhc/Inst/Num.hs lib/jhc/Foreign/C/Types.hs lib/jhc/Data/Ratio.hs lib/jhc/Jhc/List.hs \
    lib/jhc/Jhc/Type/Float.hs lib/jhc/Jhc/Class/Num.hs lib/jhc/Foreign/Marshal/Utils.hs lib/jhc/Jhc/Basics.hs lib/jhc/System/Mem.hs \
    lib/jhc/Jhc/Type/Basic.hs lib/jhc/Jhc/ForeignPtr.hs lib/jhc/Jhc/Class/Real.hs lib/jhc/Jhc/Num.hs lib/jhc/Jhc/Text/Read.hs \
    lib/jhc/Jhc/Type/Ptr.hs lib/jhc/Foreign/C/Error.hs lib/jhc/Foreign/Storable.hs lib/jhc/Jhc/Type/Word.hs lib/jhc/Foreign/Ptr.hs \
    lib/jhc/Jhc/Inst/Order.hs lib/jhc/Jhc/Enum.hs lib/jhc/Jhc/Type/Handle.hs lib/jhc/Jhc/Tuples.hs lib/jhc/Jhc/String.hs \
    lib/jhc/Prelude/Float.hs lib/jhc/Jhc/Type/C.hs lib/jhc/Jhc/Class/Ord.hs lib/jhc/Jhc/Inst/PrimEnum.hs lib/jhc/Jhc/Inst/Show.hs \
    lib/jhc/Prelude/Text.hs lib/jhc/Foreign/Marshal/Alloc.hs lib/jhc/Foreign/Marshal/Array.hs lib/jhc/Jhc/Options.hs lib/jhc/Prelude/CType.hs \
    lib/jhc/Jhc/Monad.hs lib/jhc/System/IO/Unsafe.hs lib/jhc/System/Mem/StableName.hs lib/jhc/Jhc/Inst/Read.hs lib/jhc/Jhc/Maybe.hs \
    jhc-prim-1.0.hl
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
jhc-prim-1.0.hl: lib/jhc-prim/jhc-prim.yaml lib/jhc-prim/Jhc/Prim/IO.hs lib/jhc-prim/Jhc/Prim/Prim.hs lib/jhc-prim/Jhc/Prim/Array.hs lib/jhc-prim/Jhc/Prim/Wrapper.hs \
    lib/jhc-prim/Jhc/Prim/Rts.hs lib/jhc-prim/Jhc/Prim/Bits.hs
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
haskell2010-0.8.0.hl: lib/haskell2010/haskell2010.yaml.m4 jhc-prim-1.0.hl base-1.0.hl jhc-1.0.hl
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
JHC_LIBS = base-1.0.hl applicative-1.0.hl flat-foreign-1.0.hl haskell98-1.0.hl jhc-1.0.hl \
    jhc-prim-1.0.hl haskell2010-0.8.0.hl
containers-0.3.0.0.hl: lib/ext/containers.cabal jhc-prim-1.0.hl applicative-1.0.hl base-1.0.hl jhc-1.0.hl \
    lib/ext/containers.patch
	perl utils/build_extlibs.prl $<
Diff-0.1.2.hl: lib/ext/Diff.cabal jhc-prim-1.0.hl base-1.0.hl jhc-1.0.hl
	perl utils/build_extlibs.prl $<
html-1.0.1.2.hl: lib/ext/html.cabal jhc-prim-1.0.hl base-1.0.hl jhc-1.0.hl
	perl utils/build_extlibs.prl $<
HUnit-1.2.2.1.hl: lib/ext/HUnit.cabal jhc-prim-1.0.hl base-1.0.hl jhc-1.0.hl
	perl utils/build_extlibs.prl $<
parsec-2.1.0.1.hl: lib/ext/parsec.cabal jhc-prim-1.0.hl base-1.0.hl jhc-1.0.hl
	perl utils/build_extlibs.prl $<
pretty-1.0.1.1.hl: lib/ext/pretty.cabal jhc-prim-1.0.hl base-1.0.hl jhc-1.0.hl
	perl utils/build_extlibs.prl $<
QuickCheck-1.2.0.0.hl: lib/ext/QuickCheck.cabal jhc-prim-1.0.hl base-1.0.hl jhc-1.0.hl
	perl utils/build_extlibs.prl $<
safe-0.2.hl: lib/ext/safe.cabal jhc-prim-1.0.hl base-1.0.hl jhc-1.0.hl
	perl utils/build_extlibs.prl $<
smallcheck-0.4.hl: lib/ext/smallcheck.cabal jhc-prim-1.0.hl haskell98-1.0.hl base-1.0.hl jhc-1.0.hl \
    lib/ext/smallcheck.patch
	perl utils/build_extlibs.prl $<
xhtml-3000.2.0.1.hl: lib/ext/xhtml.cabal jhc-prim-1.0.hl base-1.0.hl jhc-1.0.hl
	perl utils/build_extlibs.prl $<
transformers-0.2.1.0.hl: lib/ext/transformers.cabal jhc-prim-1.0.hl applicative-1.0.hl base-1.0.hl jhc-1.0.hl
	perl utils/build_extlibs.prl $<
filepath-1.2.0.0.hl: lib/ext/filepath.cabal jhc-prim-1.0.hl base-1.0.hl jhc-1.0.hl
	perl utils/build_extlibs.prl $<
deepseq-1.1.0.2.hl: lib/ext/deepseq.cabal jhc-prim-1.0.hl applicative-1.0.hl containers-0.3.0.0.hl base-1.0.hl \
    jhc-1.0.hl
	perl utils/build_extlibs.prl $<
bytestring-0.9.2.0.hl: lib/ext/bytestring.yaml jhc-prim-1.0.hl base-1.0.hl jhc-1.0.hl lib/ext/bytestring.patch
	perl utils/build_extlibs.prl $<
JHC_EXT_LIBS = containers-0.3.0.0.hl Diff-0.1.2.hl html-1.0.1.2.hl HUnit-1.2.2.1.hl parsec-2.1.0.1.hl \
    pretty-1.0.1.1.hl QuickCheck-1.2.0.0.hl safe-0.2.hl smallcheck-0.4.hl xhtml-3000.2.0.1.hl \
    transformers-0.2.1.0.hl filepath-1.2.0.0.hl deepseq-1.1.0.2.hl bytestring-0.9.2.0.hl
