INC= -i -i../drift_processed -i../src
GHCOPTS= -XBangPatterns $(INC) \
	-O -package QuickCheck -ignore-package lang  -optc-std=c99 \
	-XTypeFamilies -XViewPatterns -XUndecidableInstances -XOverlappingInstances \
	-XRecordWildCards -XRecursiveDo -XTupleSections -XParallelListComp \
	-XGeneralizedNewtypeDeriving -XScopedTypeVariables -XFlexibleInstances \
	-XTypeSynonymInstances -XMultiParamTypeClasses -XDeriveDataTypeable \
	-XForeignFunctionInterface -XUnliftedFFITypes -XImplicitParams \
	-XScopedTypeVariables -XUnboxedTuples -XTypeSynonymInstances -XStandaloneDeriving \
	-XDeriveDataTypeable -XDeriveFunctor -XDeriveFoldable -XDeriveTraversable \
	-XDeriveGeneric -XFlexibleContexts -XFlexibleInstances -XConstrainedClassMethods \
	-XMultiParamTypeClasses -XFunctionalDependencies -XMagicHash -XPolymorphicComponents \
	-XExistentialQuantification -XUnicodeSyntax -XPostfixOperators -XPatternGuards \
	-XLiberalTypeSynonyms -XRankNTypes -XTypeOperators \
	-XParallelListComp -XEmptyDataDecls -XKindSignatures -XGeneralizedNewtypeDeriving \
	-XNoMonoLocalBinds -XNamedFieldPuns  -I../src
# Setting -optc-g occur link error on gcc. (on GHC 7.4.1)
# Error: can't resolve `.debug-ghc-link-info' {.debug-ghc-link-info section} - `.Ltext0' {.text section}

HC= ghc

all: md5 sha1 TestParse cff SelfTest UnionSolve TempDir

UnionSolve: UnionSolve.hs ../src/Util/UnionSolve.hs
	ghc $(INC) --make $<

TempDir: TempDir.hs ../src/Support/TempDir.hs

%: %.hs
	$(HC) $(GHCOPTS) --make $< -o $@  -I../src/cbits ../src/cbits/md5sum.c ../src/StringTable/StringTable_cbits.c ../src/cbits/lookup3.c

test: all
	./SelfTest

clean:
	rm -f -- md5 sha1 TestParse cff SelfTest TempDir UnionSolve
