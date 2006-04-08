module FrontEnd.Syn.Traverse where

import HsSyn
import Control.Monad.Identity
import FrontEnd.SrcLoc

traverse_ :: Monad m => (a -> m b) -> a -> m a
traverse_ fn x = fn x >> return x


traverseHsExp_ :: MonadSetSrcLoc m => (HsExp -> m ()) -> HsExp -> m ()
traverseHsExp_ fn e = traverseHsExp (traverse_ fn) e >> return ()


traverseHsExp :: MonadSetSrcLoc m => (HsExp -> m HsExp) -> HsExp -> m HsExp
traverseHsExp fn e = f e where
    fns = mapM fn
    f (HsAsPat n e) = do
        e' <- fn e
        return $ HsAsPat n e'
    f e@HsVar {} = return e
    f e@HsCon {} = return e
    f e@HsLit {} = return e
    f (HsInfixApp hsExp1 hsExp2 hsExp3) = do
        hsExp1' <- fn hsExp1
        hsExp2' <- fn hsExp2
        hsExp3' <- fn hsExp3
        return (HsInfixApp hsExp1' hsExp2' hsExp3')
    f (HsApp hsExp1 hsExp2)  = do
        hsExp1' <- fn hsExp1
        hsExp2' <- fn hsExp2
        return (HsApp hsExp1' hsExp2')
    f (HsNegApp hsExp)  = do
        hsExp' <- fn hsExp
        return (HsNegApp hsExp')
    f (HsLambda srcLoc hsPats hsExp) = withSrcLoc srcLoc $ do
        hsExp' <- fn hsExp
        return (HsLambda srcLoc hsPats hsExp')
    f (HsIf hsExp1 hsExp2 hsExp3)  = do
        hsExp1' <- fn hsExp1
        hsExp2' <- fn hsExp2
        hsExp3' <- fn hsExp3
        return (HsIf hsExp1' hsExp2' hsExp3')
    f (HsTuple hsExps)  = do
        hsExps' <- fns hsExps
        return (HsTuple hsExps')
    f (HsList hsExps)  = do
        hsExps' <- fns hsExps
        return (HsList hsExps')
    f (HsParen hsExp)  = do
        hsExp' <- fn hsExp
        return (HsParen hsExp')
    f (HsLeftSection hsExp1 hsExp2)  = do
        hsExp1' <- fn hsExp1
        hsExp2' <- fn hsExp2
        return (HsLeftSection hsExp1' hsExp2')
    f (HsRightSection hsExp1 hsExp2)  = do
        hsExp1' <- fn hsExp1
        hsExp2' <- fn hsExp2
        return (HsRightSection hsExp1' hsExp2')
    f (HsEnumFrom hsExp)  = do
        hsExp' <- fn hsExp
        return (HsEnumFrom hsExp')
    f (HsEnumFromTo hsExp1 hsExp2)  = do
        hsExp1' <- fn hsExp1
        hsExp2' <- fn hsExp2
        return (HsEnumFromTo hsExp1' hsExp2')
    f (HsEnumFromThen hsExp1 hsExp2)  = do
        hsExp1' <- fn hsExp1
        hsExp2' <- fn hsExp2
        return (HsEnumFromThen hsExp1' hsExp2')
    f (HsEnumFromThenTo hsExp1 hsExp2 hsExp3)  = do
        hsExp1' <- fn hsExp1
        hsExp2' <- fn hsExp2
        hsExp3' <- fn hsExp3
        return (HsEnumFromThenTo hsExp1' hsExp2' hsExp3')
    f (HsExpTypeSig srcLoc hsExp hsQualType)  = withSrcLoc srcLoc $ do
        hsExp' <- fn hsExp
        return (HsExpTypeSig srcLoc hsExp' hsQualType)
    f (HsAsPat hsName hsExp)  = do
        hsExp' <- fn hsExp
        return (HsAsPat hsName hsExp')
    f (HsWildCard x) = do return (HsWildCard x)
    f (HsIrrPat hsExp)  = do
        hsExp' <- fn hsExp
        return (HsIrrPat hsExp')
    f (HsRecConstr n fus) = do
        fus' <- mapM fFieldUpdate fus
        return $ HsRecConstr n fus'
    f (HsRecUpdate e fus) = do
        fus' <- mapM fFieldUpdate fus
        e' <- fn e
        return $ HsRecUpdate e' fus'
    fFieldUpdate (HsFieldUpdate n e) = do
        e' <- fn e
        return $ HsFieldUpdate n e'

    {-
-- not done
    f (HsRecUpdate hsExp hsFieldUpdates)  = do
        hsExp' <- fn hsExp
        hsFieldUpdates' <- renameHsFieldUpdates hsFieldUpdates
        return (HsRecUpdate hsExp' hsFieldUpdates')
    fn (HsRecConstr hsName hsFieldUpdates)  = do
        hsName' <- renameHsName hsName   -- do I need to change this name?
        hsFieldUpdates' <- renameHsFieldUpdates hsFieldUpdates
        return (HsRecConstr hsName' hsFieldUpdates')
--    fn (HsCase hsExp hsAlts)  = do
--        hsExp' <- fn hsExp
--        hsAlts' <- renameHsAlts hsAlts
--        return (HsCase hsExp' hsAlts')
--    fn (HsDo hsStmts)  = do
--        let e = doToExp hsStmts
--        fn e
        --(hsStmts',_) <- renameHsStmts hsStmts
        --return (doToExp hsStmts')
    fn (HsListComp hsExp hsStmts)  = do
        (hsStmts',') <- renameHsStmts hsStmts
        hsExp' <- fn hsExp '
        return (HsListComp hsExp' hsStmts')
    fn (HsLet hsDecls hsExp)  = do
        ' <- updateSubTableWithHsDecls  hsDecls LetFun
        hsDecls' <- renameHsDecls hsDecls '
        hsExp' <- fn hsExp '
        return (HsLet hsDecls' hsExp')

-}


traverseHsType f (HsTyFun a b) = do
    a <- f a
    b <- f b
    return $ HsTyFun a b
traverseHsType f (HsTyTuple xs) = do
    xs <- mapM f xs
    return $ HsTyTuple xs
traverseHsType f (HsTyApp a b) = do
    a <- f a
    b <- f b
    return $ HsTyApp a b
traverseHsType f (HsTyForall vs qt) = do
    x <- f $ hsQualTypeType qt
    return $ HsTyForall vs qt { hsQualTypeType = x }
traverseHsType f (HsTyExists vs qt) = do
    x <- f $ hsQualTypeType qt
    return $ HsTyExists vs qt { hsQualTypeType = x }
traverseHsType _ x = return x


