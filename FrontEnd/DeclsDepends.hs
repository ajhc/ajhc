{-------------------------------------------------------------------------------

        Copyright:              The Hatchet Team (see file Contributors)
        Module:                 DeclsDepends
        Description:            Collect the names that a variable declaration
                                depends upon, for use in dependency
                                analysis.
        Primary Authors:        Bernie Pope, Robert Shelton
        Notes:                  See the file License for license information

-------------------------------------------------------------------------------}

module FrontEnd.DeclsDepends (getDeclDeps, debugDeclBindGroups) where

import Control.Monad.Writer

import FrontEnd.HsSyn
import FrontEnd.DependAnalysis(debugBindGroups)
import FrontEnd.Utils(getDeclName)
import FrontEnd.Rename(unRename)
import Name.Name
import FrontEnd.Syn.Traverse

--------------------------------------------------------------------------------

-- for printing out decl bindgroups

debugDeclBindGroups :: [[HsDecl]] -> String
debugDeclBindGroups groups
   = debugBindGroups groups (show . unRename . nameName . getDeclName)
                            (nameName . getDeclName)
                            getDeclDeps

-- HsDecl getDeps function


getDeclDeps :: HsDecl -> [HsName]

getDeclDeps (HsPatBind _pat _ rhs wheres) = getRhsDeps rhs ++ foldr (++) [] (map getLocalDeclDeps wheres)
getDeclDeps (HsActionDecl _ _ e) = getExpDeps e
getDeclDeps (HsFunBind matches) = foldr (++) [] (map getMatchDeps matches)
getDeclDeps _ = []


getMatchDeps :: HsMatch -> [HsName]
getMatchDeps (HsMatch _sloc _name _pats rhs wheres) = getRhsDeps rhs ++ foldr (++) [] (map getLocalDeclDeps wheres)

-- get the dependencies from the local definitions in a function

getLocalDeclDeps :: HsDecl -> [HsName]
getLocalDeclDeps (HsFunBind matches) = foldr (++) [] (map getMatchDeps matches)

getLocalDeclDeps (HsPatBind _sloc _hspat rhs wheres) = getRhsDeps rhs ++ foldr (++) [] (map getLocalDeclDeps wheres)
getLocalDeclDeps (HsActionDecl _sloc _ e) = getExpDeps e

getLocalDeclDeps _ = []

-- get the dependencies from the rhs of a function

getRhsDeps :: HsRhs -> [HsName]
getRhsDeps (HsUnGuardedRhs e) = getExpDeps e
getRhsDeps (HsGuardedRhss rhss) = foldr (++) [] (map getGuardedRhsDeps rhss)

getGuardedRhsDeps :: HsGuardedRhs -> [HsName]
getGuardedRhsDeps (HsGuardedRhs _sloc guardExp rhsExp)
   = getExpDeps guardExp ++ getExpDeps rhsExp



getExpDeps :: HsExp -> [HsName]
getExpDeps e = execWriter (expDeps e)

expDeps (HsVar name) = tell [name]
expDeps (HsLet decls e) = do
    expDeps e
    tell $ foldr (++) [] (map getLocalDeclDeps decls)
expDeps (HsCase e alts) = do
    expDeps e
    tell $ foldr (++) [] (map getAltDeps alts)
expDeps (HsDo stmts) = do
    tell $ foldr (++) [] (map getStmtDeps stmts)
expDeps (HsListComp e stmts) = do
    expDeps e
    tell $ foldr (++) [] (map getStmtDeps stmts)
expDeps e = traverseHsExp_ expDeps e

getAltDeps :: HsAlt -> [HsName]

getAltDeps (HsAlt _sloc _pat guardedAlts wheres)
   = getGuardedAltsDeps guardedAlts ++
     foldr (++) [] (map getLocalDeclDeps wheres)

getGuardedAltsDeps :: HsRhs -> [HsName]
getGuardedAltsDeps (HsUnGuardedRhs e) = getExpDeps e

getGuardedAltsDeps (HsGuardedRhss gAlts) = foldr (++) [] (map getGAltsDeps gAlts)

getGAltsDeps :: HsGuardedRhs -> [HsName]
getGAltsDeps (HsGuardedRhs _sloc e1 e2)
   = getExpDeps e1 ++
     getExpDeps e2

getStmtDeps :: HsStmt -> [HsName]
getStmtDeps (HsGenerator _srcLoc _pat e) = getExpDeps e

getStmtDeps (HsQualifier e) = getExpDeps e

getStmtDeps (HsLetStmt decls)
   = foldr (++) [] (map getLocalDeclDeps decls)
