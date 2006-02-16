{-------------------------------------------------------------------------------

        Copyright:              The Hatchet Team (see file Contributors)

        Module:                 DeclsDepends

        Description:            Collect the names that a variable declaration
                                depends upon, for use in dependency
                                analysis.

        Primary Authors:        Bernie Pope, Robert Shelton

        Notes:                  See the file License for license information

-------------------------------------------------------------------------------}

module DeclsDepends (getDeclDeps, debugDeclBindGroups) where

import HsSyn
import DependAnalysis(debugBindGroups)
import FrontEnd.Utils(getDeclName)
import FrontEnd.Rename(unRename)
import Name.Name

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
getDeclDeps (HsFunBind matches) = foldr (++) [] (map getMatchDeps matches)
getDeclDeps _ = []


getMatchDeps :: HsMatch -> [HsName]
getMatchDeps (HsMatch _sloc _name _pats rhs wheres) = getRhsDeps rhs ++ foldr (++) [] (map getLocalDeclDeps wheres)

-- get the dependencies from the local definitions in a function

getLocalDeclDeps :: HsDecl -> [HsName]
getLocalDeclDeps (HsFunBind matches) = foldr (++) [] (map getMatchDeps matches)

getLocalDeclDeps (HsPatBind _sloc _hspat rhs wheres) = getRhsDeps rhs ++ foldr (++) [] (map getLocalDeclDeps wheres)

getLocalDeclDeps _ = []

-- get the dependencies from the rhs of a function

getRhsDeps :: HsRhs -> [HsName]
getRhsDeps (HsUnGuardedRhs e) = getExpDeps e
getRhsDeps (HsGuardedRhss rhss) = foldr (++) [] (map getGuardedRhsDeps rhss)

getGuardedRhsDeps :: HsGuardedRhs -> [HsName]
getGuardedRhsDeps (HsGuardedRhs _sloc guardExp rhsExp)
   = getExpDeps guardExp ++ getExpDeps rhsExp

getExpDeps :: HsExp -> [HsName]
getExpDeps (HsVar name)
   = [name]

getExpDeps (HsCon _)
   = []

getExpDeps (HsLit _)
   = []

getExpDeps (HsInfixApp e1 e2 e3)
   = getExpDeps e1 ++
     getExpDeps e2 ++
     getExpDeps e3

getExpDeps (HsApp e1 e2)
   = getExpDeps e1 ++ getExpDeps e2

getExpDeps (HsNegApp e)
   = getExpDeps e

getExpDeps (HsLambda _ _ e)
   = getExpDeps e

getExpDeps (HsLet decls e)
   = foldr (++) [] (map getLocalDeclDeps decls) ++
     getExpDeps e

getExpDeps (HsIf e1 e2 e3)
   = getExpDeps e1 ++
     getExpDeps e2 ++
     getExpDeps e3

getExpDeps (HsCase e alts)
   = getExpDeps e ++
     foldr (++) [] (map getAltDeps alts)

getExpDeps (HsDo stmts)
   = foldr (++) [] (map getStmtDeps stmts)

getExpDeps (HsTuple exps)
   = foldr (++) [] (map getExpDeps exps)

getExpDeps (HsList exps)
   = foldr (++) [] (map getExpDeps exps)

getExpDeps (HsParen e)
   = getExpDeps e

getExpDeps (HsLeftSection e1 e2)
   = getExpDeps e1 ++
     getExpDeps e2

getExpDeps (HsRightSection e1 e2)
   = getExpDeps e1 ++
     getExpDeps e2

getExpDeps (HsEnumFrom e)
   = getExpDeps e

getExpDeps (HsEnumFromTo e1 e2)
   = getExpDeps e1 ++
     getExpDeps e2

getExpDeps (HsEnumFromThen e1 e2)
   = getExpDeps e1 ++
     getExpDeps e2

getExpDeps (HsEnumFromThenTo e1 e2 e3)
   = getExpDeps e1 ++
     getExpDeps e2 ++
     getExpDeps e3
getExpDeps (HsListComp e stmts) = getExpDeps e ++ foldr (++) [] (map getStmtDeps stmts)
getExpDeps (HsExpTypeSig _sloc e _qualtype) = getExpDeps e
getExpDeps (HsAsPat _name e) = getExpDeps e
getExpDeps (HsWildCard _) = []
getExpDeps (HsIrrPat e) = getExpDeps e
getExpDeps (HsRecConstr _ fs) = concat [ getExpDeps e | HsFieldUpdate _ e <- fs ]
getExpDeps (HsRecUpdate e fs) =  concat $ getExpDeps e:[ getExpDeps e | HsFieldUpdate _ e <- fs ]

getExpDeps e = error $ "getExpDeps: " ++ show e

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
