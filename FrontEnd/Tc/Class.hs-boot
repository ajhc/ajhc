module FrontEnd.Tc.Class(simplify,FrontEnd.Class.ClassHierarchy) where

import FrontEnd.Class
import Representation


simplify :: ClassHierarchy -> [Pred] -> [Pred]
