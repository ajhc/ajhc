module FrontEnd.Tc.Class(simplify,FrontEnd.Class.ClassHierarchy) where

import FrontEnd.Class
import FrontEnd.Tc.Type


simplify :: ClassHierarchy -> [Pred] -> [Pred]
