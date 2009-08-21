-----------------------------------------------------------------------------
-- |
-- Module      :  System.CPUTime
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The standard CPUTime library.
--
-----------------------------------------------------------------------------

module System.CPUTime (
    getCPUTime,
    cpuTimePrecision
    ) where


-- |Computation 'getCPUTime' returns the number of picoseconds CPU time
-- used by the current program.  The precision of this result is
-- implementation-dependent.
getCPUTime :: IO Integer
getCPUTime = error "getCPUTime"

-- |The 'cpuTimePrecision' constant is the smallest measurable difference
-- in CPU time that the implementation can record, and is given as an
-- integral number of picoseconds.
cpuTimePrecision :: Integer
cpuTimePrecision = error "cpuTimePrecision"

