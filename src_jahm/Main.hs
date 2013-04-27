{-- Jahm: Package installer for (A)jhc.

   Copyright 2013 Metasepi team.
   Contributed by Kiwamu Okabe <kiwamu@debian.or.jp>

This file is part of Metasepi arafura <http://metasepi.masterq.net/>.

Metasepi is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

Metasepi is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Metasepi arafura; see the file metasepi-arafura/COPYING.
If not see <http://www.gnu.org/licenses/old-licenses/gpl-2.0.html>.  --}

module Main where
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Network.URI (parseURI)
import Distribution.Client.HttpUtils (downloadURI)
import Distribution.Verbosity (verbose)

main :: IO ()
main = getArgs >>= mainWorker

mainWorker :: [String] -> IO ()
mainWorker ("downloadURI":u:f:[]) = do curDir <- getCurrentDirectory
                                       let Just url = parseURI u
                                       downloadURI verbose url $ curDir </> f
mainWorker _ = print "help"
