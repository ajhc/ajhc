-- module for things dealing with string constants needed by the compiler internally
{-# OPTIONS_JHC -N -fffi -funboxed-values #-}
module Jhc.String(
    eqString,
    eqUnpackedString,
    unpackStringFoldr,
    eqSingleChar,
    unpackString
    )where


import Jhc.Prim

-- TODO make it handle full UTF8

{-# VCONSTRUCTOR unpackString #-}
{-# NOINLINE unpackString #-}
unpackString :: Addr__ -> [Char]
unpackString addr = f addr where
    f addr = case constPeekByte addr of
        0# -> []
        c -> (Char c:f (increment addr))

{-
unpackFoldrString :: Addr__ -> (Char__ -> b -> b) -> b -> b
unpackFoldrString addr f e = unpack addr where
    unpack addr = case constPeekByte addr of
      '\NUL'# -> e
      ch  | ch `leChar__` '\x7F'# = ch `f` unpack (increment addr)
          | ch `leChar__` '\xDF'# = (((ch .&. '\x1f') `shiftL` 6#) .|. (constPeekByte (increment addr) .&. '\x3f')) `f` unpack (increment (increment addr))
           (chr# (((ord# ch                                  -# 0xC0#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharArray# addr (nh +# 1#)) -# 0x80#))) `f`
          unpack (nh +# 2#)
      | ch `leChar#` '\xEF'# =
           (chr# (((ord# ch                                  -# 0xE0#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexCharArray# addr (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharArray# addr (nh +# 2#)) -# 0x80#))) `f`
          unpack (nh +# 3#)
      | otherwise            =
           (chr# (((ord# ch                                  -# 0xF0#) `uncheckedIShiftL#` 18#) +#
                    ((ord# (indexCharArray# addr (nh +# 1#)) -# 0x80#) `uncheckedIShiftL#` 12#) +#
                    ((ord# (indexCharArray# addr (nh +# 2#)) -# 0x80#) `uncheckedIShiftL#`  6#) +#
                     (ord# (indexCharArray# addr (nh +# 3#)) -# 0x80#))) `f`
          unpack (nh +# 4#)
      where
	ch = indexCharArray# addr nh

-}

unpackStringFoldr :: Addr__ -> (Char -> b -> b) -> b -> b
unpackStringFoldr addr cons nil = f addr where
    f addr = case constPeekByte addr of
        0# -> nil
        c -> (Char c `cons` f (increment addr))

{-# NOINLINE eqUnpackedString #-}
eqUnpackedString :: Addr__ -> [Char] -> Bool__
eqUnpackedString addr cs = f addr cs where
    f :: Addr__ -> [Char] -> Bool__
    f offset [] = case constPeekByte offset of 0# -> 1#; _ -> 0#
    f offset (Char c:cs) = case constPeekByte offset of
        0# -> 0#
        uc -> case equalsChar uc c of
            0# -> 0#
            1# -> f (increment offset) cs

eqSingleChar :: Char__ -> [Char] -> Bool__
eqSingleChar ch (Char c:cs) = case equalsChar ch c of
    0# -> 0#
    1# -> case cs of
        [] -> 1#
        _ -> 0#


{-# NOINLINE eqUnpacked #-}
eqUnpacked :: Addr__ -> [Char] -> Bool__
eqUnpacked addr cs = f addr cs where
    f :: Addr__ -> [Char] -> Bool__
    f offset [] = case constPeekByte offset of 0# -> 1#; _ -> 0#
    f offset (Char c:cs) = case constPeekByte offset of
        0# -> 0#
        uc -> case equalsChar uc c of
            0# -> 0#
            1# -> f (increment offset) cs

-- returns it in an Char__ even though it is just a byte
foreign import primitive constPeekByte :: Addr__ -> Char__


eqString :: [Char] -> [Char] -> Bool__
eqString [] [] = 1#
eqString (Char x:xs) (Char y:ys) = case equalsChar x y of
    0# -> 0#
    1# -> eqString xs ys
eqString _ _ = 0#

foreign import primitive increment :: Addr__ -> Addr__
foreign import primitive "Eq" equalsChar :: Char__ -> Char__ -> Bool__





