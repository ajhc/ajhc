module RawFiles where

import Data.ByteString.Unsafe
import Data.ByteString
import System.IO.Unsafe

-- | Generated from src\/data\/ViaGhc.hs
{-# NOINLINE viaghc_hs #-}
viaghc_hs :: ByteString
viaghc_hs = unsafePerformIO $ unsafePackAddress "\
 \{-# OPTIONS_GHC -fglasgow-exts -fno-implicit-prelude #-}\n\
 \module Main(main) where\n\
 \\n\
 \import GHC.Int\n\
 \import GHC.Word\n\
 \import GHC.IOBase\n\
 \import GHC.Prim\n\
 \import GHC.Base\n\
 \import GHC.Ptr\n\
 \import GHC.Err\n\
 \\n\
 \type World__ = State# RealWorld\n\
 \type Array__ a = Array# a\n\
 \type MutArray__ a = MutableArray# RealWorld a\n\
 \type Ref__ a = MutVar# RealWorld a\n\
 \\n\
 \type Nothing = ()\n\
 \\n\
 \theNothing :: Nothing\n\
 \theNothing = ()\n\
 \\n\
 \type JIO a = World__ -> (# World__, a #)\n\
 \\n\
 \main :: IO ()\n\
 \main = IO $ \\rw -> case theRealMain rw of rw' -> (# rw', () #)\n\
 \\n\
 \unPtr :: Ptr a -> Addr#\n\
 \unPtr ptr = case ptr of\n\
 \    Ptr addr -> addr\n\
 \\n\
 \unFunPtr :: FunPtr a -> Addr#\n\
 \unFunPtr ptr = case ptr of\n\
 \    FunPtr addr -> addr\n\
 \\n\
 \fromBool :: Bool -> Int#\n\
 \fromBool b = case b of\n\
 \    False -> 0#\n\
 \    True -> 1#\n\
 \\n\
 \gteChar# a b = gtChar# a b || eqChar# a b\n\
 \lteChar# a b = ltChar# a b || eqChar# a b\n\
 \\n\
 \plusAddr__ :: Addr# -> Addr# -> Addr#\n\
 \plusAddr__ a1 a2 = plusAddr# a1 (addr2Int# a2)\n\
 \\n\
 \alloca__ :: Int# -> (Addr# -> JIO a) -> JIO a\n\
 \alloca__ size action s =\n\
 \     case newPinnedByteArray# size s      of { (# s, mbarr# #) ->\n\
 \     case unsafeFreezeByteArray# mbarr# s of { (# s, barr#  #) ->\n\
 \     case action (byteArrayContents# barr#) s of { (# s, r #) ->\n\
 \     case touch# barr# s of { s -> (# s, r #) }\n\
 \     }}}\n\
 \\n\
 \word2Char__ x = chr# (word2Int# x)\n\
 \char2Word__ x = int2Word# (ord# x)\n\
 \addr2Word__ x = int2Word# (addr2Int# x)\n\
 \word2Addr__ x = int2Addr# (word2Int# x)\n\
 \\n\
 \convertString :: [Char] -> ListTCon Char\n\
 \convertString [] = jhc_EmptyList\n\
 \convertString (x:xs) = jhc_Cons x (convertString xs)\n\
 \\n\
 \{-\n\
 \error__ :: Addr# -> a\n\
 \error__ s = unsafePerformIO $ do\n\
 \    error_show s\n\
 \    error_exit (I# 255#)\n\
 \\n\
 \errorInt__ :: Addr# -> Int#\n\
 \errorInt__ s = seq (unsafePerformIO $ do\n\
 \    error_show s\n\
 \    error_exit (I# 255#)) 0#\n\
 \\n\
 \errorWord__ :: Addr# -> Word#\n\
 \errorWord__ s = seq (unsafePerformIO $ do\n\
 \    error_show s\n\
 \    error_exit (I# 255#)) (int2Word# 0#)\n\
 \\n\
 \errorAddr__ :: Addr# -> Addr#\n\
 \errorAddr__ s = seq (unsafePerformIO $ do\n\
 \    error_show s\n\
 \    error_exit (I# 255#)) (int2Addr# 0#)\n\
 \foreign import ccall unsafe \"puts\" error_show :: Ptr a -> IO ()\n\
 \foreign import ccall unsafe \"exit\" error_exit :: Int -> IO a\n\
 \ -}\n\
 \\n\
 \{-# NOINLINE newWorld__ #-}\n\
 \newWorld__ :: a -> World__\n\
 \newWorld__ a = case lazy a of\n\
 \    _ -> realWorld#\n\
 \\n\
 \theRealMain :: World__ -> World__\n\
 \\n\
 \"#

-- | Generated from src\/data\/prelude.m4
{-# NOINLINE prelude_m4 #-}
prelude_m4 :: ByteString
prelude_m4 = unsafePerformIO $ unsafePackAddress "\
 \m4_changequote({{,}})\n\
 \m4_changecom({-,-})\n\
 \\n\
 \m4_define(ONCE,{{m4_ifdef(done-$1,{{m4_dnl}},{{m4_define(done-$1,1)$1}})}})\n\
 \\n\
 \m4_define({{m4_for}},{{m4_ifelse($#,0,{{{{$0}}}},{{m4_ifelse(m4_eval($2<=$3),1,\n\
 \{{m4_pushdef({{$1}},$2)$4{{}}m4_popdef({{$1}})$0({{$1}},m4_incr($2),$3,{{$4}})}})}})}})\n\
 \\n\
 \m4_define({{m4_foreach}},{{m4_ifelse(m4_eval($#>2),1,\n\
 \{{m4_pushdef({{$1}},{{$3}})$2{{}}m4_popdef({{$1}})m4_dnl\n\
 \{{}}m4_ifelse(m4_eval($#>3),1,{{$0({{$1}},{{$2}},m4_shift(m4_shift(m4_shift($@))))}})}})}})\n\
 \"#

-- | Generated from src\/data\/targets.ini
{-# NOINLINE targets_ini #-}
targets_ini :: ByteString
targets_ini = unsafePerformIO $ unsafePackAddress "\
 \;\n\
 \; configuration file for architectures and compiler options.\n\
 \;\n\
 \; the final value set is the one used.\n\
 \;\n\
 \; all '-m' parameters on the command line are parsed and processed in order.\n\
 \;\n\
 \; there is an implicit -mdefault processed first\n\
 \; entries in the user config file are appended to this one.\n\
 \;\n\
 \; the cross compilation entries in this file should be treated as examples.\n\
 \; although they work out of the box for many systems, cross compilation\n\
 \; environments differ, so you may need to override them for your\n\
 \; specific setup.\n\
 \\n\
 \[default]\n\
 \cc=gcc\n\
 \gc=jgc\n\
 \cflags=-std=gnu99 -D_GNU_SOURCE -falign-functions=4 -ffast-math -Wextra -Wall -Wno-unused-parameter -fno-strict-aliasing\n\
 \cflags_debug=-g -lm\n\
 \cflags_nodebug=-DNDEBUG -O3\n\
 \profile=false\n\
 \autoload=haskell2010,haskell-extras,haskell98\n\
 \\n\
 \\n\
 \; cross compilation entries\n\
 \\n\
 \[win32]\n\
 \cc=i386-mingw32-gcc\n\
 \executable_extension=.exe\n\
 \merge=i686\n\
 \\n\
 \[wii]\n\
 \cc=powerpc-eabi-gcc\n\
 \byteorder=be\n\
 \cflags+=-g -DGEKKO -D__WORDSIZE=32 -mrvl -mcpu=750 -meabi -mhard-float\n\
 \executable_extension=.elf\n\
 \bits=32\n\
 \bits_max=64\n\
 \merge=be32\n\
 \\n\
 \; macintosh, this is for cross compiling, not for native compilation on osx\n\
 \[osx]\n\
 \\n\
 \[osx-intel]\n\
 \cc=i686-apple-darwin9-gcc\n\
 \merge=i686\n\
 \merge=osx\n\
 \\n\
 \[osx-powerpc]\n\
 \cc=powerpc-apple-darwin9-gcc\n\
 \merge=be32\n\
 \merge=osx\n\
 \\n\
 \; a couple specific cpus\n\
 \[i686]\n\
 \merge=le32\n\
 \arch=i686\n\
 \bits_max=64\n\
 \cflags_nodebug+=-fomit-frame-pointer\n\
 \\n\
 \[x86_64]\n\
 \bits_max=64\n\
 \merge=le64\n\
 \\n\
 \[le32]\n\
 \\n\
 \byteorder=le\n\
 \merge=32\n\
 \\n\
 \[be32]\n\
 \byteorder=be\n\
 \merge=32\n\
 \\n\
 \[le64]\n\
 \byteorder=le\n\
 \merge=64\n\
 \\n\
 \[be64]\n\
 \byteorder=be\n\
 \merge=64\n\
 \\n\
 \[32]\n\
 \cflags+=-m32\n\
 \bits=32\n\
 \\n\
 \[64]\n\
 \cflags+=-m64\n\
 \bits=64\n\
 \\n\
 \"#

-- | Generated from rts\/rts\/constants.h
{-# NOINLINE constants_h #-}
constants_h :: ByteString
constants_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef RTS_CONSTANTS_H\n\
 \#define RTS_CONSTANTS_H\n\
 \/* these constants are shared between jhc-prim and the rts. */\n\
 \\n\
 \// Normal memory block.\n\
 \#define SLAB_FLAG_NONE         0\n\
 \\n\
 \// Each element has a finalizer-list as its second word.\n\
 \#define SLAB_FLAG_FINALIZER    1\n\
 \\n\
 \// In addition to whatever other finalization is done, 'free' should be called\n\
 \// on the first word of each entry.\n\
 \#define SLAB_FLAG_FREE         2\n\
 \\n\
 \// Finalizers should be delayed until entire slab is freed up and individually\n\
 \// freed members need not be kept track of.\n\
 \#define SLAB_FLAG_DELAY        4\n\
 \\n\
 \// A global finalizer exists for this slab\n\
 \#define SLAB_GLOBAL_FINALIZER  8\n\
 \\n\
 \// slab is a monolith, should be 'free'd when done with and not returned to\n\
 \// cache.\n\
 \#define SLAB_MONOLITH          16\n\
 \\n\
 \// virtual flags are never set in a cache but are used internally to keep track\n\
 \// of things.\n\
 \\n\
 \// virtual flag to indicate location is a value\n\
 \#define SLAB_VIRTUAL_VALUE     256\n\
 \\n\
 \// virtual flag to indicate location has a special intererpretation.\n\
 \#define SLAB_VIRTUAL_SPECIAL   512\n\
 \\n\
 \// virtual flag to indication location is a constant.\n\
 \#define SLAB_VIRTUAL_CONSTANT  1024\n\
 \\n\
 \// virtual flag to indication location has been freed. (for debugging)\n\
 \#define SLAB_VIRTUAL_FREED     2048\n\
 \\n\
 \// virtual flag to indication location is lazy.\n\
 \#define SLAB_VIRTUAL_LAZY      4096\n\
 \\n\
 \// virtual flag to indication location is func.\n\
 \#define SLAB_VIRTUAL_FUNC      8192\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/stableptr.c
{-# NOINLINE stableptr_c #-}
stableptr_c :: ByteString
stableptr_c = unsafePerformIO $ unsafePackAddress "\
 \#include \"sys/queue.h\"\n\
 \#include \"jhc_rts_header.h\"\n\
 \#include \"rts/stableptr.h\"\n\
 \#include \"rts/conc.h\"\n\
 \\n\
 \struct StablePtr_list root_StablePtrs = LIST_HEAD_INITIALIZER();\n\
 \\n\
 \wptr_t c_newStablePtr(sptr_t c) {\n\
 \    struct StablePtr* sp = malloc(sizeof(struct StablePtr));\n\
 \    sp->contents = c;\n\
 \\n\
 \    jhc_rts_lock();\n\
 \    LIST_INSERT_HEAD(&root_StablePtrs, sp, link);\n\
 \    jhc_rts_unlock();\n\
 \\n\
 \    assert(GET_PTYPE(sp) == 0);\n\
 \    return (wptr_t)TO_SPTR(P_VALUE,(wptr_t)sp);\n\
 \}\n\
 \\n\
 \void c_freeStablePtr(wptr_t wp) {\n\
 \    struct StablePtr *sp = FROM_SPTR((HsPtr)wp);\n\
 \\n\
 \    jhc_rts_lock();\n\
 \    LIST_REMOVE(sp, link);\n\
 \    jhc_rts_unlock();\n\
 \\n\
 \    free(sp);\n\
 \}\n\
 \\n\
 \sptr_t c_derefStablePtr(wptr_t wp) {\n\
 \    struct StablePtr *sp = FROM_SPTR((HsPtr)wp);\n\
 \    return sp->contents;\n\
 \}\n\
 \\n\
 \void hs_free_stable_ptr(HsStablePtr sp) {\n\
 \        c_freeStablePtr((HsStablePtr)sp);\n\
 \}\n\
 \void hs_free_fun_ptr(HsFunPtr fp) {}\n\
 \\n\
 \/*\n\
 \wptr_t c_castPtrToStablePtr(void *)\n\
 \void * c_castStablePtrToPtr(wptr_t)\n\
 \*/\n\
 \"#

-- | Generated from rts\/rts\/stableptr.h
{-# NOINLINE stableptr_h #-}
stableptr_h :: ByteString
stableptr_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef STABLEPTR_H\n\
 \#define STABLEPTR_H\n\
 \\n\
 \#include \"rts/jhc_rts.h\"\n\
 \\n\
 \wptr_t c_newStablePtr(sptr_t c);\n\
 \void c_freeStablePtr(wptr_t wp);\n\
 \sptr_t c_derefStablePtr(wptr_t wp);\n\
 \void hs_free_stable_ptr(HsStablePtr sp);\n\
 \void hs_free_fun_ptr(HsFunPtr fp);\n\
 \\n\
 \#endif /* STABLEPTR_H */\n\
 \"#

-- | Generated from rts\/sys\/queue.h
{-# NOINLINE queue_h #-}
queue_h :: ByteString
queue_h = unsafePerformIO $ unsafePackAddress "\
 \/*\n\
 \ * Copyright (c) 1991, 1993\n\
 \ *\x0009\&The Regents of the University of California.  All rights reserved.\n\
 \ *\n\
 \ * Redistribution and use in source and binary forms, with or without\n\
 \ * modification, are permitted provided that the following conditions\n\
 \ * are met:\n\
 \ * 1. Redistributions of source code must retain the above copyright\n\
 \ *    notice, this list of conditions and the following disclaimer.\n\
 \ * 2. Redistributions in binary form must reproduce the above copyright\n\
 \ *    notice, this list of conditions and the following disclaimer in the\n\
 \ *    documentation and/or other materials provided with the distribution.\n\
 \ * 3. Neither the name of the University nor the names of its contributors\n\
 \ *    may be used to endorse or promote products derived from this software\n\
 \ *    without specific prior written permission.\n\
 \ *\n\
 \ * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND\n\
 \ * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\n\
 \ * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE\n\
 \ * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE\n\
 \ * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL\n\
 \ * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS\n\
 \ * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)\n\
 \ * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT\n\
 \ * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY\n\
 \ * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF\n\
 \ * SUCH DAMAGE.\n\
 \ *\n\
 \ *\x0009\&@(#)queue.h\x0009\&8.5 (Berkeley) 8/20/94\n\
 \ */\n\
 \\n\
 \#ifndef\x0009\&_SYS_QUEUE_H_\n\
 \#define\x0009\&_SYS_QUEUE_H_\n\
 \\n\
 \/*\n\
 \ * This file defines five types of data structures: singly-linked lists,\n\
 \ * lists, simple queues, tail queues, and circular queues.\n\
 \ *\n\
 \ * A singly-linked list is headed by a single forward pointer. The\n\
 \ * elements are singly linked for minimum space and pointer manipulation\n\
 \ * overhead at the expense of O(n) removal for arbitrary elements. New\n\
 \ * elements can be added to the list after an existing element or at the\n\
 \ * head of the list.  Elements being removed from the head of the list\n\
 \ * should use the explicit macro for this purpose for optimum\n\
 \ * efficiency. A singly-linked list may only be traversed in the forward\n\
 \ * direction.  Singly-linked lists are ideal for applications with large\n\
 \ * datasets and few or no removals or for implementing a LIFO queue.\n\
 \ *\n\
 \ * A list is headed by a single forward pointer (or an array of forward\n\
 \ * pointers for a hash table header). The elements are doubly linked\n\
 \ * so that an arbitrary element can be removed without a need to\n\
 \ * traverse the list. New elements can be added to the list before\n\
 \ * or after an existing element or at the head of the list. A list\n\
 \ * may only be traversed in the forward direction.\n\
 \ *\n\
 \ * A simple queue is headed by a pair of pointers, one the head of the\n\
 \ * list and the other to the tail of the list. The elements are singly\n\
 \ * linked to save space, so elements can only be removed from the\n\
 \ * head of the list. New elements can be added to the list after\n\
 \ * an existing element, at the head of the list, or at the end of the\n\
 \ * list. A simple queue may only be traversed in the forward direction.\n\
 \ *\n\
 \ * A tail queue is headed by a pair of pointers, one to the head of the\n\
 \ * list and the other to the tail of the list. The elements are doubly\n\
 \ * linked so that an arbitrary element can be removed without a need to\n\
 \ * traverse the list. New elements can be added to the list before or\n\
 \ * after an existing element, at the head of the list, or at the end of\n\
 \ * the list. A tail queue may be traversed in either direction.\n\
 \ *\n\
 \ * A circle queue is headed by a pair of pointers, one to the head of the\n\
 \ * list and the other to the tail of the list. The elements are doubly\n\
 \ * linked so that an arbitrary element can be removed without a need to\n\
 \ * traverse the list. New elements can be added to the list before or after\n\
 \ * an existing element, at the head of the list, or at the end of the list.\n\
 \ * A circle queue may be traversed in either direction, but has a more\n\
 \ * complex end of list detection.\n\
 \ *\n\
 \ * For details on the use of these macros, see the queue(3) manual page.\n\
 \ */\n\
 \\n\
 \/*\n\
 \ * List definitions.\n\
 \ */\n\
 \#define\x0009\&LIST_HEAD(name, type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct name {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *lh_first;\x0009\&/* first element */\x0009\&\x0009\&\x0009\&\\\n\
 \}\n\
 \\n\
 \#define\x0009\&LIST_HEAD_INITIALIZER(head)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&{ NULL }\n\
 \\n\
 \#define\x0009\&LIST_ENTRY(type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *le_next;\x0009\&/* next element */\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type **le_prev;\x0009\&/* address of previous next element */\x0009\&\\\n\
 \}\n\
 \\n\
 \/*\n\
 \ * List functions.\n\
 \ */\n\
 \#define\x0009\&LIST_INIT(head) do {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->lh_first = NULL;\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&LIST_INSERT_AFTER(listelm, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (((elm)->field.le_next = (listelm)->field.le_next) != NULL)\x0009\&\\\n\
 \\x0009\&\x0009\&(listelm)->field.le_next->field.le_prev =\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&    &(elm)->field.le_next;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(listelm)->field.le_next = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.le_prev = &(listelm)->field.le_next;\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&LIST_INSERT_BEFORE(listelm, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.le_prev = (listelm)->field.le_prev;\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.le_next = (listelm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&*(listelm)->field.le_prev = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(listelm)->field.le_prev = &(elm)->field.le_next;\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&LIST_INSERT_HEAD(head, elm, field) do {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (((elm)->field.le_next = (head)->lh_first) != NULL)\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->lh_first->field.le_prev = &(elm)->field.le_next;\\\n\
 \\x0009\&(head)->lh_first = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.le_prev = &(head)->lh_first;\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&LIST_REMOVE(elm, field) do {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if ((elm)->field.le_next != NULL)\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(elm)->field.le_next->field.le_prev = \x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&    (elm)->field.le_prev;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&*(elm)->field.le_prev = (elm)->field.le_next;\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&LIST_FOREACH(var, head, field)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&for ((var) = ((head)->lh_first);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var) = ((var)->field.le_next))\n\
 \\n\
 \/*\n\
 \ * List access methods.\n\
 \ */\n\
 \#define\x0009\&LIST_EMPTY(head)\x0009\&\x0009\&((head)->lh_first == NULL)\n\
 \#define\x0009\&LIST_FIRST(head)\x0009\&\x0009\&((head)->lh_first)\n\
 \#define\x0009\&LIST_NEXT(elm, field)\x0009\&\x0009\&((elm)->field.le_next)\n\
 \\n\
 \/*\n\
 \ * Singly-linked List definitions.\n\
 \ */\n\
 \#define\x0009\&SLIST_HEAD(name, type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct name {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *slh_first;\x0009\&/* first element */\x0009\&\x0009\&\x0009\&\\\n\
 \}\n\
 \\n\
 \#define\x0009\&SLIST_HEAD_INITIALIZER(head)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&{ NULL }\n\
 \\n\
 \#define\x0009\&SLIST_ENTRY(type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *sle_next;\x0009\&/* next element */\x0009\&\x0009\&\x0009\&\\\n\
 \}\n\
 \\n\
 \/*\n\
 \ * Singly-linked List functions.\n\
 \ */\n\
 \#define\x0009\&SLIST_INIT(head) do {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->slh_first = NULL;\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SLIST_INSERT_AFTER(slistelm, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.sle_next = (slistelm)->field.sle_next;\x0009\&\x0009\&\\\n\
 \\x0009\&(slistelm)->field.sle_next = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SLIST_INSERT_HEAD(head, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.sle_next = (head)->slh_first;\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->slh_first = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SLIST_REMOVE_HEAD(head, field) do {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->slh_first = (head)->slh_first->field.sle_next;\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SLIST_REMOVE(head, elm, type, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if ((head)->slh_first == (elm)) {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&SLIST_REMOVE_HEAD((head), field);\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&}\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&else {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&struct type *curelm = (head)->slh_first;\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&while(curelm->field.sle_next != (elm))\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&\x0009\&curelm = curelm->field.sle_next;\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&curelm->field.sle_next =\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&    curelm->field.sle_next->field.sle_next;\x0009\&\x0009\&\\\n\
 \\x0009\&}\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SLIST_FOREACH(var, head, field)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&for((var) = (head)->slh_first; (var); (var) = (var)->field.sle_next)\n\
 \\n\
 \/*\n\
 \ * Singly-linked List access methods.\n\
 \ */\n\
 \#define\x0009\&SLIST_EMPTY(head)\x0009\&((head)->slh_first == NULL)\n\
 \#define\x0009\&SLIST_FIRST(head)\x0009\&((head)->slh_first)\n\
 \#define\x0009\&SLIST_NEXT(elm, field)\x0009\&((elm)->field.sle_next)\n\
 \\n\
 \/*\n\
 \ * Singly-linked Tail queue declarations.\n\
 \ */\n\
 \#define\x0009\&STAILQ_HEAD(name, type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct name {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *stqh_first;\x0009\&/* first element */\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type **stqh_last;\x0009\&/* addr of last next element */\x0009\&\x0009\&\\\n\
 \}\n\
 \\n\
 \#define\x0009\&STAILQ_HEAD_INITIALIZER(head)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&{ NULL, &(head).stqh_first }\n\
 \\n\
 \#define\x0009\&STAILQ_ENTRY(type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *stqe_next;\x0009\&/* next element */\x0009\&\x0009\&\x0009\&\\\n\
 \}\n\
 \\n\
 \/*\n\
 \ * Singly-linked Tail queue functions.\n\
 \ */\n\
 \#define\x0009\&STAILQ_INIT(head) do {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->stqh_first = NULL;\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->stqh_last = &(head)->stqh_first;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&STAILQ_INSERT_HEAD(head, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (((elm)->field.stqe_next = (head)->stqh_first) == NULL)\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->stqh_last = &(elm)->field.stqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->stqh_first = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&STAILQ_INSERT_TAIL(head, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.stqe_next = NULL;\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&*(head)->stqh_last = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->stqh_last = &(elm)->field.stqe_next;\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&STAILQ_INSERT_AFTER(head, listelm, elm, field) do {\x0009\&\x0009\&\\\n\
 \\x0009\&if (((elm)->field.stqe_next = (listelm)->field.stqe_next) == NULL)\\\n\
 \\x0009\&\x0009\&(head)->stqh_last = &(elm)->field.stqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&(listelm)->field.stqe_next = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&STAILQ_REMOVE_HEAD(head, field) do {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (((head)->stqh_first = (head)->stqh_first->field.stqe_next) == NULL) \\\n\
 \\x0009\&\x0009\&(head)->stqh_last = &(head)->stqh_first;\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&STAILQ_REMOVE(head, elm, type, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if ((head)->stqh_first == (elm)) {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&STAILQ_REMOVE_HEAD((head), field);\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&} else {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&struct type *curelm = (head)->stqh_first;\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&while (curelm->field.stqe_next != (elm))\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&\x0009\&curelm = curelm->field.stqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&if ((curelm->field.stqe_next =\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&\x0009\&curelm->field.stqe_next->field.stqe_next) == NULL) \\\n\
 \\x0009\&\x0009\&\x0009\&    (head)->stqh_last = &(curelm)->field.stqe_next; \\\n\
 \\x0009\&}\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&STAILQ_FOREACH(var, head, field)\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&for ((var) = ((head)->stqh_first);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var) = ((var)->field.stqe_next))\n\
 \\n\
 \#define\x0009\&STAILQ_CONCAT(head1, head2) do {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (!STAILQ_EMPTY((head2))) {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&*(head1)->stqh_last = (head2)->stqh_first;\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head1)->stqh_last = (head2)->stqh_last;\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&STAILQ_INIT((head2));\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&}\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \/*\n\
 \ * Singly-linked Tail queue access methods.\n\
 \ */\n\
 \#define\x0009\&STAILQ_EMPTY(head)\x0009\&((head)->stqh_first == NULL)\n\
 \#define\x0009\&STAILQ_FIRST(head)\x0009\&((head)->stqh_first)\n\
 \#define\x0009\&STAILQ_NEXT(elm, field)\x0009\&((elm)->field.stqe_next)\n\
 \\n\
 \/*\n\
 \ * Simple queue definitions.\n\
 \ */\n\
 \#define\x0009\&SIMPLEQ_HEAD(name, type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct name {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *sqh_first;\x0009\&/* first element */\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type **sqh_last;\x0009\&/* addr of last next element */\x0009\&\x0009\&\\\n\
 \}\n\
 \\n\
 \#define\x0009\&SIMPLEQ_HEAD_INITIALIZER(head)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&{ NULL, &(head).sqh_first }\n\
 \\n\
 \#define\x0009\&SIMPLEQ_ENTRY(type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *sqe_next;\x0009\&/* next element */\x0009\&\x0009\&\x0009\&\\\n\
 \}\n\
 \\n\
 \/*\n\
 \ * Simple queue functions.\n\
 \ */\n\
 \#define\x0009\&SIMPLEQ_INIT(head) do {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->sqh_first = NULL;\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->sqh_last = &(head)->sqh_first;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SIMPLEQ_INSERT_HEAD(head, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (((elm)->field.sqe_next = (head)->sqh_first) == NULL)\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->sqh_last = &(elm)->field.sqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->sqh_first = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SIMPLEQ_INSERT_TAIL(head, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.sqe_next = NULL;\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&*(head)->sqh_last = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->sqh_last = &(elm)->field.sqe_next;\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SIMPLEQ_INSERT_AFTER(head, listelm, elm, field) do {\x0009\&\x0009\&\\\n\
 \\x0009\&if (((elm)->field.sqe_next = (listelm)->field.sqe_next) == NULL)\\\n\
 \\x0009\&\x0009\&(head)->sqh_last = &(elm)->field.sqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&(listelm)->field.sqe_next = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SIMPLEQ_REMOVE_HEAD(head, field) do {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (((head)->sqh_first = (head)->sqh_first->field.sqe_next) == NULL) \\\n\
 \\x0009\&\x0009\&(head)->sqh_last = &(head)->sqh_first;\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SIMPLEQ_REMOVE(head, elm, type, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if ((head)->sqh_first == (elm)) {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&SIMPLEQ_REMOVE_HEAD((head), field);\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&} else {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&struct type *curelm = (head)->sqh_first;\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&while (curelm->field.sqe_next != (elm))\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&\x0009\&curelm = curelm->field.sqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&if ((curelm->field.sqe_next =\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&\x0009\&curelm->field.sqe_next->field.sqe_next) == NULL) \\\n\
 \\x0009\&\x0009\&\x0009\&    (head)->sqh_last = &(curelm)->field.sqe_next; \\\n\
 \\x0009\&}\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&SIMPLEQ_FOREACH(var, head, field)\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&for ((var) = ((head)->sqh_first);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var) = ((var)->field.sqe_next))\n\
 \\n\
 \/*\n\
 \ * Simple queue access methods.\n\
 \ */\n\
 \#define\x0009\&SIMPLEQ_EMPTY(head)\x0009\&\x0009\&((head)->sqh_first == NULL)\n\
 \#define\x0009\&SIMPLEQ_FIRST(head)\x0009\&\x0009\&((head)->sqh_first)\n\
 \#define\x0009\&SIMPLEQ_NEXT(elm, field)\x0009\&((elm)->field.sqe_next)\n\
 \\n\
 \/*\n\
 \ * Tail queue definitions.\n\
 \ */\n\
 \#define\x0009\&_TAILQ_HEAD(name, type, qual)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct name {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&qual type *tqh_first;\x0009\&\x0009\&/* first element */\x0009\&\x0009\&\\\n\
 \\x0009\&qual type *qual *tqh_last;\x0009\&/* addr of last next element */\x0009\&\\\n\
 \}\n\
 \#define TAILQ_HEAD(name, type)\x0009\&_TAILQ_HEAD(name, struct type,)\n\
 \\n\
 \#define\x0009\&TAILQ_HEAD_INITIALIZER(head)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&{ NULL, &(head).tqh_first }\n\
 \\n\
 \#define\x0009\&_TAILQ_ENTRY(type, qual)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&qual type *tqe_next;\x0009\&\x0009\&/* next element */\x0009\&\x0009\&\\\n\
 \\x0009\&qual type *qual *tqe_prev;\x0009\&/* address of previous next element */\\\n\
 \}\n\
 \#define TAILQ_ENTRY(type)\x0009\&_TAILQ_ENTRY(struct type,)\n\
 \\n\
 \/*\n\
 \ * Tail queue functions.\n\
 \ */\n\
 \#define\x0009\&TAILQ_INIT(head) do {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->tqh_first = NULL;\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->tqh_last = &(head)->tqh_first;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&TAILQ_INSERT_HEAD(head, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (((elm)->field.tqe_next = (head)->tqh_first) != NULL)\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->tqh_first->field.tqe_prev =\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&    &(elm)->field.tqe_next;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&else\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->tqh_last = &(elm)->field.tqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->tqh_first = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.tqe_prev = &(head)->tqh_first;\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&TAILQ_INSERT_TAIL(head, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.tqe_next = NULL;\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.tqe_prev = (head)->tqh_last;\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&*(head)->tqh_last = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->tqh_last = &(elm)->field.tqe_next;\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&TAILQ_INSERT_AFTER(head, listelm, elm, field) do {\x0009\&\x0009\&\\\n\
 \\x0009\&if (((elm)->field.tqe_next = (listelm)->field.tqe_next) != NULL)\\\n\
 \\x0009\&\x0009\&(elm)->field.tqe_next->field.tqe_prev = \x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&    &(elm)->field.tqe_next;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&else\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->tqh_last = &(elm)->field.tqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&(listelm)->field.tqe_next = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.tqe_prev = &(listelm)->field.tqe_next;\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&TAILQ_INSERT_BEFORE(listelm, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.tqe_prev = (listelm)->field.tqe_prev;\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.tqe_next = (listelm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&*(listelm)->field.tqe_prev = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(listelm)->field.tqe_prev = &(elm)->field.tqe_next;\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&TAILQ_REMOVE(head, elm, field) do {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (((elm)->field.tqe_next) != NULL)\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(elm)->field.tqe_next->field.tqe_prev = \x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&    (elm)->field.tqe_prev;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&else\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->tqh_last = (elm)->field.tqe_prev;\x0009\&\x0009\&\\\n\
 \\x0009\&*(elm)->field.tqe_prev = (elm)->field.tqe_next;\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&TAILQ_FOREACH(var, head, field)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&for ((var) = ((head)->tqh_first);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var) = ((var)->field.tqe_next))\n\
 \\n\
 \#define\x0009\&TAILQ_FOREACH_REVERSE(var, head, headname, field)\x0009\&\x0009\&\\\n\
 \\x0009\&for ((var) = (*(((struct headname *)((head)->tqh_last))->tqh_last));\x0009\&\\\n\
 \\x0009\&\x0009\&(var);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var) = (*(((struct headname *)((var)->field.tqe_prev))->tqh_last)))\n\
 \\n\
 \#define\x0009\&TAILQ_CONCAT(head1, head2, field) do {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if (!TAILQ_EMPTY(head2)) {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&*(head1)->tqh_last = (head2)->tqh_first;\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head2)->tqh_first->field.tqe_prev = (head1)->tqh_last;\x0009\&\\\n\
 \\x0009\&\x0009\&(head1)->tqh_last = (head2)->tqh_last;\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&TAILQ_INIT((head2));\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&}\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \/*\n\
 \ * Tail queue access methods.\n\
 \ */\n\
 \#define\x0009\&TAILQ_EMPTY(head)\x0009\&\x0009\&((head)->tqh_first == NULL)\n\
 \#define\x0009\&TAILQ_FIRST(head)\x0009\&\x0009\&((head)->tqh_first)\n\
 \#define\x0009\&TAILQ_NEXT(elm, field)\x0009\&\x0009\&((elm)->field.tqe_next)\n\
 \\n\
 \#define\x0009\&TAILQ_LAST(head, headname) \\\n\
 \\x0009\&(*(((struct headname *)((head)->tqh_last))->tqh_last))\n\
 \#define\x0009\&TAILQ_PREV(elm, headname, field) \\\n\
 \\x0009\&(*(((struct headname *)((elm)->field.tqe_prev))->tqh_last))\n\
 \\n\
 \/*\n\
 \ * Circular queue definitions.\n\
 \ */\n\
 \#define\x0009\&CIRCLEQ_HEAD(name, type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct name {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *cqh_first;\x0009\&\x0009\&/* first element */\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *cqh_last;\x0009\&\x0009\&/* last element */\x0009\&\x0009\&\\\n\
 \}\n\
 \\n\
 \#define\x0009\&CIRCLEQ_HEAD_INITIALIZER(head)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&{ (void *)&head, (void *)&head }\n\
 \\n\
 \#define\x0009\&CIRCLEQ_ENTRY(type)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \struct {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *cqe_next;\x0009\&\x0009\&/* next element */\x0009\&\x0009\&\\\n\
 \\x0009\&struct type *cqe_prev;\x0009\&\x0009\&/* previous element */\x0009\&\x0009\&\\\n\
 \}\n\
 \\n\
 \/*\n\
 \ * Circular queue functions.\n\
 \ */\n\
 \#define\x0009\&CIRCLEQ_INIT(head) do {\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->cqh_first = (void *)(head);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->cqh_last = (void *)(head);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&CIRCLEQ_INSERT_AFTER(head, listelm, elm, field) do {\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.cqe_next = (listelm)->field.cqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.cqe_prev = (listelm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if ((listelm)->field.cqe_next == (void *)(head))\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->cqh_last = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&else\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(listelm)->field.cqe_next->field.cqe_prev = (elm);\x0009\&\\\n\
 \\x0009\&(listelm)->field.cqe_next = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&CIRCLEQ_INSERT_BEFORE(head, listelm, elm, field) do {\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.cqe_next = (listelm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.cqe_prev = (listelm)->field.cqe_prev;\x0009\&\x0009\&\\\n\
 \\x0009\&if ((listelm)->field.cqe_prev == (void *)(head))\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->cqh_first = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&else\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(listelm)->field.cqe_prev->field.cqe_next = (elm);\x0009\&\\\n\
 \\x0009\&(listelm)->field.cqe_prev = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&CIRCLEQ_INSERT_HEAD(head, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.cqe_next = (head)->cqh_first;\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.cqe_prev = (void *)(head);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if ((head)->cqh_last == (void *)(head))\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->cqh_last = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&else\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->cqh_first->field.cqe_prev = (elm);\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->cqh_first = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&CIRCLEQ_INSERT_TAIL(head, elm, field) do {\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.cqe_next = (void *)(head);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(elm)->field.cqe_prev = (head)->cqh_last;\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if ((head)->cqh_first == (void *)(head))\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->cqh_first = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&else\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->cqh_last->field.cqe_next = (elm);\x0009\&\x0009\&\\\n\
 \\x0009\&(head)->cqh_last = (elm);\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&CIRCLEQ_REMOVE(head, elm, field) do {\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if ((elm)->field.cqe_next == (void *)(head))\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->cqh_last = (elm)->field.cqe_prev;\x0009\&\x0009\&\\\n\
 \\x0009\&else\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(elm)->field.cqe_next->field.cqe_prev =\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&    (elm)->field.cqe_prev;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&if ((elm)->field.cqe_prev == (void *)(head))\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(head)->cqh_first = (elm)->field.cqe_next;\x0009\&\x0009\&\\\n\
 \\x0009\&else\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(elm)->field.cqe_prev->field.cqe_next =\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&    (elm)->field.cqe_next;\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \} while (/*CONSTCOND*/0)\n\
 \\n\
 \#define\x0009\&CIRCLEQ_FOREACH(var, head, field)\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&for ((var) = ((head)->cqh_first);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var) != (const void *)(head);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var) = ((var)->field.cqe_next))\n\
 \\n\
 \#define\x0009\&CIRCLEQ_FOREACH_REVERSE(var, head, field)\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&for ((var) = ((head)->cqh_last);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var) != (const void *)(head);\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&\x0009\&(var) = ((var)->field.cqe_prev))\n\
 \\n\
 \/*\n\
 \ * Circular queue access methods.\n\
 \ */\n\
 \#define\x0009\&CIRCLEQ_EMPTY(head)\x0009\&\x0009\&((head)->cqh_first == (void *)(head))\n\
 \#define\x0009\&CIRCLEQ_FIRST(head)\x0009\&\x0009\&((head)->cqh_first)\n\
 \#define\x0009\&CIRCLEQ_LAST(head)\x0009\&\x0009\&((head)->cqh_last)\n\
 \#define\x0009\&CIRCLEQ_NEXT(elm, field)\x0009\&((elm)->field.cqe_next)\n\
 \#define\x0009\&CIRCLEQ_PREV(elm, field)\x0009\&((elm)->field.cqe_prev)\n\
 \\n\
 \#define CIRCLEQ_LOOP_NEXT(head, elm, field)\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(((elm)->field.cqe_next == (void *)(head))\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&    ? ((head)->cqh_first)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&    : (elm->field.cqe_next))\n\
 \#define CIRCLEQ_LOOP_PREV(head, elm, field)\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&(((elm)->field.cqe_prev == (void *)(head))\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&    ? ((head)->cqh_last)\x0009\&\x0009\&\x0009\&\x0009\&\x0009\&\\\n\
 \\x0009\&    : (elm->field.cqe_prev))\n\
 \\n\
 \#endif\x0009\&/* sys/queue.h */\n\
 \"#

-- | Generated from rts\/HsFFI.h
{-# NOINLINE hsffi_h #-}
hsffi_h :: ByteString
hsffi_h = unsafePerformIO $ unsafePackAddress "\
 \/* HsFFI.h for jhc */\n\
 \\n\
 \#ifndef _JHC_HSFFI_H\n\
 \#define _JHC_HSFFI_H\n\
 \\n\
 \#include <stdint.h>\n\
 \#include <stdbool.h>\n\
 \#include <stddef.h>\n\
 \\n\
 \typedef int32_t HsInt;\n\
 \typedef int8_t  HsInt8;\n\
 \typedef int16_t HsInt16;\n\
 \typedef int32_t HsInt32;\n\
 \typedef int64_t HsInt64;\n\
 \\n\
 \typedef uint32_t HsWord;\n\
 \typedef uint8_t  HsWord8;\n\
 \typedef uint16_t HsWord16;\n\
 \typedef uint32_t HsWord32;\n\
 \typedef uint64_t HsWord64;\n\
 \\n\
 \typedef wchar_t HsChar;\n\
 \typedef bool HsBool;\n\
 \\n\
 \typedef double HsDouble;\n\
 \typedef float HsFloat;\n\
 \\n\
 \typedef void *HsPtr;\n\
 \typedef void (*HsFunPtr)(void);\n\
 \typedef void *HsStablePtr;\n\
 \\n\
 \#define HS_BOOL_FALSE 0\n\
 \#define HS_BOOL_TRUE 1\n\
 \\n\
 \void hs_init (int *argc, char **argv[]);\n\
 \void hs_exit (void);\n\
 \void hs_set_argv(int argc, char *argv[]);\n\
 \void hs_perform_gc(void);\n\
 \void hs_free_stable_ptr(HsStablePtr sp);\n\
 \void hs_free_fun_ptr(HsFunPtr fp);\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/sys\/wsize.h
{-# NOINLINE wsize_h #-}
wsize_h :: ByteString
wsize_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef WSIZE_H\n\
 \#define WSIZE_H\n\
 \\n\
 \/*\n\
 \ * wsize.h\n\
 \ * define appropriate __WORDSIZE and __BYTE_ORDER macros\n\
 \ *\n\
 \ * always use operating systems headers rather than checking for architectures\n\
 \ * when possible. if adding new cases. Checking the CPU type should be a last\n\
 \ * resort.\n\
 \ *\n\
 \ */\n\
 \\n\
 \#include <limits.h>\n\
 \\n\
 \#ifdef __linux__\n\
 \#include<endian.h>\n\
 \#endif\n\
 \\n\
 \#ifndef __LITTLE_ENDIAN\n\
 \#define\x0009\&__LITTLE_ENDIAN\x0009\&1234\n\
 \#endif\n\
 \#ifndef __BIG_ENDIAN\n\
 \#define\x0009\&__BIG_ENDIAN\x0009\&4321\n\
 \#endif\n\
 \#ifndef __PDP_ENDIAN\n\
 \#define\x0009\&__PDP_ENDIAN\x0009\&3412\n\
 \#endif\n\
 \\n\
 \#ifndef __BYTE_ORDER\n\
 \#ifdef _BIG_ENDIAN\n\
 \#define __BYTE_ORDER __BIG_ENDIAN\n\
 \#elif defined(__BIG_ENDIAN__)\n\
 \#define __BYTE_ORDER __BIG_ENDIAN\n\
 \#elif defined(_LITTLE_ENDIAN)\n\
 \#define __BYTE_ORDER __LITTLE_ENDIAN\n\
 \#elif defined(__LITTLE_ENDIAN__)\n\
 \#define __BYTE_ORDER __LITTLE_ENDIAN\n\
 \#elif defined(__i386__)\n\
 \#define __BYTE_ORDER __LITTLE_ENDIAN\n\
 \#else\n\
 \#error Could not determine Byte Order\n\
 \#endif\n\
 \#endif\n\
 \\n\
 \#ifndef __WORDSIZE\n\
 \#ifdef __SIZEOF_POINTER__\n\
 \#define __WORDSIZE (CHAR_BIT*__SIZEOF_POINTER__)\n\
 \#elif defined(__i386__)\n\
 \#define __WORDSIZE 32\n\
 \#elif defined(__x86_64__)\n\
 \#define __WORDSIZE 64\n\
 \#else\n\
 \#error Could not determine bitsize\n\
 \#endif\n\
 \#endif\n\
 \\n\
 \#ifdef TEST_WSIZE\n\
 \#include <stdio.h>\n\
 \int\n\
 \main(int argc, char *argv[])\n\
 \{\n\
 \    printf(\"__WORDSIZE:   %i\\n\", __WORDSIZE);\n\
 \    printf(\"__BYTE_ORDER: %i\\n\", __BYTE_ORDER);\n\
 \    return 0;\n\
 \}\n\
 \#endif\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/sys\/bitarray.h
{-# NOINLINE bitarray_h #-}
bitarray_h :: ByteString
bitarray_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef BITARRAY_H\n\
 \#define BITARRAY_H\n\
 \\n\
 \#include <limits.h>\n\
 \#include <stdbool.h>\n\
 \\n\
 \typedef unsigned long bitarray_t;\n\
 \\n\
 \#define BITS_PER_UNIT (bitarray_t)(CHAR_BIT*sizeof(bitarray_t))\n\
 \#define BITARRAY_SIZE(bits) (((bits) + (BITS_PER_UNIT - 1)) / BITS_PER_UNIT)\n\
 \#define BITARRAY_SIZE_IN_BYTES(bits) (sizeof(bitarray_t)*BITARRAY_SIZE(bits))\n\
 \\n\
 \#define WHICH_BIT(bit)  \\\n\
 \    (1UL << ((((bitarray_t)(bit)) % BITS_PER_UNIT)))\n\
 \\n\
 \#define OFFSET_IN_ARRAY(array,bit) \\\n\
 \    (((bitarray_t *)(array))[((bitarray_t)(bit)) / BITS_PER_UNIT])\n\
 \\n\
 \#define BIT_IS_SET(array,bit)  \\\n\
 \    (OFFSET_IN_ARRAY(array,bit) & WHICH_BIT(bit))\n\
 \\n\
 \#define BIT_IS_UNSET(array,bit) \\\n\
 \    (!(BIT_IS_SET(array,bit)))\n\
 \\n\
 \#define BIT_SET(array,bit) \\\n\
 \    (OFFSET_IN_ARRAY(array,bit) |= WHICH_BIT(bit))\n\
 \\n\
 \#define BIT_UNSET(array,bit) \\\n\
 \    (OFFSET_IN_ARRAY(array,bit) &= ~WHICH_BIT(bit))\n\
 \\n\
 \#define BIT_TOGGLE(array,bit) \\\n\
 \    (OFFSET_IN_ARRAY(array,bit) ^= WHICH_BIT(bit))\n\
 \\n\
 \#define BIT_COPY(dest,src,bit)  \\\n\
 \    do { BIT_IS_SET((src),(bit)) ? BIT_SET((dest),(bit)) : BIT_UNSET((dest),(bit)) } while(0)\n\
 \\n\
 \#define BIT_VALUE(array,bit) \\\n\
 \    (BIT_IS_SET((array),(bit)) ? true : false)\n\
 \\n\
 \#define BIT_SET_VALUE(array,bit,value) \\\n\
 \    do { (value) ? BIT_SET((array),(bit)) : BIT_UNSET((array),(bit)) } while(0)\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from ChangeLog
{-# NOINLINE changelog #-}
changelog :: ByteString
changelog = unsafePerformIO $ unsafePackAddress "\
 \commit a1265f031e6b75f9346ec23ca7caec0b5ced82bc\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Sep 2 16:12:19 2013 +0900\n\
 \\n\
 \    Retry cabal.\n\
 \\n\
 \commit 7433749a6076531d8ce230737c41bed1f3ad025b\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Sep 2 16:04:07 2013 +0900\n\
 \\n\
 \    Update manual.\n\
 \\n\
 \commit adb09c463e23dd6911f131d1d35f7b219c2f2cff\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Sep 2 15:38:51 2013 +0900\n\
 \\n\
 \    Add 0.8.0.8 release note.\n\
 \\n\
 \commit bdcf65fae813fb3224cb52f9d9c780b3c653a256\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Sep 2 14:36:37 2013 +0900\n\
 \\n\
 \    Retry on jahm.\n\
 \\n\
 \commit 934ab6503717937652236ed2ba72d3302ec65727\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Sep 2 12:43:50 2013 +0900\n\
 \\n\
 \    Use hackage.haskell.org. Come back.\n\
 \\n\
 \commit 3d3f641a0a69239ace5d1952a0843ea9e54e35e5\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Sep 1 16:34:12 2013 +0900\n\
 \\n\
 \    Use cabal's userAgent.\n\
 \\n\
 \commit efba18b08f88b9cce1af3f33f281f604eed4bc99\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Sep 1 16:12:23 2013 +0900\n\
 \\n\
 \    Try to use new-hackage.haskell.org with jahm.\n\
 \\n\
 \commit eee9c8f2fac28eb759d13672bbc7c3eb02b99fe9\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Sep 1 15:37:16 2013 +0900\n\
 \\n\
 \    Skip haddock on trasvis-ci for too slow.\n\
 \\n\
 \commit 7d57bf347e717b8384c08be1a60ff1e60908ce09\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Sep 1 15:17:17 2013 +0900\n\
 \\n\
 \    Retry test.\n\
 \\n\
 \commit ead8b4ed9e902009d880b0f808047389294a2c5a\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Sep 1 14:19:39 2013 +0900\n\
 \\n\
 \    Fix IORef compile BUG.\n\
 \    \n\
 \    Some time you can see compile error using IORef.\n\
 \    The poke action should have void type?\n\
 \\n\
 \commit 3c3aae70c09122c64ae3da4e471aa0cd412af78f\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Sep 1 09:25:50 2013 +0900\n\
 \\n\
 \    Data.Bits support CInt.\n\
 \\n\
 \commit 7b59bd1ec0e93206a73a61c8d8c3d15fe357a1ff\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Aug 29 00:06:42 2013 +0900\n\
 \\n\
 \    Add --targetsini option to specify the targets.ini file.\n\
 \\n\
 \commit 70cdc744800522b035d2ce207c62ffd9c15a6949\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Aug 28 21:48:22 2013 +0900\n\
 \\n\
 \    Read targets.ini file on .cabal dir.\n\
 \\n\
 \commit bb01384639bf71a07bbd2c02cbf3249f9c29b3d2\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Aug 28 19:01:02 2013 +0900\n\
 \\n\
 \    Split apt-get command to two times.\n\
 \\n\
 \commit 92d5371be3c3e7514b5945037d55e9972d23c6f9\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Aug 28 02:39:59 2013 +0900\n\
 \\n\
 \    Change link about internal doc.\n\
 \\n\
 \commit 06c7ca06d8d971bc7e3647f9799e441db58a94c0\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue Aug 27 20:06:19 2013 +0900\n\
 \\n\
 \    Support CULLong and CLLong type.\n\
 \\n\
 \commit d97c716b77c710503bf59828956e3a01e11b863d\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Aug 15 20:28:17 2013 +0900\n\
 \\n\
 \    Add jhc_fputs_stderr function to avoid not const compile error.\n\
 \\n\
 \commit 20bdab728cc6e860cc3b684d20bd75ed620bb89d\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Aug 15 11:43:40 2013 +0900\n\
 \\n\
 \    Change _JHC_USE_OWN_STDIO macro to define.\n\
 \\n\
 \commit a8461bb87b4de2b28c6cfe3fd1099e306b772dae\n\
 \Author: mzp <mzpppp@gmail.com>\n\
 \Date:   Wed Aug 14 23:55:27 2013 +0000\n\
 \\n\
 \    Don't insert (void) cast for function call. refs #34\n\
 \\n\
 \commit 45ffb8782687ad966ec5e0464554730e21b8530f\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Aug 15 02:04:06 2013 +0900\n\
 \\n\
 \    Update README to explain minimum debs needed.\n\
 \\n\
 \commit 18808645fc2f15178dd2de5de93ba377a7551d3f\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue Aug 13 20:47:43 2013 +0900\n\
 \\n\
 \    Change maintainer.\n\
 \\n\
 \commit 8b7874fcf3ec380849c0c04e42b9a16656a15d98\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue Aug 13 19:15:41 2013 +0900\n\
 \\n\
 \    Fix indent.\n\
 \\n\
 \commit 372848315adacc7cfbb7890c5577c8fc06c211b2\n\
 \Merge: 20e4f5e f3ff3f7\n\
 \Author: Kiwamu Okabe <kiwamu@gmail.com>\n\
 \Date:   Tue Aug 13 03:13:25 2013 -0700\n\
 \\n\
 \    Merge pull request #32 from mzp/fix_limited_megablock_count\n\
 \    \n\
 \    Fix GC: don't increment megablock count, when re-use the megablock in free list\n\
 \    I'm confused. It's a BUG of my code. Merged.\n\
 \\n\
 \commit f3ff3f776c208915932cddf84a1956fa69a3c300\n\
 \Author: mzp <mzpppp@gmail.com>\n\
 \Date:   Tue Aug 13 06:20:06 2013 +0000\n\
 \\n\
 \    Fix GC: don't increment megablock count, when re-use the megablock in free list\n\
 \\n\
 \commit 20e4f5e65d3d5dc8b93a48738c3cb72cf463e603\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Aug 10 19:34:19 2013 +0900\n\
 \\n\
 \    Use own functions for jhc_utf8_{getchar,getc,putchar,putc} with _JHC_USE_OWN_STDIO flag.\n\
 \\n\
 \commit c59f40382a2d3b82539dda89e575ab838d601592\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Aug 10 19:03:40 2013 +0900\n\
 \\n\
 \    Use jhc_printf_stderr instead of fprintf.\n\
 \\n\
 \commit 48eb7579669464579ae23b76f79e676c64284266\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Jul 24 18:18:42 2013 +0900\n\
 \\n\
 \    Need libconfig-yaml-perl for regress test.\n\
 \\n\
 \commit 0be313fea254039803d444e503c1c44bcf9b7dbd\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Jul 24 17:54:18 2013 +0900\n\
 \\n\
 \    No need any perl lib.\n\
 \\n\
 \commit f6c3f4b070acad8a5012682810f0f4d7b7b9ed44\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Jul 10 10:47:57 2013 +0900\n\
 \\n\
 \    Update README for new Debian packages.\n\
 \\n\
 \commit 4a7aebe6b2693d52404efb1e27a4a65ab931bbdb\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Jul 10 02:03:15 2013 +0900\n\
 \\n\
 \    Run apt-get install cpphs on travis-ci.\n\
 \\n\
 \commit c79980df7f776fb4c0494d05732b8e707f1c1194\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Jul 10 01:48:43 2013 +0900\n\
 \\n\
 \    Use cpphs command instead of cpp.\n\
 \\n\
 \commit 41ef1a2468add8cdb47592fa6891d92ea2b87e68\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Jul 6 13:00:55 2013 +0900\n\
 \\n\
 \    Bump up version 0.8.0.8.\n\
 \\n\
 \commit a50158d44738ec87b84d669c7b107f420a96aeea\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Jul 6 12:42:58 2013 +0900\n\
 \\n\
 \    Explain _JHC_JGC_SAVING_MALLOC_HEAP option on manual.html.\n\
 \\n\
 \commit fe6ad67ed02fd86aa4f49b532cdfebb82f6b1d21\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Jul 6 12:04:49 2013 +0900\n\
 \\n\
 \    Update release note.\n\
 \\n\
 \commit 847044c26c171a29ae1e6abe72947fd0bd91b6c2\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Jul 6 11:39:08 2013 +0900\n\
 \\n\
 \    Add release note 0.8.0.7.\n\
 \\n\
 \commit dbf77e15e6b8d8f6c9cb0d7818132c1976b94fed\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Jul 5 11:30:57 2013 +0900\n\
 \\n\
 \    Update TODO.\n\
 \\n\
 \commit 8b5e6e05ef5612a723ec3c87d5b1c8a245d73633\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Jul 1 20:58:55 2013 +0900\n\
 \\n\
 \    First test for conc.\n\
 \\n\
 \commit 29fa0fd1c9be781053b14fd96b46b39f0efbd516\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Jul 1 20:09:08 2013 +0900\n\
 \\n\
 \    Ajhc does not have power to run ghc's conc test.\n\
 \\n\
 \commit a1427948df3b33c010585ca153dd99d7ad6b04e4\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Jul 1 16:47:58 2013 +0900\n\
 \\n\
 \    Copy tests from ghc-testsuite/tests/concurrent/should_run.\n\
 \\n\
 \commit f351a10d1ae512cbe8d7f868f96c5c8776429fd4\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Jul 1 16:01:54 2013 +0900\n\
 \\n\
 \    Link forkIO to forkOS.\n\
 \\n\
 \commit 7bd4c22c05dcf82654d67fba42cf907da63eeef9\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Jun 29 11:10:13 2013 +0900\n\
 \\n\
 \    Include conc_custom.h from RTS.\n\
 \\n\
 \commit 1705a52be8b717e5719fe22bdb2df0642944311a\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Jun 26 19:52:16 2013 +0900\n\
 \\n\
 \    Change option name.\n\
 \\n\
 \commit cb35146be3db33d4ff9a48d274fe75120b4df159\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Jun 26 05:24:23 2013 +0900\n\
 \\n\
 \    Remove copyright line from ajhc.cabal.\n\
 \\n\
 \commit 08f2288f6517948f7589c733a40312e447a1d9e4\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Jun 26 03:10:27 2013 +0900\n\
 \\n\
 \    Add _JHC_JGC_ECO_MALLOC_HEAP option for getting smaller malloc heap.\n\
 \\n\
 \commit ea54bc873f5e789e87cb46aec26a8250b38b2d58\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Jun 20 16:19:03 2013 +0900\n\
 \\n\
 \    Expand chunk area on StringTable_cbits.c.\n\
 \\n\
 \commit dc73403b83e67cd2f586537a85eef2195e505868\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Jun 20 15:01:20 2013 +0900\n\
 \\n\
 \    Guard StablePtr critical section.\n\
 \\n\
 \commit e85a82e8df2453082d3093f8b9ebed07aed49d31\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Jun 20 05:08:58 2013 +0900\n\
 \\n\
 \    Add copyright and bug-reports to cabal file.\n\
 \\n\
 \commit dc4d9c8155ed9dd2e9a3028330736c5dec2033c3\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Jun 20 03:23:43 2013 +0900\n\
 \\n\
 \    Update TODO.\n\
 \\n\
 \commit 5cb67f51dc08a61a26e1df0fb2ce1308389e7af2\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Jun 20 03:20:21 2013 +0900\n\
 \\n\
 \    Bump up version 0.8.0.7.\n\
 \\n\
 \commit 2b5cacf5b5d9de9ee1c6b2dd843aa5a926a6db03\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Jun 20 03:02:43 2013 +0900\n\
 \\n\
 \    Hide Paths_ajhc to pass runhaskell Setup.hs sdist.\n\
 \\n\
 \commit 097befc84ee4fbbcc26d83dd40bf297d64c6e8d7\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Jun 20 02:46:28 2013 +0900\n\
 \\n\
 \    Update po/ja.po.\n\
 \\n\
 \commit e0b624ed7b28ac6783f004e11dd8ebf3b873002f\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Jun 20 02:36:01 2013 +0900\n\
 \\n\
 \    Add Ajhc 0.8.0.6 Release Notes.\n\
 \\n\
 \commit 62c4dab28850927750932ebbb9ccc41445dafb09\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Jun 20 01:09:11 2013 +0900\n\
 \\n\
 \    Need rts/conc.c file to compile rtstest.\n\
 \\n\
 \commit 3abbce795b41342818d6be1d67519d3cf32cdd49\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Jun 20 01:03:19 2013 +0900\n\
 \\n\
 \    Lock jgc's critical sections with jhc_rts_lock().\n\
 \\n\
 \commit 1f96209b98924184764f0b789860702248be33a2\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Jun 20 00:15:20 2013 +0900\n\
 \\n\
 \    Do not use xxx_unlocked API.\n\
 \\n\
 \commit c81a3ffec150e9f85f0e233e26d91b664fd675cd\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Jun 20 00:07:28 2013 +0900\n\
 \\n\
 \    Add global mutex to lock RTS.\n\
 \\n\
 \commit ac0a0001bc2b1ef8b13928becc839b937f7f3c61\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Jun 19 22:32:23 2013 +0900\n\
 \\n\
 \    Shape forkOS interface!\n\
 \\n\
 \commit 8c884f9f556a3815f0dddeb9df288c644e5b127b\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Jun 19 19:37:48 2013 +0900\n\
 \\n\
 \    Apply EApp fromBang_. Ready to impl forkOS.\n\
 \\n\
 \commit 646a37c25c04042e543be888c82e51bf84e1b80e\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Jun 17 19:40:55 2013 +0900\n\
 \\n\
 \    Update TODO.\n\
 \\n\
 \commit c22f497cfa2fac018c2ad508b271bdf06a413dbc\n\
 \Author: Kiwamu Okabe <kiwamu@gmail.com>\n\
 \Date:   Sat Jun 15 02:09:48 2013 +0900\n\
 \\n\
 \    Update README.md\n\
 \\n\
 \commit 667fc93703b9c10fea896321cd825d4f626f6610\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Jun 13 20:27:14 2013 +0900\n\
 \\n\
 \    Fix mistake cleaning s_cache.\n\
 \\n\
 \commit f490d09bae59cd33bd6cc1d37fa197edf0655451\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Jun 12 18:31:44 2013 +0900\n\
 \\n\
 \    Use _JHC_JGC_FIXED_MEGABLOCK option for limited megablock entries.\n\
 \\n\
 \commit 5affb16c00201504c1d1439c66a8471e3e6c798f\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue Jun 11 15:32:37 2013 +0900\n\
 \\n\
 \    Apt-get install libghc-uniplate-dev on travis-ci for get speed.\n\
 \\n\
 \commit 0785f7e76065453b6aff248f83b385207e7f7d77\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue Jun 11 00:10:08 2013 +0900\n\
 \\n\
 \    Fix miss commit.\n\
 \\n\
 \commit cdfebbd6e8de8549440da496a7c68bedc585447c\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Jun 10 23:37:33 2013 +0900\n\
 \\n\
 \    Update manual.html to explain _JHC_JGC_LIMITED_NUM_GC_STACK cflag.\n\
 \\n\
 \commit 6cc57114e2e6361c70763f656bacca8127773ca1\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Jun 10 22:57:57 2013 +0900\n\
 \\n\
 \    Add _JHC_JGC_LIMITED_NUM_GC_STACK cflag.\n\
 \\n\
 \commit 4cf9ace218e7475baf285000bd46b365ee0d1a09\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Jun 10 22:55:34 2013 +0900\n\
 \\n\
 \    No need Data.Monoid.\n\
 \\n\
 \commit d9a96d5b90da74aeee7d9c8b2257a18f6c10d061\n\
 \Merge: 54d1b9f f8a9357\n\
 \Author: Kiwamu Okabe <kiwamu@gmail.com>\n\
 \Date:   Mon Jun 10 04:36:33 2013 -0700\n\
 \\n\
 \    Merge pull request #23 from master-q/feature/reentrant3\n\
 \    \n\
 \    Recycle gc_stack.\n\
 \\n\
 \commit f8a93578abb338984e6d11a63dfa96060391cbe1\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Jun 10 12:16:34 2013 +0900\n\
 \\n\
 \    Recycle gc_stack.\n\
 \\n\
 \commit 54d1b9f6c7a5d644a9bf019e6871c46abace4a1a\n\
 \Merge: f81facf 9d8e5cc\n\
 \Author: Kiwamu Okabe <kiwamu@gmail.com>\n\
 \Date:   Sun Jun 9 16:25:29 2013 -0700\n\
 \\n\
 \    Merge pull request #22 from master-q/feature/reentrant2\n\
 \    \n\
 \    Feature/reentrant2: Remove saved_gc and saved_arena\n\
 \\n\
 \commit 9d8e5cc6d1f0655897c0d5959d281828290a8d4f\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Jun 10 04:56:37 2013 +0900\n\
 \\n\
 \    Pass rtstest.\n\
 \\n\
 \commit b709b1655b3479b8f5de156cfb27287eb06b2b30\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Jun 10 04:38:29 2013 +0900\n\
 \\n\
 \    The new_arena function takes a new arena from free_arena list.\n\
 \\n\
 \commit 15c0a1f4b298384db32c05a0865edc53112f035a\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Jun 10 03:54:27 2013 +0900\n\
 \\n\
 \    Call jhc_alloc_fini at exported function's enter.\n\
 \\n\
 \commit fe31a9dd047ed0a564955a51ff51582f05f08b1f\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Jun 10 03:01:27 2013 +0900\n\
 \\n\
 \    Remove saved_gc and saved_arena.\n\
 \\n\
 \commit 5b9b35cc1d4a51d95d309d501ff9f80747b43abe\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Jun 10 02:02:27 2013 +0900\n\
 \\n\
 \    Call jhc_alloc_init and jhc_hs_init at exported function's enter.\n\
 \\n\
 \commit 889d2cf5d557b9d5b41a318efa8237d487de4142\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Jun 9 23:34:45 2013 +0900\n\
 \\n\
 \    Add \"foreign import jhc_context ccall\" for pass gc and arena to RTS.\n\
 \\n\
 \commit 8de4ce48df5c56e554c15ad48c9bb88fb8f957cd\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Jun 9 22:00:05 2013 +0900\n\
 \\n\
 \    Remove saved_arena. (cont.)\n\
 \\n\
 \commit 4a2edf6732fff649ec555d630ec446f58219384d\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Jun 9 20:59:57 2013 +0900\n\
 \\n\
 \    Move s_cache defined in main_code.c to on s_arena.\n\
 \\n\
 \commit 2c898ff294f93a6bbd6ad58c7dc26ab2aa87d8d4\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Jun 9 00:21:26 2013 +0900\n\
 \\n\
 \    Move array_caches[] to on s_arena.\n\
 \\n\
 \commit f81facf691586198955e9985a5e3299f9f2b9647\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Jun 8 17:03:40 2013 +0900\n\
 \\n\
 \    Manually install derive.\n\
 \\n\
 \commit df841f488c9fac4cb54091aa0a2d7e104908e05f\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Jun 8 16:55:39 2013 +0900\n\
 \\n\
 \    Do not install libghc-derive-dev what is not included by ubuntu.\n\
 \\n\
 \commit a5f708012ad48ddb30140a34484ebba81fb97028\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Jun 8 16:49:18 2013 +0900\n\
 \\n\
 \    No more depend on DrIFT.\n\
 \\n\
 \commit 7cbc4a528a772bf7f3407ebe6225b66c568befd4\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Jun 8 13:01:13 2013 +0900\n\
 \\n\
 \    Update README.md.\n\
 \\n\
 \commit 7f215deeadc65d3566f0001a1fbbec1dc8936d5b\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Jun 8 12:05:26 2013 +0900\n\
 \\n\
 \    Need hscolour on travis-ci.\n\
 \\n\
 \commit 4eeb6a4b3793fb5128ed86f46d0a458d95620a61\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Jun 8 11:35:47 2013 +0900\n\
 \\n\
 \    Only run haddock on travis-ci.\n\
 \\n\
 \commit 88ad44f26e3cc30b19fdd33bac91c3af88f6ebf1\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Jun 8 10:42:59 2013 +0900\n\
 \\n\
 \    Export all modules.\n\
 \\n\
 \commit e56ab3927c0329621b546c23e07ace0a1452adf4\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Jun 8 04:11:23 2013 +0900\n\
 \\n\
 \    Add publish_haddock target.\n\
 \\n\
 \commit 2c3511d7c0c6d8cc53b220b3c9482de2dac74bf1\n\
 \Author: Kiwamu Okabe <kiwamu@gmail.com>\n\
 \Date:   Sat Jun 8 03:07:38 2013 +0900\n\
 \\n\
 \    Update README.md\n\
 \\n\
 \commit bc4be9e5eaae791cc7db7320e72a6040a22b8209\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Jun 8 03:01:33 2013 +0900\n\
 \\n\
 \    Add compile flow image (png).\n\
 \\n\
 \commit a8db1b0ec2f1af7af0a2b3d16a4c930338768aba\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Jun 8 02:33:40 2013 +0900\n\
 \\n\
 \    Bump up version 0.8.0.6.\n\
 \\n\
 \commit 702d796aea22d1129491bb49b66a8a9a4b8ef901\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Jun 8 01:53:46 2013 +0900\n\
 \\n\
 \    Add Ajhc 0.8.0.5 Release Notes.\n\
 \\n\
 \commit 07c3dfbf0300b482393dae85a6a9c52599d303fa\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Jun 8 01:37:12 2013 +0900\n\
 \\n\
 \    Add public types to haddock.\n\
 \\n\
 \commit 216ce6a8fe17b0fa7c64b98db04ff9d07b2c5a39\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Jun 7 22:48:11 2013 +0900\n\
 \\n\
 \    Avoid haddock parser error.\n\
 \\n\
 \commit b4b306d1472bba4ea7bfcf743eb7e770e2e2aa9f\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Jun 7 22:10:04 2013 +0900\n\
 \\n\
 \    Need import Prelude hiding (catch) for GHC 7.4.\n\
 \\n\
 \commit 7ae0f0c227d2c49f41c80e6192dbd038c3ee0416\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Jun 7 21:47:23 2013 +0900\n\
 \\n\
 \    Now run cabal haddock by hand.\n\
 \\n\
 \commit c6044d3b4c10aa3d5b0300684be07167f0a5e8e1\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Jun 7 21:42:20 2013 +0900\n\
 \\n\
 \    Split library and program.\n\
 \\n\
 \commit f5fdf2ba0f79beab7d3cb1ef4cc3180c45d7def2\n\
 \Merge: b6d58e9 c019aa0\n\
 \Author: Kiwamu Okabe <kiwamu@gmail.com>\n\
 \Date:   Wed Jun 5 08:55:48 2013 -0700\n\
 \\n\
 \    Merge pull request #19 from master-q/feature/reentrant1\n\
 \    \n\
 \    Feature/reentrant1: Add arity \"arena\" to all functions. (Sometime selftest has fail without problem.)\n\
 \\n\
 \commit c019aa02764309b0a3364a7f54d41e671ca2b99a\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Jun 6 00:24:02 2013 +0900\n\
 \\n\
 \    Pass rtstest, again.\n\
 \\n\
 \commit f5aa9f87a0ad724830f3326123fabbf85995d5b5\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Jun 6 00:19:33 2013 +0900\n\
 \\n\
 \    Pass rtstest.\n\
 \\n\
 \commit 4f8a185bace5562e16fb9fb803a8db9d43578d54\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Jun 5 22:27:31 2013 +0900\n\
 \\n\
 \    Add arity \"arena\" to all functions.\n\
 \\n\
 \commit 73cedc4842866f48590ce191b3f79ea8392aba16\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Jun 5 16:31:54 2013 +0900\n\
 \\n\
 \    Assign FunPtr as CTYPE \"HsFunPtr\".\n\
 \\n\
 \commit b6d58e95e63be9848f267e9242dedff5e5f7b25b\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon May 27 20:07:23 2013 +0900\n\
 \\n\
 \    Better NAIVEGC.\n\
 \\n\
 \commit 31c207fd41d9a9dc1209453606db37818045d29c\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu May 23 21:23:53 2013 +0900\n\
 \\n\
 \    Update Future plan.\n\
 \\n\
 \commit 2f6663bff4abee55619f2a79b46b3dbb1ae11721\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu May 23 18:30:52 2013 +0900\n\
 \\n\
 \    Run SelfTest with only 1 thread, and output plain text log.\n\
 \\n\
 \commit 8e48fc97986e7ad0c700fdaa032a4bffc4493ebb\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed May 22 20:36:05 2013 +0900\n\
 \\n\
 \    Run testInfo and testBinary using hunit.\n\
 \\n\
 \commit 078461ca2dac8b8334f568f7197afc42eab0cd41\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed May 22 20:23:24 2013 +0900\n\
 \\n\
 \    SelfTest uses test-framework-th library.\n\
 \\n\
 \commit 4158f4b51a5a3a7a3a6a20b5d394a8c44fb0c455\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed May 22 16:02:10 2013 +0900\n\
 \\n\
 \    Skip the regress tests fail now.\n\
 \\n\
 \commit 715083aaed642271fc286eca50c98bb74283bf1b\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed May 22 14:48:32 2013 +0900\n\
 \\n\
 \    regress.prl should return error when regress test error occured.\n\
 \\n\
 \commit 6759d23c08c84ae7bb0ead556049643c81cbf2d5\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue May 21 23:40:00 2013 +0900\n\
 \\n\
 \    Swap running install libs and testing on travis-ci.\n\
 \\n\
 \commit f5db932fbd357a8330bea4daa0a770c108816c4f\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue May 21 23:10:02 2013 +0900\n\
 \\n\
 \    Need valgrind to run rtstest.\n\
 \\n\
 \commit 667106896171c441024fcd0e13c3e9d95edc5e51\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue May 21 23:02:22 2013 +0900\n\
 \\n\
 \    jhc_test and stableptr_test return number of fail.\n\
 \\n\
 \commit ea8de920f1714b7183315fd0192cbdb6b79ab132\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue May 21 22:08:07 2013 +0900\n\
 \\n\
 \    Run rtstest on travis-ci.\n\
 \\n\
 \commit cfee75da539badf3b673614a902a22c4805539e8\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed May 15 09:23:08 2013 +0900\n\
 \\n\
 \    Run selftest on travis-ci.\n\
 \\n\
 \commit feb47bbaf5e110fdebfb544d505809622b254653\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed May 15 09:19:22 2013 +0900\n\
 \\n\
 \    Modify name.acc selftest code to pass it.\n\
 \\n\
 \commit b2c52edba05eb0ca88132a08ed4b78342c515747\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Apr 28 17:12:28 2013 +0900\n\
 \\n\
 \    No need sudo for cabal install.\n\
 \\n\
 \commit 6349fd5304cf68d541538a066af73e6c9165938e\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Apr 28 17:08:51 2013 +0900\n\
 \\n\
 \    GNU make same as Cabal.\n\
 \\n\
 \commit c2fcf3d4efa07925c0a92f0baf80bd5d998962eb\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Apr 28 04:17:11 2013 +0900\n\
 \\n\
 \    More gitignore entries.\n\
 \\n\
 \commit 534d3fff7f85e48a9bc2f0c80ea35f8f296d7452\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Apr 28 04:00:09 2013 +0900\n\
 \\n\
 \    Slim Makefile.\n\
 \\n\
 \commit 3968daca16d6e1524f899c07bbd81f931b7053cc\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Apr 28 01:47:09 2013 +0900\n\
 \\n\
 \    Run apt-get install libghc-temporary-dev libghc-haskeline-dev.\n\
 \\n\
 \commit be7b6dcb59df974f3b25e4b002fe1cc65565a459\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Apr 28 01:42:44 2013 +0900\n\
 \\n\
 \    Use cabal to compile me and run regress test.\n\
 \\n\
 \commit f1581ea0ac987b4d612160db1bc17d8159f55f29\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Apr 27 22:54:10 2013 +0900\n\
 \\n\
 \    Add jahm command to download file from Hackage DB.\n\
 \    \n\
 \    Usage: jahm downloadURI http://hackage.haskell.org/packages/archive/network/2.4.1.2/network-2.4.1.2.tar.gz network-2.4.1.2.tar.gz\n\
 \\n\
 \commit d945f1e72c54674abc1d4d04c066bdc637e34610\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Apr 27 22:24:07 2013 +0900\n\
 \\n\
 \    Jahm is cmpilable.\n\
 \\n\
 \commit 1de6d91b13a6b50f67a9aef9b7f91f21c278ee5c\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Apr 27 21:24:21 2013 +0900\n\
 \\n\
 \    Copy from cabal source code.\n\
 \\n\
 \commit d2cc0fde608a0c3362a35a59f8b7eed080707efb\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Apr 27 21:09:59 2013 +0900\n\
 \\n\
 \    Copy from cabal source code.\n\
 \\n\
 \commit 6d91389f1d7f95cb005d41382a8d6a15d0eccaf3\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Apr 19 18:51:23 2013 +0900\n\
 \\n\
 \    Add dummy dir and symlink to avoid CI error on travis-ci.\n\
 \\n\
 \commit 40348972bb1c7e38effd8316bcd3135690b7f65b\n\
 \Author: Kiwamu Okabe <kiwamu@gmail.com>\n\
 \Date:   Fri Apr 19 18:13:30 2013 +0900\n\
 \\n\
 \    Update README.md\n\
 \\n\
 \commit 2963a3de703474e85a033b80b6bdf14baa34a5af\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Apr 15 10:56:52 2013 +0900\n\
 \\n\
 \    Use metasepi.org domain name.\n\
 \\n\
 \commit 1953685c18607d349485d6dc679838d9bb5e03b7\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Apr 8 02:05:26 2013 +0900\n\
 \\n\
 \    Add script to dump debug infomation.\n\
 \\n\
 \commit a2d14fd969d51e6abdbc508e04ed97a029be5bb5\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Apr 7 22:14:18 2013 +0900\n\
 \\n\
 \    Too big stack for nofib.digits-of-e1 regress test.\n\
 \\n\
 \commit 71346a8fbb2bf9496ea03975a31aee5f80b7ea3d\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Apr 7 21:50:21 2013 +0900\n\
 \\n\
 \    Use ulimit command on bash.\n\
 \\n\
 \commit 6b7980c83a9ea17803876b5e80b166ef272fd19c\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Apr 7 21:42:48 2013 +0900\n\
 \\n\
 \    Run limit on travis-ci.\n\
 \\n\
 \commit ef68ae77fd85be4129974abe007ccac653aa0a35\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Apr 7 20:15:39 2013 +0900\n\
 \\n\
 \    Show meminfo on travis-ci.\n\
 \\n\
 \commit 4ff787bdaba39c411fb585ae4da5047c94a1d751\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Apr 6 17:16:12 2013 +0900\n\
 \\n\
 \    The selftest is runnable. But it catches error.\n\
 \\n\
 \commit 97c6427a1547b2c95f88788665f365c3a83e75b0\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Apr 6 16:50:52 2013 +0900\n\
 \\n\
 \    Umm.... I think we cann't use wine on travis-ci...\n\
 \\n\
 \commit 600ab13e6ae2817c6cc61a82cef95b5400fbd013\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Apr 6 16:43:41 2013 +0900\n\
 \\n\
 \    Run apt-get update before apt-get install.\n\
 \\n\
 \commit bf252a1283275600ed04e10120324bfb704170b9\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Apr 6 16:26:57 2013 +0900\n\
 \\n\
 \    Test for travis-ci.\n\
 \\n\
 \commit c10e974cb2a7e20de8893e70e7e30d7c32236748\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Apr 6 16:18:53 2013 +0900\n\
 \\n\
 \    Can I use wine on travis-ci? (test)\n\
 \\n\
 \commit 63e18e0c5009d624496af86ea8f0bb401d3a2a64\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Apr 6 16:06:43 2013 +0900\n\
 \\n\
 \    MinGW32 settings for passing rts/HelloWorld_win regress test.\n\
 \\n\
 \commit ea94b5db63d8ef2d3e90583ed27fc760d30243e9\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Apr 6 02:20:33 2013 +0900\n\
 \\n\
 \    Find mingw-w64.\n\
 \\n\
 \commit 8003050e52ddf37c912158623114176769cce6a8\n\
 \Merge: 9aec7cb 2dc68c4\n\
 \Author: Kiwamu Okabe <kiwamu@gmail.com>\n\
 \Date:   Fri Apr 5 07:53:54 2013 -0700\n\
 \\n\
 \    Merge pull request #11 from stepcut/arafura\n\
 \    \n\
 \    Thank's!\n\
 \\n\
 \commit 2dc68c441550d09ea573da7a8800455ca4ab898e\n\
 \Author: stepcut <jeremy@n-heptane.com>\n\
 \Date:   Fri Apr 5 08:32:42 2013 -0500\n\
 \\n\
 \    The rest of the of lookupEnv patch\n\
 \\n\
 \commit 9aec7cb77a721ce09e59716ce0233daa23ee765c\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Apr 5 19:19:31 2013 +0900\n\
 \\n\
 \    File src/Options.hs need getEnv function.\n\
 \\n\
 \commit 210da9343b6b7e38a89eb28f85602471a325647a\n\
 \Merge: ff0c404 7e958b3\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Apr 5 19:10:39 2013 +0900\n\
 \\n\
 \    Merge branch 'arafura' of github.com:ajhc/ajhc into arafura\n\
 \\n\
 \commit 7e958b317eaa5473321b545f83ef06f440ac893b\n\
 \Merge: 474ebd5 aedd40c\n\
 \Author: Kiwamu Okabe <kiwamu@gmail.com>\n\
 \Date:   Fri Apr 5 03:09:31 2013 -0700\n\
 \\n\
 \    Merge pull request #10 from stepcut/arafura\n\
 \    \n\
 \    use explicit import lists for System.Enviroment.\n\
 \\n\
 \commit aedd40cd8a6c55efcb485346598bcadcbc9d4b9e\n\
 \Author: stepcut <jeremy@n-heptane.com>\n\
 \Date:   Fri Apr 5 00:26:03 2013 -0500\n\
 \\n\
 \    use explicit import lists for System.Enviroment to avoid conflicting definitions of lookupEnv.\n\
 \\n\
 \commit ff0c40426338e0ecb75cf6e4fb8c136da4bb103a\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Apr 4 02:21:18 2013 +0900\n\
 \\n\
 \    Bump up version 0.8.0.5.\n\
 \\n\
 \commit 474ebd5aa9dc6497f078b8b1999f0cb8d1b2b85f\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Apr 4 02:17:45 2013 +0900\n\
 \\n\
 \    Add Ajhc 0.8.0.4 Release Notes.\n\
 \\n\
 \commit b528b5887e5110b450fae206282c05b411c705f9\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Apr 4 01:28:45 2013 +0900\n\
 \\n\
 \    Remove test code for travis-ci.\n\
 \\n\
 \commit b115146bc0d8965b134a7f232070d45c5700bcdc\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Apr 4 01:12:52 2013 +0900\n\
 \\n\
 \    Need src/data/Typeable.h to cabal install.\n\
 \\n\
 \commit e6235203a7b0a3e2b7ce0d7ad6cf6c676218e110\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Apr 4 01:11:37 2013 +0900\n\
 \\n\
 \    Need git fetch --tags ???\n\
 \\n\
 \commit 7f6b19aae69d381815bb079b711f4325d2a74314\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Apr 4 00:32:47 2013 +0900\n\
 \\n\
 \    Test for travis-ci.\n\
 \\n\
 \commit ab602755997049b29128105203ab8c14349ac7fa\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Apr 3 23:39:53 2013 +0900\n\
 \\n\
 \    Git 1.8.2 can not use \"HEAD\" for git log????\n\
 \\n\
 \commit 7d6768f60c403063b25704b628f5b26f1707229f\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Apr 3 23:09:32 2013 +0900\n\
 \\n\
 \    Why error on travis-ci? Retry with printing git version.\n\
 \\n\
 \commit be19711376deac341f0fccd505daf3ab57e0fc5f\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Apr 3 21:41:59 2013 +0900\n\
 \\n\
 \    Need -lm option with compiling -fdebug to pass large/RayT regress test.\n\
 \\n\
 \commit 6f88fadf0c064de119e5a309ee27cfa272adad97\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Apr 3 19:30:29 2013 +0900\n\
 \\n\
 \    Need libgc-dev and gcc-multilib package to run regress test.\n\
 \\n\
 \commit 5b71f46786616d395ecdf898865d7f8c11b5c0ce\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Apr 3 18:39:36 2013 +0900\n\
 \\n\
 \    Explain Ajhc position on cabal description.\n\
 \\n\
 \commit a6f971f39d89b2d6ebf44bfb6f9b0d1591dd6161\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Apr 3 05:12:41 2013 +0900\n\
 \\n\
 \    Bump up version 0.8.0.4.\n\
 \\n\
 \commit eb0874f9543a8db6aa7eafe3d072a8d579b0b184\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Apr 3 05:00:19 2013 +0900\n\
 \\n\
 \    downcase\n\
 \\n\
 \commit 22787aa809d94c8642bc578d768045fb607f282e\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Apr 3 03:14:54 2013 +0900\n\
 \\n\
 \    Clean up drift_processed tmp autom4te.cache\n\
 \\n\
 \commit 45418eb6a70de87a9b56e73ba7234dbf4162bab4\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Apr 3 02:49:02 2013 +0900\n\
 \\n\
 \    Test cabal install on travis-ci.\n\
 \\n\
 \commit c7e7427a6e4cfc2a3e5ffa9939cb86d965301060\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue Apr 2 03:36:31 2013 +0900\n\
 \\n\
 \    Bump up version 0.8.0.3. Support Windows MinGW32 platform.\n\
 \\n\
 \commit 0c4b321887b0de5f569974d8e4bd1e18d2a45477\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue Apr 2 03:35:44 2013 +0900\n\
 \\n\
 \    Add Ajhc 0.8.0.3 Release Notes.\n\
 \\n\
 \commit f1bf6e4a473a42ac193097a84adebd6506b61209\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue Apr 2 01:23:29 2013 +0900\n\
 \\n\
 \    Avoid 500 error while running LWP::Simple getstore.\n\
 \\n\
 \commit d256d40e5a89d37d350cdbcb8e2f9c0d00289ff4\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue Apr 2 00:06:33 2013 +0900\n\
 \\n\
 \    Fix a bit miss.\n\
 \\n\
 \commit 959c0739d7804b064870bec1cdcce5b3ad70d379\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Apr 1 23:08:12 2013 +0900\n\
 \\n\
 \    Do not use \"ajhc\" name to avoid conflicting between jhc and Ajhc.\n\
 \\n\
 \commit ab4989d6e672e5488cf1ad36974e6a1cf16ddf6d\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Apr 1 22:17:42 2013 +0900\n\
 \\n\
 \    Update README.\n\
 \\n\
 \commit 4c66ba7dc37d103e8704fca2cb6923ae3832dc23\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Apr 1 21:20:23 2013 +0900\n\
 \\n\
 \    Do not forge to add utils/build_extlibs.prl to cabal sdist.\n\
 \\n\
 \commit 8a873e456a39f7b92613536a89b5205cf784f6b0\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Apr 1 21:00:46 2013 +0900\n\
 \\n\
 \    Install JHC_EXT_LIBS with cabal install.\n\
 \\n\
 \commit 5c043d61c5201af057089973a3c6bc0db026567d\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Apr 1 19:20:38 2013 +0900\n\
 \\n\
 \    Ready to cabal install me on Windows + MinGW32 env without JHC_EXT_LIBS.\n\
 \\n\
 \commit c7e2bf3f129f44a20e4350f1b7c7ad84720c9a4e\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Apr 1 17:24:46 2013 +0900\n\
 \\n\
 \    Add prefix \"sh -c \" for system function on mingw32.\n\
 \\n\
 \commit 7f81fb4a41f6a6f272f4a39ada258e8776a65bd4\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Apr 1 15:45:19 2013 +0900\n\
 \\n\
 \    Windows install Haskell Platform to directory  has path string include space.\n\
 \\n\
 \commit 012f092f7bd74a12f10a495da3c177818932653d\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Apr 1 15:04:43 2013 +0900\n\
 \\n\
 \    Fix NAIVEGC bug. Should update pg after running gc_perform_gc.\n\
 \\n\
 \commit 4b122fd809092c31211fdd8e18283622c3651513\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Apr 1 03:52:56 2013 +0900\n\
 \\n\
 \    Find data directory with getDataFileName function.\n\
 \\n\
 \commit 6b014de73f6381701bc03926f1f75683589f1694\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Apr 1 02:36:08 2013 +0900\n\
 \\n\
 \    Should write like below style for Windows + msys env.\n\
 \    \n\
 \      install FILES \"DESTDIR\"\n\
 \\n\
 \commit 232129a84378c306970b2c4ec4d5b0a3dc74958d\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Apr 1 01:45:02 2013 +0900\n\
 \\n\
 \    List up the files in \"lib\" directory to include them on cabal sdist file.\n\
 \\n\
 \commit 166f0b90ea3f319a1841961e4212213a1750374b\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Mar 31 23:42:41 2013 +0900\n\
 \\n\
 \    Build JHC_LIBS with cabal. (not yet JHC_EXT_LIBS)\n\
 \\n\
 \commit a7e0de37f89d1f8f4cdae4a873c11412c342ea8a\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Mar 31 21:25:17 2013 +0900\n\
 \\n\
 \    Update TODO.\n\
 \\n\
 \commit e1a0fbd87e56a8f45c127492adf9433c85df5618\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Mar 31 21:24:37 2013 +0900\n\
 \\n\
 \    Build ajhc with cabal without hl files.\n\
 \\n\
 \commit e4a396ef63dd26518073b789480b1c3f5268e444\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Mar 31 20:13:23 2013 +0900\n\
 \\n\
 \    No more depend on haskell98 package.\n\
 \\n\
 \commit 2e58f4f259507eb7eb67a894db81c23e056465d8\n\
 \Merge: 8de6364 58a1bf6\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 30 01:23:53 2013 +0900\n\
 \\n\
 \    Merge branch 'arafura' of github.com:ajhc/ajhc into arafura\n\
 \\n\
 \commit 8de63645df6cc30f9874b12b18da032acc0a28d0\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 30 01:23:16 2013 +0900\n\
 \\n\
 \    Add BUGGY cabal file.\n\
 \\n\
 \commit 58a1bf61639157e3ac3e9c55cc2f4ba33d592eb7\n\
 \Author: Kiwamu Okabe <kiwamu@gmail.com>\n\
 \Date:   Fri Mar 29 21:53:45 2013 +0900\n\
 \\n\
 \    a bit change.\n\
 \\n\
 \commit 7675b1599fe41531b4c89a2ada947fa8f601161d\n\
 \Author: Kiwamu Okabe <kiwamu@gmail.com>\n\
 \Date:   Fri Mar 29 21:44:42 2013 +0900\n\
 \\n\
 \    Build on Windows (in the make)\n\
 \\n\
 \commit 38275f78b7ff585616e2a7846cedccbf2f0dd0b8\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Mar 29 20:13:56 2013 +0900\n\
 \\n\
 \    Add msys path to library search path.\n\
 \\n\
 \commit cbfe1a5cd1bc660adc126056bfe17dbce529f8bd\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Mar 29 19:31:22 2013 +0900\n\
 \\n\
 \    Update README.md.\n\
 \\n\
 \commit e99327bbe221b35c63e4c2639462330d1c978b6c\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Mar 29 17:55:26 2013 +0900\n\
 \\n\
 \    Explain how to build on Windows.\n\
 \\n\
 \commit 376103c6d2dfe3d77920b340f8295d174c274b79\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Mar 29 17:28:28 2013 +0900\n\
 \\n\
 \    Ready to build me on Windows.\n\
 \\n\
 \commit 6024a1823e6782f3d03c8263bcfd50bbba83ba33\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Mar 29 08:57:29 2013 +0900\n\
 \\n\
 \    Stacktrace with running \"ajhcp Foo.hs +RTS -p -xc\".\n\
 \\n\
 \commit 20edf78270d89fc8602e37863980bec0228bb647\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Mar 29 07:44:28 2013 +0900\n\
 \\n\
 \    Use \"DrIFT-cabalized\" command name, if use DrIFT-cabalized Haskell package.\n\
 \\n\
 \commit e3a1a65e326e1a3b31fc5eb03c89a611a56d1970\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Mar 28 22:20:22 2013 +0900\n\
 \\n\
 \    Find and use DrIFT-cabalized.\n\
 \\n\
 \commit 293eb5a89fe27f25c623996e7670cd8984768319\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Mar 27 13:16:56 2013 +0900\n\
 \\n\
 \    Use System.IO.Temp instead of mkdtemp.\n\
 \\n\
 \commit 8ab1f6aebdfd466be988f70611c60450664ca47b\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Mar 27 12:16:50 2013 +0900\n\
 \\n\
 \    Switch compile for Windows or UNIX with USE_WIN32 flag.\n\
 \\n\
 \commit 76671e2eb7b91b482a413d93d657bbd374c66f1d\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Mar 27 06:00:41 2013 +0900\n\
 \\n\
 \    Fix setting EXEEXT in Makefile.\n\
 \\n\
 \commit e49aebef308f40a1e8b1c9b6594c81708b4ce08b\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Mar 27 03:18:58 2013 +0900\n\
 \\n\
 \    Use haskeline haskell package instead of readline or editline. (cont.)\n\
 \\n\
 \commit 751f34f46df2afad207748b3d4af66155a9b1c64\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue Mar 26 01:20:05 2013 +0900\n\
 \\n\
 \    Dump megablocks on gdb.\n\
 \\n\
 \commit c8d53949450b38555c38ce93ebd6d8003d334256\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue Mar 26 00:25:57 2013 +0900\n\
 \\n\
 \    Dump s_cache on gdb.\n\
 \\n\
 \commit 9ceab0702c715ca3012da68c350f7e7357722c6d\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 23 03:34:53 2013 +0900\n\
 \\n\
 \    Rich gdb command print_jgcheap. (cont.)\n\
 \\n\
 \commit a572ddeb55dc28124c4c2fba09330e8b97a6dce5\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Mar 22 22:49:15 2013 +0900\n\
 \\n\
 \    Do not use gdb.parse_and_eval in loop. It's very slow.\n\
 \\n\
 \commit cbdcfce5d3bc0d74d67d097f0e0ccf4a77ecb00c\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Mar 22 22:20:44 2013 +0900\n\
 \\n\
 \    Pytyon script to dump Haskell heap on gdb.\n\
 \\n\
 \commit 25cb7c8e298eb75e26dcdf56d4bd532d6bbd641b\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Mar 22 02:21:18 2013 +0900\n\
 \\n\
 \    Update Future plan.\n\
 \\n\
 \commit b7e82942abed041e519c4709a591096367bdf158\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Wed Mar 20 12:21:38 2013 +0900\n\
 \\n\
 \    ignore .ditz-config file on git command\n\
 \\n\
 \commit 46ba77fa0f2ed1fa16acdedae737deab365c9c25\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue Mar 19 21:52:39 2013 +0900\n\
 \\n\
 \    Use \"JHC_XXX\" name in Makefile to share source code with jhc.\n\
 \\n\
 \commit 830a6f81207bac4394f8981e3f432821358d862b\n\
 \Merge: 870430b 2adf411\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue Mar 19 20:50:19 2013 +0900\n\
 \\n\
 \    Merge branch 'master' into arafura\n\
 \    \n\
 \    Conflicts:\n\
 \    \x0009\&library_deps.make\n\
 \\n\
 \commit 870430b35629ec2ca8d498b5f198bd2d8610edfe\n\
 \Merge: f1fb1c6 ddafc44\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Mar 17 16:30:24 2013 +0900\n\
 \\n\
 \    Merge branch 'master' into arafura\n\
 \    \n\
 \    Conflicts:\n\
 \    \x0009\&Makefile.am\n\
 \    \x0009\&po/ja.po\n\
 \    \x0009\&src/Grin/Grin.hs\n\
 \\n\
 \commit f1fb1c6304bc85b71d3fb0558fad7804d5565d60\n\
 \Merge: fa1a95b de85f4e\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sun Mar 17 16:19:20 2013 +0900\n\
 \\n\
 \    Merge branch 'master' into arafura\n\
 \    \n\
 \    Conflicts:\n\
 \    \x0009\&rts/rts/gc_jgc.c\n\
 \\n\
 \commit 2adf411caac22cf15b1d63d2de80ec9e3b12e843\n\
 \Author: kiwamu <kiwamu@debian.or.jp>\n\
 \Date:   Sun Mar 17 16:06:29 2013 +0900\n\
 \\n\
 \    Upgrade some JHC_EXT_LIBS.\n\
 \    \n\
 \    Ignore-this: d51be66b69fbe0240775f1464a4d46e7\n\
 \    \n\
 \    darcs-hash:20130317070629-99646-8d122ceea5273835030a18ac965842896f8f42e1.gz\n\
 \\n\
 \commit ac8ddb00ddc5aa3580ce3e68cb2773f1536cb638\n\
 \Author: kiwamu <kiwamu@debian.or.jp>\n\
 \Date:   Sun Mar 17 02:21:07 2013 +0900\n\
 \\n\
 \    Explain RTS cflags for tiny cpu.\n\
 \    \n\
 \    Ignore-this: b09e6da5e1704b9da4a51ae6a23f57ee\n\
 \    \n\
 \    darcs-hash:20130316172107-99646-d9545e56b6d11bf6511bf35a793f2246cae9b697.gz\n\
 \\n\
 \commit fa1a95b48fec10d2ee2adc30f5c447cdb36564e5\n\
 \Author: Kiwamu Okabe <kiwamu@gmail.com>\n\
 \Date:   Sat Mar 16 23:10:41 2013 +0900\n\
 \\n\
 \    About darcs send command.\n\
 \\n\
 \commit ddafc4422bea272707775cd680058232b9b40e07\n\
 \Author: John Meacham <john@repetae.net>\n\
 \Date:   Sat Mar 16 22:14:24 2013 +0900\n\
 \\n\
 \    add Japanese translation\n\
 \    \n\
 \    Ignore-this: 771722900017d423f40b25f917392680\n\
 \    \n\
 \    darcs-hash:20130316131424-1a7c6-1f6707f0212f861d0af8aad5a573efa1a4107879.gz\n\
 \\n\
 \commit c753139eff5275fe8a48b0ce7fccd0a387409406\n\
 \Author: John Meacham <john@repetae.net>\n\
 \Date:   Sat Mar 16 22:07:34 2013 +0900\n\
 \\n\
 \    improve error message\n\
 \    \n\
 \    Ignore-this: e90f98c675cc6df2770c8bba3a7c313\n\
 \    \n\
 \    darcs-hash:20130316130734-1a7c6-5e34d40efe9979f39d53361dbd0c194b1d050af3.gz\n\
 \\n\
 \commit 340727f1a7eb881ba8fec88d77218ad622beb20a\n\
 \Author: John Meacham <john@repetae.net>\n\
 \Date:   Sat Mar 16 22:07:04 2013 +0900\n\
 \\n\
 \    add printing of attributed, conjunctive, and disjunctive types.\n\
 \    \n\
 \    Ignore-this: 71f5c1b073bf7a11bc6187345e03e6d0\n\
 \    \n\
 \    darcs-hash:20130316130704-1a7c6-4a14b9c3d272dc9d345f3b837fd2adb13997e8cb.gz\n\
 \\n\
 \commit 712c3748d829da2c6c9c8b5cd3044066dab68f2d\n\
 \Author: John Meacham <john@repetae.net>\n\
 \Date:   Sat Mar 16 22:04:01 2013 +0900\n\
 \\n\
 \    convert complex types to hardware implemented complex numbers when available\n\
 \    \n\
 \    Ignore-this: ec581ab1eaf5f8e1c920e5741fba50ee\n\
 \    \n\
 \    darcs-hash:20130316130401-1a7c6-162e1c4a44528c0b6c3ec8692027ed79a11468ea.gz\n\
 \\n\
 \commit db29b9d6356de33dae1678f3f4d1f22eab8502ab\n\
 \Author: John Meacham <john@repetae.net>\n\
 \Date:   Sat Mar 16 21:56:30 2013 +0900\n\
 \\n\
 \    Add TyComplex and TyVector to basic types\n\
 \    \n\
 \    Ignore-this: f8a61d1dc3dd99cf7a481a50c192125f\n\
 \    \n\
 \    darcs-hash:20130316125630-1a7c6-82f16b518bcc6bd4d4f692ca8575cd0a65e9c361.gz\n\
 \\n\
 \commit c3db54ed132774c12ba8b8909f458b5681277ad9\n\
 \Author: John Meacham <john@repetae.net>\n\
 \Date:   Sat Mar 16 21:51:21 2013 +0900\n\
 \\n\
 \    from Kiwamu Okabe: fix ghc warnings.\n\
 \    \n\
 \    Ignore-this: 245f7d05dfa93475d82dac08ade9c007\n\
 \    \n\
 \    darcs-hash:20130316125121-1a7c6-489b97df0250c3724bcc458a0210cd23cc5aae47.gz\n\
 \\n\
 \commit 4d8f741cd23aecd87b032ea2b41eaa6fa4e75558\n\
 \Author: John Meacham <john@repetae.net>\n\
 \Date:   Sat Mar 16 21:48:22 2013 +0900\n\
 \\n\
 \    from Kiwamu Okabe: RTS fix for embedded ARM CPUs\n\
 \    \n\
 \    Ignore-this: 40ed352498ff8e20d43410ea772f96c0\n\
 \    \n\
 \    darcs-hash:20130316124822-1a7c6-51653050afc3b9359b57d2b290e480804995a9c4.gz\n\
 \\n\
 \commit d85bd01d86dcfb6067545175f01e6ff2efae7299\n\
 \Author: John Meacham <john@repetae.net>\n\
 \Date:   Sat Mar 16 21:46:30 2013 +0900\n\
 \\n\
 \    from Kiwamu Okabe: ghc 7.6 compatibility\n\
 \    \n\
 \    Ignore-this: 953ccf73fee4a1d604eec7b0bfd360d8\n\
 \    \n\
 \    darcs-hash:20130316124630-1a7c6-0fe41016dc7bdf9b1aa6e2d44beab198a3a7ea7d.gz\n\
 \\n\
 \commit 6adebb83357aa04650f0e734182e2ab18581b998\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 16 21:24:46 2013 +0900\n\
 \\n\
 \    Explain demo on tiny CPU.\n\
 \\n\
 \commit 22c38dc56117680319a2c8e3c9fae6b425d353ed\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 16 17:56:43 2013 +0900\n\
 \\n\
 \    Add Ajhc 0.8.0.2 Release Note.\n\
 \\n\
 \commit 904b30ed92df33403c6bae58c7837aca627d1775\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 16 11:43:34 2013 +0900\n\
 \\n\
 \    Do not run CI on master branch.\n\
 \\n\
 \commit 76d810b331b63a7b9accdc7217f9af4a7727eafb\n\
 \Author: Kiwamu Okabe <kiwamu@gmail.com>\n\
 \Date:   Sat Mar 16 11:17:42 2013 +0900\n\
 \\n\
 \    Add future plan.\n\
 \\n\
 \commit fad2d6340e429bb0a3d0ae86f50615911a6bb1de\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Mar 15 21:10:22 2013 +0900\n\
 \\n\
 \    Use git instead of darcs.\n\
 \\n\
 \commit e03a1ee09758f8a0e565e30abd5c88a60f72ee5c\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Mar 15 21:09:49 2013 +0900\n\
 \\n\
 \    Update ja.po.\n\
 \\n\
 \commit 600cb0c4d6ecbbe2ce2e6eea2ddf11a91333635c\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Mar 15 14:32:15 2013 +0900\n\
 \\n\
 \    s/jhc/Ajhc/g on manual_ja.html.\n\
 \\n\
 \commit 971ac8c04a0546b64a5708ddda6be362cb7f76ef\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Mar 15 13:57:55 2013 +0900\n\
 \\n\
 \    s/jhc/Ajhc/g on manual.html.\n\
 \\n\
 \commit 57948b53329b6ee108ca14571bf1c6e15d541894\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Mar 15 13:27:03 2013 +0900\n\
 \\n\
 \    A bit change.\n\
 \\n\
 \commit dc57a14f8b5c86e92b034a2706250e66b07d6ce5\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Mar 15 13:09:18 2013 +0900\n\
 \\n\
 \    Explain new cflags for jgc.\n\
 \\n\
 \commit 80aa12fb9b57622bba2f0e911d7ebc0c04ddb662\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Fri Mar 15 01:00:47 2013 +0900\n\
 \\n\
 \    Bump up version 0.8.0.1. New RTS for tiny memory.\n\
 \\n\
 \commit 3167551530b0576cf1f42f928865868ce9aa0b50\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Mar 14 05:09:01 2013 +0900\n\
 \\n\
 \    Copy RTS from demo-cortex-m3.\n\
 \\n\
 \commit eb7206805ca012bca6d7bfeceed668b9c3f6a3d5\n\
 \Author: Kiwamu Okabe <kiwamu@gmail.com>\n\
 \Date:   Sun Mar 10 02:59:43 2013 +0900\n\
 \\n\
 \    Explain branch policy.\n\
 \\n\
 \commit 202d113021baf66f5f7534c0eb1776e730e53a48\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Thu Mar 7 19:50:06 2013 +0900\n\
 \\n\
 \    Drop volatile poke patch.\n\
 \    \n\
 \    Read the thread.\n\
 \    http://www.haskell.org/pipermail/jhc/2013-March/001008.html\n\
 \\n\
 \commit de85f4eb5d8688143c83352631c161792e8cc883\n\
 \Author: John Meacham <john@repetae.net>\n\
 \Date:   Wed Mar 6 22:02:31 2013 +0900\n\
 \\n\
 \    Kiwamu Okabe's fix for jhc_aligned_malloc\n\
 \    \n\
 \    Ignore-this: 4413ee55fe27c401d2fb08a752d8811d\n\
 \    \n\
 \    darcs-hash:20130306130231-1a7c6-6f4a8a5ba9870d07ef7548da3f2451a37d1d78f2.gz\n\
 \\n\
 \commit a187199849a4a4cef21bf24c87548374673d81e5\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue Mar 5 22:47:11 2013 +0900\n\
 \\n\
 \    Add typesig.\n\
 \\n\
 \commit 8b41239136cc607eed0d7974b0d6eb643b95faf8\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue Mar 5 19:50:15 2013 +0900\n\
 \\n\
 \    Fix \"Not in scope: `catch'\" error on ghc 7.6.2.\n\
 \\n\
 \commit 647878e884c106439c12c3d271407002dd4196ec\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue Mar 5 02:12:31 2013 +0900\n\
 \\n\
 \    Use DoRec instead of RecursiveDo.\n\
 \\n\
 \commit 86f4306ce526f2081724f446e0a253fc3eb3d33b\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Tue Mar 5 01:25:45 2013 +0900\n\
 \\n\
 \    Fix many ghc warning. (cont.)\n\
 \\n\
 \commit cbb7aa047150bb98b76abec2f68a3f397853413f\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Mar 4 22:48:10 2013 +0900\n\
 \\n\
 \    Fix many ghc warning. (cont.)\n\
 \\n\
 \commit 4acae1778d0960e0bcf5038edc2a7158a0582744\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Mar 4 21:57:20 2013 +0900\n\
 \\n\
 \    Remove unused UNPACK pragma.\n\
 \\n\
 \commit 7409b637f4871d4bda765068c389a8d77c6a2d26\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Mar 4 21:36:12 2013 +0900\n\
 \\n\
 \    Extract functions derived by DrIFT for fixing \"Defined but not used\" warning.\n\
 \\n\
 \commit 61a96383ad866f599d3950dc1fb7b84a6d7000ca\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Mar 4 20:16:13 2013 +0900\n\
 \\n\
 \    Fix pattern match(es) are non-exhaustive.\n\
 \\n\
 \commit 151af38ece7ccba7e7f5970948c7fbca3c3bea2d\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Mar 4 18:06:41 2013 +0900\n\
 \\n\
 \    Add iocatch function to use old Prelude.catch.\n\
 \\n\
 \commit 90a685b03ea75fa4e2c7a13f00b5c1d22d8232b2\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Mar 4 17:16:09 2013 +0900\n\
 \\n\
 \    Fix many ghc warning. (cont.)\n\
 \\n\
 \commit db79e4312182cc8fc5d3a1d0afa48a343faf3729\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Mar 4 17:14:56 2013 +0900\n\
 \\n\
 \    Set version 0.8.0.0. Ajhc use version number as 0.8.0.x.\n\
 \\n\
 \commit 502b91a34e7b4c8841f563c0eb458ccab858d3e5\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Mar 4 15:41:23 2013 +0900\n\
 \\n\
 \    Fix many ghc warning. / Add cleanup files.\n\
 \\n\
 \commit f64cb5a883c096c2ffd7ce85e22d3c6ec1494892\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Mon Mar 4 13:06:24 2013 +0900\n\
 \\n\
 \    Fix some warning on compile ajhc.\n\
 \\n\
 \commit b3a50378e3d7c5e9f8d52bece2c5250b07f97595\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 2 18:26:24 2013 +0900\n\
 \\n\
 \    Build manual_ja.html with po4a.\n\
 \\n\
 \commit 531599b10155db4aeb953da3499437320f68efb0\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 2 17:46:02 2013 +0900\n\
 \\n\
 \    Poke function should use volatile prefix to write memory.\n\
 \\n\
 \commit 7d28d966944f8e29e8b870cb93d82af76a539690\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 2 17:44:23 2013 +0900\n\
 \\n\
 \    rename aligned_alloc to use it with C11 libc\n\
 \\n\
 \commit 48f25d2099c7e355768ef410e5464f7561a9be36\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 2 17:23:05 2013 +0900\n\
 \\n\
 \    Remove pbuilder setting, because travis-ci can't use chroot.\n\
 \    \"W: Failure trying to run: chroot /var/cache/pbuilder/build/15916/. mount -t proc proc /proc\"\n\
 \\n\
 \commit 186e80ebf33094e033dc2e537e54c3713a042e56\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 2 16:55:11 2013 +0900\n\
 \\n\
 \    Fix option miss.\n\
 \\n\
 \commit fbb3fd2aa54dd51288adf8f233d76142b1bac402\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 2 16:37:30 2013 +0900\n\
 \\n\
 \    Test pbuilder on travis-ci with pbuilderrc file.\n\
 \\n\
 \commit b99be49d897b23663762ebdbe027489b42312581\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 2 16:09:30 2013 +0900\n\
 \\n\
 \    Test pbuilder on travis-ci.\n\
 \\n\
 \commit cf28f1e69aee1732c335ee79524a9832eeb92776\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 2 14:10:13 2013 +0900\n\
 \\n\
 \    s/jhc/ajhc/g\n\
 \\n\
 \commit d1002d3d8e296dcdc488a41e604eb6c2898a4731\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 2 11:44:53 2013 +0900\n\
 \\n\
 \    Add README.md file for github web page and travis-ci build status.\n\
 \\n\
 \commit ac5f7963e6ae1c27769afa2d00200c1197d48755\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 2 11:41:56 2013 +0900\n\
 \\n\
 \    Jhs does not support make -j?\n\
 \\n\
 \commit a41169ffc25083c5a6a8429956b237562ec2cdb3\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 2 11:31:45 2013 +0900\n\
 \\n\
 \    Add -j3 option for make command.\n\
 \\n\
 \commit b0f9637ea5617438058c187216d5ad924c7c87cb\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 2 11:15:40 2013 +0900\n\
 \\n\
 \    Need libconfig-yaml-perl to regress on travis-ci.\n\
 \\n\
 \commit 69a66d5f4faa8410ceed63845e51179e60fb9b29\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 2 11:00:16 2013 +0900\n\
 \\n\
 \    Do not use locales-all. It's not found on travis-ci.\n\
 \\n\
 \commit 6cbc218a48fd7b70d5743a6f3b0a78b287f583b7\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 2 10:54:42 2013 +0900\n\
 \\n\
 \    Write .travis.yml from scratch.\n\
 \\n\
 \commit 2fa490f923f444840ee1fc51bf0e2e378d3b989a\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 2 07:23:15 2013 +0900\n\
 \\n\
 \    Add regress target to Makefile.\n\
 \\n\
 \commit f1e72fd1bdfd1d78634d009471e0c76ae61f2f66\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 2 06:59:16 2013 +0900\n\
 \\n\
 \    Use autoconf on travis-ci.\n\
 \\n\
 \commit d80e701a15200d4958b98f0cb318347b6dd6f56f\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 2 06:42:01 2013 +0900\n\
 \\n\
 \    Add Travis CI setting file.\n\
 \\n\
 \commit 5892ff8176c0d5179e75ab4c27ea6f58ddc067b9\n\
 \Author: Kiwamu Okabe <kiwamu@debian.or.jp>\n\
 \Date:   Sat Mar 2 02:44:13 2013 +0900\n\
 \\n\
 \    Add gitignore file. / Use git for darcs to dump Changelog.\n\
 \"#

-- | Generated from src\/data\/shortchange.txt
{-# NOINLINE shortchange_txt #-}
shortchange_txt :: ByteString
shortchange_txt = unsafePerformIO $ unsafePackAddress "\
 \a1265f031e6b75f9346ec23ca7caec0b5ced82bc\
 \"#

-- | Generated from rts\/rts\/slub.c
{-# NOINLINE slub_c #-}
slub_c :: ByteString
slub_c = unsafePerformIO $ unsafePackAddress "\
 \#if _JHC_GC == _JHC_GC_JGC\n\
 \\n\
 \#ifdef JHC_HEADER\n\
 \\n\
 \struct s_arena;\n\
 \struct s_cache;\n\
 \static void *s_alloc(gc_t gc, struct s_cache *sc) A_UNUSED;\n\
 \static struct s_cache *find_cache(struct s_cache **rsc, struct s_arena *arena,\n\
 \                                  unsigned short size, unsigned short num_ptrs) A_UNUSED;\n\
 \static bool s_set_used_bit(void *val) A_UNUSED;\n\
 \static void clear_used_bits(struct s_arena *arena) A_UNUSED;\n\
 \\n\
 \#define S_BLOCK(val) ((struct s_block *)((uintptr_t)(val) & ~ (BLOCK_SIZE - 1)))\n\
 \#define BLOCK_SIZE     (1UL << 12)\n\
 \#define MEGABLOCK_SIZE (1UL << 20)\n\
 \\n\
 \#else\n\
 \\n\
 \#include \"sys/bitarray.h\"\n\
 \#include \"sys/queue.h\"\n\
 \#include \"rts/profile.h\"\n\
 \\n\
 \struct s_arena {\n\
 \        struct s_megablock *current_megablock;\n\
 \        SLIST_HEAD(,s_block) free_blocks;\n\
 \        unsigned block_used;\n\
 \        unsigned block_threshold;\n\
 \        SLIST_HEAD(,s_cache) caches;\n\
 \        SLIST_HEAD(,s_megablock) megablocks;\n\
 \};\n\
 \\n\
 \struct s_megablock {\n\
 \        void *base;\n\
 \        unsigned next_free;\n\
 \        SLIST_ENTRY(s_megablock) next;\n\
 \};\n\
 \\n\
 \struct s_block_info {\n\
 \        unsigned char color;\n\
 \        unsigned char size;\n\
 \        unsigned char num_ptrs;\n\
 \        unsigned char flags;\n\
 \};\n\
 \\n\
 \struct s_block {\n\
 \        SLIST_ENTRY(s_block) link;\n\
 \        struct s_block_info pi;\n\
 \        unsigned short num_free;\n\
 \        unsigned short next_free;\n\
 \        bitarray_t used[];\n\
 \};\n\
 \\n\
 \struct s_cache {\n\
 \        SLIST_ENTRY(s_cache) next;\n\
 \        SLIST_HEAD(,s_block) blocks;\n\
 \        SLIST_HEAD(,s_block) full_blocks;\n\
 \        struct s_block_info pi;\n\
 \        unsigned short num_entries;\n\
 \        struct s_arena *arena;\n\
 \};\n\
 \\n\
 \/* This finds a bit that isn't set, sets it, then returns its index.  It\n\
 \ * assumes that a bit is available to be found, otherwise it goes into an\n\
 \ * infinite loop. */\n\
 \\n\
 \static unsigned\n\
 \bitset_find_free(unsigned *next_free,int n,bitarray_t ba[static n]) {\n\
 \        assert(*next_free < (unsigned)n);\n\
 \        unsigned i = *next_free;\n\
 \        do {\n\
 \                int o = __builtin_ffsl(~ba[i]);\n\
 \                if(__predict_true(o)) {\n\
 \                        ba[i] |= (1UL << (o - 1));\n\
 \                        *next_free = i;\n\
 \                        return (i*BITS_PER_UNIT + (o - 1));\n\
 \                }\n\
 \                i = (i + 1) % n;\n\
 \                assert(i != *next_free);\n\
 \        } while (1);\n\
 \}\n\
 \\n\
 \struct s_megablock *\n\
 \s_new_megablock(struct s_arena *arena)\n\
 \{\n\
 \        struct s_megablock *mb = malloc(sizeof(*mb));\n\
 \#if defined(__WIN32__)\n\
 \        mb->base = _aligned_malloc(MEGABLOCK_SIZE, BLOCK_SIZE);\n\
 \        int ret = !mb->base;\n\
 \#elif defined(__ARM_EABI__)\n\
 \        mb->base = memalign(BLOCK_SIZE,MEGABLOCK_SIZE);\n\
 \        int ret = !mb->base;\n\
 \#elif (defined(__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__) && __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ <  1060)\n\
 \        assert(sysconf(_SC_PAGESIZE) == BLOCK_SIZE);\n\
 \        mb->base = valloc(MEGABLOCK_SIZE);\n\
 \        int ret = !mb->base;\n\
 \#else\n\
 \        int ret = posix_memalign(&mb->base,BLOCK_SIZE,MEGABLOCK_SIZE);\n\
 \#endif\n\
 \        if(ret != 0) {\n\
 \                jhc_printf_stderr(\"Unable to allocate memory for megablock\\n\");\n\
 \                abort();\n\
 \        }\n\
 \        VALGRIND_MAKE_MEM_NOACCESS(mb->base,MEGABLOCK_SIZE);\n\
 \        //VALGRIND_FREELIKE_BLOCK(mb->base,0);\n\
 \        mb->next_free = 0;\n\
 \        return mb;\n\
 \}\n\
 \\n\
 \/* block allocator */\n\
 \\n\
 \static struct s_block *\n\
 \get_free_block(gc_t gc, struct s_arena *arena) {\n\
 \        arena->block_used++;\n\
 \        if(__predict_true(SLIST_FIRST(&arena->free_blocks))) {\n\
 \                struct s_block *pg = SLIST_FIRST(&arena->free_blocks);\n\
 \                SLIST_REMOVE_HEAD(&arena->free_blocks,link);\n\
 \                return pg;\n\
 \        } else {\n\
 \                if((arena->block_used >= arena->block_threshold)) {\n\
 \                        gc_perform_gc(gc);\n\
 \                        // if we are still using 80% of the heap after a gc, raise the threshold.\n\
 \                        if(__predict_false((unsigned)arena->block_used * 10 >= arena->block_threshold * 9)) {\n\
 \                                arena->block_threshold *= 2;\n\
 \                        }\n\
 \                }\n\
 \                if(__predict_false(!arena->current_megablock))\n\
 \                        arena->current_megablock = s_new_megablock(arena);\n\
 \                struct s_megablock *mb = arena->current_megablock;\n\
 \                struct s_block *pg = mb->base + BLOCK_SIZE*mb->next_free;\n\
 \                mb->next_free++;\n\
 \                if(mb->next_free == MEGABLOCK_SIZE / BLOCK_SIZE) {\n\
 \                        SLIST_INSERT_HEAD(&arena->megablocks,mb, next);\n\
 \                        arena->current_megablock = NULL;\n\
 \                }\n\
 \                VALGRIND_MAKE_MEM_UNDEFINED(pg,sizeof(struct s_block));\n\
 \                pg->num_free = 0;\n\
 \                return pg;\n\
 \        }\n\
 \}\n\
 \\n\
 \static void\n\
 \s_cleanup_blocks(struct s_arena *arena) {\n\
 \        struct s_cache *sc = SLIST_FIRST(&arena->caches);\n\
 \        for(;sc;sc = SLIST_NEXT(sc,next)) {\n\
 \\n\
 \                // 'best' keeps track of the block with the fewest free spots\n\
 \                // and percolates it to the front, effectively a single pass\n\
 \                // of a bubblesort to help combat fragmentation. It does\n\
 \                // not increase the complexity of the cleanup algorithm as\n\
 \                // we had to scan every block anyway, but over many passes\n\
 \                // of the GC it will eventually result in a more sorted list\n\
 \                // than would occur by chance.\n\
 \\n\
 \                struct s_block *best = NULL;\n\
 \                int free_best = 4096;\n\
 \                struct s_block *pg = SLIST_FIRST(&sc->blocks);\n\
 \                struct s_block *fpg = SLIST_FIRST(&sc->full_blocks);\n\
 \                SLIST_INIT(&sc->blocks);\n\
 \                SLIST_INIT(&sc->full_blocks);\n\
 \                if(!pg) {\n\
 \                        pg = fpg;\n\
 \                        fpg = NULL;\n\
 \                }\n\
 \                while(pg) {\n\
 \                        struct s_block *npg = SLIST_NEXT(pg,link);\n\
 \                        if(__predict_false(pg->num_free == 0)) {\n\
 \                                SLIST_INSERT_HEAD(&sc->full_blocks,pg,link);\n\
 \                        } else if(__predict_true(pg->num_free == sc->num_entries)) {\n\
 \                                arena->block_used--;\n\
 \                                VALGRIND_MAKE_MEM_NOACCESS((char *)pg + sizeof(struct s_block), BLOCK_SIZE - sizeof(struct s_block));\n\
 \                                SLIST_INSERT_HEAD(&arena->free_blocks,pg,link);\n\
 \                        } else {\n\
 \                                if(!best) {\n\
 \                                        free_best = pg->num_free;\n\
 \                                        best = pg;\n\
 \                                } else {\n\
 \                                        if(pg->num_free < free_best) {\n\
 \                                                struct s_block *tmp = best;\n\
 \                                                best = pg; pg = tmp;\n\
 \                                                free_best = pg->num_free;\n\
 \                                        }\n\
 \                                        SLIST_INSERT_HEAD(&sc->blocks,pg,link);\n\
 \                                }\n\
 \                        }\n\
 \                        if(!npg && fpg) {\n\
 \                                pg = fpg;\n\
 \                                fpg = NULL;\n\
 \                        } else\n\
 \                                pg = npg;\n\
 \                }\n\
 \                if(best)\n\
 \                        SLIST_INSERT_HEAD(&sc->blocks,best,link);\n\
 \        }\n\
 \}\n\
 \\n\
 \inline static void\n\
 \clear_block_used_bits(unsigned num_entries, struct s_block *pg)\n\
 \{\n\
 \        pg->num_free = num_entries;\n\
 \        memset(pg->used,0,BITARRAY_SIZE_IN_BYTES(num_entries) - sizeof(pg->used[0]));\n\
 \        int excess = num_entries % BITS_PER_UNIT;\n\
 \        pg->used[BITARRAY_SIZE(num_entries) - 1] = ~((1UL << excess) - 1);\n\
 \#if JHC_VALGRIND\n\
 \                unsigned header =  sizeof(struct s_block) + BITARRAY_SIZE_IN_BYTES(num_entries);\n\
 \                VALGRIND_MAKE_MEM_NOACCESS((char *)pg + header, BLOCK_SIZE - header);\n\
 \#endif\n\
 \}\n\
 \\n\
 \static void *\n\
 \s_alloc(gc_t gc, struct s_cache *sc)\n\
 \{\n\
 \        struct s_block *pg = SLIST_FIRST(&sc->blocks);\n\
 \        if(__predict_false(!pg)) {\n\
 \                pg = get_free_block(gc, sc->arena);\n\
 \                VALGRIND_MAKE_MEM_NOACCESS(pg, BLOCK_SIZE);\n\
 \                VALGRIND_MAKE_MEM_DEFINED(pg, sizeof(struct s_block));\n\
 \                if(sc->num_entries != pg->num_free)\n\
 \                        VALGRIND_MAKE_MEM_UNDEFINED((char *)pg->used,BITARRAY_SIZE_IN_BYTES(sc->num_entries));\n\
 \                else\n\
 \                        VALGRIND_MAKE_MEM_DEFINED((char *)pg->used,BITARRAY_SIZE_IN_BYTES(sc->num_entries));\n\
 \                assert(pg);\n\
 \                pg->pi = sc->pi;\n\
 \                pg->next_free = 0;\n\
 \                SLIST_INSERT_HEAD(&sc->blocks,pg,link);\n\
 \                if(sc->num_entries != pg->num_free)\n\
 \                        clear_block_used_bits(sc->num_entries, pg);\n\
 \                pg->used[0] = 1; //set the first bit\n\
 \                pg->num_free = sc->num_entries - 1;\n\
 \                return (uintptr_t *)pg + pg->pi.color;\n\
 \        } else {\n\
 \                __builtin_prefetch(pg->used,1);\n\
 \                pg->num_free--;\n\
 \                unsigned next_free = pg->next_free;\n\
 \                unsigned found = bitset_find_free(&next_free,BITARRAY_SIZE(sc->num_entries),pg->used);\n\
 \                pg->next_free = next_free;\n\
 \                void *val = (uintptr_t *)pg + pg->pi.color + found*pg->pi.size;\n\
 \                if(__predict_false(0 == pg->num_free)) {\n\
 \                        assert(pg == SLIST_FIRST(&sc->blocks));\n\
 \                        SLIST_REMOVE_HEAD(&sc->blocks,link);\n\
 \                        SLIST_INSERT_HEAD(&sc->full_blocks,pg,link);\n\
 \                }\n\
 \                assert(S_BLOCK(val) == pg);\n\
 \                //jhc_printf_stderr(\"s_alloc: val: %p s_block: %p size: %i color: %i found: %i num_free: %i\\n\", val, pg, pg->pi.size, pg->pi.color, found, pg->num_free);\n\
 \                return val;\n\
 \        }\n\
 \}\n\
 \\n\
 \/*\n\
 \static void\n\
 \s_free(void *val)\n\
 \{\n\
 \        assert(val);\n\
 \        struct s_block *pg = s_block(val);\n\
 \        unsigned int offset = ((uintptr_t *)val - (uintptr_t *)pg) - pg->pi.color;\n\
 \//        jhc_printf_stderr(\"s_free:  val: %p s_block: %p size: %i color: %i num_free: %i offset: %i bit: %i\\n\", val, pg, pg->pi.size, pg->pi.color, pg->num_free, offset, offset/pg->pi.size);\n\
 \        assert(BIT_VALUE(pg->used,offset/(pg->pi.size)));\n\
 \        BIT_UNSET(pg->used,offset/(pg->pi.size));\n\
 \        pg->num_free++;\n\
 \}\n\
 \*/\n\
 \\n\
 \static struct s_cache *\n\
 \new_cache(struct s_arena *arena, unsigned short size, unsigned short num_ptrs)\n\
 \{\n\
 \        struct s_cache *sc = malloc(sizeof(*sc));\n\
 \        sc->arena = arena;\n\
 \        sc->pi.size = size;\n\
 \        sc->pi.num_ptrs = num_ptrs;\n\
 \        sc->pi.flags = 0;\n\
 \        size_t excess = BLOCK_SIZE - sizeof(struct s_block);\n\
 \        sc->num_entries = (8*excess) / (8*sizeof(uintptr_t)*size + 1) - 1;\n\
 \        //sc->num_entries = (8*excess) / (8*size*sizeof(uintptr_t) + 1);\n\
 \        sc->pi.color = (sizeof(struct s_block) + BITARRAY_SIZE_IN_BYTES(sc->num_entries) + sizeof(uintptr_t) - 1) / sizeof(uintptr_t);\n\
 \        SLIST_INIT(&sc->blocks);\n\
 \        SLIST_INIT(&sc->full_blocks);\n\
 \        SLIST_INSERT_HEAD(&arena->caches,sc,next);\n\
 \        //print_cache(sc);\n\
 \        return sc;\n\
 \}\n\
 \\n\
 \// clear all used bits, must be followed by a marking phase.\n\
 \static void\n\
 \clear_used_bits(struct s_arena *arena)\n\
 \{\n\
 \        struct s_cache *sc = SLIST_FIRST(&arena->caches);\n\
 \        for(;sc;sc = SLIST_NEXT(sc,next)) {\n\
 \                struct s_block *pg = SLIST_FIRST(&sc->blocks);\n\
 \                struct s_block *fpg = SLIST_FIRST(&sc->full_blocks);\n\
 \                do {\n\
 \                        for(;pg;pg = SLIST_NEXT(pg,link))\n\
 \                                clear_block_used_bits(sc->num_entries,pg);\n\
 \                        pg = fpg;\n\
 \                        fpg = NULL;\n\
 \                }  while(pg);\n\
 \        }\n\
 \}\n\
 \\n\
 \// set a used bit. returns true if the\n\
 \// tagged node should be scanned by the GC.\n\
 \// this happens when the used bit was not previously set\n\
 \// and the node contains internal pointers.\n\
 \\n\
 \static bool\n\
 \s_set_used_bit(void *val)\n\
 \{\n\
 \        assert(val);\n\
 \        struct s_block *pg = S_BLOCK(val);\n\
 \        unsigned int offset = ((uintptr_t *)val - (uintptr_t *)pg) - pg->pi.color;\n\
 \        if(__predict_true(BIT_IS_UNSET(pg->used,offset/pg->pi.size))) {\n\
 \                BIT_SET(pg->used,offset/pg->pi.size);\n\
 \                pg->num_free--;\n\
 \                return (bool)pg->pi.num_ptrs;\n\
 \        }\n\
 \        return false;\n\
 \}\n\
 \\n\
 \static struct s_cache *\n\
 \find_cache(struct s_cache **rsc, struct s_arena *arena, unsigned short size, unsigned short num_ptrs)\n\
 \{\n\
 \        if(__predict_true(rsc && *rsc))\n\
 \                return *rsc;\n\
 \        struct s_cache *sc = SLIST_FIRST(&arena->caches);\n\
 \        for(;sc;sc = SLIST_NEXT(sc,next)) {\n\
 \                if(sc->pi.size == size && sc->pi.num_ptrs == num_ptrs)\n\
 \                        goto found;\n\
 \        }\n\
 \        sc = new_cache(arena,size,num_ptrs);\n\
 \found:\n\
 \        if(rsc)\n\
 \                *rsc = sc;\n\
 \        return sc;\n\
 \}\n\
 \\n\
 \struct s_arena *\n\
 \new_arena(void) {\n\
 \        struct s_arena *arena = malloc(sizeof(struct s_arena));\n\
 \        SLIST_INIT(&arena->caches);\n\
 \        SLIST_INIT(&arena->free_blocks);\n\
 \        SLIST_INIT(&arena->megablocks);\n\
 \        arena->block_used = 0;\n\
 \        arena->block_threshold = 8;\n\
 \        arena->current_megablock = NULL;\n\
 \        return arena;\n\
 \}\n\
 \\n\
 \void\n\
 \print_cache(struct s_cache *sc) {\n\
 \        jhc_printf_stderr( \"num_entries: %i\\n\",(int)sc->num_entries);\n\
 \//        jhc_printf_stderr(\"  entries: %i words\\n\",(int)(sc->num_entries*sc->pi.size));\n\
 \        jhc_printf_stderr( \"  header: %lu bytes\\n\", sizeof(struct s_block) + BITARRAY_SIZE_IN_BYTES(sc->num_entries));\n\
 \        jhc_printf_stderr( \"  size: %i words\\n\",(int)sc->pi.size);\n\
 \//        jhc_printf_stderr(\"  color: %i words\\n\",(int)sc->pi.color);\n\
 \        jhc_printf_stderr( \"  nptrs: %i words\\n\",(int)sc->pi.num_ptrs);\n\
 \//        jhc_printf_stderr(\"  end: %i bytes\\n\",(int)(sc->pi.color+ sc->num_entries*sc->pi.size)*sizeof(uintptr_t));\n\
 \        jhc_printf_stderr( \"%20s %9s %9s %s\\n\", \"block\", \"num_free\", \"next_free\", \"status\");\n\
 \        struct s_block *pg;\n\
 \        SLIST_FOREACH(pg,&sc->blocks,link) {\n\
 \            jhc_printf_stderr( \"%20p %9i %9i %c\\n\", pg, pg->num_free, pg->next_free, 'P');\n\
 \        }\n\
 \        jhc_printf_stderr( \"  full_blocks:\\n\");\n\
 \        SLIST_FOREACH(pg,&sc->full_blocks,link) {\n\
 \            jhc_printf_stderr( \"%20p %9i %9i %c\\n\", pg, pg->num_free, pg->next_free, 'F');\n\
 \        }\n\
 \}\n\
 \\n\
 \#endif\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/gc_jgc.c
{-# NOINLINE gc_jgc_c #-}
gc_jgc_c :: ByteString
gc_jgc_c = unsafePerformIO $ unsafePackAddress "\
 \#include \"jhc_rts_header.h\"\n\
 \#include \"sys/queue.h\"\n\
 \#include \"sys/bitarray.h\"\n\
 \#include \"rts/cdefs.h\"\n\
 \#include \"rts/constants.h\"\n\
 \#include \"rts/gc_jgc_internal.h\"\n\
 \#include \"rts/conc.h\"\n\
 \\n\
 \#if _JHC_GC == _JHC_GC_JGC\n\
 \\n\
 \#if defined(_JHC_JGC_LIMITED_NUM_MEGABLOCK)\n\
 \static char aligned_megablock[(MEGABLOCK_SIZE)*(_JHC_JGC_LIMITED_NUM_MEGABLOCK)] __attribute__ ((aligned(BLOCK_SIZE)));\n\
 \#endif\n\
 \#if defined(_JHC_JGC_LIMITED_NUM_GC_STACK)\n\
 \static char gc_stack_base_area[(GC_STACK_SIZE)*sizeof(gc_t)*(_JHC_JGC_LIMITED_NUM_GC_STACK)];\n\
 \#endif\n\
 \SLIST_HEAD(,s_arena) used_arenas;\n\
 \SLIST_HEAD(,s_arena) free_arenas;\n\
 \SLIST_HEAD(,s_megablock) free_megablocks;\n\
 \SLIST_HEAD(,s_block) free_monolithic_blocks;\n\
 \\n\
 \#define TO_GCPTR(x) (entry_t *)(FROM_SPTR(x))\n\
 \\n\
 \void gc_perform_gc(gc_t gc, arena_t arena) A_STD;\n\
 \static bool s_set_used_bit(void *val) A_UNUSED;\n\
 \static void clear_used_bits(arena_t arena) A_UNUSED;\n\
 \static void s_cleanup_blocks(arena_t arena);\n\
 \static struct s_block *get_free_block(gc_t gc, arena_t arena, bool retry);\n\
 \static void *jhc_aligned_alloc(unsigned size);\n\
 \\n\
 \typedef struct {\n\
 \        sptr_t ptrs[0];\n\
 \} entry_t;\n\
 \\n\
 \static const void *nh_start, *nh_end;\n\
 \\n\
 \static bool\n\
 \gc_check_heap(entry_t *s)\n\
 \{\n\
 \        return (s < (entry_t *)nh_start || s > (entry_t *)nh_end);\n\
 \}\n\
 \\n\
 \struct stack {\n\
 \        unsigned size;\n\
 \        unsigned ptr;\n\
 \        entry_t * *stack;\n\
 \};\n\
 \\n\
 \#define EMPTY_STACK { 0, 0, NULL }\n\
 \\n\
 \static void\n\
 \stack_grow(struct stack *s, unsigned grow)\n\
 \{\n\
 \        s->size += grow;\n\
 \        s->stack = realloc(s->stack, sizeof(s->stack[0])*s->size);\n\
 \        assert(s->stack);\n\
 \        debugf(\"stack:\");\n\
 \        for(unsigned i = 0; i < s->ptr; i++) {\n\
 \                debugf(\" %p\", (void *)s->stack[i]);\n\
 \        }\n\
 \        debugf(\"\\n\");\n\
 \}\n\
 \\n\
 \inline static void\n\
 \stack_check(struct stack *s, unsigned n) {\n\
 \        if(__predict_false(s->size - s->ptr < n)) {\n\
 \#ifndef _JHC_JGC_STACKGROW\n\
 \#define _JHC_JGC_STACKGROW (1024)\n\
 \#endif\n\
 \                stack_grow(s,n + (_JHC_JGC_STACKGROW));\n\
 \        }\n\
 \}\n\
 \\n\
 \static struct stack root_stack = EMPTY_STACK;\n\
 \\n\
 \void gc_add_root(gc_t gc, arena_t arena, void *root)\n\
 \{\n\
 \        if(IS_PTR(root)) {\n\
 \                entry_t *nroot = TO_GCPTR(root);\n\
 \                if(gc_check_heap(nroot)) {\n\
 \                        stack_check(&root_stack,1);\n\
 \                        root_stack.stack[root_stack.ptr++] = nroot;\n\
 \                }\n\
 \        }\n\
 \}\n\
 \\n\
 \static void\n\
 \gc_add_grey(struct stack *stack, entry_t *s)\n\
 \{\n\
 \        VALGRIND_MAKE_MEM_DEFINED(s,(S_BLOCK(s))->u.pi.size * sizeof(uintptr_t));\n\
 \        if(gc_check_heap(s) && s_set_used_bit(s))\n\
 \                stack->stack[stack->ptr++] = s;\n\
 \}\n\
 \\n\
 \static void\n\
 \gc_mark_deeper(struct stack *stack, unsigned *number_redirects)\n\
 \{\n\
 \        while(stack->ptr) {\n\
 \                entry_t *e = stack->stack[--stack->ptr];\n\
 \                struct s_block *pg = S_BLOCK(e);\n\
 \                if(!(pg->flags & SLAB_MONOLITH))\n\
 \                        VALGRIND_MAKE_MEM_DEFINED(e,pg->u.pi.size * sizeof(uintptr_t));\n\
 \                debugf(\"Processing Grey: %p\\n\",e);\n\
 \                unsigned num_ptrs = pg->flags & SLAB_MONOLITH ? pg->u.m.num_ptrs : pg->u.pi.num_ptrs;\n\
 \                stack_check(stack, num_ptrs);\n\
 \                for(unsigned i = 0; i < num_ptrs; i++) {\n\
 \                        if(1 && (P_LAZY == GET_PTYPE(e->ptrs[i]))) {\n\
 \                                VALGRIND_MAKE_MEM_DEFINED(FROM_SPTR(e->ptrs[i]), sizeof(uintptr_t));\n\
 \                                if(!IS_LAZY(GETHEAD(FROM_SPTR(e->ptrs[i])))) {\n\
 \                                        *number_redirects++;\n\
 \                                        debugf(\" *\");\n\
 \                                        e->ptrs[i] = (sptr_t)GETHEAD(FROM_SPTR(e->ptrs[i]));\n\
 \                                }\n\
 \                        }\n\
 \                        if(IS_PTR(e->ptrs[i])) {\n\
 \                                entry_t * ptr = TO_GCPTR(e->ptrs[i]);\n\
 \                                debugf(\"Following: %p %p\\n\",e->ptrs[i], ptr);\n\
 \                                gc_add_grey(stack, ptr);\n\
 \                        }\n\
 \                }\n\
 \        }\n\
 \}\n\
 \\n\
 \#if defined(_JHC_JGC_SAVING_MALLOC_HEAP)\n\
 \#define DO_GC_MARK_DEEPER(S,N)  gc_mark_deeper((S),(N))\n\
 \#else\n\
 \#define DO_GC_MARK_DEEPER(S,N)  do { } while (/* CONSTCOND */ 0)\n\
 \#endif\n\
 \\n\
 \void A_STD\n\
 \gc_perform_gc(gc_t gc, arena_t arena)\n\
 \{\n\
 \        profile_push(&gc_gc_time);\n\
 \        arena->number_gcs++;\n\
 \\n\
 \        unsigned number_redirects = 0;\n\
 \        unsigned number_stack = 0;\n\
 \        unsigned number_ptr = 0;\n\
 \        struct stack stack = EMPTY_STACK;\n\
 \\n\
 \        clear_used_bits(arena);\n\
 \\n\
 \        debugf(\"Setting Roots:\");\n\
 \        stack_check(&stack, root_stack.ptr);\n\
 \        for(unsigned i = 0; i < root_stack.ptr; i++) {\n\
 \                gc_add_grey(&stack, root_stack.stack[i]);\n\
 \                debugf(\" %p\", root_stack.stack[i]);\n\
 \                DO_GC_MARK_DEEPER(&stack, &number_redirects);\n\
 \        }\n\
 \        debugf(\" # \");\n\
 \        struct StablePtr *sp;\n\
 \\n\
 \        jhc_rts_lock();\n\
 \        LIST_FOREACH(sp, &root_StablePtrs, link) {\n\
 \            gc_add_grey(&stack, (entry_t *)sp);\n\
 \            debugf(\" %p\", sp);\n\
 \            DO_GC_MARK_DEEPER(&stack, &number_redirects);\n\
 \        }\n\
 \        jhc_rts_unlock();\n\
 \\n\
 \        debugf(\"\\n\");\n\
 \        debugf(\"Trace:\");\n\
 \#if defined(_JHC_JGC_SAVING_MALLOC_HEAP)\n\
 \        stack_check(&stack, 1); // Just alloc\n\
 \#else\n\
 \        stack_check(&stack, gc - arena->gc_stack_base);\n\
 \#endif\n\
 \        number_stack = gc - arena->gc_stack_base;\n\
 \        for(unsigned i = 0; i < number_stack; i++) {\n\
 \                debugf(\" |\");\n\
 \                // TODO - short circuit redirects on stack\n\
 \                sptr_t ptr = arena->gc_stack_base[i];\n\
 \                if(1 && (IS_LAZY(ptr))) {\n\
 \                        assert(GET_PTYPE(ptr) == P_LAZY);\n\
 \                        VALGRIND_MAKE_MEM_DEFINED(FROM_SPTR(ptr), sizeof(uintptr_t));\n\
 \                        if(!IS_LAZY(GETHEAD(FROM_SPTR(ptr)))) {\n\
 \                                void *gptr = TO_GCPTR(ptr);\n\
 \                                if(gc_check_heap(gptr))\n\
 \                                        s_set_used_bit(gptr);\n\
 \                                number_redirects++;\n\
 \                                debugf(\" *\");\n\
 \                                ptr = (sptr_t)GETHEAD(FROM_SPTR(ptr));\n\
 \                        }\n\
 \                }\n\
 \                if(__predict_false(!IS_PTR(ptr))) {\n\
 \                        debugf(\" -\");\n\
 \                        continue;\n\
 \                }\n\
 \                number_ptr++;\n\
 \                entry_t *e = TO_GCPTR(ptr);\n\
 \                debugf(\" %p\",(void *)e);\n\
 \                gc_add_grey(&stack, e);\n\
 \                DO_GC_MARK_DEEPER(&stack, &number_redirects);\n\
 \        }\n\
 \        debugf(\"\\n\");\n\
 \\n\
 \        gc_mark_deeper(&stack, &number_redirects); // Final marking\n\
 \        free(stack.stack);\n\
 \        s_cleanup_blocks(arena);\n\
 \        if (JHC_STATUS) {\n\
 \                jhc_printf_stderr(\"%3u - %6u Used: %4u Thresh: %4u Ss: %5u Ps: %5u Rs: %5u Root: %3u\\n\",\n\
 \                        arena->number_gcs,\n\
 \                        arena->number_allocs,\n\
 \                        (unsigned)arena->block_used,\n\
 \                        (unsigned)arena->block_threshold,\n\
 \                        number_stack,\n\
 \                        number_ptr,\n\
 \                        number_redirects,\n\
 \                        (unsigned)root_stack.ptr\n\
 \                       );\n\
 \                arena->number_allocs = 0;\n\
 \        }\n\
 \        profile_pop(&gc_gc_time);\n\
 \}\n\
 \\n\
 \/* Enter with rts_lock. */\n\
 \static gc_t\n\
 \new_gc_stack(arena_t arena) {\n\
 \        if (!arena->gc_stack_base) {\n\
 \#if defined(_JHC_JGC_LIMITED_NUM_GC_STACK)\n\
 \                static int count = 0;\n\
 \                if (count >= _JHC_JGC_LIMITED_NUM_GC_STACK) {\n\
 \                        abort();\n\
 \                }\n\
 \                arena->gc_stack_base = (void *) (gc_stack_base_area +\n\
 \                    (GC_STACK_SIZE) * sizeof(gc_t) * count);\n\
 \                count++;\n\
 \#else\n\
 \                arena->gc_stack_base = malloc((GC_STACK_SIZE)*sizeof(arena->gc_stack_base[0]));\n\
 \#endif\n\
 \        }\n\
 \        return arena->gc_stack_base;\n\
 \}\n\
 \\n\
 \static int jhc_alloc_init_count = 0;\n\
 \void\n\
 \jhc_alloc_init(gc_t *gc_p,arena_t *arena_p) {\n\
 \        VALGRIND_PRINTF(\"Jhc-Valgrind mode active.\\n\");\n\
 \\n\
 \        jhc_rts_lock();\n\
 \        if (!jhc_alloc_init_count++) {\n\
 \                SLIST_INIT(&used_arenas);\n\
 \                SLIST_INIT(&free_arenas);\n\
 \                SLIST_INIT(&free_megablocks);\n\
 \                SLIST_INIT(&free_monolithic_blocks);\n\
 \                if(nh_stuff[0]) {\n\
 \                        nh_end = nh_start = nh_stuff[0];\n\
 \                        for(int i = 1; nh_stuff[i]; i++) {\n\
 \                                if(nh_stuff[i] < nh_start)\n\
 \                                        nh_start = nh_stuff[i];\n\
 \                                if(nh_stuff[i] > nh_end)\n\
 \                                        nh_end = nh_stuff[i];\n\
 \                        }\n\
 \                }\n\
 \        }\n\
 \        *arena_p = new_arena();\n\
 \        SLIST_INSERT_HEAD(&used_arenas, *arena_p, link);\n\
 \        *gc_p = new_gc_stack(*arena_p);\n\
 \        jhc_rts_unlock();\n\
 \}\n\
 \\n\
 \void\n\
 \jhc_alloc_fini(gc_t gc,arena_t arena) {\n\
 \        struct s_block *pg;\n\
 \        struct s_megablock *mb;\n\
 \        struct s_cache *sc;\n\
 \\n\
 \        if(_JHC_PROFILE || JHC_STATUS) {\n\
 \                jhc_printf_stderr(\"arena: %p\\n\", arena);\n\
 \                jhc_printf_stderr(\"  block_used: %i\\n\", arena->block_used);\n\
 \                jhc_printf_stderr(\"  block_threshold: %i\\n\", arena->block_threshold);\n\
 \                struct s_cache *sc;\n\
 \                SLIST_FOREACH(sc,&arena->caches,next)\n\
 \                        print_cache(sc);\n\
 \        }\n\
 \\n\
 \        jhc_rts_lock();\n\
 \        SLIST_FOREACH(pg, &arena->monolithic_blocks, link) {\n\
 \                SLIST_INSERT_HEAD(&free_monolithic_blocks, pg, link);\n\
 \        }\n\
 \        SLIST_FOREACH(mb, &arena->megablocks, next) {\n\
 \                SLIST_INSERT_HEAD(&free_megablocks, mb, next);\n\
 \        }\n\
 \        if(arena->current_megablock) {\n\
 \                SLIST_INSERT_HEAD(&free_megablocks, arena->current_megablock, next);\n\
 \        }\n\
 \\n\
 \        SLIST_FOREACH(sc, &arena->caches, next) {\n\
 \                SLIST_INIT(&sc->blocks);\n\
 \                SLIST_INIT(&sc->full_blocks);\n\
 \#if _JHC_PROFILE\n\
 \                sc->allocations = 0;\n\
 \#endif\n\
 \        }\n\
 \\n\
 \        SLIST_REMOVE(&used_arenas, arena, s_arena, link);\n\
 \        SLIST_INSERT_HEAD(&free_arenas, arena, link);\n\
 \        jhc_rts_unlock();\n\
 \}\n\
 \\n\
 \heap_t A_STD\n\
 \(gc_alloc)(gc_t gc, arena_t arena, struct s_cache **sc, unsigned count, unsigned nptrs)\n\
 \{\n\
 \        assert(nptrs <= count);\n\
 \        entry_t *e = s_alloc(gc, arena, find_cache(sc, arena, count, nptrs));\n\
 \        VALGRIND_MAKE_MEM_UNDEFINED(e,sizeof(uintptr_t)*count);\n\
 \        debugf(\"gc_alloc: %p %i %i\\n\",(void *)e, count, nptrs);\n\
 \        return (void *)e;\n\
 \}\n\
 \\n\
 \static heap_t A_STD\n\
 \s_monoblock(arena_t arena, unsigned size, unsigned nptrs, unsigned flags) {\n\
 \        jhc_rts_lock();\n\
 \        struct s_block *b = SLIST_FIRST(&free_monolithic_blocks);\n\
 \        if (b) {\n\
 \                SLIST_REMOVE(&free_monolithic_blocks, b, s_block, link);\n\
 \        } else {\n\
 \                b = jhc_aligned_alloc(size * sizeof(uintptr_t));\n\
 \        }\n\
 \        jhc_rts_unlock();\n\
 \\n\
 \        b->flags = flags | SLAB_MONOLITH;\n\
 \        b->color = (sizeof(struct s_block) + BITARRAY_SIZE_IN_BYTES(1) +\n\
 \                    sizeof(uintptr_t) - 1) / sizeof(uintptr_t);\n\
 \        b->u.m.num_ptrs = nptrs;\n\
 \        SLIST_INSERT_HEAD(&arena->monolithic_blocks, b, link);\n\
 \        b->used[0] = 1;\n\
 \        return (void *)b + b->color*sizeof(uintptr_t);\n\
 \}\n\
 \\n\
 \// Allocate an array of count garbage collectable locations in the garbage\n\
 \// collected heap.\n\
 \heap_t A_STD\n\
 \gc_array_alloc(gc_t gc, arena_t arena, unsigned count)\n\
 \{\n\
 \        if (!count)\n\
 \               return NULL;\n\
 \        if (count <= GC_STATIC_ARRAY_NUM)\n\
 \                return (wptr_t)s_alloc(gc, arena, arena->array_caches[count - 1]);\n\
 \        if (count < GC_MAX_BLOCK_ENTRIES)\n\
 \                return s_alloc(gc, arena, find_cache(NULL, arena, count, count));\n\
 \        return s_monoblock(arena, count, count, 0);\n\
 \        abort();\n\
 \}\n\
 \\n\
 \// Allocate an array of count non-garbage collectable locations in the garbage\n\
 \// collected heap.\n\
 \heap_t A_STD\n\
 \gc_array_alloc_atomic(gc_t gc, arena_t arena, unsigned count, unsigned flags)\n\
 \{\n\
 \        if (!count)\n\
 \               return NULL;\n\
 \        if (count <= GC_STATIC_ARRAY_NUM && !flags)\n\
 \                return (wptr_t)s_alloc(gc, arena, arena->array_caches_atomic[count - 1]);\n\
 \        if (count < GC_MAX_BLOCK_ENTRIES && !flags)\n\
 \                return s_alloc(gc, arena, find_cache(NULL, arena, count, 0));\n\
 \        return s_monoblock(arena, count, count, flags);\n\
 \        abort();\n\
 \}\n\
 \\n\
 \/* This finds a bit that isn't set, sets it, then returns its index.  It\n\
 \ * assumes that a bit is available to be found, otherwise it goes into an\n\
 \ * infinite loop. */\n\
 \\n\
 \static unsigned\n\
 \bitset_find_free(unsigned *next_free,int n,bitarray_t ba[static n]) {\n\
 \        assert(*next_free < (unsigned)n);\n\
 \        unsigned i = *next_free;\n\
 \        do {\n\
 \                int o = __builtin_ffsl(~ba[i]);\n\
 \                if(__predict_true(o)) {\n\
 \                        ba[i] |= (1UL << (o - 1));\n\
 \                        *next_free = i;\n\
 \                        return (i*BITS_PER_UNIT + (o - 1));\n\
 \                }\n\
 \                i = (i + 1) % n;\n\
 \                assert(i != *next_free);\n\
 \        } while (1);\n\
 \}\n\
 \\n\
 \static void *\n\
 \jhc_aligned_alloc(unsigned size) {\n\
 \        void *base;\n\
 \#if defined(__WIN32__)\n\
 \        base = _aligned_malloc(MEGABLOCK_SIZE, BLOCK_SIZE);\n\
 \        int ret = !base;\n\
 \#elif defined(__ARM_EABI__)\n\
 \        base = memalign(BLOCK_SIZE, MEGABLOCK_SIZE);\n\
 \        int ret = !base;\n\
 \#elif (defined(__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__) && __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ <  1060)\n\
 \        assert(sysconf(_SC_PAGESIZE) == BLOCK_SIZE);\n\
 \        base = valloc(MEGABLOCK_SIZE);\n\
 \        int ret = !base;\n\
 \#else\n\
 \        int ret = posix_memalign(&base,BLOCK_SIZE,MEGABLOCK_SIZE);\n\
 \#endif\n\
 \        if(ret != 0) {\n\
 \                jhc_printf_stderr(\"Unable to allocate memory for aligned alloc: %u\\n\", size);\n\
 \                abort();\n\
 \        }\n\
 \        return base;\n\
 \}\n\
 \\n\
 \struct s_megablock *\n\
 \s_new_megablock(arena_t arena)\n\
 \{\n\
 \        jhc_rts_lock();\n\
 \        struct s_megablock *mb = SLIST_FIRST(&free_megablocks);\n\
 \        if (mb) {\n\
 \                SLIST_REMOVE(&free_megablocks, mb, s_megablock, next);\n\
 \        } else {\n\
 \                mb = malloc(sizeof(*mb));\n\
 \#ifdef _JHC_JGC_LIMITED_NUM_MEGABLOCK\n\
 \                static int count = 0;\n\
 \                if (count >= _JHC_JGC_LIMITED_NUM_MEGABLOCK) {\n\
 \                        abort();\n\
 \                }\n\
 \                mb->base = aligned_megablock + (MEGABLOCK_SIZE) * count;\n\
 \                count++;\n\
 \#else\n\
 \                mb->base = jhc_aligned_alloc(MEGABLOCK_SIZE);\n\
 \#endif\n\
 \        }\n\
 \        jhc_rts_unlock();\n\
 \\n\
 \        VALGRIND_MAKE_MEM_NOACCESS(mb->base,MEGABLOCK_SIZE);\n\
 \        mb->next_free = 0;\n\
 \        return mb;\n\
 \}\n\
 \\n\
 \/* block allocator */\n\
 \\n\
 \static struct s_block *\n\
 \get_free_block(gc_t gc, arena_t arena, bool retry) {\n\
 \        arena->block_used++;\n\
 \        if(__predict_true(SLIST_FIRST(&arena->free_blocks))) {\n\
 \                struct s_block *pg = SLIST_FIRST(&arena->free_blocks);\n\
 \                SLIST_REMOVE_HEAD(&arena->free_blocks,link);\n\
 \                return pg;\n\
 \        } else {\n\
 \#ifdef _JHC_JGC_NAIVEGC\n\
 \                if(retry == false) {\n\
 \                        gc_perform_gc(gc, arena);\n\
 \                        return NULL;\n\
 \                }\n\
 \#else\n\
 \                if((arena->block_used >= arena->block_threshold)) {\n\
 \                        gc_perform_gc(gc, arena);\n\
 \                        // if we are still using 80% of the heap after a gc, raise the threshold.\n\
 \                        if(__predict_false((unsigned)arena->block_used * 10 >= arena->block_threshold * 9)) {\n\
 \                                arena->block_threshold *= 2;\n\
 \                        }\n\
 \                }\n\
 \#endif\n\
 \                if(__predict_false(!arena->current_megablock))\n\
 \                        arena->current_megablock = s_new_megablock(arena);\n\
 \                struct s_megablock *mb = arena->current_megablock;\n\
 \                struct s_block *pg = mb->base + BLOCK_SIZE*mb->next_free;\n\
 \                mb->next_free++;\n\
 \                if(mb->next_free == MEGABLOCK_SIZE / BLOCK_SIZE) {\n\
 \                        SLIST_INSERT_HEAD(&arena->megablocks,mb, next);\n\
 \                        arena->current_megablock = NULL;\n\
 \                }\n\
 \                VALGRIND_MAKE_MEM_UNDEFINED(pg,sizeof(struct s_block));\n\
 \                pg->u.pi.num_free = 0;\n\
 \                return pg;\n\
 \        }\n\
 \}\n\
 \\n\
 \typedef void (*finalizer_ptr)(HsPtr arg);\n\
 \typedef void (*finalizer_env_ptr)(HsPtr env, HsPtr arg);\n\
 \\n\
 \void hs_foreignptr_env_helper(HsPtr env, HsPtr arg) {\n\
 \        ((finalizer_ptr)env)(arg);\n\
 \}\n\
 \\n\
 \static void\n\
 \s_cleanup_blocks(arena_t arena) {\n\
 \        struct s_block *pg = SLIST_FIRST(&arena->monolithic_blocks);\n\
 \        SLIST_INIT(&arena->monolithic_blocks);\n\
 \        while (pg) {\n\
 \                if (pg->used[0]) {\n\
 \                        SLIST_INSERT_HEAD(&arena->monolithic_blocks, pg, link);\n\
 \                        pg = SLIST_NEXT(pg,link);\n\
 \                } else {\n\
 \                        if (pg->flags & SLAB_FLAG_FINALIZER) {\n\
 \                                HsPtr *ptr = (HsPtr *)pg;\n\
 \                                if(ptr[pg->color + 1]) {\n\
 \                                        finalizer_ptr *fp = ptr[pg->color + 1];\n\
 \                                        do {\n\
 \                                                fp[0](ptr[pg->color]);\n\
 \                                        } while(*++fp);\n\
 \                                }\n\
 \                        }\n\
 \                        void *ptr = pg;\n\
 \                        pg = SLIST_NEXT(pg,link);\n\
 \                        free(ptr);\n\
 \                }\n\
 \        }\n\
 \        struct s_cache *sc = SLIST_FIRST(&arena->caches);\n\
 \        for(;sc;sc = SLIST_NEXT(sc,next)) {\n\
 \                // 'best' keeps track of the block with the fewest free spots\n\
 \                // and percolates it to the front, effectively a single pass\n\
 \                // of a bubblesort to help combat fragmentation. It does\n\
 \                // not increase the complexity of the cleanup algorithm as\n\
 \                // we had to scan every block anyway, but over many passes\n\
 \                // of the GC it will eventually result in a more sorted list\n\
 \                // than would occur by chance.\n\
 \\n\
 \                struct s_block *best = NULL;\n\
 \                int free_best = 4096;\n\
 \                pg = SLIST_FIRST(&sc->blocks);\n\
 \                struct s_block *fpg = SLIST_FIRST(&sc->full_blocks);\n\
 \                SLIST_INIT(&sc->blocks);\n\
 \                SLIST_INIT(&sc->full_blocks);\n\
 \                if(!pg) {\n\
 \                        pg = fpg;\n\
 \                        fpg = NULL;\n\
 \                }\n\
 \                while(pg) {\n\
 \                        struct s_block *npg = SLIST_NEXT(pg,link);\n\
 \                        if(__predict_false(pg->u.pi.num_free == 0)) {\n\
 \                                // Add full blockes to the cache's full block list.\n\
 \                                SLIST_INSERT_HEAD(&sc->full_blocks,pg,link);\n\
 \                        } else if(__predict_true(pg->u.pi.num_free == sc->num_entries)) {\n\
 \                                // Return completely free block to arena free block list.\n\
 \                                arena->block_used--;\n\
 \                                VALGRIND_MAKE_MEM_NOACCESS((char *)pg + sizeof(struct s_block),\n\
 \                                                           BLOCK_SIZE - sizeof(struct s_block));\n\
 \                                SLIST_INSERT_HEAD(&arena->free_blocks,pg,link);\n\
 \                        } else {\n\
 \                                if(!best) {\n\
 \                                        free_best = pg->u.pi.num_free;\n\
 \                                        best = pg;\n\
 \                                } else {\n\
 \                                        if(pg->u.pi.num_free < free_best) {\n\
 \                                                struct s_block *tmp = best;\n\
 \                                                best = pg; pg = tmp;\n\
 \                                                free_best = pg->u.pi.num_free;\n\
 \                                        }\n\
 \                                        SLIST_INSERT_HEAD(&sc->blocks,pg,link);\n\
 \                                }\n\
 \                        }\n\
 \                        if(!npg && fpg) {\n\
 \                                pg = fpg;\n\
 \                                fpg = NULL;\n\
 \                        } else\n\
 \                                pg = npg;\n\
 \                }\n\
 \                if(best)\n\
 \                        SLIST_INSERT_HEAD(&sc->blocks,best,link);\n\
 \        }\n\
 \}\n\
 \\n\
 \inline static void\n\
 \clear_block_used_bits(unsigned num_entries, struct s_block *pg)\n\
 \{\n\
 \        pg->u.pi.num_free = num_entries;\n\
 \        memset(pg->used,0,BITARRAY_SIZE_IN_BYTES(num_entries) - sizeof(pg->used[0]));\n\
 \        int excess = num_entries % BITS_PER_UNIT;\n\
 \        pg->used[BITARRAY_SIZE(num_entries) - 1] = ~((1UL << excess) - 1);\n\
 \#if JHC_VALGRIND\n\
 \                unsigned header =  sizeof(struct s_block) + BITARRAY_SIZE_IN_BYTES(num_entries);\n\
 \                VALGRIND_MAKE_MEM_NOACCESS((char *)pg + header, BLOCK_SIZE - header);\n\
 \#endif\n\
 \}\n\
 \\n\
 \/*\n\
 \ * allocators\n\
 \ */\n\
 \\n\
 \heap_t A_STD\n\
 \s_alloc(gc_t gc, arena_t arena, struct s_cache *sc)\n\
 \{\n\
 \#if _JHC_PROFILE\n\
 \       sc->allocations++;\n\
 \       sc->arena->number_allocs++;\n\
 \#endif\n\
 \        bool retry = false;\n\
 \        struct s_block *pg;\n\
 \        if (__predict_false(arena->force_gc_next_s_alloc)) {\n\
 \                arena->force_gc_next_s_alloc = 0;\n\
 \                gc_perform_gc(gc, arena);\n\
 \        }\n\
 \retry_s_alloc:\n\
 \        pg = SLIST_FIRST(&sc->blocks);\n\
 \        if(__predict_false(!pg)) {\n\
 \                pg = get_free_block(gc, sc->arena, retry);\n\
 \                if(__predict_false(!pg)) {\n\
 \                        retry = true;\n\
 \                        goto retry_s_alloc;\n\
 \                }\n\
 \                VALGRIND_MAKE_MEM_NOACCESS(pg, BLOCK_SIZE);\n\
 \                VALGRIND_MAKE_MEM_DEFINED(pg, sizeof(struct s_block));\n\
 \                if(sc->num_entries != pg->u.pi.num_free)\n\
 \                        VALGRIND_MAKE_MEM_UNDEFINED((char *)pg->used,\n\
 \                                                    BITARRAY_SIZE_IN_BYTES(sc->num_entries));\n\
 \                else\n\
 \                        VALGRIND_MAKE_MEM_DEFINED((char *)pg->used,\n\
 \                                                  BITARRAY_SIZE_IN_BYTES(sc->num_entries));\n\
 \                assert(pg);\n\
 \                pg->flags = sc->flags;\n\
 \                pg->color = sc->color;\n\
 \                pg->u.pi.num_ptrs = sc->num_ptrs;\n\
 \                pg->u.pi.size = sc->size;\n\
 \                pg->u.pi.next_free = 0;\n\
 \                SLIST_INSERT_HEAD(&sc->blocks,pg,link);\n\
 \                if(sc->num_entries != pg->u.pi.num_free)\n\
 \                        clear_block_used_bits(sc->num_entries, pg);\n\
 \                pg->used[0] = 1; //set the first bit\n\
 \                pg->u.pi.num_free = sc->num_entries - 1;\n\
 \                return (uintptr_t *)pg + pg->color;\n\
 \        } else {\n\
 \                __builtin_prefetch(pg->used,1);\n\
 \                pg->u.pi.num_free--;\n\
 \                unsigned next_free = pg->u.pi.next_free;\n\
 \                unsigned found = bitset_find_free(&next_free,BITARRAY_SIZE(sc->num_entries),pg->used);\n\
 \                pg->u.pi.next_free = next_free;\n\
 \                void *val = (uintptr_t *)pg + pg->color + found*pg->u.pi.size;\n\
 \                if(__predict_false(0 == pg->u.pi.num_free)) {\n\
 \                        assert(pg == SLIST_FIRST(&sc->blocks));\n\
 \                        SLIST_REMOVE_HEAD(&sc->blocks,link);\n\
 \                        SLIST_INSERT_HEAD(&sc->full_blocks,pg,link);\n\
 \                }\n\
 \                assert(S_BLOCK(val) == pg);\n\
 \                //jhc_printf_stderr(\"s_alloc: val: %p s_block: %p size: %i color: %i found: %i num_free: %i\\n\", val, pg, pg->pi.size, pg->pi.color, found, pg->num_free);\n\
 \                return val;\n\
 \        }\n\
 \}\n\
 \\n\
 \struct s_cache *\n\
 \new_cache(arena_t arena, unsigned short size, unsigned short num_ptrs)\n\
 \{\n\
 \        struct s_cache *sc = malloc(sizeof(*sc));\n\
 \        memset(sc,0,sizeof(*sc));\n\
 \        sc->arena = arena;\n\
 \        sc->size = size;\n\
 \        sc->num_ptrs = num_ptrs;\n\
 \        sc->flags = 0;\n\
 \        size_t excess = BLOCK_SIZE - sizeof(struct s_block);\n\
 \        sc->num_entries = (8*excess) / (8*sizeof(uintptr_t)*size + 1) - 1;\n\
 \        sc->color = (sizeof(struct s_block) + BITARRAY_SIZE_IN_BYTES(sc->num_entries) +\n\
 \                        sizeof(uintptr_t) - 1) / sizeof(uintptr_t);\n\
 \        SLIST_INIT(&sc->blocks);\n\
 \        SLIST_INIT(&sc->full_blocks);\n\
 \        SLIST_INSERT_HEAD(&arena->caches,sc,next);\n\
 \        return sc;\n\
 \}\n\
 \\n\
 \// clear all used bits, must be followed by a marking phase.\n\
 \static void\n\
 \clear_used_bits(arena_t arena)\n\
 \{\n\
 \        struct s_block *pg;\n\
 \        SLIST_FOREACH(pg, &arena->monolithic_blocks, link)\n\
 \            pg->used[0] = 0;\n\
 \        struct s_cache *sc = SLIST_FIRST(&arena->caches);\n\
 \        for(;sc;sc = SLIST_NEXT(sc,next)) {\n\
 \                SLIST_FOREACH(pg, &sc->blocks, link)\n\
 \                    clear_block_used_bits(sc->num_entries,pg);\n\
 \                SLIST_FOREACH(pg, &sc->full_blocks, link)\n\
 \                    clear_block_used_bits(sc->num_entries,pg);\n\
 \        }\n\
 \}\n\
 \\n\
 \// Set a used bit. returns true if the tagged node should be scanned by the GC.\n\
 \// this happens when the used bit was not previously set and the node contains\n\
 \// internal pointers.\n\
 \\n\
 \static bool\n\
 \s_set_used_bit(void *val)\n\
 \{\n\
 \        assert(val);\n\
 \        struct s_block *pg = S_BLOCK(val);\n\
 \        unsigned int offset = ((uintptr_t *)val - (uintptr_t *)pg) - pg->color;\n\
 \        if(__predict_true(BIT_IS_UNSET(pg->used,offset/pg->u.pi.size))) {\n\
 \                if (pg->flags & SLAB_MONOLITH) {\n\
 \                        pg->used[0] = 1;\n\
 \                        return (bool)pg->u.m.num_ptrs;\n\
 \\n\
 \                } else {\n\
 \                        BIT_SET(pg->used,offset/pg->u.pi.size);\n\
 \                        pg->u.pi.num_free--;\n\
 \                        return (bool)pg->u.pi.num_ptrs;\n\
 \                }\n\
 \        }\n\
 \        return false;\n\
 \}\n\
 \\n\
 \struct s_cache *\n\
 \find_cache(struct s_cache **rsc, arena_t arena,\n\
 \           unsigned short size, unsigned short num_ptrs)\n\
 \{\n\
 \        if(__predict_true(rsc && *rsc))\n\
 \                return *rsc;\n\
 \        struct s_cache *sc = SLIST_FIRST(&arena->caches);\n\
 \        for(;sc;sc = SLIST_NEXT(sc,next)) {\n\
 \                if(sc->size == size && sc->num_ptrs == num_ptrs)\n\
 \                        goto found;\n\
 \        }\n\
 \        sc = new_cache(arena,size,num_ptrs);\n\
 \found:\n\
 \        if(rsc)\n\
 \                *rsc = sc;\n\
 \        return sc;\n\
 \}\n\
 \\n\
 \void\n\
 \alloc_public_caches(arena_t arena, size_t size) {\n\
 \        if (arena->public_caches_p == NULL) {\n\
 \                arena->public_caches_p = malloc(size);\n\
 \        }\n\
 \}\n\
 \\n\
 \struct s_caches_pub *\n\
 \public_caches(arena_t arena) {\n\
 \        return arena->public_caches_p;\n\
 \}\n\
 \\n\
 \/* Enter with rts_lock. */\n\
 \arena_t\n\
 \new_arena(void) {\n\
 \        arena_t arena = SLIST_FIRST(&free_arenas);\n\
 \        if (arena) {\n\
 \                SLIST_REMOVE(&free_arenas, arena, s_arena, link);\n\
 \        } else {\n\
 \                arena = malloc(sizeof(struct s_arena));\n\
 \                memset(arena, 0, sizeof(*arena));\n\
 \                // Following menbers isn't clear at jhc_alloc_fini for keeping caches.\n\
 \                SLIST_INIT(&arena->caches);\n\
 \                arena->public_caches_p = NULL;\n\
 \        }\n\
 \        SLIST_INIT(&arena->free_blocks);\n\
 \        SLIST_INIT(&arena->megablocks);\n\
 \        SLIST_INIT(&arena->monolithic_blocks);\n\
 \        arena->block_used = 0;\n\
 \        arena->block_threshold = 8;\n\
 \        arena->current_megablock = NULL;\n\
 \\n\
 \        for (int i = 0; i < GC_STATIC_ARRAY_NUM; i++) {\n\
 \                find_cache(&arena->array_caches[i], arena, i + 1, i + 1);\n\
 \                find_cache(&arena->array_caches_atomic[i], arena, i + 1, 0);\n\
 \        }\n\
 \        return arena;\n\
 \}\n\
 \\n\
 \uint32_t\n\
 \get_heap_flags(void * sp) {\n\
 \        uint32_t ret = 0;\n\
 \        switch (GET_PTYPE(sp)) {\n\
 \        case P_VALUE: return SLAB_VIRTUAL_VALUE;\n\
 \        case P_FUNC: return SLAB_VIRTUAL_FUNC;\n\
 \        case P_LAZY:\n\
 \                     ret |= SLAB_VIRTUAL_LAZY;\n\
 \        case P_WHNF:\n\
 \                     if (S_BLOCK(sp) == NULL)\n\
 \                             return (ret | SLAB_VIRTUAL_SPECIAL);\n\
 \                     if ((void *)sp >= nh_start && (void *)sp <= nh_end)\n\
 \                             return (ret | SLAB_VIRTUAL_CONSTANT);\n\
 \                     return ret |= S_BLOCK(sp)->flags;\n\
 \        }\n\
 \        return ret;\n\
 \}\n\
 \\n\
 \heap_t A_STD\n\
 \gc_malloc_foreignptr(gc_t gc, arena_t arena, unsigned alignment, unsigned size, bool finalizer) {\n\
 \        // we don't allow higher alignments yet.\n\
 \        assert (alignment <= sizeof(uintptr_t));\n\
 \        // no finalizers yet\n\
 \        assert (!finalizer);\n\
 \        unsigned spacing = 1 + finalizer;\n\
 \        wptr_t *res = gc_array_alloc_atomic(gc, arena, spacing + TO_BLOCKS(size),\n\
 \                                             finalizer ? SLAB_FLAG_FINALIZER : SLAB_FLAG_NONE);\n\
 \        res[0] = (wptr_t)(res + spacing);\n\
 \        if (finalizer)\n\
 \                res[1] = NULL;\n\
 \        return TO_SPTR(P_WHNF, res);\n\
 \}\n\
 \\n\
 \heap_t A_STD\n\
 \gc_new_foreignptr(gc_t gc, arena_t arena, HsPtr ptr) {\n\
 \        HsPtr *res = gc_array_alloc_atomic(gc, arena, 2, SLAB_FLAG_FINALIZER);\n\
 \        res[0] = ptr;\n\
 \        res[1] = NULL;\n\
 \        return TO_SPTR(P_WHNF, res);\n\
 \}\n\
 \\n\
 \bool A_STD\n\
 \gc_add_foreignptr_finalizer(wptr_t fp, HsFunPtr finalizer) {\n\
 \        if (!(SLAB_FLAG_FINALIZER & get_heap_flags(fp)))\n\
 \                return false;\n\
 \        HsFunPtr **res = (HsFunPtr**)FROM_SPTR(fp);\n\
 \        unsigned len = 0;\n\
 \        if (res[1])\n\
 \                while(res[1][len++]);\n\
 \        else\n\
 \                len = 1;\n\
 \        res[1] = realloc(res[1], (len + 1) * sizeof(HsFunPtr));\n\
 \        HsFunPtr *ptrs = res[1];\n\
 \        ptrs[len - 1] = finalizer;\n\
 \        ptrs[len] = NULL;\n\
 \        return true;\n\
 \}\n\
 \\n\
 \void\n\
 \print_cache(struct s_cache *sc) {\n\
 \        jhc_printf_stderr(\"num_entries: %i with %lu bytes of header\\n\",\n\
 \                (int)sc->num_entries, sizeof(struct s_block) +\n\
 \                BITARRAY_SIZE_IN_BYTES(sc->num_entries));\n\
 \        jhc_printf_stderr(\"  size: %i words %i ptrs\\n\",\n\
 \                (int)sc->size,(int)sc->num_ptrs);\n\
 \#if _JHC_PROFILE\n\
 \        jhc_printf_stderr(\"  allocations: %lu\\n\", (unsigned long)sc->allocations);\n\
 \#endif\n\
 \        if(SLIST_EMPTY(&sc->blocks) && SLIST_EMPTY(&sc->full_blocks))\n\
 \                return;\n\
 \        jhc_printf_stderr(\"  blocks:\\n\");\n\
 \        jhc_printf_stderr(\"%20s %9s %9s %s\\n\", \"block\", \"num_free\", \"next_free\", \"status\");\n\
 \        struct s_block *pg;\n\
 \        SLIST_FOREACH(pg,&sc->blocks,link)\n\
 \            jhc_printf_stderr(\"%20p %9i %9i %c\\n\", pg, pg->u.pi.num_free, pg->u.pi.next_free, 'P');\n\
 \        SLIST_FOREACH(pg,&sc->full_blocks,link)\n\
 \            jhc_printf_stderr(\"%20p %9i %9i %c\\n\", pg, pg->u.pi.num_free, pg->u.pi.next_free, 'F');\n\
 \}\n\
 \\n\
 \void hs_perform_gc(void) {\n\
 \        arena_t arena;\n\
 \        jhc_rts_lock();\n\
 \        SLIST_FOREACH(arena, &used_arenas, link) {\n\
 \                arena->force_gc_next_s_alloc = 1;\n\
 \        }\n\
 \        jhc_rts_unlock();\n\
 \}\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/gc_jgc.h
{-# NOINLINE gc_jgc_h #-}
gc_jgc_h :: ByteString
gc_jgc_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef JHC_GC_JGC_H\n\
 \#define JHC_GC_JGC_H\n\
 \\n\
 \#include <stdbool.h>\n\
 \#include <stdint.h>\n\
 \#include \"sys/queue.h\"\n\
 \#include \"HsFFI.h\"\n\
 \\n\
 \struct sptr;\n\
 \struct s_arena;\n\
 \struct s_cache;\n\
 \typedef struct s_arena *arena_t;\n\
 \typedef void* *gc_t;\n\
 \typedef void* heap_t;  // a pointer into the GCed heap.\n\
 \\n\
 \#if defined(_JHC_JGC_BLOCK_SHIFT) && defined(_JHC_JGC_MEGABLOCK_SHIFT)\n\
 \#if (_JHC_JGC_BLOCK_SHIFT) >= (_JHC_JGC_MEGABLOCK_SHIFT)\n\
 \#error \"_JHC_JGC_MEGABLOCK_SHIFT should be larger than _JHC_JGC_BLOCK_SHIFT.\"\n\
 \#endif\n\
 \#elif defined(_JHC_JGC_BLOCK_SHIFT) || defined(_JHC_JGC_MEGABLOCK_SHIFT)\n\
 \#error \"Should define both _JHC_JGC_BLOCK_SHIFT and _JHC_JGC_MEGABLOCK_SHIFT.\"\n\
 \#else\n\
 \#define _JHC_JGC_BLOCK_SHIFT     12\n\
 \#define _JHC_JGC_MEGABLOCK_SHIFT 20\n\
 \#endif /* defined(_JHC_JGC_BLOCK_SHIFT) && defined(_JHC_JGC_MEGABLOCK_SHIFT) */\n\
 \\n\
 \#if !defined(_JHC_JGC_GC_STACK_SHIFT)\n\
 \#define _JHC_JGC_GC_STACK_SHIFT  18\n\
 \#endif /* !defined(_JHC_JGC_BLOCK_SHIFT) */\n\
 \\n\
 \#define BLOCK_SIZE     (1UL << (_JHC_JGC_BLOCK_SHIFT))\n\
 \#define MEGABLOCK_SIZE (1UL << (_JHC_JGC_MEGABLOCK_SHIFT))\n\
 \#define GC_STACK_SIZE  (1UL << (_JHC_JGC_GC_STACK_SHIFT))\n\
 \#define S_BLOCK(val) ((struct s_block *)((uintptr_t)(val) & ~(BLOCK_SIZE - 1)))\n\
 \#define TO_BLOCKS(x) (((x) + sizeof(uintptr_t) - 1)/sizeof(uintptr_t))\n\
 \\n\
 \extern arena_t saved_arena;\n\
 \extern gc_t saved_gc;\n\
 \\n\
 \void print_cache(struct s_cache *sc);\n\
 \struct s_cache *new_cache(arena_t arena, unsigned short size,\n\
 \                          unsigned short num_ptrs);\n\
 \arena_t new_arena(void);\n\
 \struct s_cache *find_cache(struct s_cache **rsc, arena_t arena,\n\
 \                           unsigned short size, unsigned short num_ptrs);\n\
 \void alloc_public_caches(arena_t arena, size_t size);\n\
 \struct s_caches_pub *public_caches(arena_t arena);\n\
 \void gc_add_root(gc_t gc, arena_t arena, void * root);\n\
 \void A_STD gc_perform_gc(gc_t gc, arena_t arena);\n\
 \uint32_t get_heap_flags(void* sp);\n\
 \\n\
 \heap_t s_alloc(gc_t gc, arena_t arena, struct s_cache *sc) A_STD;\n\
 \heap_t (gc_alloc)(gc_t gc, arena_t arena,struct s_cache **sc, unsigned count, unsigned nptrs) A_STD;\n\
 \heap_t gc_array_alloc(gc_t gc, arena_t arena, unsigned count) A_STD;\n\
 \heap_t gc_array_alloc_atomic(gc_t gc, arena_t arena, unsigned count, unsigned slab_flags) A_STD;\n\
 \/* foreignptr, saved_gc must be set properly. */\n\
 \heap_t gc_malloc_foreignptr(gc_t gc, arena_t arena, unsigned alignment, unsigned size, bool finalizer) A_STD;\n\
 \heap_t gc_new_foreignptr(gc_t gc, arena_t arena, HsPtr ptr) A_STD;\n\
 \bool gc_add_foreignptr_finalizer(struct sptr* fp, HsFunPtr finalizer) A_STD;\n\
 \\n\
 \#define gc_frame0(gc,n,...) void *ptrs[n] = { __VA_ARGS__ }; \\\n\
 \        for(int i = 0; i < n; i++) gc[i] = (sptr_t)ptrs[i]; \\\n\
 \        gc_t sgc = gc;  gc_t gc = sgc + n;\n\
 \#define gc_frame1(gc,p1) gc[0] = (sptr_t)p1; gc_t sgc = gc;  gc_t gc = sgc + 1;\n\
 \#define gc_frame2(gc,p1,p2) gc[0] = (sptr_t)p1; gc[1] = (sptr_t)p2; \\\n\
 \                                    gc_t sgc = gc;  gc_t gc = sgc + 2;\n\
 \\n\
 \struct StablePtr {\n\
 \    LIST_ENTRY(StablePtr) link;\n\
 \    struct sptr* contents;\n\
 \};\n\
 \\n\
 \extern LIST_HEAD(StablePtr_list, StablePtr) root_StablePtrs;\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/profile.c
{-# NOINLINE profile_c #-}
profile_c :: ByteString
profile_c = unsafePerformIO $ unsafePackAddress "\
 \// profiling and debugging code.\n\
 \\n\
 \#if defined(__WIN32__)\n\
 \#define HAVE_TIMES 0\n\
 \#else\n\
 \#define HAVE_TIMES 1\n\
 \#endif\n\
 \\n\
 \#include <stdio.h>\n\
 \#include <stdlib.h>\n\
 \#if HAVE_TIMES\n\
 \#include <sys/times.h>\n\
 \#include <time.h>\n\
 \#endif\n\
 \#include <unistd.h>\n\
 \\n\
 \#include \"jhc_rts_header.h\"\n\
 \\n\
 \#if 0\n\
 \void A_UNUSED\n\
 \profile_print_header(FILE *file, char *value_unit)\n\
 \{\n\
 \        fprintf(file, \"JOB \\\"%s\", jhc_progname);\n\
 \        for(int i = 0; i < jhc_argc; i++)\n\
 \                fprintf(file, \" %s\", jhc_argv[i]);\n\
 \        fprintf(file, \"\\\"\\n\");\n\
 \        fprintf(file, \"DATE \\\"%s\\\"\\n\", ctime(NULL));\n\
 \        fprintf(file, \"SAMPLE_UNIT \\\"seconds\\\"\\n\");\n\
 \        fprintf(file, \"VALUE_UNIT \\\"%s\\\"\\n\", value_unit ? value_unit : \"bytes\");\n\
 \}\n\
 \#endif /* 0 */\n\
 \\n\
 \#if HAVE_TIMES\n\
 \struct profile_stack {\n\
 \    struct tms tm_total;\n\
 \    struct tms tm_pushed;\n\
 \};\n\
 \\n\
 \struct profile_stack gc_alloc_time;\n\
 \struct profile_stack gc_gc_time;\n\
 \\n\
 \void\n\
 \jhc_profile_push(struct profile_stack *ps)\n\
 \{\n\
 \        times(&ps->tm_pushed);\n\
 \}\n\
 \\n\
 \void\n\
 \jhc_profile_pop(struct profile_stack *ps)\n\
 \{\n\
 \    struct tms tm;\n\
 \    times(&tm);\n\
 \    ps->tm_total.tms_utime += tm.tms_utime - ps->tm_pushed.tms_utime;\n\
 \    ps->tm_total.tms_stime += tm.tms_stime - ps->tm_pushed.tms_stime;\n\
 \}\n\
 \\n\
 \void print_times(struct tms *tm) {\n\
 \#if  !defined(__WIN32__) && !defined(__ARM_EABI__)\n\
 \    float cpt = (float)sysconf(_SC_CLK_TCK);\n\
 \    jhc_printf_stderr(\"User Time:   %.2fs\\n\", (float)tm->tms_utime/cpt);\n\
 \    jhc_printf_stderr(\"System Time: %.2fs\\n\", (float)tm->tms_stime/cpt);\n\
 \    jhc_printf_stderr(\"Total Time:  %.2fs\\n\", (float)(tm->tms_stime + tm->tms_utime)/cpt);\n\
 \#endif\n\
 \    return;\n\
 \}\n\
 \#else\n\
 \\n\
 \struct profile_stack;\n\
 \void jhc_profile_push(struct profile_stack *ps) {}\n\
 \void jhc_profile_pop(struct profile_stack *ps) {}\n\
 \\n\
 \#endif\n\
 \\n\
 \void A_COLD\n\
 \jhc_print_profile(void) {\n\
 \        if(!(_JHC_PROFILE || getenv(\"AJHC_RTS_PROFILE\"))) return;\n\
 \        jhc_printf_stderr(\"\\n-----------------\\n\");\n\
 \        jhc_printf_stderr(\"Profiling: %s\\n\", jhc_progname);\n\
 \        jhc_printf_stderr(\"Command: %s\\n\", jhc_command);\n\
 \        jhc_printf_stderr(\"Complie: %s\\n\", jhc_c_compile);\n\
 \        jhc_printf_stderr(\"Version: %s\\n\\n\", jhc_version);\n\
 \#if HAVE_TIMES\n\
 \        struct tms tm;\n\
 \        times(&tm);\n\
 \        print_times(&tm);\n\
 \#endif\n\
 \#if _JHC_PROFILE\n\
 \        print_times(&gc_gc_time.tm_total);\n\
 \        print_times(&gc_alloc_time.tm_total);\n\
 \#endif\n\
 \        jhc_printf_stderr(\"-----------------\\n\");\n\
 \}\n\
 \\n\
 \#if _JHC_PROFILE && _JHC_GC != _JHC_GC_JGC\n\
 \\n\
 \#define BUCKETS 7\n\
 \static unsigned alloced[BUCKETS];\n\
 \static unsigned alloced_atomic[BUCKETS];\n\
 \\n\
 \static void\n\
 \alloc_count(int n,int atomic)\n\
 \{\n\
 \        n = n ? ((n - 1)/sizeof(void *)) + 1 : 0;\n\
 \        n = n > BUCKETS - 1 ? BUCKETS - 1 : n;\n\
 \        (atomic ? alloced_atomic : alloced)[n]++;\n\
 \}\n\
 \\n\
 \static void\n\
 \print_alloc_size_stats(void) {\n\
 \        char fmt[] = \"%10s %10s %10s %10s %10s\\n\";\n\
 \        char fmt2[] = \"%10u %10u %10u %10u %10u\\n\";\n\
 \        jhc_printf_stderr(fmt,\"Size\",\"Normal\",\"Atomic\",\"Total\",\"Accum\");\n\
 \        jhc_printf_stderr(fmt,\"----\",\"------\",\"------\",\"-----\",\"-----\");\n\
 \        unsigned accum = 0;\n\
 \        for(int i = 0; i < BUCKETS; i++) {\n\
 \                accum += alloced[i] + alloced_atomic[i];\n\
 \                jhc_printf_stderr(fmt2,i,alloced[i],alloced_atomic[i],alloced_atomic[i] + alloced[i], accum);\n\
 \        }\n\
 \}\n\
 \#endif\n\
 \\n\
 \#if JHC_MEM_ANNOTATE && _JHC_GC == _JHC_GC_JGC\n\
 \#include <Judy.h>\n\
 \\n\
 \static Pvoid_t mem_annotate = NULL;\n\
 \\n\
 \#define XSTR(x) #x\n\
 \#define STR(x) XSTR(x)\n\
 \#define gc_alloc(gc,sc,c,nptrs) \\\n\
 \    gc_alloc_annot(gc,sc,c,nptrs,(__FILE__ \":\" STR(__LINE__)))\n\
 \\n\
 \A_UNUSED static void *\n\
 \gc_alloc_annot(gc_t gc,struct s_cache **sc, unsigned count, unsigned nptrs, char *str)\n\
 \{\n\
 \        void *ret = (gc_alloc)(gc,sc,count,nptrs);\n\
 \        PWord_t pval;\n\
 \        JLI(pval,mem_annotate,(Word_t)ret);\n\
 \        *pval = (Word_t)str;\n\
 \        return ret;\n\
 \}\n\
 \\n\
 \char *\n\
 \gc_lookup(void *ptr)\n\
 \{\n\
 \        PWord_t pval;\n\
 \        JLG(pval,mem_annotate,(Word_t)ptr & ~(Word_t)3);\n\
 \        return pval ? (char *)*pval : \"(none)\";\n\
 \}\n\
 \\n\
 \#endif\n\
 \\n\
 \#if _JHC_DEBUG  && _JHC_GC == _JHC_GC_JGC\n\
 \\n\
 \// these ensure the type synonyms are available to the debugger\n\
 \uintptr_t _dummy1;\n\
 \node_t *_dummy2;\n\
 \dnode_t *_dummy3;\n\
 \sptr_t *_dummy4;\n\
 \fptr_t *_dummy5;\n\
 \wptr_t *_dummy6;\n\
 \\n\
 \bool A_UNUSED\n\
 \jhc_valid_whnf(wptr_t s)\n\
 \{\n\
 \        return ((GET_PTYPE(s) == P_VALUE) || ((GET_PTYPE(s) == P_WHNF) && jhc_malloc_sanity(s,P_WHNF)));\n\
 \}\n\
 \\n\
 \bool A_UNUSED\n\
 \jhc_valid_lazy(sptr_t s)\n\
 \{\n\
 \        if(jhc_valid_whnf((wptr_t)s))\n\
 \                return true;\n\
 \        assert(GET_PTYPE(s) == P_LAZY);\n\
 \        node_t *ds = (node_t *)FROM_SPTR(s);\n\
 \        assert(jhc_malloc_sanity(ds,P_LAZY));\n\
 \        if(IS_LAZY(ds->head)) {\n\
 \                if(ds->head == BLACK_HOLE) return true;\n\
 \                assert(GET_PTYPE(ds->head) == P_FUNC);\n\
 \                return true;\n\
 \        } else\n\
 \                return jhc_valid_whnf((wptr_t)ds->head);\n\
 \}\n\
 \\n\
 \#endif\n\
 \\n\
 \#if _JHC_DEBUG\n\
 \wptr_t A_STD\n\
 \promote(sptr_t s)\n\
 \{\n\
 \        assert(!IS_LAZY(s));\n\
 \        assert(jhc_valid_whnf((wptr_t)s));\n\
 \        return (wptr_t)s;\n\
 \}\n\
 \\n\
 \sptr_t A_STD\n\
 \demote(wptr_t s)\n\
 \{\n\
 \        assert(!IS_LAZY(s));\n\
 \        assert(jhc_valid_whnf(s));\n\
 \        return (sptr_t)s;\n\
 \}\n\
 \\n\
 \void A_STD\n\
 \update(void * thunk, wptr_t new)\n\
 \{\n\
 \        assert(GETHEAD(thunk) == BLACK_HOLE);\n\
 \        assert(!IS_LAZY(new));\n\
 \        GETHEAD(thunk) = (fptr_t)new;\n\
 \}\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/profile.h
{-# NOINLINE profile_h #-}
profile_h :: ByteString
profile_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef RTS_PROFILE_H\n\
 \#define RTS_PROFILE_H\n\
 \\n\
 \#include <stdio.h>\n\
 \#include \"rts/cdefs.h\"\n\
 \\n\
 \#ifndef JHC_VALGRIND\n\
 \#define JHC_VALGRIND 0\n\
 \#endif\n\
 \\n\
 \#ifndef JHC_MEM_ANNOTATE\n\
 \#define JHC_MEM_ANNOTATE 0\n\
 \#endif\n\
 \\n\
 \#ifndef _JHC_PROFILE\n\
 \#define _JHC_PROFILE 0\n\
 \#endif\n\
 \\n\
 \#if JHC_VALGRIND\n\
 \#include <valgrind/valgrind.h>\n\
 \#include <valgrind/memcheck.h>\n\
 \#else\n\
 \#define VALGRIND_MAKE_MEM_UNDEFINED(x,y) \\\n\
 \    do { } while (0)\n\
 \#define VALGRIND_MAKE_MEM_DEFINED(x,y) \\\n\
 \    do { } while (0)\n\
 \#define VALGRIND_MAKE_MEM_NOACCESS(x,y) \\\n\
 \    do { } while (0)\n\
 \#define VALGRIND_PRINTF(...) \\\n\
 \    do { } while (0)\n\
 \#endif\n\
 \\n\
 \// void A_UNUSED profile_print_header(FILE *file, char *value_unit);\n\
 \void A_COLD jhc_print_profile(void);\n\
 \\n\
 \#if _JHC_PROFILE\n\
 \struct profile_stack;\n\
 \extern struct profile_stack gc_alloc_time;\n\
 \extern struct profile_stack gc_gc_time;\n\
 \void jhc_profile_push(struct profile_stack *ps);\n\
 \void jhc_profile_pop(struct profile_stack *ps);\n\
 \#define profile_push(x) jhc_profile_push(x)\n\
 \#define profile_pop(x)  jhc_profile_pop(x)\n\
 \#else\n\
 \#define profile_push(x)          do { } while(0)\n\
 \#define profile_pop(x)           do { } while(0)\n\
 \#define alloc_count(x,y)         do { } while(0)\n\
 \#define print_alloc_size_stats() do { } while(0)\n\
 \#endif\n\
 \\n\
 \#ifdef _JHC_USE_OWN_STDIO\n\
 \/* Implement us! */\n\
 \int jhc_printf_stderr(const char *fmt, ...);\n\
 \int jhc_fputs_stderr(const char *s);\n\
 \int jhc_fflush_stdout(void);\n\
 \#else\n\
 \#define jhc_printf_stderr(...) fprintf(stderr,__VA_ARGS__)\n\
 \#define jhc_fputs_stderr(S)    fputs(S, stderr)\n\
 \#define jhc_fflush_stdout()    fflush(stdout)\n\
 \#endif /* _JHC_USE_OWN_STDIO */\n\
 \\n\
 \#if JHC_STATUS > 1\n\
 \#define debugf(...) jhc_printf_stderr(__VA_ARGS__)\n\
 \#else\n\
 \#define debugf(...) do { } while (0)\n\
 \#endif\n\
 \\n\
 \#endif /* RTS_PROFILE_H */\n\
 \"#

-- | Generated from rts\/rts\/cdefs.h
{-# NOINLINE cdefs_h #-}
cdefs_h :: ByteString
cdefs_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef RTS_CDEFS_H\n\
 \#define RTS_CDEFS_H\n\
 \\n\
 \// GNU attributes\n\
 \#if !defined(__predict_true)\n\
 \#ifdef __GNUC__\n\
 \#  define __predict_true(exp)     __builtin_expect(!!(exp), 1)\n\
 \#  define __predict_false(exp)    __builtin_expect(!!(exp), 0)\n\
 \#else\n\
 \#  define __predict_true(exp)     (exp)\n\
 \#  define __predict_false(exp)    (exp)\n\
 \#endif\n\
 \#endif\n\
 \\n\
 \#ifdef __GNUC__\n\
 \#define A_ALIGNED  __attribute__ ((aligned))\n\
 \#define A_CONST    __attribute__ ((const))\n\
 \#define A_MALLOC   __attribute__ ((malloc))\n\
 \#define A_MAYALIAS __attribute__ ((__may_alias__))\n\
 \#define A_NORETURN __attribute__ ((noreturn))\n\
 \#define A_PURE     __attribute__ ((pure))\n\
 \#define A_UNUSED   __attribute__ ((unused))\n\
 \#ifdef __i386__\n\
 \#define A_REGPARM __attribute__ ((fastcall))\n\
 \#else\n\
 \#define A_REGPARM\n\
 \#endif\n\
 \#define A_STD    A_REGPARM\n\
 \\n\
 \#else\n\
 \#define A_ALIGNED\n\
 \#define A_CONST\n\
 \#define A_MALLOC\n\
 \#define A_MAYALIAS\n\
 \#define A_NORETURN\n\
 \#define A_PURE\n\
 \#define A_UNUSED\n\
 \#define A_STD\n\
 \#endif\n\
 \\n\
 \// these should be enabled with newer versions of gcc\n\
 \#define A_HOT\n\
 \#define A_COLD\n\
 \#define A_FALIGNED\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/rts_support.c
{-# NOINLINE rts_support_c #-}
rts_support_c :: ByteString
rts_support_c = unsafePerformIO $ unsafePackAddress "\
 \#include <stdio.h>\n\
 \#include <stdlib.h>\n\
 \#include <locale.h>\n\
 \\n\
 \#include \"HsFFI.h\"\n\
 \#include \"rts/rts_support.h\"\n\
 \#include \"rts/profile.h\"\n\
 \#include \"rts/gc.h\"\n\
 \#include \"rts/conc.h\"\n\
 \\n\
 \jmp_buf jhc_uncaught;\n\
 \int jhc_argc;\n\
 \char **jhc_argv;\n\
 \char *jhc_progname;\n\
 \\n\
 \#ifdef __WIN32__\n\
 \A_UNUSED char *jhc_options_os =  \"mingw32\";\n\
 \A_UNUSED char *jhc_options_arch = \"i386\";\n\
 \#elif defined(__ARM_EABI__)\n\
 \A_UNUSED char *jhc_options_os =  \"nds\";\n\
 \A_UNUSED char *jhc_options_arch = \"ARM\";\n\
 \#else\n\
 \A_UNUSED char *jhc_options_os = \"(unknown os)\";\n\
 \A_UNUSED char *jhc_options_arch = \"(unknown arch)\";\n\
 \#endif\n\
 \\n\
 \void\n\
 \hs_set_argv(int argc, char *argv[])\n\
 \{\n\
 \        jhc_argc = argc - 1;\n\
 \        jhc_argv = argv + 1;\n\
 \        jhc_progname = argv[0];\n\
 \}\n\
 \\n\
 \void A_NORETURN A_UNUSED A_COLD\n\
 \jhc_exit(int n) {\n\
 \        jhc_fflush_stdout();\n\
 \        jhc_print_profile();\n\
 \        exit(n);\n\
 \}\n\
 \\n\
 \void  A_NORETURN A_UNUSED  A_COLD\n\
 \jhc_error(char *s) {\n\
 \        jhc_fflush_stdout();\n\
 \        jhc_fputs_stderr(s);\n\
 \        jhc_fputs_stderr(\"\\n\");\n\
 \        jhc_exit(1);\n\
 \}\n\
 \\n\
 \void  A_NORETURN A_UNUSED  A_COLD\n\
 \jhc_case_fell_off(int n) {\n\
 \        jhc_fflush_stdout();\n\
 \        jhc_printf_stderr(\"\\n%s:%i: case fell off\\n\", __FILE__, n);\n\
 \        abort();\n\
 \}\n\
 \\n\
 \#if _JHC_GC == _JHC_GC_JGC\n\
 \void jhc_hs_init(gc_t gc,arena_t arena);\n\
 \#else\n\
 \void jhc_hs_init();\n\
 \#endif\n\
 \\n\
 \static int hs_init_count = 0;\n\
 \void\n\
 \hs_init(int *argc, char **argv[])\n\
 \{\n\
 \        if(!hs_init_count++) {\n\
 \                jhc_conc_init();\n\
 \                hs_set_argv(*argc,*argv);\n\
 \#if JHC_isPosix\n\
 \                struct utsname jhc_utsname;\n\
 \                if(!uname(&jhc_utsname)) {\n\
 \                        jhc_options_arch = jhc_utsname.machine;\n\
 \                        jhc_options_os   = jhc_utsname.sysname;\n\
 \                }\n\
 \#endif\n\
 \                setlocale(LC_ALL,\"\");\n\
 \        }\n\
 \}\n\
 \\n\
 \void\n\
 \hs_exit(void)\n\
 \{\n\
 \        if(!hs_init_count) {\n\
 \                jhc_printf_stderr(\"hs_exit() called before hs_init()\\n\");\n\
 \                abort();\n\
 \        }\n\
 \        if(!--hs_init_count) {\n\
 \                jhc_exit(0);\n\
 \        }\n\
 \}\n\
 \"#

-- | Generated from rts\/rts\/rts_support.h
{-# NOINLINE rts_support_h #-}
rts_support_h :: ByteString
rts_support_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef RTS_SUPPORT_H\n\
 \#define RTS_SUPPORT_H\n\
 \\n\
 \#include <setjmp.h>\n\
 \#include \"rts/cdefs.h\"\n\
 \\n\
 \extern jmp_buf jhc_uncaught;\n\
 \A_UNUSED extern char *jhc_options_os;\n\
 \A_UNUSED extern char *jhc_options_arch;\n\
 \extern int jhc_argc;\n\
 \extern char **jhc_argv;\n\
 \extern char *jhc_progname;\n\
 \\n\
 \extern char jhc_c_compile[];\n\
 \extern char jhc_command[];\n\
 \extern char jhc_version[];\n\
 \\n\
 \void A_NORETURN A_UNUSED A_COLD jhc_exit(int n);\n\
 \void A_NORETURN A_UNUSED A_COLD jhc_error(char *s);\n\
 \void A_NORETURN A_UNUSED A_COLD jhc_case_fell_off(int n);\n\
 \\n\
 \#define jhc_setjmp(jb) setjmp(*(jb))\n\
 \#define jhc_longjmp(jb) longjmp(*(jb),1)\n\
 \\n\
 \#define prim_umaxbound(t) ((t)~((t)0))\n\
 \#define prim_maxbound(t) ((t)(~((t)1 << (sizeof(t)*CHAR_BIT - 1))))\n\
 \#define prim_minbound(t) ((t)(((t)1 << (sizeof(t)*CHAR_BIT - 1))))\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/gc.h
{-# NOINLINE gc_h #-}
gc_h :: ByteString
gc_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef JHC_GC_H\n\
 \#define JHC_GC_H\n\
 \\n\
 \#define _JHC_GC_NONE   0\n\
 \#define _JHC_GC_JGC    1\n\
 \#define _JHC_GC_BOEHM  2\n\
 \#define _JHC_GC_REGION 3\n\
 \\n\
 \#ifndef _JHC_GC\n\
 \#define _JHC_GC _JHC_GC_NONE\n\
 \#endif\n\
 \\n\
 \#include \"rts/gc_none.h\"\n\
 \#include \"rts/gc_jgc.h\"\n\
 \\n\
 \#if _JHC_GC == _JHC_GC_JGC\n\
 \void jhc_alloc_init(gc_t *gc_p,arena_t *arena_p);\n\
 \void jhc_alloc_fini(gc_t gc,arena_t arena);\n\
 \#else\n\
 \void jhc_alloc_init(void);\n\
 \void jhc_alloc_fini(void);\n\
 \#endif\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/gc_none.c
{-# NOINLINE gc_none_c #-}
gc_none_c :: ByteString
gc_none_c = unsafePerformIO $ unsafePackAddress "\
 \#include <stdlib.h>\n\
 \#include <stdio.h>\n\
 \#include \"rts/gc.h\"\n\
 \#include \"rts/profile.h\"\n\
 \\n\
 \#if _JHC_GC == _JHC_GC_BOEHM\n\
 \\n\
 \void hs_perform_gc(void) {\n\
 \        GC_gcollect();\n\
 \}\n\
 \\n\
 \void jhc_alloc_init(void) { GC_INIT(); }\n\
 \void jhc_alloc_fini(void) { }\n\
 \\n\
 \#elif _JHC_GC == _JHC_GC_NONE\n\
 \\n\
 \// memory allocated in 1MB chunks.\n\
 \#define JHC_MEM_CHUNK_SIZE (1 << 20)\n\
 \\n\
 \static char initial_chunk[JHC_MEM_CHUNK_SIZE];\n\
 \\n\
 \static void *jhc_current_chunk = initial_chunk;\n\
 \static unsigned mem_chunks,mem_offset;\n\
 \\n\
 \void jhc_alloc_init(void) {}\n\
 \\n\
 \void\n\
 \jhc_alloc_fini(void) {\n\
 \        if(_JHC_PROFILE) {\n\
 \                jhc_printf_stderr(\"Memory Allocated: %u bytes\\n\", (JHC_MEM_CHUNK_SIZE*(mem_chunks)) + mem_offset);\n\
 \                print_alloc_size_stats();\n\
 \        }\n\
 \}\n\
 \\n\
 \static void\n\
 \jhc_malloc_grow(void) {\n\
 \        void *c = malloc(JHC_MEM_CHUNK_SIZE);\n\
 \        if(!c) {\n\
 \                jhc_fputs_stderr(\"Out of memory!\\n\");\n\
 \                abort();\n\
 \        }\n\
 \        mem_chunks++;\n\
 \        jhc_current_chunk = c;\n\
 \        mem_offset = 0;\n\
 \}\n\
 \\n\
 \#define M_ALIGN(a,n) ((n) - 1 + ((a) - ((n) - 1) % (a)))\n\
 \\n\
 \static inline void * A_MALLOC\n\
 \jhc_malloc_basic(size_t n) {\n\
 \        n = M_ALIGN(sizeof(void *),n);\n\
 \        if (n > (JHC_MEM_CHUNK_SIZE - mem_offset))\n\
 \                jhc_malloc_grow();\n\
 \        void *ret = jhc_current_chunk + mem_offset;\n\
 \        mem_offset += n;\n\
 \        return ret;\n\
 \}\n\
 \\n\
 \#if _JHC_DEBUG\n\
 \\n\
 \void * A_MALLOC\n\
 \jhc_malloc_debug(size_t n,int line,int atomic) {\n\
 \        alloc_count(n,atomic);\n\
 \        void *ret = jhc_malloc_basic(n + sizeof(uintptr_t));\n\
 \        *((uintptr_t *)ret) = line;\n\
 \        return ret + sizeof(uintptr_t);\n\
 \}\n\
 \\n\
 \#else\n\
 \\n\
 \void * A_MALLOC\n\
 \jhc_malloc(size_t n) {\n\
 \        alloc_count(n,0);\n\
 \        return jhc_malloc_basic(n);\n\
 \}\n\
 \\n\
 \#undef jhc_malloc_atomic\n\
 \void * A_MALLOC\n\
 \jhc_malloc_atomic(size_t n) {\n\
 \        alloc_count(n,1);\n\
 \        return jhc_malloc_basic(n);\n\
 \}\n\
 \\n\
 \#endif\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/gc_none.h
{-# NOINLINE gc_none_h #-}
gc_none_h :: ByteString
gc_none_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef GC_NONE_H\n\
 \#define GC_NONE_H\n\
 \\n\
 \#include <stddef.h>\n\
 \#include \"rts/cdefs.h\"\n\
 \\n\
 \#define jhc_malloc_sanity(p,t) (1)\n\
 \\n\
 \#if _JHC_GC == _JHC_GC_BOEHM\n\
 \\n\
 \#include \"rts/profile.h\"\n\
 \\n\
 \#include <gc/gc.h>\n\
 \\n\
 \#define jhc_malloc GC_malloc\n\
 \#define jhc_malloc_atomic GC_malloc_atomic\n\
 \\n\
 \#elif _JHC_GC == _JHC_GC_NONE\n\
 \\n\
 \#if _JHC_DEBUG\n\
 \void * A_MALLOC jhc_malloc_debug(size_t n,int line,int atomic);\n\
 \#define jhc_malloc(n) jhc_malloc_debug(n,__LINE__,0)\n\
 \#define jhc_malloc_atomic(n) jhc_malloc_debug(n,__LINE__,1)\n\
 \#else\n\
 \void * A_MALLOC jhc_malloc(size_t n);\n\
 \void * A_MALLOC jhc_malloc_atomic(size_t n);\n\
 \#endif\n\
 \\n\
 \#endif\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/jhc_rts.c
{-# NOINLINE jhc_rts_c #-}
jhc_rts_c :: ByteString
jhc_rts_c = unsafePerformIO $ unsafePackAddress "\
 \/*@Internals\n\
 \\n\
 \# The Run Time System\n\
 \\n\
 \Ajhc is very minimalist in that it does not have a precompiled run time system,\n\
 \but rather generates what is needed as part of the compilation process.\n\
 \However, back ends do have specific run-time representations of data, which can\n\
 \be affected by things like the choice of garbage collector. The following\n\
 \describes the general layout for the C based back-ends, but compiler options\n\
 \such as garbage collection method or whether we do full program analysis, will\n\
 \affect which features are used and whether certain optimized layouts are\n\
 \possible.\n\
 \\n\
 \Unboxed values directly translate to values in the target language, an unboxed\n\
 \Int will translate directly into an 'int' as an argument and an unboxed pointer\n\
 \will be a raw pointer. Unboxed values have no special interpretation and are\n\
 \_not_ followed by the garbage collector. If the target language does not\n\
 \support a feature such as multiple return values, it will have to be simulated.\n\
 \It would not be wrong to think of Grin code that only deals with unboxed values\n\
 \to be isomorphic to C-- or C augmented with multiple return values.\n\
 \\n\
 \Boxed values have a standard representation and can be followed. Unlike some\n\
 \other implementation, being boxed does not imply the object is located on the\n\
 \heap. It may be on the stack, heap, or even embedded within the smart pointer\n\
 \itself. Being boxed only means that the object may be represented by a smart\n\
 \pointer, which may or may not actually be a pointer in the traditional sense.\n\
 \\n\
 \A boxed value in Ajhc is represented by a 'smart pointer' of c type sptr_t. a\n\
 \smart pointer is the size of a native pointer, but can take on different roles\n\
 \depending on a pair of tag bits, called the ptype.\n\
 \\n\
 \smart pointers take on a general form as follows:\n\
 \\n\
 \    -------------------------\n\
 \    |    payload        | GL|\n\
 \    -------------------------\n\
 \\n\
 \      G - if set, then the garbage collector should not treat value as a pointer to be followed\n\
 \      L - lazy, this bit being set means the value is potentially not in WHNF\n\
 \\n\
 \A sptr_t on its own in the wild can only take on one of the following forms:\n\
 \\n\
 \    -------------------------\n\
 \    |    whnf raw value | 10|\n\
 \    -------------------------\n\
 \\n\
 \    -------------------------\n\
 \    |    whnf location  | 00|\n\
 \    -------------------------\n\
 \\n\
 \WHNF stands for 'Weak Head Normal Form' and means that the value is not a\n\
 \suspended function and hence not a pointer to a thunk. It may be directly\n\
 \examined and need not be evaluated. wptr_t is an alias for sptr_t that is\n\
 \guarenteed to be of one of the above forms. It is used to improve safety for\n\
 \when we can statically know that a value is WHNF and hence we can skip the\n\
 \expensive 'eval'.\n\
 \\n\
 \The difference between the raw value and the whnf location is that the first\n\
 \contains uninterpreted bits, while the second is a pointer to a location on the\n\
 \heap or stack and hence the garbage collector should follow it. The format of\n\
 \the memory pointed to by the whnf location is unspecified and dependent on the\n\
 \actual type being represented.\n\
 \\n\
 \Partial (unsaturated) applications are normal WHNF values. Saturated\n\
 \applications which may be 'eval'ed and updated are called thunks and must not\n\
 \be pointed to by WHNF pointers. Their representation follows.\n\
 \\n\
 \    -------------------------\n\
 \    |   lazy location   | 01|\n\
 \    -------------------------\n\
 \\n\
 \A lazy location points to either a thunk, or a redirection to a WHNF value. A\n\
 \lazy location is always a pointer to an allocated block of memory which always\n\
 \begins with a restricted smart pointer. This restricted smart pointer is represented by\n\
 \the C type alias 'fptr_t'. fptr_t's only occur as the first entry in a lazy\n\
 \location, they never are passed around as objects in their own right.\n\
 \\n\
 \A fptr_t may be a whnf value or a code pointer. If a fptr_t is a whnf value (of one of\n\
 \the two forms given above) then it is called a redirection, the lazy location should be\n\
 \treated exactly as if it were the whnf given. This is used to redirect an evaluated\n\
 \thunk to its computed value.\n\
 \\n\
 \A fptr_t may also be a 'code pointer' in which case the lazy location is called\n\
 \a thunk. A code pointer is a pointer to executable machine code that evaluates\n\
 \a closure and returns a wptr_t, the returned wptr_t is then generally written\n\
 \over the code pointer, turning the thunk into a redirection. It is the\n\
 \responsibility of the code pointed to to perform this redirection.\n\
 \\n\
 \    -------------------------\n\
 \    |    code pointer   | 11|\n\
 \    -------------------------\n\
 \    |     data ...          |\n\
 \\n\
 \When debugging, the special code pointer BLACK_HOLE is also sometimes stored in\n\
 \a fptr_t to detect certain run-time errors.\n\
 \\n\
 \Note that unlike other implementations, a fptr_t may _not_ be another lazy\n\
 \location. you can not have chained redirections, a redirection is always a\n\
 \redirection to a whnf value.\n\
 \\n\
 \    sptr_t - a tagged smart pointer, may contain a whnf value or a lazy location.\n\
 \    wptr_t - a tagged smart pointer that contains a whnf value (either raw or a location)\n\
 \    fptr_t - a tagged smart pointer, may contain a whnf value indicating a redirection, or a code pointer indicating a thunk.\n\
 \\n\
 \*/\n\
 \\n\
 \#include \"jhc_rts_header.h\"\n\
 \\n\
 \#if _JHC_GC == _JHC_GC_JGC\n\
 \typedef wptr_t (*eval_fn)(gc_t gc,arena_t arena,node_t *node) A_STD;\n\
 \#else\n\
 \typedef wptr_t (*eval_fn)(node_t *node) A_STD;\n\
 \#endif\n\
 \\n\
 \// like eval but you know the target is in WHNF or is a already evaluated indirection\n\
 \static inline wptr_t A_STD A_UNUSED  A_HOT\n\
 \follow(sptr_t s)\n\
 \{\n\
 \        assert(jhc_valid_lazy(s));\n\
 \        if(IS_LAZY(s)) {\n\
 \                sptr_t h = (sptr_t)(GETHEAD(FROM_SPTR(s)));\n\
 \                assert(!IS_LAZY(h));\n\
 \                return (wptr_t)h;\n\
 \        }\n\
 \        return (wptr_t)s;\n\
 \}\n\
 \\n\
 \wptr_t A_STD A_UNUSED  A_HOT\n\
 \#if _JHC_GC == _JHC_GC_JGC\n\
 \eval(gc_t gc,arena_t arena,sptr_t s)\n\
 \#else\n\
 \eval(sptr_t s)\n\
 \#endif\n\
 \{\n\
 \        assert(jhc_valid_lazy(s));\n\
 \        if(IS_LAZY(s)) {\n\
 \                assert(GET_PTYPE(s) == P_LAZY);\n\
 \                void *ds = FROM_SPTR(s);\n\
 \                sptr_t h = (sptr_t)(GETHEAD(ds));\n\
 \                assert((fptr_t)h != BLACK_HOLE);\n\
 \                if(IS_LAZY(h)) {\n\
 \                        eval_fn fn = (eval_fn)FROM_SPTR(h);\n\
 \                        assert(GET_PTYPE(h) == P_FUNC);\n\
 \#if _JHC_DEBUG\n\
 \                        GETHEAD(ds) = BLACK_HOLE;\n\
 \#endif\n\
 \                        fn = (eval_fn)SET_THUMB_BIT(fn);\n\
 \#if _JHC_GC == _JHC_GC_JGC\n\
 \                        wptr_t r = (*fn)(gc,arena,NODEP(ds));\n\
 \#else\n\
 \                        wptr_t r = (*fn)(NODEP(ds));\n\
 \#endif\n\
 \#if _JHC_DEBUG\n\
 \                        assert(GETHEAD(ds) != BLACK_HOLE);\n\
 \#endif\n\
 \                        return r;\n\
 \                }\n\
 \                return (wptr_t)h;\n\
 \        }\n\
 \        assert(jhc_valid_whnf((wptr_t)s));\n\
 \        return (wptr_t)s;\n\
 \}\n\
 \\n\
 \#if _JHC_STANDALONE\n\
 \int\n\
 \main(int argc, char *argv[])\n\
 \{\n\
 \        hs_init(&argc,&argv);\n\
 \        if (jhc_setjmp(&jhc_uncaught))\n\
 \                jhc_error(\"Uncaught Exception\");\n\
 \        else\n\
 \                _amain();\n\
 \        hs_exit();\n\
 \        return 0;\n\
 \}\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/jhc_rts.h
{-# NOINLINE jhc_rts_h #-}
jhc_rts_h :: ByteString
jhc_rts_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef JHC_RTS_H\n\
 \#define JHC_RTS_H\n\
 \\n\
 \#include \"rts/profile.h\"\n\
 \#include \"rts/gc.h\"\n\
 \\n\
 \struct sptr;\n\
 \struct wptr;\n\
 \struct fptr;\n\
 \\n\
 \// we use dummy structs here so the compiler will catch any attempt\n\
 \// to use one type in anothers place\n\
 \typedef struct sptr * sptr_t;\n\
 \typedef struct sptr * wptr_t;\n\
 \typedef struct fptr * fptr_t;\n\
 \typedef uintptr_t     what_t;\n\
 \\n\
 \typedef struct node {\n\
 \        fptr_t head;\n\
 \        sptr_t rest[];\n\
 \} A_MAYALIAS node_t;\n\
 \\n\
 \typedef struct dnode {\n\
 \        what_t what;\n\
 \        sptr_t rest[];\n\
 \} A_MAYALIAS dnode_t;\n\
 \\n\
 \#define P_WHNF  0x0\n\
 \#define P_LAZY  0x1\n\
 \#define P_VALUE 0x2\n\
 \#define P_FUNC  0x3\n\
 \\n\
 \#define IS_LAZY(x)     (bool)(((uintptr_t)(x)) & 0x1)\n\
 \#define IS_PTR(x)      (bool)(!(((uintptr_t)(x)) & 0x2))\n\
 \\n\
 \#define FROM_SPTR(x)   (typeof (x))((uintptr_t)(x) & ~0x3)  // remove a ptype from a smart pointer\n\
 \#define GET_PTYPE(x)   ((uintptr_t)(x) & 0x3)               // return the ptype associated with a smart pointer\n\
 \#define TO_SPTR(t,x)   (typeof (x))((uintptr_t)(x) | (t))   // attach a ptype to a smart pointer\n\
 \#define TO_SPTR_C(t,x) (typeof (x))((uintptr_t)(x) + (t))   // attach a ptype to a smart pointer, suitable for use by constant initialializers\n\
 \\n\
 \#define GETHEAD(x)   (NODEP(x)->head)\n\
 \#define NODEP(x)     ((node_t *)(x))\n\
 \#define DNODEP(x)    ((dnode_t *)(x))\n\
 \\n\
 \#define MKLAZY(fn)    TO_SPTR(P_LAZY,(sptr_t)fn)\n\
 \#define MKLAZY_C(fn)  TO_SPTR_C(P_LAZY,(sptr_t)fn)\n\
 \#define TO_FPTR(fn)   TO_SPTR_C(P_FUNC,(fptr_t)fn)\n\
 \\n\
 \#define RAW_SET_F(n)   ((wptr_t)(((intptr_t)(n) << 2) | P_VALUE))\n\
 \#define RAW_SET_UF(n)  ((wptr_t)(((uintptr_t)(n) << 2) | P_VALUE))\n\
 \#define RAW_GET_F(n)   ((intptr_t)(n) >> 2)\n\
 \#define RAW_GET_UF(n)  ((uintptr_t)(n) >> 2)\n\
 \\n\
 \#define RAW_SET_16(w)  (wptr_t)(((uintptr_t)(w) << 16) | P_VALUE)\n\
 \#define RAW_GET_16(n)  ((intptr_t)(n) >> 16)\n\
 \#define RAW_GET_U16(n) ((uintptr_t)(n) >> 16)\n\
 \\n\
 \// demote is always safe, we must only promote when we know the argument is a WHNF\n\
 \#define PROMOTE(n)   ((wptr_t)(n))\n\
 \#define DEMOTE(n)    ((sptr_t)(n))\n\
 \\n\
 \#define FETCH_TAG(x)      RAW_GET_U16(IS_PTR(x) ? FETCH_MEM_TAG(x) : (what_t)(x))\n\
 \#define FETCH_RAW_TAG(x)  RAW_GET_U16(x)\n\
 \#define SET_RAW_TAG(x)    RAW_SET_16(x)\n\
 \#define FETCH_MEM_TAG(x)  (DNODEP(x)->what)\n\
 \#define SET_MEM_TAG(x,v)  (DNODEP(x)->what = (what_t)RAW_SET_16(v))\n\
 \\n\
 \#define BLACK_HOLE TO_FPTR(0xDEADBEE0)\n\
 \\n\
 \wptr_t A_STD\n\
 \#if _JHC_GC == _JHC_GC_JGC\n\
 \eval(gc_t gc,arena_t arena,sptr_t s);\n\
 \#else\n\
 \eval(sptr_t s);\n\
 \#endif\n\
 \\n\
 \// both promote and demote evaluate to nothing when debugging is not enabled\n\
 \// otherwise, they check that their arguments are in the correct form.\n\
 \#if _JHC_DEBUG\n\
 \wptr_t A_STD promote(sptr_t s);\n\
 \sptr_t A_STD demote(wptr_t s);\n\
 \void   A_STD update(void *, wptr_t);\n\
 \#else\n\
 \#define promote(x) PROMOTE(x)\n\
 \#define demote(x) DEMOTE(x)\n\
 \inline static void update(void *t, wptr_t n) { GETHEAD(t) = (fptr_t)n; }\n\
 \#endif\n\
 \\n\
 \#if _JHC_DEBUG && _JHC_GC == _JHC_GC_JGC\n\
 \bool jhc_valid_whnf(wptr_t s);\n\
 \bool jhc_valid_lazy(sptr_t s);\n\
 \#else\n\
 \#define jhc_valid_whnf(x) true\n\
 \#define jhc_valid_lazy(x) true\n\
 \#endif\n\
 \\n\
 \#endif\n\
 \\n\
 \/*\n\
 \ * Detail:\n\
 \ * http://communities.mentor.com/community/cs/archives/arm-gnu/msg01904.html\n\
 \ */\n\
 \#ifdef _JHC_ARM_STAY_IN_THUMB_MODE\n\
 \#define SET_THUMB_BIT(fn)    TO_SPTR(0x1,(sptr_t)fn)\n\
 \#else\n\
 \#define SET_THUMB_BIT(fn)    (fn)\n\
 \#endif\n\
 \"#

-- | Generated from rts\/lib\/lib_cbits.c
{-# NOINLINE lib_cbits_c #-}
lib_cbits_c :: ByteString
lib_cbits_c = unsafePerformIO $ unsafePackAddress "\
 \/* this file contains C only needed to help support the\n\
 \ * standard libraries */\n\
 \\n\
 \#include <stdio.h>\n\
 \\n\
 \#include \"HsFFI.h\"\n\
 \#include \"rts/cdefs.h\"\n\
 \\n\
 \HsInt jhc_stdrnd[2] A_UNUSED = { 1 , 1 };\n\
 \HsInt jhc_data_unique A_UNUSED;\n\
 \\n\
 \HsBool A_UNUSED\n\
 \jhc_wait_for_input(FILE *f,HsInt timeout) {\n\
 \#if JHC_isPosix\n\
 \        fd_set fds;\n\
 \        FD_ZERO(&fds);\n\
 \        FD_SET(fileno(f),&fds);\n\
 \        struct timeval to = {  0, timeout * 1000 };\n\
 \        int retval = select(1,&fds,NULL,&fds,&to);\n\
 \        if(retval)\n\
 \                return HS_BOOL_TRUE;\n\
 \        else\n\
 \                return HS_BOOL_FALSE;\n\
 \#else\n\
 \        return HS_BOOL_FALSE;\n\
 \#endif\n\
 \}\n\
 \\n\
 \uint32_t\n\
 \jhc_hash32(uint32_t key)\n\
 \{\n\
 \  int c2=0x27d4eb2d; // a prime or an odd constant\n\
 \  key = (key ^ 61) ^ (key >> 16);\n\
 \  key = key + (key << 3);\n\
 \  key = key ^ (key >> 4);\n\
 \  key = key * c2;\n\
 \  key = key ^ (key >> 15);\n\
 \  return key;\n\
 \}\n\
 \\n\
 \uint64_t jhc_hash64(uint64_t key)\n\
 \{\n\
 \  key = (~key) + (key << 21); // key = (key << 21) - key - 1;\n\
 \  key = key ^ (key >> 24);\n\
 \  key = (key + (key << 3)) + (key << 8); // key * 265\n\
 \  key = key ^ (key >> 14);\n\
 \  key = (key + (key << 2)) + (key << 4); // key * 21\n\
 \  key = key ^ (key >> 28);\n\
 \  key = key + (key << 31);\n\
 \  return key;\n\
 \}\n\
 \\n\
 \uintptr_t\n\
 \jhc_hashptr(uintptr_t key)\n\
 \{\n\
 \    if (sizeof(uintptr_t) == sizeof(uint32_t)) {\n\
 \        return (uintptr_t)jhc_hash32((uint32_t)key);\n\
 \    } else {\n\
 \        return (uintptr_t)jhc_hash64((uint64_t)key);\n\
 \    }\n\
 \}\n\
 \"#

-- | Generated from rts\/jhc_rts_header.h
{-# NOINLINE jhc_rts_header_h #-}
jhc_rts_header_h :: ByteString
jhc_rts_header_h = unsafePerformIO $ unsafePackAddress "\
 \#include <assert.h>\n\
 \#include <errno.h>\n\
 \#include <float.h>\n\
 \#include <limits.h>\n\
 \#include <locale.h>\n\
 \#include <math.h>\n\
 \#include <stdio.h>\n\
 \#include <stdlib.h>\n\
 \#include <string.h>\n\
 \#include <time.h>\n\
 \#include <unistd.h>\n\
 \#include <wchar.h>\n\
 \#include <setjmp.h>\n\
 \#ifndef __WIN32__\n\
 \#ifdef __ARM_EABI__\n\
 \#include <malloc.h>\n\
 \#else\n\
 \#include <sys/select.h>\n\
 \#include <sys/utsname.h>\n\
 \#endif\n\
 \#include <sys/times.h>\n\
 \#include <sys/types.h>\n\
 \#include <sys/param.h>\n\
 \#else\n\
 \#include <malloc.h>\n\
 \#endif\n\
 \\n\
 \#include \"HsFFI.h\"\n\
 \#include \"sys/wsize.h\"\n\
 \#include \"rts/cdefs.h\"\n\
 \\n\
 \#ifndef _JHC_DEBUG\n\
 \#ifdef NDEBUG\n\
 \#define _JHC_DEBUG 0\n\
 \#else\n\
 \#define _JHC_DEBUG 1\n\
 \#endif\n\
 \#endif\n\
 \\n\
 \#ifndef _JHC_STANDALONE\n\
 \#define _JHC_STANDALONE 1\n\
 \#endif\n\
 \\n\
 \#ifndef JHC_STATUS\n\
 \#define JHC_STATUS 0\n\
 \#endif\n\
 \\n\
 \#ifdef __WIN32__\n\
 \#define JHC_isWindows   1\n\
 \#define JHC_isBigEndian 0\n\
 \#else\n\
 \#define JHC_isWindows 0\n\
 \#define JHC_isBigEndian (__BYTE_ORDER == __BIG_ENDIAN)\n\
 \#endif\n\
 \\n\
 \#define JHC_isPosix (!JHC_isWindows && !defined(__ARM_EABI__))\n\
 \\n\
 \#include \"rts/profile.h\"\n\
 \#include \"rts/rts_support.h\"\n\
 \#include \"rts/gc.h\"\n\
 \#include \"rts/jhc_rts.h\"\n\
 \#include \"lib/lib_cbits.h\"\n\
 \\n\
 \// the program will provide the following\n\
 \void _amain(void);\n\
 \#if _JHC_GC == _JHC_GC_JGC\n\
 \void jhc_hs_init(gc_t gc,arena_t arena);\n\
 \#else\n\
 \void jhc_hs_init();\n\
 \#endif\n\
 \extern const void * const nh_stuff[];\n\
 \"#

-- | Generated from rts\/lib\/lib_cbits.h
{-# NOINLINE lib_cbits_h #-}
lib_cbits_h :: ByteString
lib_cbits_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef LIB_CBITS_H\n\
 \#define LIB_CBITS_H\n\
 \\n\
 \#include \"HsFFI.h\"\n\
 \struct FILE;\n\
 \\n\
 \extern HsInt jhc_stdrnd[2];\n\
 \extern HsInt jhc_data_unique;\n\
 \HsBool jhc_wait_for_input(FILE *f,HsInt timeout);\n\
 \\n\
 \#ifdef _JHC_USE_OWN_STDIO\n\
 \/* Implement us! */\n\
 \int jhc_utf8_getchar(void);\n\
 \int jhc_utf8_getc(FILE *f);\n\
 \int jhc_utf8_putchar(int ch);\n\
 \int jhc_utf8_putc(int ch, FILE *f);\n\
 \#else\n\
 \inline static int A_UNUSED\n\
 \jhc_utf8_getchar(void)\n\
 \{\n\
 \    return getchar();\n\
 \}\n\
 \\n\
 \inline static int A_UNUSED\n\
 \jhc_utf8_getc(FILE *f)\n\
 \{\n\
 \    return getc(f);\n\
 \}\n\
 \\n\
 \inline static int A_UNUSED\n\
 \jhc_utf8_putchar(int ch)\n\
 \{\n\
 \    return putchar(ch);\n\
 \}\n\
 \\n\
 \inline static int A_UNUSED\n\
 \jhc_utf8_putc(int ch, FILE *f)\n\
 \{\n\
 \    return putc(ch,f);\n\
 \}\n\
 \#endif /* _JHC_USE_OWN_STDIO */\n\
 \\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/gc_jgc_internal.h
{-# NOINLINE gc_jgc_internal_h #-}
gc_jgc_internal_h :: ByteString
gc_jgc_internal_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef GC_JGC_INTERNAL_H\n\
 \#define GC_JGC_INTERNAL_H\n\
 \\n\
 \#include \"rts/gc_jgc.h\"\n\
 \#include \"sys/bitarray.h\"\n\
 \#include \"sys/queue.h\"\n\
 \\n\
 \#if _JHC_GC == _JHC_GC_JGC\n\
 \\n\
 \struct s_caches_pub;\n\
 \\n\
 \struct s_arena {\n\
 \        SLIST_ENTRY(s_arena) link;\n\
 \        struct s_megablock *current_megablock;\n\
 \        SLIST_HEAD(,s_block) free_blocks;\n\
 \        unsigned block_used;\n\
 \        unsigned block_threshold;\n\
 \        SLIST_HEAD(,s_cache) caches;\n\
 \        SLIST_HEAD(,s_block) monolithic_blocks;\n\
 \        SLIST_HEAD(,s_megablock) megablocks;\n\
 \        unsigned number_gcs;    // number of garbage collections\n\
 \        unsigned number_allocs; // number of allocations since last garbage collection\n\
 \        gc_t gc_stack_base;\n\
 \// 7 to share caches with the first 7 tuples\n\
 \#define GC_STATIC_ARRAY_NUM 7\n\
 \#define GC_MAX_BLOCK_ENTRIES 150\n\
 \        struct s_cache *array_caches[GC_STATIC_ARRAY_NUM];\n\
 \        struct s_cache *array_caches_atomic[GC_STATIC_ARRAY_NUM];\n\
 \        struct s_caches_pub *public_caches_p; // access from main_code.c\n\
 \        int force_gc_next_s_alloc;\n\
 \};\n\
 \\n\
 \struct s_megablock {\n\
 \        void *base;\n\
 \        unsigned next_free;\n\
 \        SLIST_ENTRY(s_megablock) next;\n\
 \};\n\
 \\n\
 \struct s_block {\n\
 \        SLIST_ENTRY(s_block) link;\n\
 \        unsigned char flags;  // defined in rts/constants.h\n\
 \        unsigned char color;  // offset in words to first entry.\n\
 \        union {\n\
 \                // A normal block.\n\
 \                struct {\n\
 \                        unsigned char num_ptrs;\n\
 \                        unsigned char size;\n\
 \                        unsigned short num_free;\n\
 \                        unsigned short next_free;\n\
 \                } pi;\n\
 \                // A monolithic block.\n\
 \                struct {\n\
 \                        unsigned num_ptrs;\n\
 \                } m;\n\
 \        } u;\n\
 \        bitarray_t used[];\n\
 \};\n\
 \\n\
 \struct s_cache {\n\
 \        SLIST_ENTRY(s_cache) next;\n\
 \        SLIST_HEAD(,s_block) blocks;\n\
 \        SLIST_HEAD(,s_block) full_blocks;\n\
 \        unsigned char color;\n\
 \        unsigned char size;\n\
 \        unsigned char num_ptrs;\n\
 \        unsigned char flags;\n\
 \        unsigned short num_entries;\n\
 \        struct s_arena *arena;\n\
 \#if _JHC_PROFILE\n\
 \        unsigned allocations;\n\
 \#endif\n\
 \};\n\
 \#endif\n\
 \#endif\n\
 \"#

-- | Generated from rts\/rts\/conc.c
{-# NOINLINE conc_c #-}
conc_c :: ByteString
conc_c = unsafePerformIO $ unsafePackAddress "\
 \#include \"rts/conc.h\"\n\
 \\n\
 \static jhc_mutex_t jhc_rts_mutex;\n\
 \\n\
 \void\n\
 \jhc_conc_init()\n\
 \{\n\
 \        jhc_mutex_init(&jhc_rts_mutex);\n\
 \}\n\
 \\n\
 \void\n\
 \jhc_rts_lock()\n\
 \{\n\
 \        jhc_mutex_lock(&jhc_rts_mutex);\n\
 \}\n\
 \\n\
 \void\n\
 \jhc_rts_unlock()\n\
 \{\n\
 \        jhc_mutex_unlock(&jhc_rts_mutex);\n\
 \}\n\
 \\n\
 \#if _JHC_CONC == _JHC_CONC_NONE\n\
 \jhc_threadid_t\n\
 \forkOS_createThread(void *(*wrapper) (void *), void *entry, int *err)\n\
 \{\n\
 \        (*wrapper)(entry);\n\
 \        return 0; /* xxx */\n\
 \}\n\
 \\n\
 \#elif _JHC_CONC == _JHC_CONC_PTHREAD\n\
 \jhc_threadid_t\n\
 \forkOS_createThread(void *(*wrapper) (void *), void *entry, int *err)\n\
 \{\n\
 \        pthread_t tid;\n\
 \        *err = pthread_create(&tid, NULL, wrapper, entry);\n\
 \        if (*err) {\n\
 \                pthread_detach(tid);\n\
 \        }\n\
 \        return tid;\n\
 \}\n\
 \\n\
 \#elif _JHC_CONC == _JHC_CONC_CUSTOM\n\
 \/* You should impl me at your side. */\n\
 \#else\n\
 \#error \"You should choose _JHC_CONC.\"\n\
 \#endif /* _JHC_CONC == ??? */\n\
 \"#

-- | Generated from rts\/rts\/conc.h
{-# NOINLINE conc_h #-}
conc_h :: ByteString
conc_h = unsafePerformIO $ unsafePackAddress "\
 \#ifndef CONC_H\n\
 \#define CONC_H\n\
 \\n\
 \#define _JHC_CONC_NONE    0\n\
 \#define _JHC_CONC_PTHREAD 1\n\
 \#define _JHC_CONC_CUSTOM  2\n\
 \\n\
 \#ifndef _JHC_CONC\n\
 \#define _JHC_CONC _JHC_CONC_NONE\n\
 \#endif\n\
 \\n\
 \#if _JHC_CONC == _JHC_CONC_NONE\n\
 \#define jhc_threadid_t          int\n\
 \#define jhc_mutex_t             int\n\
 \#define jhc_mutex_init(M)\x0009\&(*(M) = 0)\n\
 \#define jhc_mutex_lock(M)\x0009\&do { } while (/* CONSTCOND */ 0)\n\
 \#define jhc_mutex_unlock(M)\x0009\&do { } while (/* CONSTCOND */ 0)\n\
 \\n\
 \#elif _JHC_CONC == _JHC_CONC_PTHREAD\n\
 \#include <pthread.h>\n\
 \#define jhc_threadid_t          pthread_t\n\
 \#define jhc_mutex_t             pthread_mutex_t\n\
 \#define jhc_mutex_init(M)\x0009\&(void) pthread_mutex_init((M), NULL)\n\
 \#define jhc_mutex_lock(M)\x0009\&pthread_mutex_lock((M))\n\
 \#define jhc_mutex_unlock(M)\x0009\&pthread_mutex_unlock((M))\n\
 \\n\
 \#elif _JHC_CONC == _JHC_CONC_CUSTOM\n\
 \#include \"conc_custom.h\"\n\
 \/* You should impl \"jhc_threadid_t\" and \"jhc_mutex_t\". */\n\
 \void jhc_mutex_init(jhc_mutex_t *mutex);\n\
 \void jhc_mutex_lock(jhc_mutex_t *mutex);\n\
 \void jhc_mutex_unlock(jhc_mutex_t *mutex);\n\
 \\n\
 \#else\n\
 \#error \"You should choose _JHC_CONC.\"\n\
 \\n\
 \#endif /* _JHC_CONC == ??? */\n\
 \\n\
 \/* Common functions */\n\
 \jhc_threadid_t forkOS_createThread(void *(*wrapper) (void *), void *entry, int *err);\n\
 \void jhc_conc_init(void);\n\
 \void jhc_rts_lock(void);\n\
 \void jhc_rts_unlock(void);\n\
 \\n\
 \#endif /* CONC_H */\n\
 \"#



