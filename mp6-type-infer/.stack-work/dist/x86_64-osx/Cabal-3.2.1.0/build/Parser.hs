{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
module Parser where

import Data.Char
import Common
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

newtype HappyAbsSyn t29 = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap8 = HappyWrap8 (Dec)
happyIn8 :: (Dec) -> (HappyAbsSyn t29)
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap8 x)
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn t29) -> HappyWrap8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
newtype HappyWrap9 = HappyWrap9 (Exp)
happyIn9 :: (Exp) -> (HappyAbsSyn t29)
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap9 x)
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn t29) -> HappyWrap9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 (Exp)
happyIn10 :: (Exp) -> (HappyAbsSyn t29)
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn t29) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 (Exp)
happyIn11 :: (Exp) -> (HappyAbsSyn t29)
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn t29) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 (Exp)
happyIn12 :: (Exp) -> (HappyAbsSyn t29)
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn t29) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
newtype HappyWrap13 = HappyWrap13 (Exp)
happyIn13 :: (Exp) -> (HappyAbsSyn t29)
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap13 x)
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn t29) -> HappyWrap13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
newtype HappyWrap14 = HappyWrap14 (Exp)
happyIn14 :: (Exp) -> (HappyAbsSyn t29)
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap14 x)
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn t29) -> HappyWrap14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
newtype HappyWrap15 = HappyWrap15 (Exp)
happyIn15 :: (Exp) -> (HappyAbsSyn t29)
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn t29) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
newtype HappyWrap16 = HappyWrap16 (Binop)
happyIn16 :: (Binop) -> (HappyAbsSyn t29)
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap16 x)
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn t29) -> HappyWrap16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 (Binop)
happyIn17 :: (Binop) -> (HappyAbsSyn t29)
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn t29) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
newtype HappyWrap18 = HappyWrap18 (Monop)
happyIn18 :: (Monop) -> (HappyAbsSyn t29)
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap18 x)
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn t29) -> HappyWrap18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
newtype HappyWrap19 = HappyWrap19 (Const)
happyIn19 :: (Const) -> (HappyAbsSyn t29)
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap19 x)
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn t29) -> HappyWrap19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
newtype HappyWrap20 = HappyWrap20 ([Constraint])
happyIn20 :: ([Constraint]) -> (HappyAbsSyn t29)
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn t29) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 ([Constraint])
happyIn21 :: ([Constraint]) -> (HappyAbsSyn t29)
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn t29) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 (Constraint)
happyIn22 :: (Constraint) -> (HappyAbsSyn t29)
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn t29) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 ([(String, PolyTy)])
happyIn23 :: ([(String, PolyTy)]) -> (HappyAbsSyn t29)
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn t29) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
newtype HappyWrap24 = HappyWrap24 ([(String, PolyTy)])
happyIn24 :: ([(String, PolyTy)]) -> (HappyAbsSyn t29)
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn t29) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
newtype HappyWrap25 = HappyWrap25 ((String, PolyTy))
happyIn25 :: ((String, PolyTy)) -> (HappyAbsSyn t29)
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap25 x)
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn t29) -> HappyWrap25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
newtype HappyWrap26 = HappyWrap26 (PolyTy)
happyIn26 :: (PolyTy) -> (HappyAbsSyn t29)
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap26 x)
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn t29) -> HappyWrap26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
newtype HappyWrap27 = HappyWrap27 (MonoTy)
happyIn27 :: (MonoTy) -> (HappyAbsSyn t29)
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn t29) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
newtype HappyWrap28 = HappyWrap28 (MonoTy)
happyIn28 :: (MonoTy) -> (HappyAbsSyn t29)
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap28 x)
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn t29) -> HappyWrap28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: t29 -> (HappyAbsSyn t29)
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn t29) -> t29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyInTok :: (Token) -> (HappyAbsSyn t29)
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn t29) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\xe0\xe2\x0f\xe0\x29\x00\x00\x00\x00\x70\xf1\x07\xf0\x14\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x08\xe0\x01\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x17\x7f\x00\x4f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf8\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5c\x00\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x17\x7f\x00\x4f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\xb8\xf9\x03\x78\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x17\x7f\x00\x4f\x01\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x20\x80\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x01\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x80\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x08\x00\x00\x01\x3c\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x20\x80\x07\x00\x00\x00\x80\x00\x00\x10\xc0\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\xe2\x0f\xe0\x29\x00\x00\x00\x00\x70\xf1\x07\xf0\x14\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\xc5\x1f\xc0\x53\x00\x00\x00\x00\xe0\xe2\x0f\xe0\x29\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\xb8\xf8\x03\x78\x0a\x00\x00\x00\x00\x40\x00\x00\x08\xe0\x01\x00\x00\x00\x10\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x20\x80\x07\x00\x00\x00\x00\x04\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x2e\xfe\x00\x9e\x02\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x08\xe0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x70\xf1\x07\xf0\x14\x00\x00\x00\x00\xb8\xf8\x03\x78\x0a\x00\x00\x00\x00\x5c\xfc\x01\x3c\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x8b\x3f\x80\xa7\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5c\xfc\x01\x3c\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","%start_parseExp","%start_parseEnv","%start_parseType","%start_parseEqList","Dec","Exp","RBinExp","LBinExp","RCExp","ControlExp","AppExp","AtomExp","RBinop","LBinop","Monop","Const","EqList","EqNEList","Equity","Env","NEEnv","Bind","PolyTy","MonoTy","LinTy","AtomTy","true","false","'['","']'","'('","')'","'{'","'}'","hd","tl","print","'~'","fst","snd","not","'+'","'-'","'*'","'/'","'^'","','","cons","'='","'>'","id","int","str","if","then","else","fun","arrow","let","in","rec","t_int","t_bool","t_str","t_unit","t_list","'.'","%eof"]
        bit_start = st Prelude.* 71
        bit_end = (st Prelude.+ 1) Prelude.* 71
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..70]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x71\x00\x92\x00\x04\x00\x01\x00\x0b\x00\x92\x00\x00\x00\x00\x00\x00\x00\x54\x01\x00\x00\x31\x00\x5d\x01\x92\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x92\x00\xfa\xff\xec\xff\xf0\xff\xfd\xff\xf0\xff\x00\x00\xf1\xff\x00\x00\x05\x00\xed\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf4\xff\xfc\xff\xf4\xff\xf4\xff\xef\xff\x3d\x00\x2f\x00\x00\x00\x00\x00\x37\x00\x13\x00\x43\x00\x00\x00\x17\x00\x17\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x51\x00\x54\x00\x4e\x00\x53\x00\x6f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x92\x00\x92\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x66\x01\x00\x00\x00\x00\x92\x00\x92\x00\x75\x00\x92\x00\x17\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\xff\xff\x76\x00\x92\x00\x67\x00\x7a\x00\x00\x00\x7d\x00\x00\x00\x00\x00\x29\x00\x00\x00\x83\x00\x8f\x00\x00\x00\x8b\x00\x92\x00\x92\x00\x92\x00\x00\x00\x00\x00\x92\x00\x88\x00\x00\x00\x88\x00\x00\x00\x92\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xb4\x00\xbf\x00\xa0\x00\x32\x00\xa4\x00\xca\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x02\x00\x00\x00\xd5\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xeb\x00\x00\x00\x00\x00\x00\x00\x52\x00\x00\x00\x00\x00\x00\x00\x00\x00\x64\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xac\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x00\x00\x85\x00\x6c\x01\x00\x00\xb0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x58\x01\x9d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf6\x00\x01\x01\x00\x00\x0c\x01\x6f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\x00\x00\x00\x00\x00\x17\x01\x00\x00\x00\x00\x00\x00\xb6\x00\x00\x00\x00\x00\x73\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x01\x2d\x01\x38\x01\x00\x00\x00\x00\x43\x01\x00\x00\x00\x00\x00\x00\x00\x00\x4e\x01\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfa\xff\xf7\xff\xf5\xff\x00\x00\xf3\xff\xeb\xff\xe9\xff\x00\x00\xe8\xff\xd4\xff\xd3\xff\x00\x00\x00\x00\xdc\xff\xdb\xff\xda\xff\xd9\xff\xd8\xff\xd7\xff\xd6\xff\xe7\xff\xd5\xff\xd2\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc4\xff\xc1\xff\xbf\xff\x00\x00\xba\xff\xbe\xff\xbd\xff\xbc\xff\xbb\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc7\xff\xca\xff\x00\x00\x00\x00\x00\x00\xba\xff\x00\x00\x00\x00\xc0\xff\xcc\xff\x00\x00\xcf\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\xff\xd1\xff\xec\xff\xea\xff\x00\x00\x00\x00\xe4\xff\xe3\xff\xe2\xff\xe1\xff\xe0\xff\xdf\xff\xe5\xff\xde\xff\xdd\xff\xf4\xff\xe9\xff\xf6\xff\xe6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc2\xff\xc3\xff\xb9\xff\xc5\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf9\xff\x00\x00\xc9\xff\x00\x00\xc6\xff\xce\xff\x00\x00\xcb\xff\x00\x00\x00\x00\xf0\xff\x00\x00\x00\x00\x00\x00\x00\x00\xcd\xff\xc8\xff\x00\x00\xf8\xff\xee\xff\x00\x00\xef\xff\x00\x00\xed\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x04\x00\x05\x00\x12\x00\x08\x00\x19\x00\x05\x00\x08\x00\x19\x00\x07\x00\x05\x00\x07\x00\x04\x00\x0b\x00\x03\x00\x23\x00\x04\x00\x20\x00\x23\x00\x19\x00\x15\x00\x19\x00\x29\x00\x1a\x00\x05\x00\x28\x00\x2a\x00\x1a\x00\x05\x00\x15\x00\x2a\x00\x1a\x00\x05\x00\x24\x00\x25\x00\x26\x00\x27\x00\x24\x00\x25\x00\x26\x00\x27\x00\x24\x00\x25\x00\x26\x00\x27\x00\x1a\x00\x05\x00\x08\x00\x09\x00\x1a\x00\x01\x00\x02\x00\x03\x00\x1a\x00\x05\x00\x24\x00\x25\x00\x26\x00\x27\x00\x24\x00\x25\x00\x26\x00\x27\x00\x24\x00\x25\x00\x26\x00\x27\x00\x1a\x00\x12\x00\x13\x00\x14\x00\x15\x00\x19\x00\x06\x00\x19\x00\x1a\x00\x1b\x00\x24\x00\x25\x00\x26\x00\x27\x00\x01\x00\x02\x00\x03\x00\x17\x00\x05\x00\x06\x00\x20\x00\x0c\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x0e\x00\x12\x00\x13\x00\x14\x00\x15\x00\x13\x00\x14\x00\x15\x00\x17\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x19\x00\x20\x00\x1f\x00\x1d\x00\x21\x00\x01\x00\x02\x00\x03\x00\x06\x00\x05\x00\x13\x00\x14\x00\x15\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x0e\x00\x12\x00\x13\x00\x14\x00\x15\x00\x13\x00\x14\x00\x15\x00\x22\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x19\x00\x19\x00\x1f\x00\x17\x00\x21\x00\x01\x00\x02\x00\x03\x00\x19\x00\x05\x00\x13\x00\x14\x00\x15\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x05\x00\x06\x00\x07\x00\x22\x00\x17\x00\x0a\x00\x0b\x00\x1e\x00\x22\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x0f\x00\x0c\x00\x1f\x00\x11\x00\x21\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x10\x00\x0d\x00\x0a\x00\x0b\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x11\x00\xff\xff\x0a\x00\x0b\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\xff\xff\xff\xff\x0a\x00\x0b\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\xff\xff\xff\xff\x0a\x00\x0b\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\xff\xff\xff\xff\x0a\x00\x0b\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\xff\xff\xff\xff\x0a\x00\x0b\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\xff\xff\xff\xff\x0a\x00\x0b\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\xff\xff\xff\xff\x0a\x00\x0b\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\xff\xff\xff\xff\x0a\x00\x0b\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\xff\xff\xff\xff\x0a\x00\x0b\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\xff\xff\xff\xff\x0a\x00\x0b\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\xff\xff\xff\xff\x0a\x00\x0b\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\xff\xff\xff\xff\x0a\x00\x0b\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\xff\xff\xff\xff\x0a\x00\x0b\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\xff\xff\xff\xff\x0a\x00\x0b\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\xff\xff\xff\xff\x0a\x00\x0b\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x10\x00\x11\x00\x12\x00\x13\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x13\x00\x14\x00\x15\x00\x13\x00\x14\x00\x15\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x3f\x00\x27\x00\x3a\x00\x35\x00\x40\x00\x27\x00\x68\x00\x32\x00\x47\x00\x27\x00\x2e\x00\x6b\x00\x0e\x00\x22\x00\x41\x00\x46\x00\x3b\x00\x33\x00\x42\x00\x69\x00\x36\x00\x37\x00\x39\x00\x27\x00\x3c\x00\xff\xff\x28\x00\x27\x00\x6c\x00\xff\xff\x39\x00\x27\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x28\x00\x27\x00\x48\x00\x49\x00\x39\x00\x10\x00\x11\x00\x12\x00\x28\x00\x13\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x39\x00\x22\x00\x23\x00\x24\x00\x25\x00\x64\x00\x60\x00\x1b\x00\x1c\x00\x1d\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x10\x00\x11\x00\x12\x00\x65\x00\x13\x00\x45\x00\x62\x00\x5c\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x3c\x00\x60\x00\x23\x00\x24\x00\x25\x00\x3d\x00\x24\x00\x25\x00\x5b\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x5a\x00\x59\x00\x1f\x00\x58\x00\x20\x00\x10\x00\x11\x00\x12\x00\x57\x00\x13\x00\x37\x00\x24\x00\x25\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x74\x00\x69\x00\x23\x00\x24\x00\x25\x00\x3d\x00\x24\x00\x25\x00\x74\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x6f\x00\x67\x00\x1f\x00\x77\x00\x31\x00\x10\x00\x11\x00\x12\x00\x36\x00\x13\x00\x5e\x00\x24\x00\x25\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x53\x00\x0b\x00\x54\x00\x74\x00\x73\x00\x0d\x00\x0e\x00\x72\x00\x7c\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x2c\x00\x20\x00\x1f\x00\x33\x00\x20\x00\x2f\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x62\x00\x5c\x00\x0d\x00\x0e\x00\x2e\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x75\x00\x00\x00\x0d\x00\x0e\x00\x06\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x00\x00\x0d\x00\x0e\x00\x46\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x00\x00\x0d\x00\x0e\x00\x43\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x00\x00\x0d\x00\x0e\x00\x42\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x00\x00\x0d\x00\x0e\x00\x70\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x00\x00\x0d\x00\x0e\x00\x6f\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x00\x00\x0d\x00\x0e\x00\x6d\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x00\x00\x0d\x00\x0e\x00\x65\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x00\x00\x0d\x00\x0e\x00\x7a\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x00\x00\x0d\x00\x0e\x00\x79\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x00\x00\x0d\x00\x0e\x00\x78\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x00\x00\x0d\x00\x0e\x00\x77\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x00\x00\x0d\x00\x0e\x00\x7c\x00\x07\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x00\x00\x0d\x00\x0e\x00\x55\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x00\x00\x00\x00\x0d\x00\x0e\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\xf1\xff\xf1\xff\xf1\xff\xf1\xff\xf1\xff\xf1\xff\xf1\xff\xf1\xff\xf1\xff\xf2\xff\xf2\xff\xf2\xff\xf2\xff\xf2\xff\xf2\xff\xf2\xff\xf2\xff\xf2\xff\x5d\x00\x24\x00\x25\x00\x6c\x00\x24\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (5, 70) [
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70)
	]

happy_n_terms = 43 :: Prelude.Int
happy_n_nonterms = 22 :: Prelude.Int

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_5 = happySpecReduce_1  0# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOut9 happy_x_1 of { (HappyWrap9 happy_var_1) -> 
	happyIn8
		 (AnonDec happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_6 = happyReduce 4# 0# happyReduction_6
happyReduction_6 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (ID happy_var_2) -> 
	case happyOut9 happy_x_4 of { (HappyWrap9 happy_var_4) -> 
	happyIn8
		 (LetDec happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_7 = happyReduce 6# 0# happyReduction_7
happyReduction_7 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_3 of { (ID happy_var_3) -> 
	case happyOutTok happy_x_4 of { (ID happy_var_4) -> 
	case happyOut9 happy_x_6 of { (HappyWrap9 happy_var_6) -> 
	happyIn8
		 (LetRec happy_var_3 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_8 = happySpecReduce_1  1# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOut10 happy_x_1 of { (HappyWrap10 happy_var_1) -> 
	happyIn9
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_9 = happySpecReduce_3  2# happyReduction_9
happyReduction_9 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut16 happy_x_2 of { (HappyWrap16 happy_var_2) -> 
	case happyOut10 happy_x_3 of { (HappyWrap10 happy_var_3) -> 
	happyIn10
		 (BinOpExp happy_var_2 happy_var_1 happy_var_3
	)}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_10 = happySpecReduce_1  2# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	happyIn10
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_11 = happySpecReduce_3  3# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut17 happy_x_2 of { (HappyWrap17 happy_var_2) -> 
	case happyOut13 happy_x_3 of { (HappyWrap13 happy_var_3) -> 
	happyIn11
		 (BinOpExp happy_var_2 happy_var_1 happy_var_3
	)}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_12 = happySpecReduce_1  3# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOut13 happy_x_1 of { (HappyWrap13 happy_var_1) -> 
	happyIn11
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_13 = happySpecReduce_3  4# happyReduction_13
happyReduction_13 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	case happyOut17 happy_x_2 of { (HappyWrap17 happy_var_2) -> 
	case happyOut15 happy_x_3 of { (HappyWrap15 happy_var_3) -> 
	happyIn12
		 (BinOpExp happy_var_2 happy_var_1 happy_var_3
	)}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_14 = happySpecReduce_1  4# happyReduction_14
happyReduction_14 happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	happyIn12
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_15 = happyReduce 4# 5# happyReduction_15
happyReduction_15 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (ID happy_var_2) -> 
	case happyOut9 happy_x_4 of { (HappyWrap9 happy_var_4) -> 
	happyIn13
		 (FunExp happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_16 = happyReduce 6# 5# happyReduction_16
happyReduction_16 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	case happyOut9 happy_x_4 of { (HappyWrap9 happy_var_4) -> 
	case happyOut9 happy_x_6 of { (HappyWrap9 happy_var_6) -> 
	happyIn13
		 (IfExp happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_17 = happyReduce 6# 5# happyReduction_17
happyReduction_17 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_2 of { (ID happy_var_2) -> 
	case happyOut9 happy_x_4 of { (HappyWrap9 happy_var_4) -> 
	case happyOut9 happy_x_6 of { (HappyWrap9 happy_var_6) -> 
	happyIn13
		 (LetExp happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_18 = happyReduce 8# 5# happyReduction_18
happyReduction_18 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOutTok happy_x_3 of { (ID happy_var_3) -> 
	case happyOutTok happy_x_4 of { (ID happy_var_4) -> 
	case happyOut9 happy_x_6 of { (HappyWrap9 happy_var_6) -> 
	case happyOut9 happy_x_8 of { (HappyWrap9 happy_var_8) -> 
	happyIn13
		 (LetRecExp happy_var_3 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest}}}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_19 = happySpecReduce_2  5# happyReduction_19
happyReduction_19 happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	happyIn13
		 (MonOpExp happy_var_1 happy_var_2
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_20 = happySpecReduce_1  5# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOut14 happy_x_1 of { (HappyWrap14 happy_var_1) -> 
	happyIn13
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_21 = happySpecReduce_2  6# happyReduction_21
happyReduction_21 happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { (HappyWrap14 happy_var_1) -> 
	case happyOut15 happy_x_2 of { (HappyWrap15 happy_var_2) -> 
	happyIn14
		 (AppExp happy_var_1 happy_var_2
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_22 = happySpecReduce_1  6# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	happyIn14
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_23 = happySpecReduce_1  7# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut19 happy_x_1 of { (HappyWrap19 happy_var_1) -> 
	happyIn15
		 (ConstExp happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_24 = happySpecReduce_1  7# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOutTok happy_x_1 of { (ID happy_var_1) -> 
	happyIn15
		 (VarExp happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_25 = happySpecReduce_3  7# happyReduction_25
happyReduction_25 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut9 happy_x_2 of { (HappyWrap9 happy_var_2) -> 
	happyIn15
		 (happy_var_2
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_26 = happySpecReduce_1  8# happyReduction_26
happyReduction_26 happy_x_1
	 =  happyIn16
		 (ConsOp
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_27 = happySpecReduce_1  9# happyReduction_27
happyReduction_27 happy_x_1
	 =  happyIn17
		 (IntPlusOp
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_28 = happySpecReduce_1  9# happyReduction_28
happyReduction_28 happy_x_1
	 =  happyIn17
		 (IntMinusOp
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_29 = happySpecReduce_1  9# happyReduction_29
happyReduction_29 happy_x_1
	 =  happyIn17
		 (IntTimesOp
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_30 = happySpecReduce_1  9# happyReduction_30
happyReduction_30 happy_x_1
	 =  happyIn17
		 (IntDivOp
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_31 = happySpecReduce_1  9# happyReduction_31
happyReduction_31 happy_x_1
	 =  happyIn17
		 (ConcatOp
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_32 = happySpecReduce_1  9# happyReduction_32
happyReduction_32 happy_x_1
	 =  happyIn17
		 (CommaOp
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_33 = happySpecReduce_1  9# happyReduction_33
happyReduction_33 happy_x_1
	 =  happyIn17
		 (EqOp
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_34 = happySpecReduce_1  9# happyReduction_34
happyReduction_34 happy_x_1
	 =  happyIn17
		 (GreaterOp
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_35 = happySpecReduce_1  10# happyReduction_35
happyReduction_35 happy_x_1
	 =  happyIn18
		 (HdOp
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_36 = happySpecReduce_1  10# happyReduction_36
happyReduction_36 happy_x_1
	 =  happyIn18
		 (TlOp
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_37 = happySpecReduce_1  10# happyReduction_37
happyReduction_37 happy_x_1
	 =  happyIn18
		 (PrintOp
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_38 = happySpecReduce_1  10# happyReduction_38
happyReduction_38 happy_x_1
	 =  happyIn18
		 (IntNegOp
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_39 = happySpecReduce_1  10# happyReduction_39
happyReduction_39 happy_x_1
	 =  happyIn18
		 (FstOp
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_40 = happySpecReduce_1  10# happyReduction_40
happyReduction_40 happy_x_1
	 =  happyIn18
		 (SndOp
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_41 = happySpecReduce_1  10# happyReduction_41
happyReduction_41 happy_x_1
	 =  happyIn18
		 (NotOp
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_42 = happySpecReduce_1  11# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOutTok happy_x_1 of { (INT happy_var_1) -> 
	happyIn19
		 (IntConst happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_43 = happySpecReduce_1  11# happyReduction_43
happyReduction_43 happy_x_1
	 =  happyIn19
		 (BoolConst True
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_44 = happySpecReduce_1  11# happyReduction_44
happyReduction_44 happy_x_1
	 =  happyIn19
		 (BoolConst False
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_45 = happySpecReduce_1  11# happyReduction_45
happyReduction_45 happy_x_1
	 =  case happyOutTok happy_x_1 of { (STRING happy_var_1) -> 
	happyIn19
		 (StringConst happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_46 = happySpecReduce_2  11# happyReduction_46
happyReduction_46 happy_x_2
	happy_x_1
	 =  happyIn19
		 (NilConst
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_47 = happySpecReduce_2  11# happyReduction_47
happyReduction_47 happy_x_2
	happy_x_1
	 =  happyIn19
		 (UnitConst
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_48 = happySpecReduce_2  12# happyReduction_48
happyReduction_48 happy_x_2
	happy_x_1
	 =  happyIn20
		 ([]
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_49 = happyReduce 4# 12# happyReduction_49
happyReduction_49 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut22 happy_x_2 of { (HappyWrap22 happy_var_2) -> 
	case happyOut21 happy_x_3 of { (HappyWrap21 happy_var_3) -> 
	happyIn20
		 (happy_var_2 : reverse happy_var_3
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_50 = happySpecReduce_3  13# happyReduction_50
happyReduction_50 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { (HappyWrap21 happy_var_1) -> 
	case happyOut22 happy_x_3 of { (HappyWrap22 happy_var_3) -> 
	happyIn21
		 (happy_var_3 : happy_var_1
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_51 = happySpecReduce_0  13# happyReduction_51
happyReduction_51  =  happyIn21
		 ([]
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_52 = happySpecReduce_3  14# happyReduction_52
happyReduction_52 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	case happyOut27 happy_x_3 of { (HappyWrap27 happy_var_3) -> 
	happyIn22
		 (happy_var_1 :~: happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_53 = happySpecReduce_2  15# happyReduction_53
happyReduction_53 happy_x_2
	happy_x_1
	 =  happyIn23
		 ([]
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_54 = happyReduce 4# 15# happyReduction_54
happyReduction_54 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut25 happy_x_2 of { (HappyWrap25 happy_var_2) -> 
	case happyOut24 happy_x_3 of { (HappyWrap24 happy_var_3) -> 
	happyIn23
		 (happy_var_2 : reverse happy_var_3
	) `HappyStk` happyRest}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_55 = happySpecReduce_3  16# happyReduction_55
happyReduction_55 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	case happyOut25 happy_x_3 of { (HappyWrap25 happy_var_3) -> 
	happyIn24
		 (happy_var_3 : happy_var_1
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_56 = happySpecReduce_0  16# happyReduction_56
happyReduction_56  =  happyIn24
		 ([]
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_57 = happySpecReduce_3  17# happyReduction_57
happyReduction_57 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (ID happy_var_1) -> 
	case happyOut26 happy_x_3 of { (HappyWrap26 happy_var_3) -> 
	happyIn25
		 ((happy_var_1, happy_var_3)
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_58 = happySpecReduce_3  18# happyReduction_58
happyReduction_58 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOutTok happy_x_1 of { (INT happy_var_1) -> 
	case happyOut26 happy_x_3 of { (HappyWrap26 happy_var_3) -> 
	happyIn26
		 (let Forall l tau = happy_var_3 
                                              in Forall (happy_var_1:l) tau
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_59 = happySpecReduce_1  18# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	happyIn26
		 (Forall [] happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_60 = happySpecReduce_3  19# happyReduction_60
happyReduction_60 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	case happyOut27 happy_x_3 of { (HappyWrap27 happy_var_3) -> 
	happyIn27
		 (pairTy happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_61 = happySpecReduce_3  19# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	case happyOut27 happy_x_3 of { (HappyWrap27 happy_var_3) -> 
	happyIn27
		 (funTy happy_var_1 happy_var_3
	)}}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_62 = happySpecReduce_1  19# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	happyIn27
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_63 = happySpecReduce_2  20# happyReduction_63
happyReduction_63 happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	happyIn28
		 (listTy happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_64 = happySpecReduce_1  20# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_65 = happySpecReduce_1  21# happyReduction_65
happyReduction_65 happy_x_1
	 =  happyIn29
		 (intTy
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_66 = happySpecReduce_1  21# happyReduction_66
happyReduction_66 happy_x_1
	 =  happyIn29
		 (boolTy
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_67 = happySpecReduce_1  21# happyReduction_67
happyReduction_67 happy_x_1
	 =  happyIn29
		 (stringTy
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_68 = happySpecReduce_1  21# happyReduction_68
happyReduction_68 happy_x_1
	 =  happyIn29
		 (unitTy
	)

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_69 = happySpecReduce_1  21# happyReduction_69
happyReduction_69 happy_x_1
	 =  case happyOutTok happy_x_1 of { (INT happy_var_1) -> 
	happyIn29
		 (TyVar happy_var_1
	)}

#if __GLASGOW_HASKELL__ >= 710
#endif
happyReduce_70 = happySpecReduce_3  21# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_2 of { (HappyWrap27 happy_var_2) -> 
	happyIn29
		 (happy_var_2
	)}

happyNewToken action sts stk [] =
	happyDoAction 42# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	TRUE -> cont 1#;
	FALSE -> cont 2#;
	LBRAC -> cont 3#;
	RBRAC -> cont 4#;
	LPAREN -> cont 5#;
	RPAREN -> cont 6#;
	LARCHER -> cont 7#;
	RARCHER -> cont 8#;
	HD -> cont 9#;
	TL -> cont 10#;
	PRINT -> cont 11#;
	NEG -> cont 12#;
	FST -> cont 13#;
	SND -> cont 14#;
	NOT -> cont 15#;
	PLUS -> cont 16#;
	MINUS -> cont 17#;
	TIMES -> cont 18#;
	DIV -> cont 19#;
	CARAT -> cont 20#;
	COMMA -> cont 21#;
	DCOLON -> cont 22#;
	EQUALS -> cont 23#;
	GE -> cont 24#;
	ID happy_dollar_dollar -> cont 25#;
	INT happy_dollar_dollar -> cont 26#;
	STRING happy_dollar_dollar -> cont 27#;
	IF -> cont 28#;
	THEN -> cont 29#;
	ELSE -> cont 30#;
	FUN -> cont 31#;
	ARROW -> cont 32#;
	LET -> cont 33#;
	IN -> cont 34#;
	REC -> cont 35#;
	T_INT -> cont 36#;
	T_BOOL -> cont 37#;
	T_STRING -> cont 38#;
	T_UNIT -> cont 39#;
	T_LIST -> cont 40#;
	DOT -> cont 41#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 42# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap8 x') = happyOut8 x} in x'))

parseExp tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (let {(HappyWrap9 x') = happyOut9 x} in x'))

parseEnv tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (let {(HappyWrap23 x') = happyOut23 x} in x'))

parseType tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (let {(HappyWrap26 x') = happyOut26 x} in x'))

parseEqList tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (let {(HappyWrap20 x') = happyOut20 x} in x'))

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token 
  = TRUE | FALSE 
  | LBRAC | RBRAC | LPAREN | RPAREN | LARCHER | RARCHER
  | HD | TL | PRINT | NEG | FST | SND | NOT
  | PLUS | MINUS | TIMES | DIV | CARAT | COMMA | DCOLON
  | EQUALS | GE | ID String | INT Int | STRING String
  | IF | THEN | ELSE | FUN | ARROW | LET | IN | REC
  | T_INT | T_BOOL | T_STRING | T_UNIT | T_LIST | DOT

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
  | isSpace c = lexer cs
  | isAlpha c = lexWord (c:cs)
  | isDigit c = lexInt (c:cs)
lexer ('"':cs) = lexStrConst cs ""
lexer ('[':cs) = LBRAC : lexer cs
lexer (']':cs) = RBRAC : lexer cs
lexer ('(':cs) = LPAREN : lexer cs
lexer (')':cs) = RPAREN : lexer cs
lexer ('{':cs) = LARCHER : lexer cs
lexer ('}':cs) = RARCHER : lexer cs
lexer ('~':cs) = NEG : lexer cs
lexer ('+':cs) = PLUS : lexer cs
lexer ('-':'>':cs) = ARROW : lexer cs
lexer ('-':cs) = MINUS : lexer cs
lexer ('*':cs) = TIMES : lexer cs
lexer ('/':cs) = DIV : lexer cs
lexer ('^':cs) = CARAT : lexer cs
lexer (',':cs) = COMMA : lexer cs
lexer (':':':':cs) = DCOLON : lexer cs
lexer ('=':cs) = EQUALS : lexer cs
lexer ('>':cs) = GE : lexer cs
lexer ('.':cs) = DOT : lexer cs
lexer (_:cs)   = lexer cs

lexInt cs =
  let (num, cs') = span isDigit cs
    in INT (read num) : lexer cs'

lexWord cs =
  case span isAlpha cs of
    ("true", cs)   -> TRUE : lexer cs
    ("false", cs)  -> FALSE : lexer cs
    ("hd", cs)     -> HD : lexer cs
    ("tl", cs)     -> TL : lexer cs
    ("print", cs)  -> PRINT : lexer cs
    ("fst", cs)    -> FST : lexer cs
    ("snd", cs)    -> SND : lexer cs
    ("not", cs)    -> NOT : lexer cs
    ("if", cs)     -> IF : lexer cs
    ("then", cs)   -> THEN : lexer cs
    ("else", cs)   -> ELSE : lexer cs
    ("fun", cs)    -> FUN : lexer cs
    ("let", cs)    -> LET : lexer cs
    ("in", cs)     -> IN : lexer cs
    ("rec", cs)    -> REC : lexer cs
    ("int", cs)    -> T_INT : lexer cs
    ("bool", cs)   -> T_BOOL : lexer cs
    ("string", cs) -> T_STRING : lexer cs
    ("unit", cs)   -> T_UNIT : lexer cs
    ("list", cs)   -> T_LIST : lexer cs
    (id, cs)       -> ID id : lexer cs

lexStrConst [] s = [STRING $ reverse s]
lexStrConst ('"':cs) s = STRING (reverse s) : lexer cs
lexStrConst (c:cs) s = lexStrConst cs (c:s)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Prelude.Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
