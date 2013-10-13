{-# LANGUAGE DeriveFunctor, TemplateHaskell, TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -w #-}
module CompLogo where

import Data.Comp
import Data.Comp.Derive
import Prelude hiding (repeat)

type Distance = Double

{-
data Logo = FD Distance | RT Dir | Repeat Int [Logo]
  deriving (Eq, Show)
-}

type Pos = (Double, Double)
type Dir = Double

data PrimLogoF r = FD Distance | RT Dir
  deriving (Functor, Eq, Show)

data SugarLogoF r = Repeat Int [r]
  deriving (Functor, Eq, Show)

type LogoF = PrimLogoF :+: SugarLogoF

-- data (f :+: g) r = Inl (f r) | Inr (g r)

-- data Term f = Term (f (Term f))

derive [smartConstructors] [''PrimLogoF, ''SugarLogoF]

type PrimLogo = Term PrimLogoF
type Logo     = Term LogoF

data SVG = Path [Step]
  deriving (Eq, Show)

data Step = M Pos | L Pos
  deriving (Eq, Show)

data Turtle = Turtle Pos Dir
  deriving (Eq, Show)

prog :: [Logo]
prog  = [iFD 100, iRT 90, iFD 100, iRT 90, iFD 100, iRT 90, iFD 100, iRT 90]
prog' :: [Logo]
prog' = [iRepeat 4 [iFD 100, iRT 90]]

{-
eval :: [Logo] -> SVG
eval prog = Path $ go prog (Turtle (0,0) 0)
  where
    go []          _                = []
    go (FD d : ls) (Turtle pos dir) = let pos' = offset pos dir d
                                      in L pos' : go ls (Turtle pos' dir)
    go (RT d : ls) (Turtle pos dir) = go ls (Turtle pos (dir + d))
    go (Repeat n is : ls) t         = go (concat (replicate n is) ++ ls)
                                         t
-}

eval :: [PrimLogo] -> SVG
eval pr = Path $ foldr go (const []) pr (Turtle (0,0) 0)
  where
    go :: PrimLogo -> (Turtle -> [Step]) -> Turtle -> [Step]
    go (Term (FD d)) r (Turtle pos dir) = let pos' = offset pos dir d
                                          in L pos' : r (Turtle pos' dir)
    go (Term (RT d)) r (Turtle pos dir) = r (Turtle pos (dir + d))

offset :: Pos -> Dir -> Distance -> Pos
offset (x, y) dir d = ( x + d * cos (dir / 180 * pi)
                      , y + d * sin (dir / 180 * pi)
                      )

unroll :: [Logo] -> [PrimLogo]
unroll = foldr go []
  where
    go :: Logo -> [PrimLogo] -> [PrimLogo]
    go (Term (proj -> Just (Repeat n is))) r =
      concat (replicate n (unroll is)) ++ r
    go (Term (proj -> Just (FD d))) r =
      iFD d : r
    go (Term (proj -> Just (RT d))) r =
      iRT d : r
    -- go (Term (proj -> Just (i :: PrimLogoF Logo))) r =
    --   Term (inj i) : r
    -- go i                                   r = i : r

