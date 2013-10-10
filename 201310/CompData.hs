module CompData where

type Distance = Double
type Pos      = (Double, Double)
type Dir      = Double


data Logo = FD Distance
          | RT Dir
          | Repeat Int [Logo]
    deriving (Show, Eq)

data SVG = Path [Step]
    deriving (Show, Eq)

data Step = M Pos
          | L Pos
    deriving (Show, Eq)

data Turtle = Turtle Pos Dir
    deriving (Show, Eq)

prog :: [Logo]
prog = [FD 100, RT 90, FD 100, RT 90, FD 100, RT 90, FD 100, RT 90]

prog' = [Repeat 4 [FD 100, RT 90]]

eval :: [Logo] -> SVG
eval prog = Path $ go prog (Turtle (0, 0) 0)
  where
    go [] _ = []
    go (FD d : ls) (Turtle pos dir) = let pos' = offset pos dir d
                                      in  L pos' : go ls (Turtle pos' dir)
    go (RT d : ls) (Turtle pos dir) = go ls (Turtle pos (dir + d))
    go (Repeat n is : ls) t = go (concat (replicate n is) ++ ls) t

offset :: Pos -> Dir -> Distance -> Pos
offset (x, y) dir d = ( x + d * cos (dir / 180 * pi)
                      , y + d * sin (dir / 180 * pi)
                      )
