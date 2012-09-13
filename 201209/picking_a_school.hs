{- When it comes to secondary schools, there are various factors of getting into
 - the school that you want.
 -
 - We have a prioritised list of students that have a prioritised list of
 - preferred schools
 -
 - We could do a Alternative Vote style: take the first child and put them in
 - their first choice. Remove them from the list of other schools and move onto
 - the next child... If you can't get in your first you try their second and so
 - own.
 -
 - Sounds like a Nash Equilibrium problem.
 - Could also be the Stable Marriage Problem.
 -}

module Schools where

import qualified Data.Map as Map

type Student = String

-- Pan = pupil allocation number = number of children in the school
data School = School {schPan :: Int, schPriority :: [Student]}
    deriving (Show, Eq, Ord)

data Pref = Pref {prefStudent :: Student, prefChoices :: [Int]}
    deriving (Show, Eq, Ord)

data Offer = Offer {offStudent :: Student, offSchool :: Int}
    deriving (Show, Eq, Ord)

schools :: [School]
schools =
    [ School 2 ["A", "B", "D", "C"]
    , School 2 ["B", "D", "A", "C"]
    ]

prefs :: [Pref]
prefs =
    [ Pref "A" [0,1]
    , Pref "B" [1,0]
    , Pref "C" [1,0]
    , Pref "D" [0,1]
    ]

type State = ([Int],[Offer],[Pref])

allocate :: [School] -> [Pref] -> [Offer]
allocate schools prefs = offers
    where
        (_,offers,_) = foldl go (pans,[],[]) prefs
        ranking :: School -> Map.Map Student Int
        ranking (School _ priority) = Map.fromList $ zip priority [0..]

        schools' :: [Map.Map Student Int]
        schools' = map ranking schools

        pans = map schPan schools

        go :: State -> Pref -> State
        go (pans, offers, unallocated) p@(Pref student choices) =
            foldr f (pans,offers,p:unallocated) choices
                where
                    f :: Int -> State -> State
                    f sch cont =
                        if rank < pans !! sch
                        then (pans',Offer student sch : offers, unallocated)
                        else cont
                        where
                            rank = (schools' !! sch) Map.! student
                            pans' = zipWith conditionalInc [0..] pans

                            conditionalInc i pan | i == sch  = pan
                                                 | otherwise = pan + 1
