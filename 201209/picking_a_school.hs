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
    [ School 2 ["A", "B", "C", "D"]
    , School 2 ["B", "D", "A", "C"]
    ]

prefs :: [Pref]
prefs =
    [ Pref "A" [0,1]
    , Pref "B" [0,1]
    , Pref "C" [1,0]
    , Pref "D" [0,1]
    ]

allocate :: [School] -> [Pref] -> [Offer]
allocate schools prefs = []
