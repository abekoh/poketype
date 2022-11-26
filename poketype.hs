{-# OPTIONS -Wall -Werror #-}

data PokeType
  = None
  | Normal
  | Fire
  | Water
  | Electric
  | Grass
  | Ice
  | Fighting
  | Poison
  | Ground
  | Flying
  | Psychic
  | Bug
  | Rock
  | Ghost
  | Dragon
  | Dark
  | Steel
  | Fairy
  deriving (Show)

data Result
  = Quadruple
  | Twice
  | Same
  | Half
  | Quater
  | Zero
  deriving (Show)

data Pokemon = Pokemon {type1 :: PokeType, type2 :: PokeType} deriving (Show)

class PokeTypeChart a where
  useMove :: a -> a -> Result

instance PokeTypeChart PokeType where
  useMove Fire Fire = Half
  useMove Fire Water = Half
  useMove Fire Grass = Twice
  useMove Water Fire = Twice
  useMove Water Water = Half
  useMove Water Grass = Half
  useMove Grass Fire = Half
  useMove Grass Water = Twice
  useMove Grass Grass = Half
  useMove _ _ = Same

multiplyResult :: Result -> Result -> Result
multiplyResult Zero _ = Zero
multiplyResult Quadruple _ = Twice -- maybe not used
multiplyResult Quater _ = Quater -- maybe not used
multiplyResult Twice Twice = Quadruple
multiplyResult Twice Same = Twice
multiplyResult Twice Half = Same
multiplyResult Same Same = Same
multiplyResult Same Half = Half
multiplyResult Half Half = Quater
multiplyResult r1 r2 = multiplyResult r2 r1

attack :: PokeType -> Pokemon -> Result
attack pokeType (Pokemon t1 t2) = multiplyResult (useMove pokeType t1) (useMove pokeType t2)

main :: IO ()
main = do
  print $ useMove Normal Fire
  print $ useMove Grass Water
  print $ multiplyResult Twice Half
  print $ multiplyResult Same Zero
  print $ attack Fire (Pokemon Fire None)
