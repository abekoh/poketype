{-# OPTIONS -Wall -Werror #-}

data PokeType = None | Normal | Fire | Water | Grass deriving (Show)

data Result = SuperSuperEffective | SuperEffective | Effective | NotVeryEffective | LittleEffective | NoEffect deriving (Show)

data Pokemon = Pokemon {type1 :: PokeType, type2 :: PokeType} deriving (Show)

class PokeTypeChart a where
  useMove :: a -> a -> Result

instance PokeTypeChart PokeType where
  useMove Fire Fire = NotVeryEffective
  useMove Fire Water = NotVeryEffective
  useMove Fire Grass = SuperEffective
  useMove Water Fire = SuperEffective
  useMove Water Water = NotVeryEffective
  useMove Water Grass = NotVeryEffective
  useMove Grass Fire = NotVeryEffective
  useMove Grass Water = SuperEffective
  useMove Grass Grass = NotVeryEffective
  useMove _ _ = Effective

multiplyResult :: Result -> Result -> Result
multiplyResult NoEffect _ = NoEffect
multiplyResult SuperSuperEffective _ = SuperSuperEffective -- maybe not used
multiplyResult LittleEffective _ = LittleEffective -- maybe not used
multiplyResult SuperEffective SuperEffective = SuperSuperEffective
multiplyResult SuperEffective Effective = SuperEffective
multiplyResult SuperEffective NotVeryEffective = Effective
multiplyResult Effective Effective = Effective
multiplyResult Effective NotVeryEffective = NotVeryEffective
multiplyResult NotVeryEffective NotVeryEffective = LittleEffective
multiplyResult r1 r2 = multiplyResult r2 r1

attack :: PokeType -> Pokemon -> Result
attack pokeType (Pokemon t1 t2) = multiplyResult (useMove pokeType t1) (useMove pokeType t2)

main :: IO ()
main = do
  print $ useMove Normal Fire
  print $ useMove Grass Water
  print $ multiplyResult SuperEffective NotVeryEffective
  print $ multiplyResult Effective NoEffect
  print $ attack Fire (Pokemon Fire None)
