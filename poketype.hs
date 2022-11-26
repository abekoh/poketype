{-# OPTIONS -Wall -Werror #-}

import Data.List

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
  deriving (Eq, Show, Enum, Bounded)

allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound ..]

allPokeTypes :: [PokeType]
allPokeTypes = filter (/= None) allValues

data Result
  = Quadruple
  | Twice
  | Same
  | Half
  | Quater
  | Zero
  deriving (Show)

data Pokemon = Pokemon {type1 :: PokeType, type2 :: PokeType} deriving (Show)

class ShowChartIcon a where
  showChartIcon :: a -> String

instance ShowChartIcon Result where
  showChartIcon Twice = " ◎ "
  showChartIcon Half = " △ "
  showChartIcon Zero = " ✕ "
  showChartIcon _ = "   "

instance ShowChartIcon PokeType where
  showChartIcon Normal = "NOR"
  showChartIcon Fire = "FIR"
  showChartIcon Water = "WAT"
  showChartIcon Electric = "ELE"
  showChartIcon Grass = "GRA"
  showChartIcon Ice = "ICE"
  showChartIcon Fighting = "FIG"
  showChartIcon Poison = "POI"
  showChartIcon Ground = "GRO"
  showChartIcon Flying = "FLY"
  showChartIcon Psychic = "PSY"
  showChartIcon Bug = "BUG"
  showChartIcon Rock = "ROC"
  showChartIcon Ghost = "GHO"
  showChartIcon Dragon = "DRA"
  showChartIcon Dark = "DAR"
  showChartIcon Steel = "STE"
  showChartIcon Fairy = "FAI"
  showChartIcon _ = ""

class Capatibility a where
  useMove :: a -> a -> Result

instance Capatibility PokeType where
  useMove Normal Rock = Half
  useMove Normal Ghost = Zero
  useMove Normal Steel = Half
  useMove Fire Fire = Half
  useMove Fire Water = Half
  useMove Fire Grass = Twice
  useMove Fire Ice = Twice
  useMove Fire Bug = Twice
  useMove Fire Rock = Half
  useMove Fire Dragon = Half
  useMove Fire Steel = Twice
  useMove Water Fire = Twice
  useMove Water Water = Half
  useMove Water Grass = Half
  useMove Water Ground = Twice
  useMove Water Rock = Twice
  useMove Water Dragon = Half
  useMove Electric Water = Twice
  useMove Electric Electric = Half
  useMove Electric Grass = Half
  useMove Electric Ground = Zero
  useMove Electric Flying = Twice
  useMove Electric Dragon = Half
  useMove Grass Fire = Half
  useMove Grass Water = Twice
  useMove Grass Grass = Half
  useMove Grass Poison = Half
  useMove Grass Ground = Twice
  useMove Grass Flying = Half
  useMove Grass Bug = Half
  useMove Grass Rock = Twice
  useMove Grass Dragon = Half
  useMove Grass Steel = Half
  useMove Ice Fire = Half
  useMove Ice Water = Half
  useMove Ice Grass = Twice
  useMove Ice Ice = Half
  useMove Ice Ground = Twice
  useMove Ice Flying = Twice
  useMove Ice Dragon = Twice
  useMove Ice Steel = Half
  useMove Fighting Normal = Twice
  useMove Fighting Ice = Twice
  useMove Fighting Poison = Half
  useMove Fighting Flying = Half
  useMove Fighting Psychic = Half
  useMove Fighting Bug = Half
  useMove Fighting Rock = Twice
  useMove Fighting Ghost = Zero
  useMove Fighting Dark = Twice
  useMove Fighting Steel = Twice
  useMove Fighting Fairy = Half
  useMove Poison Grass = Twice
  useMove Poison Poison = Half
  useMove Poison Ground = Half
  useMove Poison Rock = Half
  useMove Poison Ghost = Half
  useMove Poison Steel = Zero
  useMove Poison Fairy = Twice
  useMove Ground Fire = Twice
  useMove Ground Electric = Twice
  useMove Ground Grass = Half
  useMove Ground Poison = Twice
  useMove Ground Flying = Zero
  useMove Ground Bug = Half
  useMove Ground Rock = Twice
  useMove Ground Steel = Twice
  useMove Flying Electric = Half
  useMove Flying Grass = Twice
  useMove Flying Fighting = Twice
  useMove Flying Bug = Twice
  useMove Flying Rock = Half
  useMove Flying Steel = Half
  useMove Psychic Fighting = Twice
  useMove Psychic Poison = Twice
  useMove Psychic Psychic = Half
  useMove Psychic Dark = Zero
  useMove Psychic Steel = Half
  useMove Bug Fire = Half
  useMove Bug Grass = Twice
  useMove Bug Fighting = Half
  useMove Bug Poison = Half
  useMove Bug Flying = Half
  useMove Bug Psychic = Twice
  useMove Bug Ghost = Half
  useMove Bug Dark = Twice
  useMove Bug Steel = Half
  useMove Bug Fairy = Half
  useMove Rock Fire = Twice
  useMove Rock Ice = Twice
  useMove Rock Fighting = Half
  useMove Rock Ground = Half
  useMove Rock Flying = Twice
  useMove Rock Bug = Twice
  useMove Rock Steel = Half
  useMove Ghost Normal = Zero
  useMove Ghost Psychic = Twice
  useMove Ghost Ghost = Twice
  useMove Ghost Dark = Half
  useMove Dragon Dragon = Twice
  useMove Dragon Steel = Half
  useMove Dragon Fairy = Zero
  useMove Dark Fighting = Half
  useMove Dark Psychic = Twice
  useMove Dark Ghost = Twice
  useMove Dark Dark = Half
  useMove Dark Fairy = Half
  useMove Steel Fire = Half
  useMove Steel Water = Half
  useMove Steel Electric = Half
  useMove Steel Ice = Twice
  useMove Steel Rock = Twice
  useMove Steel Steel = Half
  useMove Steel Fairy = Twice
  useMove Fairy Fire = Half
  useMove Fairy Fighting = Twice
  useMove Fairy Poison = Half
  useMove Fairy Dragon = Twice
  useMove Fairy Dark = Twice
  useMove Fairy Steel = Half
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

generateTypeChart :: String
generateTypeChart =
  let effectResults = map (\x -> (x, map (useMove x) allPokeTypes)) allPokeTypes
      header = "    " ++ unwords (map showChartIcon allPokeTypes) ++ "\n"
      content = intercalate "" (map (\x -> showChartIcon (fst x) ++ " " ++ unwords (map showChartIcon (snd x)) ++ "\n") effectResults)
   in header ++ content

main :: IO ()
main = do
  print $ useMove Normal Fire
  print $ useMove Grass Water
  print $ multiplyResult Twice Half
  print $ multiplyResult Same Zero
  print $ attack Fire (Pokemon Fire None)
  print $ attack Electric (Pokemon Flying Water)
  print $ attack Dragon (Pokemon Fairy Ghost)
  print $ attack Normal (Pokemon Fairy Ghost)
  print $ attack Poison (Pokemon Fairy Ghost)
  print $ attack Steel (Pokemon Fairy Ghost)
  print allPokeTypes
  putStrLn generateTypeChart

-- print generateTypeChart
