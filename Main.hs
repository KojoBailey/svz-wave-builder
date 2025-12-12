import GHC.Natural
import Data.List (intersperse)

data Enemy = 
  CrawlingTorsoTutorial |
  LightZombieMeleeTutorial |
  LightZombieSword |
  LightZombieAxe
  deriving (Show, Read)

data WaveCommand = Spawn Enemy | Delay Natural | DeathDelay deriving (Show)

type Wave = [WaveCommand]

data Focus = Start | Filler | Main deriving (Show, Read, Eq)

data WaveFactory = WaveFactory {
  waveFactoryEnemies :: [Enemy],
  waveFactoryCount :: Natural,
  waveFactoryFocus :: Focus }
  deriving (Show)

main :: IO ()
main = do
  input <- getLine;
  let waveFactory = stringToWaveFactory input;
  print waveFactory
  let wave = generateWave waveFactory
  putStrLn $ "\n" ++ formatWave wave 1

stringToWaveFactory :: String -> [WaveFactory]
stringToWaveFactory str = map (\(x, y, z) -> WaveFactory {
  waveFactoryEnemies = x,
  waveFactoryCount = y,
  waveFactoryFocus = z }
  ) parsed
  where
    parsed = read str :: [([Enemy], Natural, Focus)]

generateWave :: [WaveFactory] -> Wave
generateWave factory = foldr1 (++) $
  map (\x -> addDelays (generateWaveGroup 0 (waveFactoryEnemies x) (waveFactoryCount x) (waveFactoryFocus x)) (waveFactoryFocus x)) factory
  where
    generateWaveGroup :: Natural -> [Enemy] -> Natural -> Focus -> Wave
    generateWaveGroup i enemies n focus
      | i >= n    = []
      | otherwise = Spawn (enemies !! (fromIntegral i `mod` length enemies))
        : generateWaveGroup (i+1) enemies n focus

    addDelays :: Wave -> Focus -> Wave
    addDelays wave focus = case focus of
      Start -> intersperse (Delay 6) wave ++ [Delay 4]
      Filler -> intersperse (Delay 2) wave ++ [Delay 2]
      Main   -> intersperse (Delay 3) wave ++ [DeathDelay]

formatWave :: Wave -> Natural -> String
formatWave wave n = "[Wave_" ++ formatNat n ++ "]\n" ++ formatWaveRec 1 wave
  where
    formatWaveRec :: Int -> Wave -> String
    formatWaveRec _ [] = "\n"
    formatWaveRec i (x:xs) = tab ++ show i ++ ":\n" ++ tab ++ tab ++
      case x of
        Spawn enemy    -> "enemy = Enemies." ++ enemyToString enemy
        Delay duration -> let d = show duration in "command = " ++ d ++ "," ++ d
        DeathDelay     -> "command = HOLD"
      ++ "\n" ++ formatWaveRec (i+1) xs
    
    tab = "  "

    formatNat :: Natural -> String
    formatNat n
      | n >= 100  = show n
      | n >= 10   = "0" ++ show n
      | otherwise = "00" ++ show n

enemyToString :: Enemy -> String
enemyToString enemy = case enemy of
  CrawlingTorsoTutorial     -> "CrawlingTorso_Tutorial"
  LightZombieMeleeTutorial  -> "l_zombie_melee_tutorial"
  LightZombieSword          -> "l_zombie_sword"
  LightZombieAxe            -> "l_zombie_ax"