{-# LANGUAGE ForeignFunctionInterface #-}
import Control.Concurrent
import System.Console.ANSI
import System.IO
import System.Random
import qualified Data.Map.Strict as M
import Data.Char
import Foreign.C.Types
{-
getch :: IO Char
getch = do
  -- получаем старый режим буферизации для потока stdin
  oldBufferMode <- hGetBuffering stdin
  -- отключаем буферизацию для stdin
  hSetBuffering stdin NoBuffering
  c <- getChar                   -- читаем символ из stdin
  -- восстанавливаем режим буферизации для stdin
  hSetBuffering stdin oldBufferMode
  return c
-}
getHiddenChar = fmap (chr.fromEnum) getChar
--foreign import ccall "curses.h getch" -- unsafe "curses.h getch"
--    c_getch :: IO CInt

(cols, rows, lboard, uboard, numStrScore) = (10, 20, 25, 4, 14)
scoreChart = map (^2) [0..]
 
type Point = (Int, Int)        -- (row, column)
type Field = [(Int, [Point])]  -- (type index, points)
type Item  = (Int, Int, Point) -- (type index, orientation, central point)
data GameState =
     GameState { field :: Field, item :: Item, run :: Bool, score :: Int, timerDelay :: Int }
 
makeGS f i r s t = GameState { field = f, item = i, run = r, score = s, timerDelay = t }
 
 
timer :: MVar GameState -> IO ()
timer m = do
    inGS @ GameState { field = f, item = i, run = r, score = s, timerDelay = t } <- takeMVar m
 
    newGameState <- if not r then return inGS else do
 
        let i' = move (-1) 0 i
        if testItem f i' then do
            drawPoints ' ' $ itemTypePoints i
            drawPoints '*' $ itemTypePoints i'
            return $ makeGS f i' r s t
 
        else do
            let f' = itemTypePoints i : f
                (fnew, delLines) = compressField f'
                snew = (s+) . sum . map (scoreChart !!) $ delLines
                tnew = t -- надо бы дописать увеличение скорости
 
            if null delLines then return () else do
                setCursorPosition numStrScore 0
                putStr $ "Score: " ++ show snew
                setCursorPosition (numStrScore + 2) 0
                putStr $ "Speed: " ++ show (tnew `div` 10^5)
                mapM_ (drawPoints ' ') f'
                mapM_ (drawPoints '*') fnew
 
            inew <- newItem
            if testItem fnew inew then do
                drawPoints '*' $ itemTypePoints inew
                return $ makeGS fnew inew r snew tnew
 
            else do
                setCursorPosition (numStrScore + 4) 0 >> putStr "Game over!"
                return $ makeGS fnew inew False snew tnew
 
    putMVar m $! newGameState
    threadDelay t
    timer m
 
 
waitKeyPress :: MVar GameState -> IO ()
waitKeyPress m = do
    a <- getHiddenChar --getChar
    inGS @ GameState { field = f, item = i, run = r, score = s, timerDelay = t } <- takeMVar m
 
    let execCommand transform = do
            let i' = transform i
            if not $ testItem f i' then return inGS else do
                drawPoints ' ' $ itemTypePoints i
                drawPoints '*' $ itemTypePoints i'
                return $ makeGS f i' r s t
 
    newGameState <- case a of
 
        'n' -> do                          -- new game
            drawBackground
            inew <- newItem
            drawPoints '*' $ itemTypePoints inew
            return $ makeGS [] inew True 0 (10^6)
 
        'p' -> do                         -- pause / go
            setCursorPosition (numStrScore + 4) 0
            putStr $ if r then "Pause..." else "        "
            return $ makeGS f i (not r) s t
 
        'a' -> execCommand $ move 0 (-1)  -- move left
        'd' -> execCommand $ move 0 1     -- move right
        'w' -> execCommand $ move 1 0     -- move up :)
        's' -> execCommand $ move (-1) 0  -- move down :)
        'q' -> execCommand $ rotate 1     -- rotate left
        'e' -> execCommand $ rotate (-1)  -- rotate right
        ' ' -> execCommand $ fallDown f   -- fall down
        _   -> return inGS                -- nothing
 
    putMVar m $! newGameState
    waitKeyPress m
 
 
itO = [(0,-1),(0,0),(-1,-1),(-1,0)]; itI = [(0,-2),(0,-1),(0,0),(0,1)]
itS = [(-1,-1),(-1,0),(0,0),(0,1)];  itZ = [(0,-1),(0,0),(-1,0),(-1,1)]
itL = [(0,-1),(0,0),(0,1),(-1,-1)];  itJ = [(0,-1),(0,0),(0,1),(-1,1)]
itT = [(0,-1),(0,0),(0,1),(-1,0)]
 
items :: [[[Point]]]
items = [replicate 4 itO, or2 itI, or2 itS, or2 itZ, ori 4 itL, ori 4 itJ, ori 4 itT] where
    ori k = take k . iterate (map $ \(r,c) -> (c,-r)) -- for right rotate \(r,c) -> (-c,r)
    or2 i = ori 2 i ++ ori 2 i
 
itemTypePoints :: Item -> (Int, [Point])
itemTypePoints (n, o, (cpr, cpc)) = (,) n . map (\(r,c) -> (r+cpr, c+cpc)) $ items !! n !! o
 
move :: Int -> Int -> Item -> Item
move dr dc (n, o, (cpr, cpc)) = (n, o, (cpr+dr, cpc+dc))
 
rotate :: Int -> Item -> Item
rotate d (n, o, cp) = (n, (o+d) `mod` 4, cp)
 
testItem :: Field -> Item -> Bool
testItem f = all (\p@(r,c) -> r>=0 && c>=0 && c<cols && all (not . elem p) (map snd f))
    . snd . itemTypePoints
 
fallDown :: Field -> Item -> Item
fallDown f i | testItem f i' = fallDown f i' | otherwise = i where i' = move (-1) 0 i
 
compressField :: Field -> (Field, [Int])
compressField f | null fl  = (f, []) | otherwise = (f', [length fl]) where
    fl = M.keys . M.filter (>=cols) . M.fromListWith (+) . map (\(r,_) -> (r,1)) . concat . map snd $ f
    f' = filter (not . null . snd) . map (fmap $ map (\(r,c) -> (r - length (filter (<r) fl), c))
         . filter (not . (`elem` fl) . fst) ) $ f
 
newItem :: IO Item
newItem = getStdRandom (randomR (0, 6)) >>= \n -> return (n, 0, (rows - 1, cols `div` 2))
 
drawPoint :: Char -> Point -> IO ()
drawPoint sym (r,c) =
    setCursorPosition r0 c0 >> putStr s >> setCursorPosition (r0+1) c0 >> putStr s where
        r0 = uboard + (rows - 1 - r)*2
        c0 = lboard + c*2
        s  = [sym, sym]
 
drawPoints :: Char -> (Int, [Point]) -> IO ()
drawPoints sym (n, ps) = do
    if sym == ' ' then setSGR [Reset]
    else do
        setSGR [SetColor Background Dull White]
        setSGR [SetColor Foreground Vivid (toEnum $ n + 1)]
    mapM_ (drawPoint sym) ps
    setSGR [Reset]
 
drawBackground :: IO ()
drawBackground = do
    clearScreen
 
    setCursorPosition uboard 0
    let leftSpase = replicate (lboard - 1) ' '
    putStr $ unlines $ replicate (rows*2) (leftSpase ++ '|':replicate (cols*2) ' ' ++ "|")
    putStr $ leftSpase ++ '+':replicate (cols*2) '-' ++ "+"
 
    setCursorPosition uboard 0
    putStrLn "Controls:"
    putStrLn ""
    putStrLn "move left - 'a'"
    putStrLn "    right - 'd'"
    putStrLn "rot  left - 'q'"
    putStrLn "    right - 'e'"
    putStrLn "fall down - ' '"
    putStrLn "new  game - 'n'"
 
    setCursorPosition numStrScore 0 >> putStr "Score: 0"
    setCursorPosition (numStrScore + 2) 0 >> putStr "Speed: 0"

main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
 
    setTitle "Tetris v 1.0"
    hideCursor
    setCursorPosition uboard 0
    putStrLn "Hello, Windows user! Resize the heigth of terminal window to maximum"
    putStrLn "and press any key..."
    _ <- getHiddenChar --getChar
    drawBackground
 
    m <- newEmptyMVar
    putMVar m $! makeGS [] (0, 0, (0,0)) False 0 (10^6)
    forkIO (timer m)
    waitKeyPress m
