module Day2

parseInt : String -> Int
parseInt = cast


namespace Part1
  getBounds : List Int -> (Int, Int)
  getBounds (x :: xs) = go xs (x, x)
    where
      go : List Int -> (Int, Int) -> (Int, Int)
      go [] acc = acc
      go (y :: xs) (ma, mi) = go xs (max ma y, min mi y)

  getPartialSum : (Int, Int) -> Int
  getPartialSum (ma, mi) = ma - mi

  checksum : List (List Int) -> Int
  checksum spreadsheet = foldr (\line, acc => acc + (getPartialSum . getBounds $ line)) 0 spreadsheet

  main : IO ()
  main = do
    Right contents <- readFile "input.txt"
      | Left err => putStrLn ("Erro! " ++ show err)
    let spreadsheet = (map . map) parseInt $ map words . lines $ contents
    let result = checksum spreadsheet
    -- putStrLn (show result)
    -- putStrLn (show (map (getPartialSum . getBounds) spreadsheet))
    pure ()

namespace Part2
  findNum : List Int -> Int
  findNum [] = 0
  findNum lst@(x :: xs) =
    let sorted = sort lst
    in go sorted
      where
        go : List Int -> Int
        go [] = 0
        go (y :: xs) =
          let divided = map (\z => (z, mod z y)) xs
              filtered = filter (\(z, mz) => mz == 0) divided
          in case filtered of
            [] => go xs
            ((res, _) :: _) => div res y

  main : IO ()
  main = do
    Right contents <- readFile "input.txt"
      | Left err => putStrLn ("Erro! " ++ show err)
    let spreadsheet = (map . map) parseInt $ map words . lines $ contents
    let results = map findNum spreadsheet
    print (sum results)
    pure ()
