import System.Exit

countFrame :: Int -> [Int] -> Int
countFrame _ [] = 0
countFrame frameNumber pins =
  (frameScore pins) + (countFrame (frameNumber + 1) (nextFramePins frameNumber pins))

frameScore :: [Int] -> Int
frameScore pins
  | isStrike pins = pins!!0 + pins!!1 + (if length pins > 2 then pins!!2 else 0)
  | isSpare pins  = pins!!0 + pins!!1 + pins!!2
  | otherwise     = pins!!0 + pins!!1

nextFramePins :: Int -> [Int] -> [Int]
nextFramePins 10 _ = []
nextFramePins frameNumber pins
  | isStrike pins = tail pins
  | otherwise     = drop 2 pins

isStrike pins =
  head pins == 10

isSpare pins =
  pins!!0 + pins!!1 == 10

zeros = [
  0, 0, 0, 0,
  0, 0, 0, 0,
  0, 0, 0, 0,
  0, 0, 0, 0,
  0, 0, 0, 0]

ones = [
  1, 1, 1, 1,
  1, 1, 1, 1,
  1, 1, 1, 1,
  1, 1, 1, 1,
  1, 1, 1, 1]

spare = [
  5, 5, 3, 0,
  0, 0, 0, 0,
  0, 0, 0, 0,
  0, 0, 0, 0,
  0, 0, 0, 0]

strike = [
  10,   3, 4,
  0, 0, 0, 0,
  0, 0, 0, 0,
  0, 0, 0, 0,
  0, 0, 0, 0]

perfect = [
  10,   10,
  10,   10,
  10,   10,
  10,   10,
  10,   10,
  10,   10]

total pins =
  countFrame 1 pins

main
  | total(zeros) == 0
    && total(ones) == 20
    && total(spare) == 16
    && total(strike) == 24
    && total(perfect) == 300 = exitSuccess
  | otherwise                = exitFailure
