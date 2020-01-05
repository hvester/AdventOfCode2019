#load "Day1.fsx"
#load "Day2.fsx"
#load "Day3.fsx"
#load "Day4.fsx"
#load "Day5.fsx"
#load "Day6.fsx"
#load "Day7.fsx"
#load "Day8.fsx"
#load "Day9.fsx"
#load "Day10.fsx"
#load "Day11.fsx"
#load "Day12.fsx"
#load "Day13.fsx"
#load "Day14.fsx"
#load "Day15.fsx"

#load "Day17.fsx"
#load "Day18.fsx"
#load "Day19.fsx"

#load "Day21.fsx"

#load "Day23.fsx"
#load "Day24.fsx"

open System

let check result expected =
    if result <> expected then
        failwithf "Result %A does not equal %A" result expected 

check Day1.result1 3329926
check Day1.result2 4992008

check Day2.result1 3716250L
check Day2.result2 6472L

check Day3.result1 293
check Day3.result2 27306

check Day4.result1 1890
check Day4.result2 1277

check Day5.result1 5577461L
check Day5.result2 7161591L

check Day6.result1 333679
check Day6.result2 370

check Day7.result1 65464L
check Day7.result2 1518124L

check Day8.result1 1548

[| " ##  #### #  # #  #  ##  "
   "#  # #    # #  #  # #  # "
   "#    ###  ##   #  # #  # "
   "#    #    # #  #  # #### "
   "#  # #    # #  #  # #  # "
   " ##  #### #  #  ##  #  # " |]
|> fun rows -> String.Join("\n", rows)
|> check Day8.result2

check Day9.result1 3454977209L
check Day9.result2 50120L

check Day10.result1 314
check Day10.result2 1513

check Day11.result1

[| "...##.####.###....##.###...##...##....##..."
   "....#....#.#..#....#.#..#.#..#.#..#....#..."
   "....#...#..#..#....#.#..#.#..#.#.......#..."
   "....#..#...###.....#.###..####.#.##....#..."
   ".#..#.#....#....#..#.#.#..#..#.#..#.#..#..."
   "..##..####.#.....##..#..#.#..#..###..##...." |]
|> fun rows -> String.Join("\n", rows)
|> check Day11.result2

check Day12.result1 7179
check Day12.result2 428576638953552L

check Day13.result1 247
check Day13.result2 12954

check Day14.result1 720484L
check Day14.result2 1993284L

check Day15.result1 266
check Day15.result2 274


check Day17.result1 14332
check Day17.result2 1034009L

check Day18.result1 4620

check Day19.result1 201
check Day19.result2 6610984

check Day21.result1 19362822
check Day21.result2 1143625214

check Day23.result1 19473L
check Day23.result2 12475L

check Day24.result1 18350099
check Day24.result2 2037