#load "Interpreter.fsx"
#load "Day1.fsx"
#load "Day2.fsx"
#load "Day3.fsx"
#load "Day4.fsx"
#load "Day5.fsx"
#load "Day6.fsx"

let check result expected =
    if result <> expected then
        failwithf "Result %A does not equal %A" result expected 

check Day1.result1 3329926
check Day1.result2 4992008

check Day2.result1 3716250
check Day2.result2 6472

check Day3.result1 293
check Day3.result2 27306

check Day4.result1 1890
check Day4.result2 1277

check Day5.result1 5577461
check Day5.result2 7161591

check Day6.result1 333679
check Day6.result2 370