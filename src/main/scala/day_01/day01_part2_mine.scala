package day01_part2_mine

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part2(): Unit =
    println(s"La solution est : ${solvePart2(loadInput())}")

def loadInput(): String = 
    val inputPath = s"$currentDir/../../../../input/day01"
    loadFileSync(inputPath)

def solvePart2(input: String): Int =
    val it = input.linesIterator
    val digitsAsString = Map(
    "one" -> "1",
    "two" -> "2",
    "three" -> "3",
    "four" -> "4",
    "five" -> "5",
    "six" -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine" -> "9",
    ).withDefaultValue("NULL")
    val possibleStrings = digitsAsString.keySet 
    def sum(it: Iterator[String], acc: Int): Int =
        if(it.hasNext)
            var lue = it.next()
            var lowestFirstIdx = 100000
            var lowestDigit = ""
            var highestLastIdx = -1
            var highestDigit = ""
            for((str, digit) <- digitsAsString)
                val curFirstIdx = lue.indexOf(str)
                if(curFirstIdx > -1 && curFirstIdx < lowestFirstIdx)
                    lowestFirstIdx = curFirstIdx
                    lowestDigit = digit
            if(lowestDigit != "")
                lue = s"${lue.slice(0, lowestFirstIdx)}$lowestDigit${lue.slice(lowestFirstIdx, lue.length)}"
            for((str, digit) <- digitsAsString)
                val curLastIdx = lue.lastIndexOf(str)
                if(curLastIdx > -1 && curLastIdx > highestLastIdx)
                    highestLastIdx = curLastIdx
                    highestDigit = digit
            if(highestDigit != "")
                lue = s"${lue.slice(0, highestLastIdx)}$highestDigit${lue.slice(highestLastIdx, lue.length)}"
            
            val numbersOnly = lue.replaceAll("[a-zA-Z]", "")
            sum(it, acc + s"${numbersOnly.head}${numbersOnly.last}".toInt)
        else
            acc
    sum(it, 0)