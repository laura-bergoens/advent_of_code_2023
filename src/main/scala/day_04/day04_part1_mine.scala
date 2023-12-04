package day04_part1_mine

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1(): Unit =
    println(s"La solution est : ${solvePart1(loadInput())}")

def loadInput(): String = 
    val inputPath = s"$currentDir/../../../../input/day04"
    loadFileSync(inputPath)

def solvePart1(input: String): Double =
    val it = input.linesIterator
    var res = 0.0
    while it.hasNext do
        val Array(winningNumbers, myNumbers) = it.next().split(": ")(1).split("\\|").map(_.trim().split(" ").filter(_.length > 0)).map(_.toList)
        val matchingNumbers = winningNumbers.intersect(myNumbers)
        if(matchingNumbers.length > 0) res += scala.math.pow(2, matchingNumbers.length-1)
    res