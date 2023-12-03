package day02_part2_mine

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part2(): Unit =
    println(s"La solution est : ${solvePart2(loadInput())}")

def loadInput(): String = 
    val inputPath = s"$currentDir/../../../../input/day02"
    loadFileSync(inputPath)

def solvePart2(input: String): Int =
    val it = input.linesIterator
    def checkPossibleGamesRecursive(it: Iterator[String], acc: Int): Int =
        if(it.hasNext)
            val lue = it.next()
            val redIt = "[0-9]+ red".r.findAllIn(lue)
            val greenIt = "[0-9]+ green".r.findAllIn(lue)
            val blueIt = "[0-9]+ blue".r.findAllIn(lue)
            val gameId = "Game [0-9]+".r.findFirstIn(lue).get.split(" ")(1).toInt
            var maxRed = redIt.map(_.split(" ")(0).toInt).max
            var maxGreen = greenIt.map(_.split(" ")(0).toInt).max
            var maxBlue = blueIt.map(_.split(" ")(0).toInt).max
            checkPossibleGamesRecursive(it, acc + (maxRed * maxGreen * maxBlue))
        else
            acc
    checkPossibleGamesRecursive(it, 0)