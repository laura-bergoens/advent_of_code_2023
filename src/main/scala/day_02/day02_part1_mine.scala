package day02_part1_mine

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1(): Unit =
    println(s"La solution est : ${solvePart1(loadInput())}")

def loadInput(): String = 
    val inputPath = s"$currentDir/../../../../input/day02"
    loadFileSync(inputPath)

def solvePart1(input: String): Int =
    val it = input.linesIterator
    def checkPossibleGamesRecursive(it: Iterator[String], acc: Int): Int =
        if(it.hasNext)
            val lue = it.next()
            val redIt = "[0-9]+ red".r.findAllIn(lue)
            val greenIt = "[0-9]+ green".r.findAllIn(lue)
            val blueIt = "[0-9]+ blue".r.findAllIn(lue)
            val gameId = "Game [0-9]+".r.findFirstIn(lue).get.split(" ")(1).toInt
            var isRedOk = if(redIt.filter(_.split(" ")(0).toInt > 12).size > 0) false else true
            var isGreenOk = if(greenIt.filter(_.split(" ")(0).toInt > 13).size > 0) false else true
            var isBlueOk = if(blueIt.filter(_.split(" ")(0).toInt > 14).size > 0) false else true
            checkPossibleGamesRecursive(it, if(isRedOk && isGreenOk && isBlueOk) acc + gameId else acc)
        else
            acc
    checkPossibleGamesRecursive(it, 0)