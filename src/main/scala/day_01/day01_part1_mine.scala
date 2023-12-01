package day01_mine

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1(): Unit =
    println(s"La solution est : ${solvePart1(loadInput())}")

def loadInput(): String = 
    val inputPath = s"$currentDir/../../../../input/day01"
    loadFileSync(inputPath)

def solvePart1(input: String): Int =
    val it = input.linesIterator
    def sum(it: Iterator[String], acc: Int): Int =
        if(it.hasNext)
            val numbersOnly = it.next().replaceAll("[a-zA-Z]", "")
            sum(it, acc + s"${numbersOnly.head}${numbersOnly.last}".toInt)
        else
            acc
    sum(it, 0)