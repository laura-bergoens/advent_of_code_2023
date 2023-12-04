package day04_part2_mine

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.collection.mutable.ListBuffer

@main def part2(): Unit =
    println(s"La solution est : ${solvePart2(loadInput())}")

def loadInput(): String = 
    val inputPath = s"$currentDir/../../../../input/day04"
    loadFileSync(inputPath)

def solvePart2(input: String): Double =
    val it = input.linesIterator
    val copiesToAdd = scala.collection.mutable.HashMap.empty[Int,Int]
    copiesToAdd += (1 -> 0)
    var res = 0
    while it.hasNext do
        val lue = it.next()
        val cardNumberRegex = "Card *([0-9]*)".r
        var cardNumber = 0
        for cardNumberMatch <- cardNumberRegex.findAllMatchIn(lue) do
            cardNumber = cardNumberMatch.group(1).toInt
        if(!(copiesToAdd contains cardNumber)) copiesToAdd += (cardNumber -> 0)
        val loops = 1 + copiesToAdd(cardNumber)
        val Array(winningNumbers, myNumbers) = lue.split(": ")(1).split("\\|").map(_.trim().split(" ").filter(_.length > 0)).map(_.toList)
        val matchingNumbersCnt = winningNumbers.intersect(myNumbers).length
        if(matchingNumbersCnt > 0)
            for i <- 1 to matchingNumbersCnt do
                val nextCardNumber = cardNumber + i
                if(!(copiesToAdd contains nextCardNumber)) copiesToAdd += (nextCardNumber -> 0)
                copiesToAdd(nextCardNumber) += loops
        res += loops
    res