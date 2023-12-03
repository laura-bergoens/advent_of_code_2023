package day03_part1_mine

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import Array._
import scala.collection.mutable.ListBuffer

@main def part1(): Unit =
    println(s"La solution est : ${solvePart1(loadInput())}")

def loadInput(): String = 
    val inputPath = s"$currentDir/../../../../input/debug"
    loadFileSync(inputPath)

def solvePart1(input: String): Int =
    val itForLengthY = input.linesIterator
    val itForLengthX = input.linesIterator
    val itForLoop = input.linesIterator
    val yLength = itForLengthY.length
    val xLength = itForLengthX.next().size
    val grid = ofDim[String](yLength, xLength)
    // build grid
    for (i <- 0 to yLength - 1)
        grid(i) = itForLoop.next().split("")
    // scan grid
    var currentNumber = ""
    var yStart = 0
    var xStart = 0
    var xEnd = 0
    var sum = 0
    def possibleCoordinates(y: Int, xStart: Int, xEnd: Int, maxY: Int, maxX: Int): List[String] =
        var possibleCoordinates = new ListBuffer[String]()
        var farTopLeft = s"${if(y-1 > -1) y-1 else "NON"};${if(xStart-1 > -1) xStart-1 else "NON"}"
        if(!farTopLeft.contains("NON")) possibleCoordinates += farTopLeft
        var farMidLeft = s"$y;${if(xStart-1 > -1) xStart-1 else "NON"}"
        if(!farMidLeft.contains("NON")) possibleCoordinates += farMidLeft
        var farBottomLeft = s"${if(y+1 < maxY) y+1 else "NON"};${if(xStart-1 > -1) xStart-1 else "NON"}"
        if(!farBottomLeft.contains("NON")) possibleCoordinates += farBottomLeft
        var farTopRight = s"${if(y-1 > -1) y-1 else "NON"};${if(xEnd+1 < maxX) xEnd+1 else "NON"}"
        if(!farTopRight.contains("NON")) possibleCoordinates += farTopRight
        var forMidRight = s"$y;${if(xEnd+1 < maxX) xEnd+1 else "NON"}"
        if(!forMidRight.contains("NON")) possibleCoordinates += forMidRight
        var farBottomRight = s"${if(y+1 < maxY) y+1 else "NON"};${if(xEnd+1 < maxX) xEnd+1 else "NON"}"
        if(!farBottomRight.contains("NON")) possibleCoordinates += farBottomRight
        for
            x <- xStart to xEnd
        do
            var top = s"${if(y-1 > -1) y-1 else "NON"};$x"
            var bottom = s"${if(y+1 < maxY) y+1 else "NON"};$x"
            if(!top.contains("NON")) possibleCoordinates += top
            if(!bottom.contains("NON")) possibleCoordinates += bottom
        possibleCoordinates.toList
    def checkCoordinate(coord: String, grid: Array[Array[String]]): Boolean =
        val y = coord.split(";")(0).toInt
        val x = coord.split(";")(1).toInt
        grid(y)(x) != "." && !grid(y)(x).matches("[0-9]")
    for (i <- 0 to yLength - 1)
        if(currentNumber.length > 0)
            xEnd = xLength - 1
            val coords = possibleCoordinates(yStart, xStart, xEnd, yLength, xLength)
            val isPartNumber = coords.exists(_ != ".")
            println(currentNumber)
            //coords.map(println)
            println(isPartNumber)
            if(isPartNumber) sum += currentNumber.toInt
            //sum += currentNumber.toInt
            currentNumber = ""
        for (j <- 0 to xLength - 1)
            if(grid(i)(j).matches("[0-9]"))
                if(currentNumber.length == 0)
                    yStart = i
                    xStart = j
                currentNumber += grid(i)(j)
            else
                if(currentNumber.length > 0)
                    xEnd = j-1
                    // check number
                    val coords = possibleCoordinates(yStart, xStart, xEnd, yLength, xLength)
                    val isPartNumber = coords.exists(checkCoordinate(_, grid))
                    println(currentNumber)
                    //coords.map(println)
                    println(isPartNumber)
                    if(isPartNumber) sum += currentNumber.toInt
                    //sum += currentNumber.toInt
                    currentNumber = ""
    sum