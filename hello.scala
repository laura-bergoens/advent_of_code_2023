import scala.io.StdIn.readLine

@main def hello() = 
    println("Quel est ton nom ?")
    val name = readLine()
    println("Coucou " + name + " !!")