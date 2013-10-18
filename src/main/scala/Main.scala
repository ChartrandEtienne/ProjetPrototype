package hello

class Perso(val nom: String, val x: Int, val y: Int) {
  override def toString = nom + ", " + x.toString + ", " + y.toString
}

class Map(val map: Array[Array[Char]], val persos: List[Perso]) {
  override def toString = persos.mkString(", ") + "\n" + map.map(_.mkString("\t")).mkString("\n\n")
}

// object Main extends App {
object Test {
  // tableau de tableau de carracteres, map, quoi
  var mapCars = scala.io.Source.fromFile("testconfig").getLines.mkString("\n").split("\n").map(_.toArray)
  println("hello, world")
  def intern(elem: Char, seed: (Int, List[(Char, Int)])): (Int, List[(Char, Int)]) = (elem, seed) match {
    case ('0', (count, list)) => (count + 1, list)
    case ('1', (count, list)) => (count + 1, list)
    case (char: Char, (count, list)) => (count + 1, (char, count) :: list)
    
  }
  val persos = mapCars.flatten.foldRight((0, List[(Char, Int)]()))(intern)._2
  val vraiPersos = persos.map({x => new Perso(x._1.toString, x._2 / mapCars(1).length, x._2 % mapCars(1).length)})
  val map = new Map(mapCars, vraiPersos)
}


// def fileToCars(file: String) = scala.io.Source.fromFile(file).getLines.mkString("\n").split("\n").map(_.toArray)


// def ok(arg: Array[Array[Char]]) = arg.flatten.foldRight((0, List[(Char, Int)]()))(intern)

// lol.flatten.foldRight((0, List[Char]()))({(elem: Char, seed: (Int, List[Char])) => (seed._1 + 1, elem :: seed._2)})

