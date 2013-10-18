package hello

class Position(x: Int, y: Int) {
  override def toString = "Pos: " + x.toString + ", " + y.toString
}

class Perso(val nom: String, var pos: Position) {
  override def toString = nom + ", " + pos.toString
  def deplacer(_pos: Position) {
    pos = _pos
  }
}

class Map(val array: Array[Array[Char]]) {
//, val persos: List[Perso]
  override def toString = array.array(_.mkString("\t")).mkString("\n\n")
}

class Jeu(val map: Map, var persos: List[Perso]) {
  // constructeur aux a partir de la localisation du fichier config. 
  // laid
  def this(configFile: String) = {
    var mapCars = scala.io.Source.fromFile(configFile).getLines.mkString("\n").split("\n").map(_.toArray)
    def intern(elem: Char, seed: (Int, List[(Char, Int)])): (Int, List[(Char, Int)]) = (elem, seed) match {
      case ('0', (count, list)) => (count + 1, list)
      case ('1', (count, list)) => (count + 1, list)
      case (char: Char, (count, list)) => (count + 1, (char, count) :: list)
          
    }
    val persosBuffer = mapCars.flatten.foldRight((0, List[(Char, Int)]()))(intern)._2
    val vraiPersos = persosBuffer.map({x => new Perso(x._1.toString, x._2 / mapCars(1).length, x._2 % mapCars(1).length)})
    val mapBuffer = new Map(mapCars)
    this(mapBuffer, vraiPersos)
  }

  override def toString = persos.mkString(", ") + "\n" + map.toString


}

abstract class Ordre

case class Deplacer(nom: String, pos: Position) extends Ordre

case class Attaquer(nom: String, pos: Position) extends Ordre

// object Main extends App {
object Test {
  /*
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
  */
  val jeu = new Jeu("testconfig")
}
