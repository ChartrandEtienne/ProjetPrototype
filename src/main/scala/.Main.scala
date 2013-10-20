package hello

import scala.xml.XML
import scala.xml.Elem

class Position(val x: Int, val y: Int) {
  override def toString = "Pos: " + y.toString + ", " + x.toString
}

class Perso(val nom: String, var pos: Position, val stat: Elem, var hp: Int) {
  override def toString = nom + ", " + pos.toString
  def deplacer(_pos: Position): Boolean = {
    pos = _pos;
    true
  }
  def id(_nom: String): Boolean = _nom == nom
  def prendreOrdre(ord: Ordre): Boolean = ord match {
    case Deplacer(_, _pos) => { pos = _pos; true }
    case _ => false
  }
}

class Case(var occupe: Char) {
  override def toString = occupe.toString
}

object Afficher {
  def afficherCarteJoueurs(map: Map, persos: List[Perso]) = {

    val inter0 = map.array.map(_.map(_.toString))

    for (perso <- persos) { inter0(perso.pos.x)(perso.pos.y) = perso.nom(0).toString} 

    // aerer tout ca
    val inter1 = inter0.map(_.mkString("\t"))

    // numerotation cote
    val inter2 = inter1.foldRight((inter1.length - 1, List[String]()))({(elem, seed) => (seed._1 - 1, seed._1.toString + "|\t" + elem.toString :: seed._2)})._2

    // numerotation dessus
    val inter3 = ".\t" + 
    (for (x <- 0 to inter2.length + 1) yield {x.toString + "\t" }).mkString + 
    "\n\t" + 
    (for (x <- 0 to inter2.length + 1) yield { "_\t"}).mkString + "\n\n" + inter2.mkString("\n\n") +
    "\n\n"

    println(inter3)
  }
}

class Map(val array: Array[Array[Char]]) {
  override def toString = "priere d'utiliser l'objet Afficher pour cet usage. Merci"
}

class Jeu(val map: Map, var persos: List[Perso]) {

//  override def toString = persos.mkString(", ") + "\n\n" + map.toStringPersos(persos) + "\n\n"
  override def toString = persos.mkString(", ") + "\n\n" + map.toString + "\n\n"

  // retourne un True si la commmande s'est rendue
  // laid, mais bon
  def passerOrdre(ord: Ordre): Boolean = ord match {
    case Deplacer(_nom, _pos) => persos.find(_.id(_nom)).map(_.deplacer(_pos)) match { case Some(thing) => thing; case None => false }
    case _ => { println("erreur: ???"); false }
  }
  
  def afficherSimple = {
    Afficher.afficherCarteJoueurs(map, persos)
  }

}

import java.io.File

object Jeu {
  // parse un fichier bitmap en carte plus joueurs
  def getJoueurs = {
    val dossierJoueurs = new File("joueurs/") 
    val contenuDossier = dossierJoueurs.listFiles
    contenuDossier.map(_.getPath).filter // lol
    val isXml = """[a-zA-Z0-9]+\.xml$""".r
    val pathJoueurs = contenuDossier.map(_.getPath).filter({nomFichier => isXml findFirstIn nomFichier match { case Some(_) => true case None => false }})
    val fichiersJoueursValides = pathJoueurs.map(new File(_))
    
  }
  def apply(configFile: String) = {
    var mapCars = scala.io.Source.fromFile(configFile).getLines.mkString("\n").split("\n").map(_.toArray)
    def intern(elem: Char, seed: (Int, List[(Char, Int)])): (Int, List[(Char, Int)]) = (elem, seed) match {
      case ('0', (count, list)) => (count + 1, list)
      case ('1', (count, list)) => (count + 1, list)
      case (char: Char, (count, list)) => (count + 1, (char, count) :: list)
          
    }
    val persosBuffer = mapCars.flatten.foldRight((0, List[(Char, Int)]()))(intern)._2
//    val vraiPersos = persosBuffer.map({x => new Perso(x._1.toString, new Position(x._2 / mapCars(1).length, x._2 % mapCars(1).length))})
//    val mapBuffer2 = mapCars.map(_.map({x => if (x == '1') '1' else '0'}))
    val mapBuffer2 = mapCars.map(_.map({x => if (x == '1') '1' else '0'}).reverse).reverse
    val mapBuffer = new Map(mapBuffer2)
    new Jeu(mapBuffer, vraiPersos)
  }
}

abstract class Ordre

case class Erreur(msg: String) extends Ordre

case class Deplacer(val nom: String, val pos: Position) extends Ordre

case class Attaquer(val nom: String, val pos: Position) extends Ordre

import scala.util.matching.Regex
import java.util.Scanner

class ControleJoueur(val nomJoueur: String) {
//  val scanner = new Scanner(System.in)
  val commandePattern = """([a-z]+),(\d+),(\d+)""".r
  def getAction: Ordre = commandePattern.findFirstIn(readLine("veuillez entrer votre commande:\n")) match {
    case Some(commandePattern("dep", _x, _y)) => Deplacer(nomJoueur, new Position(_x.toInt, _y.toInt))
    case Some(commandePattern("atk", _x, _y)) => Attaquer(nomJoueur, new Position(_x.toInt, _y.toInt))
    case _ => Erreur("commande non recunnue (ou supportee)")
  }
    
}

 object Main extends App {
  val jeu = Jeu("testconfig")
  jeu.afficherSimple
  val joueur = new ControleJoueur("p")
  jeu.passerOrdre(joueur.getAction)
  jeu.afficherSimple
}
