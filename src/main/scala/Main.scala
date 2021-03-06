package hello

import scala.xml.XML
import scala.xml.Elem
import java.io.File

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

object LireConfigs {
  def getJoueursXML = {
    val dossierJoueurs = new File("joueurs/") 
    val contenuDossier = dossierJoueurs.listFiles
//    contenuDossier.map(_.getPath).filter // lol
    val isXml = """[a-zA-Z0-9]+\.xml$""".r
    val pathJoueurs = contenuDossier.map(_.getPath).filter({nomFichier => isXml findFirstIn nomFichier match { case Some(_) => true case None => false }})
    pathJoueurs.map(XML.loadFile(_))
  }
}

/*
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
*/

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
  println("hello, world") 
  val configs = LireConfigs.getJoueursXML
  println(configs.mkString("\n\n======\n\n"))
}
