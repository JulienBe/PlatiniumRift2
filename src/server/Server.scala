package server

import java.io.File

import player._
import test.ZergAI

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Created by julien on 3/12/16.
 *
 * The game is played on a continent that is shaped using hexagonal zones.
 * Links between zones are provided at the start of a game as a graph.
 *
 * When the game starts, all zones are neutral.
 * Platinum-beds are located on a given number of zones.
 * They produce 1 to 6 bars of Platinum per game round.
 * Taking ownership of zones allow you to win more Platinum and get more PODs.
 * You get ownership of a neutral zone by placing or moving a POD on it.
 * Your goal is to conquer the enemy HQ using PODs.
 *
 * Due to an intense fog, units can only see the Platinum and units located on tiles adjacent to or under themselves.
 * The general layout of the map will however be given: the fog will only affect Platinum and enemy PODs.
 *
 * The HQ of both players is always visible, which will let you detect the enemy base on the very first turn.
 */

class Server {

  val maps = listOfMaps()

  def run() = {
    val mapDir = maps(util.Random.nextInt(maps.length))
    val map = createMap(mapDir)
    val ids = getIds(mapDir)
    val player1 = new SimulatedPlayer(Game(Competitor(0, 1), Competitor(1, 1), map, 0, ArrayBuffer[Zone]()), new Explorer(), ids(0))
    val player2 = new SimulatedPlayer(Game(Competitor(1, 1), Competitor(0, 1), map, 0, ArrayBuffer[Zone]()), new Explorer(), ids(1))
  }

  def getIds(dir: File): Array[Int] = {
    val lines = Source.fromFile(new File(dir.getAbsolutePath + "/ids")).getLines()
    val line = lines.next().split(":")
    Array(line(0).toInt, line(1).toInt)
  }

  def createMap(dir: File): Map = {
    val zones = getZones(dir)
    populateLinks(dir, zones)
    Map(zones)
  }

  def populateLinks(dir: File, zones: Array[Zone]) = {
    val links = Source.fromFile(new File(dir.getAbsolutePath + "/links")).getLines()
    for (line <- links) {
      val link = line.split(":")
      zones(link(0).toInt).links :+= zones(link(1).toInt)
    }
  }

  def getZones(dir: File): Array[Zone] = {
    val zones = Source.fromFile(new File(dir.getAbsolutePath + "/zones")).getLines()
    initHelper.initZoneArray(zones.length - 1)
  }

  def listOfMaps(): List[File] = {
    val mapDir = new File("resources/")
    mapDir.listFiles().toList
  }

}
