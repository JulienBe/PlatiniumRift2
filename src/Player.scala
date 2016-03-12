/**
 * Created by julien on 3/12/16.
 *
 * Zones, maps and everything are here because CodinGame only support one class
 */

case class Game(me: Competitor, other: Competitor, map: Map)

case class Competitor(id: Int, var ownedZones: Int)

case class Zone(id: Int, var platinum: Int, var links: Array[Zone])

case class Map(zones: Array[Zone])

object Player extends App {

  val game = init

  def init: Game = {
    val Array(playercount, myid, zonecount, linkcount) = for (i <- readLine split " ") yield i.toInt
    val map = Map(
      Array.tabulate[Zone](zonecount){
      n => Zone(n + 1, 0, Array[Zone]())
    }
    )
    val game = Game(Competitor(myid, 1), Competitor(1 - myid, 1), map)

    // Just here to get the readLine going
    for (i <- 0 until zonecount) {
      // zoneid: this zone's ID (between 0 and zoneCount-1), platinumsource: Because of the fog, will always be 0
      val Array(zoneid, platinumsource) = for (i <- readLine split " ") yield i.toInt
    }

    for (i <- 0 until linkcount) {
      val Array(zone1, zone2) = for (i <- readLine split " ") yield i.toInt
      // don't really care about effenciency in the init for now.
      map.zones(zone1).links :+= map.zones(zone2)
    }
    game
  }

  // game loop
  while (true) {
    val myplatinum = readInt // your available Platinum
    for (i <- 0 until game.map.zones.length) {
      // zid: this zone's ID
      // ownerid: the player who owns this zone (-1 otherwise)
      // podsp0: player 0's PODs on this zone
      // podsp1: player 1's PODs on this zone
      // visible: 1 if one of your units can see this tile, else 0
      // platinum: the amount of Platinum this zone can provide (0 if hidden by fog)
      val Array(zid, ownerid, podsp0, podsp1, visible, platinum) = for (i <- readLine split " ") yield i.toInt
    }

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

    println("WAIT") // first line for movement commands, second line no longer used (see the protocol in the statement for details)
    println("WAIT")
  }
}