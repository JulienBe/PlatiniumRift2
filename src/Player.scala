import scala.collection.mutable.ArrayBuffer

/* Created by julien on 3/12/16. */

object Player extends App {
  val game = init(initHelper.getValues)
  def init(values: InitValues) = { initHelper.init(values) }
  game.play(RandomAI)
}

trait AI {  def act(game: Game) }
object RandomAI extends AI {
  override def act(game: Game) = {
    var command = ""
    for (zone <- game.myPodZones)
      command += "1 " + zone.id + " " + zone.links(randomLink(zone)).id + " "
    println(command)
  }
  def randomLink(zone: Zone) = {    scala.util.Random.nextInt(zone.links.length)  }
}

case class Game(me: Competitor, other: Competitor, map: Map, var myPlatinum: Int, myPodZones: ArrayBuffer[Zone]) {
  def play(ai: AI) = {
    while (true) {
      myPlatinum = readInt
      myPodZones.clear()
      for (zone <- map.zones) {
        // visible: 1 if one of your units can see this tile, else 0
        // platinum: the amount of Platinum this zone can provide (0 if hidden by fog)
        val Array(zoneId, ownerid, podsp0, podsp1, visible, platinum) = for (i <- readLine split " ") yield i.toInt
        zone.owner = ownerid
        if (me.id == 0)  updatePods(zone, podsp0, podsp1)
        else             updatePods(zone, podsp1, podsp0)
        if (visible == 1) zone.platinum = platinum
        zone.visible = visible
      }
      ai.act(this)
      println("WAIT") // second line no longer used (see the protocol in the statement for details)
    }
  }

  def updatePods(zone: Zone, myPods: Int, otherPods: Int) = {
    for (i <- 0 until myPods)
      myPodZones += zone
    zone.myPod = myPods
    zone.otherPod = otherPods
  }
}

case class Competitor(id: Int, var ownedZones: Int)
case class Zone(id: Int, var platinum: Int, var links: Array[Zone], var owner: Int, var myPod: Int, var otherPod: Int, var visible: Int)
case class Map(zones: Array[Zone])
case class InitValues(myId: Int, zoneCount: Int, links: Array[Array[Int]])
case class Pod(var position: Zone, var canMove: Boolean)

object initHelper {
  def getValues: InitValues = {
    val Array(playercount, myid, zonecount, linkcount) = for (i <- readLine split " ") yield i.toInt
    // Just here to get the readLine going
    for (i <- 0 until zonecount) {
      val Array(zoneid, platinumsource) = for (i <- readLine split " ") yield i.toInt
    }

    val links = Array.ofDim[Int](linkcount, 2)
    for (i <- 0 until linkcount) {
      links(i) = for (i <- readLine split " ") yield i.toInt
    }
    new InitValues(myid, zonecount, links)
  }

  def init(values: InitValues): Game = {
    val map = Map(zoneArray(values.zoneCount))
    val game = Game(Competitor(values.myId, 1), Competitor(1 - values.myId, 1), map, 0, ArrayBuffer[Zone]())

    for (i <- 0 until values.links.length) {
      // don't really care about effenciency in the init for now.
      map.zones(values.links(i)(0)).links :+= map.zones(values.links(i)(1))
      map.zones(values.links(i)(1)).links :+= map.zones(values.links(i)(0))
    }
    game
  }

  def zoneArray(zoneCount: Int): Array[Zone] = {
    Array.tabulate[Zone](zoneCount){
        n => Zone((n + 1) - 1, 0, Array[Zone](), -1, 0, 0, 0)
      }
  }
}