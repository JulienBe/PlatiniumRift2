import scala.collection.mutable.ArrayBuffer

/* Created by julien on 3/12/16. */

object Player extends App {
  val game = init(initHelper.getValues)
  def init(values: InitValues) = { initHelper.init(values) }
  game.play(new ClassicAI)
}

trait AI {
  def act(game: Game)
  def buildCommand(from: Zone, to: Zone, nbPod: Int) = {
    from.myPod -= 1
    to.myPod += 1
    nbPod + " " + from.id + " " + to.id + " "
  }
}
// RANK : 333 / 912
class RandomAI extends ClassicAI {  override def eval(zone: Zone, game: Game) = {scala.util.Random.nextFloat()}}
// RANK : 192 / 912
class ZergAI extends ClassicAI {    override def eval(zone: Zone, game: Game) = {-zone.distanceFromEnemy}}
// RANK :  89 / 913
class ClassicAI extends AI {

  def eval(zone: Zone, game: Game) = {
    var value = 1f
    value += zone.otherPod
    value += util.Random.nextFloat()
    if (zone.owner != game.me.id) {
      value += zone.platinum
      value += 3
    }
    for (neigh <- zone.links)
      evalNeigh(neigh, game, value)
    value /= 1 + zone.myPod

    if (zone.myPod > 3) {
      value -= zone.myPod * zone.myPod
    }
    value += (game.fartest - zone.distanceFromEnemy) * 0.5f
    value
  }

  def evalNeigh(neigh: Zone, game: Game, value: Float) = {
    var rep = value
    if (neigh.owner != game.me.id) {
      rep += 2
      rep += neigh.platinum / 3
    } else
      rep -= 2
    if (neigh.myPod >= 3)
      rep -= neigh.myPod * neigh.myPod
    rep -= neigh.myPod
    rep
  }

  override def act(game: Game) = {
    var command = ""
    for (zone <- game.myPodZones) {
      val destinations = zone.links.sortWith(eval(_, game) > eval(_, game))
      command += buildCommand(zone, destinations(0), 1)
    }
    println(command)
  }
}

case class Game(me: Competitor, other: Competitor, map: Map, var myPlatinum: Int, myPodZones: ArrayBuffer[Zone], var fartest: Int = -1) {

  def play(ai: AI) = {
    while (true) {
      myPlatinum = readInt
      myPodZones.clear()
      for (zone <- map.zones) {
        // visible: 1 if one of your units can see this tile, else 0
        // platinum: the amount of Platinum this zone can provide (0 if hidden by fog)
        val Array(zoneId, ownerid, podsp0, podsp1, visible, platinum) = for (i <- readLine split " ") yield i.toInt
        findEnemyBase(zone, ownerid)
        updateDatas(zone, ownerid, podsp0, podsp1, visible, platinum)
      }
      ai.act(this)
      println("WAIT") // second line no longer used (see the protocol in the statement for details)
    }
  }

  def updateDatas(zone: Zone, ownerid: Int, podsp0: Int, podsp1: Int, visible: Int, platinum: Int): Unit = {
    zone.owner = ownerid
    zone.visible = visible
    if (me.id == 0) updatePods(zone, podsp0, podsp1)
    else updatePods(zone, podsp1, podsp0)
    if (platinum != 0) zone.platinum = platinum
  }

  def computeDistances(zone: Zone, currentDist: Int): Unit = {
    if (zone.distanceFromEnemy != -1 && currentDist > zone.distanceFromEnemy)
      return
    zone.distanceFromEnemy = currentDist
    if (currentDist > fartest)
      fartest = currentDist
    for (neigh <- zone.links)
      computeDistances(neigh, currentDist + 1)
  }

  def findEnemyBase(zone: Zone, ownerid: Int): Unit = {
    if (other.base == null && ownerid == other.id) {
      other.base = zone
      computeDistances(zone, 0)
    }
  }

  def updatePods(zone: Zone, myPods: Int, otherPods: Int) = {
    for (i <- 0 until myPods)
      myPodZones += zone
    zone.myPod = myPods
    zone.otherPod = otherPods
  }
}

case class Competitor(id: Int, var ownedZones: Int, var base: Zone = null)
case class Zone(id: Int, var platinum: Int, var links: Array[Zone], var owner: Int, var myPod: Int, var otherPod: Int, var visible: Int, var distanceFromEnemy: Int = -1)
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