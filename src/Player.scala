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
class RandomAI extends ClassicAI {  override def eval(zone: Zone, game: Game, from: Zone) = {scala.util.Random.nextFloat()}}
// RANK : 192 / 912
class ZergAI extends ClassicAI {    override def eval(zone: Zone, game: Game, from: Zone) = {-zone.distanceFromEnemy}}
// RANK :  89 / 913
class ClassicAI extends AI {

  def evalOnResources(zone: Zone, game: Game) = {
    var value = 0
    if (!zone.isMine(game))
      value += zone.platinum * 4
    for (neigh <- zone.links) {
      if (!zone.isMine(game))
        value += neigh.platinum * 2
    }
    value
  }

  def evalOnLocation(zone: Zone, game: Game) = {
    var value = game.fartest - zone.distanceFromEnemy
    for (neigh <- zone.links)
      if (neigh.hasNeverBeenSeen())
        value += 10
    value
  }

  def evalOnPod(zone: Zone, game: Game, from: Zone) = {
    var value = 1f
    for (neigh <- zone.links)
      if (neigh.id != from.id) {
        if (neigh.isMine(game)) value -= 2
        else                    value += 2
      }
    value /= zone.links.length
    if (zone.isMine(game))    value -= 4
    else                      value += 2
    value -= zone.myPod * 4
    value
  }

  def hasCandidates(zone: Zone, game: Game) = {
    !zone.isMine(game) || zone.links.find(_.isMine(game)).isEmpty
  }

  def eval(zone: Zone, game: Game, from: Zone): Float = {
    var value = util.Random.nextFloat
    value += evalOnResources(zone, game)
    value += evalOnLocation(zone, game)
    value += evalOnPod(zone, game, from)
    value
  }

  override def act(game: Game) = {
    var command = ""
    for (zone <- game.myPodZones) {
      val destinations = zone.links.sortWith(eval(_, game, zone) > eval(_, game, zone))
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
    if (visible != 0)
      zone.updateSetPlatinum(platinum)
  }

  def computeDistances(zone: Zone, currentDist: Int): Unit = {
    if (zone.distanceFromEnemy != -1 && currentDist > zone.distanceFromEnemy)
      return
    // use spare time in other turns
    if (currentDist > 80)
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
case class Zone(id: Int, var platinum: Int, var links: Array[Zone], var owner: Int, var myPod: Int, var otherPod: Int, var visible: Int, var distanceFromEnemy: Int = -1, var resourceValue: Float = 0) {
  def hasNeverBeenSeen(): Boolean = platinum == -1
  def isMine(game: Game) = owner == game.me.id
  def updateSetPlatinum(newPlatinum: Int) = {
    if (platinum == -1 || platinum < newPlatinum) {
      platinum = newPlatinum
      if (platinum != 0)
        updateResourceValue(platinum, Seq.empty[Int])
    }
  }
  def updateResourceValue(value: Float, toExclude: Seq[Int]): Unit = {
    if (toExclude contains id)    return
    if (value < .5f)              return
    if (toExclude.length > 10)    return
    resourceValue += value

    val nextVal = value / 2
    val nextExclude = toExclude :+ id
    for (neigh <- links)
      neigh.updateResourceValue(nextVal, nextExclude)
  }
}

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
        n => Zone((n + 1) - 1, -1, Array[Zone](), -1, 0, 0, 0)
      }
  }
}