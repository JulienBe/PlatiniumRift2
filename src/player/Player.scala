package player

import scala.collection.mutable.ArrayBuffer

/**
 *  Created by julien on 3/12/16.
 *  Crappy limit on 1 class in cg ide :(
 */

object Player extends App {
  val game = init(initHelper.getValues)
  def init(values: InitValues) = { initHelper.init(values) }
  game.play(new Explorer)
}

trait AI {
  var command = ""
  def act(game: Game): Unit = printCommand(command)
  def printCommand(command: String) = {
    println(command)
    this.command = ""
  }
  def buildCommand(from: Zone, to: Zone, nbPod: Int): String = {
    from.myPod -= 1
    to.myPod += 1
    nbPod + " " + from.id + " " + to.id + " "
  }
  def eval(zone: Zone, game: Game, from: Zone): Float
}

class Explorer extends AI {
  override def act(game: Game): Unit = {
    for (zone <- game.myPodZones) {
      val candidates = zone.links.filter(
        z => z.links.exists(_.hasNeverBeenSeen())
      )
      if (candidates.length > 0) {
        command += buildCommand(zone, candidates(util.Random.nextInt(candidates.length)), 1)
      } else {
        zone.links.sortWith(_.myPod > _.myPod)
        command += buildCommand(zone, zone.links(util.Random.nextInt(zone.links.length)), 1)
      }
    }
    super.act(game)
  }

  override def eval(zone: Zone, game: Game, from: Zone): Float = ???
}

case class Game(me: Competitor, other: Competitor, map: Map, var myPlatinum: Int, myPodZones: ArrayBuffer[Zone], var fartest: Int = -1, var turn: Int = 1) {

  def play(ai: AI) = {
    while (true) {
      myPlatinum = readInt
      myPodZones.clear()
      map.zones.foreach(processZoneInfo(_))
      ai.act(this)
      turn += 1
      println("WAIT") // second line no longer used (see the protocol in the statement for details)
    }
  }

  def processZoneInfo(zone: Zone): Unit = {
    val Array(zoneId, ownerId, podId0, podId1, visible, platinum) = for (i <- readLine split " ") yield i.toInt
    findEnemyBase(zone, ownerId)
    updatezoneDatas(zone, ownerId, podId0, podId1, visible, platinum)
  }

  def updatezoneDatas(zone: Zone, ownerid: Int, podsp0: Int, podsp1: Int, visible: Int, platinum: Int): Unit = {
    zone.owner = ownerid
    zone.visible = visible
    if (me.id == 0) updatePods(zone, podsp0, podsp1)
    else            updatePods(zone, podsp1, podsp0)
    if (visible != 0)
      zone.updateSetPlatinum(platinum)
  }

  def computeDistances(zone: Zone, currentDist: Int): Unit = {
    if (zone.distanceFromEnemy != -1 && currentDist > zone.distanceFromEnemy)
      return
    // TODO : use spare time in other turns
    if (currentDist > 80)
      return
    if (currentDist > fartest)
      fartest = currentDist

    zone.distanceFromEnemy = currentDist
    zone.links.foreach(computeDistances(_, currentDist + 1))
  }

  def findEnemyBase(zone: Zone, ownerid: Int): Unit = {
    if (other.base == null && ownerid == other.id) {
      other.base = zone
      computeDistances(other.base, 0)
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

case class Zone(id: Int, var platinum: Int, var links: Array[Zone], var owner: Int, var myPod: Int, var otherPod: Int, var visible: Int, var distanceFromEnemy: Int = -1, var resourceValue: Float = 0, var globalValue: Float = 0) {

  def hasNeverBeenSeen(): Boolean = platinum == -1

  def isThreatened(game: Game): Boolean = links.exists(!_.isMine(game))
  def isMine(game: Game):       Boolean = owner == game.me.id

  def updateSetPlatinum(newPlatinum: Int) = {
    if (platinum == -1 || platinum < newPlatinum) {
      platinum = newPlatinum
      Console.err.println(id + "::" + platinum)
      if (platinum != 0)
        updateResourceValue(platinum * 2, Seq.empty[Int])
    }
  }
  def updateResourceValue(value: Float, toExclude: Seq[Int]): Unit = {
    if (toExclude contains id)    return
    if (value < .5f)              return
    if (toExclude.length > 10)    return
    resourceValue += value

    val nextVal = value / 2
    val nextExclude = toExclude :+ id
    links.foreach(_.updateResourceValue(nextVal, nextExclude))
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
    val map = Map(initZoneArray(values.zoneCount))
    val game = Game(Competitor(values.myId, 1), Competitor(1 - values.myId, 1), map, 0, ArrayBuffer[Zone]())

    for (i <- 0 until values.links.length) {
      // don't really care about effenciency in the init for now.
      map.zones(values.links(i)(0)).links :+= map.zones(values.links(i)(1))
      map.zones(values.links(i)(1)).links :+= map.zones(values.links(i)(0))
    }
    game
  }

  def initZoneArray(zoneCount: Int): Array[Zone] = {
    Array.tabulate[Zone](zoneCount){
        n => Zone((n + 1) - 1, -1, Array[Zone](), -1, 0, 0, 0)
      }
  }
}