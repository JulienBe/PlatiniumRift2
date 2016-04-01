package test

import player.{AI, Game, Zone}

/**
 * Created by julien on 3/28/16.
 */
class GlobalSort132 extends AI {

  def eval(zone: Zone, game: Game, from: Zone): Float = { 0 }

  def evalGlobal(zone: Zone, game: Game) = {
    var value = util.Random.nextFloat()
    value += game.fartest - zone.distanceFromEnemy
    if (!zone.isMine(game)) {
      value += zone.resourceValue
      value += zone.platinum * 2
      value *= 2
    }
    value
  }

  override def act(game: Game) = {
    game.map.zones.foreach(zone => zone.globalValue = evalGlobal(zone, game) )
    command += game.myPodZones.foreach(chooseDestination(game, _))
    super.act(game)
  }

  def chooseDestination(game: Game, zone: Zone): String = {
    val destinations = zone.links.sortWith(_.globalValue > _.globalValue)
    destinations(0).globalValue /= 1 + (2.5f / game.turn)
    buildCommand(zone, destinations(0), 1)
  }
}

class GlobalSort116 extends GlobalSort132 {
  override def eval(zone: Zone, game: Game, from: Zone): Float = {
    var value = super.eval(zone, game, from)
    if (zone.isMine(game) && zone.isThreatened(game))
      value += zone.resourceValue
    value
  }
}

class GlobalSort179 extends  GlobalSort116 {
  override def eval(zone: Zone, game: Game, from: Zone): Float = {
    var value = super.eval(zone, game, from)
    zone.links.foreach( z => if(!z.isMine(game)) value *= 1.5f)
    value
  }
}