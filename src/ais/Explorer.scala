package ais

import player.{Game, Zone, AI}

/**
 * Created by julien on 3/28/16.
 */
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
