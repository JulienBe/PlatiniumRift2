package test

import player.{AI, Game, Zone}

/**
 * Created by julien on 3/28/16.
 */
// RANK : 192 / 912
class ZergAI extends AI {
  override def eval(zone: Zone, game: Game, from: Zone) = -zone.distanceFromEnemy
}
