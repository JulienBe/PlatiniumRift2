/**
 * Created by julien on 3/28/16.
 */
// RANK : 192 / 912
class ZergAI extends ClassicAI {
  override def eval(zone: Zone, game: Game, from: Zone) = -zone.distanceFromEnemy
}
