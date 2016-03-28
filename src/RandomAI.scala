/**
 * Created by julien on 3/28/16.
 */
// RANK : 333 / 912
class RandomAI extends ClassicAI {
  override def eval(zone: Zone, game: Game, from: Zone) = scala.util.Random.nextFloat()
}
