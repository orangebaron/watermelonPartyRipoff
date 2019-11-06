import scala.util.Random
case class Tile(a: TileDirection, b: TileDirection) {
	def takePlayer(playerFrom: TileDirection): Option[TileDirection] = // returns where the next direction to go to would be if player can go here
		if (playerFrom == -a) Some(b)
		else if (playerFrom == -b) Some(a)
		else None
	override def toString =
		if (a == TileDirection.Up && b == TileDirection.Down || a == TileDirection.Down && b == TileDirection.Up) "|"
		else if (a == TileDirection.Left && b == TileDirection.Right || a == TileDirection.Right
&& b == TileDirection.Left) "-"
		else if (a == TileDirection.Up && b == TileDirection.Right || a == TileDirection.Right && b == TileDirection.Up) "⌞"
		else if (a == TileDirection.Right && b == TileDirection.Down || a == TileDirection.Down && b == TileDirection.Right) "⌜"
		else if (a == TileDirection.Down && b == TileDirection.Left || a == TileDirection.Left && b == TileDirection.Down) "⌝"
		else if (a == TileDirection.Left && b == TileDirection.Up || a == TileDirection.Up && b == TileDirection.Left) "⌟"
		else "X"
}

object Tile {
	def randomTile(board: Board): Tile = {
		val tile = Tile(TileDirection(Random.nextBoolean, Random.nextBoolean), TileDirection(Random.nextBoolean, Random.nextBoolean))
		if (tile.toString != "X") tile else randomTile(board)
	}
}
