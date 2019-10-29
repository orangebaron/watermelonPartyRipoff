import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.util.Random

case class Point(x: Int = 0, y: Int = 0) {
	def +(a: Point) = Point(x + a.x, y + a.y)
	def inRange(max: Point) =
		x > 0 &&
		y > 0 &&
		x < max.x &&
		y < max.y
}

case class TileDirection(horizontal: Boolean, positive: Boolean) {
	def unary_- = TileDirection(horizontal, !positive)
	def asPoint = {
		val num = if (positive) 1 else -1
		Point(
			if (horizontal) num else 0,
			if (horizontal) 0 else num
		)
	}
}

object TileDirection {
	val Up = TileDirection(false, false)
	val Down = TileDirection(false, true)
	val Left = TileDirection(true, false)
	val Right = TileDirection(true, true)
	def randomDirection(rand: Random) =
		TileDirection(rand.nextBoolean, rand.nextBoolean)
	def randomDirectionExcept(rand: Random, except: TileDirection): TileDirection = {
		val thisTry = randomDirection(rand)
		if (thisTry == except)
			randomDirectionExcept(rand, except)
		else thisTry
	}
}
case class Tile(a: TileDirection, b: TileDirection) {
	def takePlayer(playerFrom: TileDirection): Option[TileDirection] = // returns where the next direction to go to would be if player can go here
		if (playerFrom == -a) Some(b)
		else if (playerFrom == -b) Some(a)
		else None
}

object Tile {
	def randomTile(rand: Random) = {
		val firstDir = TileDirection.randomDirection(rand)
		Tile(firstDir, TileDirection.randomDirectionExcept(rand, firstDir))
	}
}

class Board(rand: Random, size: Point, watermelons: Map[Point, Tile]) { //TODO: val needed?
	val tiles: Map[Point, Tile] = new HashMap[Point, Tile]
	var nextTile = Tile.randomTile(rand)
	var playerLoc = Point(y = -1)
	var playerFacing = TileDirection.Down
	var watermelonsCollected: Int = 0
	def nextLoc = playerLoc + playerFacing.asPoint
	def nextFacing = for {
		nextTile <- tiles get nextLoc
		nextFacing <- nextTile.takePlayer(playerFacing)
	} yield nextFacing
	def tryCollectWatermelon = if (watermelons contains playerLoc) {
		watermelonsCollected += 1
		watermelons remove playerLoc
	}
	def tryWalk = nextFacing match {
		case Some(dir) => {
			playerLoc = nextLoc
			playerFacing = dir
			tryCollectWatermelon
		}
		case _ => ()
	}
	def resetPlayer = {
		playerLoc = Point(y = -1)
		playerFacing = TileDirection.Down
	}
	def bombLocation(loc: Point) = {
		if (loc == playerLoc) resetPlayer
		tiles remove loc
	}
	def bombAroundLocation(loc: Point) =
		for (x <- -1 to 1; y <- -1 to 1) bombLocation(Point(x, y))
	def placeTile(loc: Point) = if (!(tiles contains loc) && loc.inRange(size)) {
		tiles put (loc, nextTile)
		nextTile = Tile.randomTile(rand)
	}
}
