
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
}
case class Tile(a: TileDirection, b: TileDirection) {
	def takePlayer(playerFrom: TileDirection): Option[TileDirection] = // returns where the next direction to go to would be if player can go here
		if (playerFrom == -a) Some(b)
		else if (playerFrom == -b) Some(a)
		else None
}

class Board(size: Point, private var _watermelons: Map[Point, Tile], tileGenerator: Board => Tile) { //TODO: val needed?
	def watermelons = _watermelons
	private var _tiles: Map[Point, Tile] = Map()
	def tiles = _tiles
	private var _nextTile = tileGenerator(this)
	def nextTile = _nextTile
	private var _playerLoc = Point(y = -1)
	def playerLoc = _playerLoc
	private var _playerFacing = TileDirection.Down
	def playerFacing = _playerFacing
	private var _watermelonsCollected: Int = 0
	def watermelonsCollected = _watermelonsCollected
	def nextLoc = playerLoc + playerFacing.asPoint
	def nextFacing = for {
		nextTile <- tiles get nextLoc
		nextFacing <- nextTile.takePlayer(playerFacing)
	} yield nextFacing
	private def tryCollectWatermelon = if (watermelons contains playerLoc) {
		_watermelonsCollected += 1
		_watermelons = _watermelons - playerLoc
	}
	def tryWalk = nextFacing match {
		case Some(dir) => {
			_playerLoc = nextLoc
			_playerFacing = dir
			tryCollectWatermelon
		}
		case _ => ()
	}
	private def resetPlayer = {
		_playerLoc = Point(y = -1)
		_playerFacing = TileDirection.Down
	}
	private def bombLocation(loc: Point) = {
		if (loc == playerLoc) resetPlayer
		_tiles = _tiles - loc
	}
	def bombAroundLocation(loc: Point) =
		for (x <- -1 to 1; y <- -1 to 1) bombLocation(Point(x, y))
	def placeTile(loc: Point) = if (!(tiles contains loc) && loc.inRange(size)) {
		_tiles = _tiles + (loc -> nextTile)
		_nextTile = tileGenerator(this)
	}
}
