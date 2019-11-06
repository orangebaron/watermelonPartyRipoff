class TileMap(size: Point, tileGenerator: Board => Tile, private var _nextTile: Tile) {
	def nextTile = _nextTile
	private def nextTile_=(newVal: Tile) { _nextTile = newVal }

	private var _tiles: Map[Point, Tile] = Map()
	def tiles = _tiles
	private def tiles_=(newVal: Map[Point, Tile]) { _tiles = newVal }

	def bombAroundLocation(loc: Point) =
		for (x <- -1 to 1; y <- -1 to 1) {
			tiles = tiles - loc
		}

	def placeTile(loc: Point, board: Board) =
		if (!(tiles contains loc) && loc.inRange(size)) {
			tiles = tiles + (loc -> nextTile)
			nextTile = tileGenerator(board)
		}

	override def toString = {
		var returnVal = ""
		for (y <- 0 until size.y) {
			for (x <- 0 until size.x) {
				returnVal += (tiles getOrElse (Point(x, y),  Tile(TileDirection.Up, TileDirection.Up)))
			}
			returnVal += "\n"
		}
		returnVal += "next tile: " + nextTile + "\n"
		returnVal
	}
}
