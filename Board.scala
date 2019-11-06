class Board(size: Point, startWatermelons: Set[Point], tileGenerator: Board => Tile) {
	private val playerLocMgr = new PlayerLocMgr
	private val watermelonMap = new WatermelonMap(startWatermelons)
	private val tileMap = new TileMap(size, tileGenerator, tileGenerator(this))

	def tiles = tileMap.tiles
	def nextTile = tileMap.nextTile
	def bombAroundLocation(loc: Point) = {
		tileMap.bombAroundLocation(loc)
		for (x <- -1 to 1; y <- -1 to 1)
			if (playerLoc == Point(x, y))
				playerLocMgr.resetPlayer
	}
	def placeTile(loc: Point) = tileMap.placeTile(loc, this)
	def playerLoc = playerLocMgr.playerLoc
	def playerFacing = playerLocMgr.playerFacing
	def nextLoc = playerLocMgr.nextLoc
	def nextFacing = playerLocMgr.nextFacing(tileMap)
	def tryWalk = playerLocMgr.tryWalk(tileMap, watermelonMap)
	def watermelons = watermelonMap.watermelons
	def watermelonsColelcted = watermelonMap.watermelonsCollected
	override def toString = "" + tileMap + playerLocMgr + watermelonMap
}
