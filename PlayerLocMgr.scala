class PlayerLocMgr(maxWalkPoint: Int = 0) {
	private var _playerLoc = Point(y = -1)
	def playerLoc = _playerLoc
	private def playerLoc_=(newVal: Point) { _playerLoc = newVal }

	private var _playerFacing = TileDirection.Down
	def playerFacing = _playerFacing
	private def playerFacing_=(newVal: TileDirection) { _playerFacing = newVal }

	private var _playerWalkPoint: Int = maxWalkPoint
	def playerWalkPoint = _playerWalkPoint
	def playerWalkPoint_=(newVal: Int) = { _playerWalkPoint = newVal }

	def nextLoc = playerLoc + playerFacing.asPoint

	def nextFacing(tileMap: TileMap) = for {
		nextTile <- tileMap.tiles get nextLoc
		nextFacing <- nextTile.takePlayer(playerFacing)
	} yield nextFacing

	private def tryWalkAdvance(tileMap: TileMap, watermelonMap: WatermelonMap) = nextFacing(tileMap) match {
		case Some(dir) => {
			playerLoc = nextLoc
			playerFacing = dir
			watermelonMap.tryCollectWatermelon(playerLoc)
			playerWalkPoint = 0
		}
		case _ => {
			playerWalkPoint = maxWalkPoint
		}
	}

	def walkTick(tileMap: TileMap, watermelonMap: WatermelonMap) = {
		playerWalkPoint += 1
		if (playerWalkPoint > maxWalkPoint)
			tryWalkAdvance(tileMap: TileMap, watermelonMap: WatermelonMap)
	}

	def resetPlayer = {
		playerLoc = Point(y = -1)
		playerFacing = TileDirection.Down
		playerWalkPoint = maxWalkPoint
	}

	override def toString =
		"player loc: " + playerLoc + "\n" +
		"player facing: " + playerFacing.asPoint + "\n" +
		"player walk point: " + playerWalkPoint + " / " + maxWalkPoint + "\n"
}
