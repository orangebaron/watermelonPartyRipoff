class PlayerLocMgr {
	private var _playerLoc = Point(y = -1)
	def playerLoc = _playerLoc
	private def playerLoc_=(newVal: Point) { _playerLoc = newVal }

	private var _playerFacing = TileDirection.Down
	def playerFacing = _playerFacing
	private def playerFacing_=(newVal: TileDirection) { _playerFacing = newVal }

	def nextLoc = playerLoc + playerFacing.asPoint

	def nextFacing(tileMap: TileMap) = for {
		nextTile <- tileMap.tiles get nextLoc
		nextFacing <- nextTile.takePlayer(playerFacing)
	} yield nextFacing

	def tryWalk(tileMap: TileMap, watermelonMap: WatermelonMap) = nextFacing(tileMap) match {
		case Some(dir) => {
			playerLoc = nextLoc
			playerFacing = dir
			watermelonMap.tryCollectWatermelon(playerLoc)
		}
		case _ => ()
	}

	def resetPlayer = {
		playerLoc = Point(y = -1)
		playerFacing = TileDirection.Down
	}

	override def toString =
		"player loc: " + playerLoc + "\n" +
		"player facing: " + playerFacing.asPoint + "\n"
}
