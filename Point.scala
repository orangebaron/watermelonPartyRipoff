case class Point(x: Int = 0, y: Int = 0) {
	def +(a: Point) = Point(x + a.x, y + a.y)
	def inRange(max: Point) =
		x >= 0 &&
		y >= 0 &&
		x < max.x &&
		y < max.y
	override def toString = "(" + x + "," + y + ")"
}
