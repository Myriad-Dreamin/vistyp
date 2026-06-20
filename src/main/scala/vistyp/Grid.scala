package vistyp

case class GridSettings(enabled: Boolean = true, size: Double = Grid.DefaultSize)

object Grid:
  val DefaultSize: Double = 10

  def active(settings: GridSettings): Boolean =
    settings.enabled && isFinite(settings.size) && settings.size > 0

  def snap(value: Double, settings: GridSettings): Double =
    if active(settings) then clean(math.round(value / settings.size) * settings.size)
    else clean(value)

  def snapPoint(
      point: (Double, Double),
      settings: GridSettings,
  ): (Double, Double) =
    snap(point._1, settings) -> snap(point._2, settings)

  def snapPointWithOffset(
      point: (Double, Double),
      offset: (Double, Double),
      settings: GridSettings,
  ): (Double, Double) =
    val reference = (point._1 + offset._1) -> (point._2 + offset._2)
    val snappedReference = snapPoint(reference, settings)
    clean(snappedReference._1 - offset._1) ->
      clean(snappedReference._2 - offset._2)

  def clean(value: Double): Double =
    val rounded = math.round(value * 1000) / 1000d
    if math.abs(rounded) < 0.0005 then 0 else rounded

  private def isFinite(value: Double): Boolean =
    !value.isNaN && !value.isInfinity
end Grid
