package vistyp

import scala.scalajs.js
import scala.scalajs.js.annotation._

import com.raquo.laminar.api.L.{*, given}

@js.native
trait Monaco extends js.Object {
  def editor: MonacoEditor = js.native
}

@js.native
trait MonacoEditor extends js.Object {
  def create(element: js.Object, options: js.Object): MonacoEditor = js.native
  def getValue(): String = js.native
  def setValue(value: String): Unit = js.native
//   def getModel(): MonacoModel = js.native
}

val monacoLoadVar: Var[Option[Monaco]] = Var(None)
val monacoLoadSignal = monacoLoadVar.signal
