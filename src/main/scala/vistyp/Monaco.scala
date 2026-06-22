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
  def getModel(): MonacoModel = js.native
  def trigger(source: String, handlerId: String, payload: js.Any): Unit =
    js.native
  def focus(): Unit = js.native
  def layout(): Unit = js.native
  def executeEdits(source: String, edits: js.Array[js.Object]): Boolean =
    js.native
  def pushUndoStop(): Boolean = js.native
  def dispose(): Unit = js.native
}

@js.native
trait MonacoModel extends js.Object {
  def getFullModelRange(): js.Object = js.native
  def onDidChangeContent(
      callback: js.Function1[MonacoModelContentChange, Unit],
  ): MonacoModel = js.native
}

@js.native
trait MonacoModelContentChange extends js.Object {
  def isFlush: Boolean = js.native
}

val monacoLoadVar: Var[Option[Monaco]] = Var(None)
val monacoLoadSignal = monacoLoadVar.signal
