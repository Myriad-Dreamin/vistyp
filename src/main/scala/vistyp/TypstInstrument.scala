package vistyp.instrument

import org.scalajs.dom
import org.scalajs.dom.{Element, SVGElement, HTMLCollection}
import scala.collection.mutable.{ArrayBuffer, Map => MutableMap}

object TypstSvgInstrument:

end TypstSvgInstrument

def processSvg(svg: SVGElement, tagMapping: Map[String, String]): SVGElement = {
  InstrumentWorker(tagMapping).work(svg).asInstanceOf[SVGElement]
}

private class InstrumentWorker(tagMapping: Map[String, String]) {

  def work(svg: Element): Element = {
    workChildren(svg, svg.children)
  }

  def workChildren(
      parent: Element,
      children: HTMLCollection[Element],
  ): Element = {

    var elementStack = List[(String, ArrayBuffer[Element])]()
    var results = ArrayBuffer[Element]()
    results.sizeHint(children.length)

    def pushTag(tag: String) = {
      val newResults = ArrayBuffer[Element]()
      elementStack = (tag, results) :: elementStack
      results = newResults
    }

    def popTag(tag: String, lastResults: ArrayBuffer[Element]) = {
      val newElement = dom.document.createElementNS(
        "http://www.w3.org/2000/svg",
        "g",
      )
      newElement.setAttribute("data-element-id", s"cetz-app-${tag}")
      newElement.setAttribute("class", "typst-cetz-elem")
      newElement.append(results.toArray: _*)

      lastResults += newElement
      results = lastResults
    }

    // println(s"Check Tags ${children.map(identifyElement).toList}")
    for (child <- children.toArray) {
      identifyElement(child) match {
        case RawTypstElem.Just(elem) =>
          results += work(elem)
        case RawTypstElem.Empty(elem) =>
          results += elem
        case RawTypstElem.Tag(tag) =>
          // println(s"Check Tag ${tag} ${elementStack}")
          elementStack match {
            case Nil => pushTag(tag)
            case (lastTag, lastResults) :: tail =>
              if (lastTag == tag) {
                elementStack = tail
                popTag(tag, lastResults)
              } else {
                pushTag(tag)
              }
          }
      }
    }

    elementStack.foreach { case (tag, lastResults) =>
      popTag(tag, lastResults)
    }

    parent.replaceChildren(results.toArray: _*)
    parent
  }

  val elements = MutableMap[Element, RawTypstElem]()

  /// Identify the element and return the tag
  /// Rule:
  /// 1. If the element is an empty group or all its children are "Empty", then it is "Empty"
  /// 2. If the element's children is either "Empty" or "Tag", then it is the only "Tag".
  /// 3. The rest is "Just"
  def identifyElement(elem: Element): RawTypstElem = {
    elements.getOrElse(elem, doIdentifyElement(elem))
  }

  def doIdentifyElement(elem: Element): RawTypstElem = {
    // println(s"Identifying SVG ${elem} ${elem.attributes.toMap.toString()}")
    val strokeWidth = elem.getAttribute("stroke-width")
    val isInstrumentTag =
      strokeWidth != null && strokeWidth.toFloatOption.exists(strokeWidth =>
        (strokeWidth - 0.00012345).abs < 1e-8,
      )
    if (isInstrumentTag) {
      val stroke = elem.getAttribute("stroke")
      val tag = tagMapping.get(stroke)
      return tag match {
        case Some(tag) => RawTypstElem.Tag(tag)
        case None      => RawTypstElem.Empty(elem)
      }
    }

    elem.tagName match {
      case "g" =>
        val children = elem.children
        if (children.length == 0) {
          return RawTypstElem.Empty(elem)
        }
        val tag = children
          .map(identifyElement)
          .reduceOption((a, b) =>
            import RawTypstElem.*
            (a, b) match {
              case (Tag(tag), Empty(_)) => Tag(tag)
              case (Empty(_), Tag(tag)) => Tag(tag)
              case (Tag(_), Tag(_))     => Just(elem)
              case (Just(elem), _)      => Just(elem)
              case (_, Just(elem))      => Just(elem)
              case _                    => b
            },
          )
        tag match {
          case Some(RawTypstElem.Tag(tag)) => return RawTypstElem.Tag(tag)
          case Some(RawTypstElem.Empty(_)) => return RawTypstElem.Empty(elem)
          case Some(RawTypstElem.Just(_)) | None =>
        }
      case _ =>
    }

    RawTypstElem.Just(elem)
  }
}

enum RawTypstElem {
  case Empty(elem: Element)
  case Just(elem: Element)
  case Tag(tag: String)
}
