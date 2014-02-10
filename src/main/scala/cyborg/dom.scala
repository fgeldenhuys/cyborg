package cyborg

import scalaz._, Scalaz._
import org.w3c.dom.Node
import cyborg.util.control._

object dom {

  implicit class DomNodeExt(val node: Node) extends AnyVal {
    def attribute(name: String): Option[String] = {
      tryOption {
        for {
          elementAttributes <- Option(node.getAttributes)
          idAttribute <- Option(elementAttributes.getNamedItem(name))
          value <- Option(idAttribute.getNodeValue)
        } yield value
      } .flatMap(identity)
    }

    def name: Option[String] = tryOption(node.getNodeName)
    def value: Option[String] = tryOption(node.getNodeValue)

    def firstChild: Option[Node] = tryOption(node.getFirstChild)
    def lastChild: Option[Node] = tryOption(node.getLastChild)
    def childWithName(name: String): Option[Node] = {
      tryOption {
        val children = node.getChildNodes
        (0 until children.getLength).find { i =>
          children.item(i).getNodeName == name
        } map children.item
      } .flatMap(identity)
    }
  }

}
