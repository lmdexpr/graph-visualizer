package graph

import scalafx.Includes._
import scalafx.collections._
import scalafx.event.ActionEvent
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scalafx.scene.control._
import scalafx.scene.shape._

import scalafxml.core.macros.sfxml

import graph.Algorithm._
import graph.types._

@sfxml
class Controller(
  private val canvasPane: Pane,
  private val list: ListView[Node],
  private val algorithm: ComboBox[String],
  private val prevBtn: Button,
  private val nextBtn: Button,
  private val nodeBtn: RadioButton,
  private val connectBtn: RadioButton,
  private val deleteBtn: RadioButton,
  private val searchBtn: RadioButton,
  private val control: ToggleGroup) {

  val radius = 30
  val nodes = new ObservableBuffer[Node]
  val edges = new ObservableBuffer[Edge]

  var label = 1
  var focus = Option.empty[Node]
  var animation = Option.empty[Animation]

  //control.selectedToggle onChange initialization
  
  def initialization() {
    nodes foreach {n =>
      n.unactivate
      n.edges foreach (_.unactivate)
    }
    focus = None
    animation = None
  }

  def focusing(node: Node, f: (Node, Node) => Any) = {
    focus match {
      case Some(focused) =>
        focus = focused.unactivate
        Some(f(focused, node))
      case None =>
        focus = node.activate
        None
    }
  }

  def search(node: Node) {
    animation = algorithm.value.value match {
      case "DFS" => Some(dfs(node))
      case "BFS" => Some(bfs(node))
      case "Dijkstra" => focusing(node, dijkstra(_,_, nodes.toSeq)).asInstanceOf[Option[Animation]]
      case "Kruskal" => Some(kruskal(nodes.toSeq, edges))
      case _ => None
    }
  }

  def prev {
    if (!searchBtn.selected()) return
    animation foreach (_.prev)
  }

  def next {
    if (!searchBtn.selected()) return
    animation foreach (_.next)
  }

  def delete(node: Node) {
    canvasPane.children.removeAll(node.circle, node.tag)
    node.edges foreach { e => canvasPane.children.removeAll(e.spinner, e.line) }
    nodes -= node
    node.edges foreach { e => edges -= e }
    node.delete()
  }

  def connect(src: Node, dist: Node) {
    if (!src.connected(dist)) {
      val newEdge = Edge(src, dist, 0)
      canvasPane.children.add(0, newEdge.spinner)
      canvasPane.children.add(0, newEdge.line)
      edges += newEdge
    }
  }

  def onClicked(event: MouseEvent) {
    if (nodes.exists(_.circle.contains(event.x, event.y))) return
    
    initialization()

    if (!nodeBtn.selected()) return

    val newNode = Node(label.toString, event.x, event.y, radius, canvasPane.width(), canvasPane.height())

    newNode.circle.onMouseClicked = { e: MouseEvent =>
      if (deleteBtn.selected()) delete(newNode)
      else if (connectBtn.selected()) focusing(newNode, connect(_,_))
      else if (searchBtn.selected()) search(newNode)
    }

    canvasPane.children.addAll(newNode.circle, newNode.tag)
    nodes += newNode
    label += 1
    println(s"add node [id: ${newNode.id}]")
  }
}
