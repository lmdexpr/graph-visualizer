package graph.types

import collection.mutable.Stack

import scalafx.Includes._
import scalafx.beans.binding._
import scalafx.beans.property._
import scalafx.scene.shape._
import scalafx.scene.input.MouseEvent
import scalafx.scene.text.Text
import scalafx.scene.control.Spinner
import scalafx.scene.paint.Color
import scalafx.collections.ObservableBuffer

case class Node(label: String, x: Double, y: Double, r: Double, maxX: Double, maxY: Double) {
  val id = s"node-${java.util.UUID.randomUUID.toString}"
  val edges = new ObservableBuffer[Edge]

  val circle = new Circle {
    centerX = x
    centerY = y
    radius  = r
    fill    = Color.LightBlue
    onMouseDragged = { event: MouseEvent =>
      if (0 <= event.x - r && event.x + r <= maxX) centerX = event.x
      if (0 <= event.y - r && event.y + r <= maxY) centerY = event.y
    }
  }

  val tag = new Text(label)

  tag.layoutX <==> circle.centerX
  tag.layoutY <==> circle.centerY

  override def toString = label

  def connected(dist: Node) = dist.id == this.id || edges.exists(e => e.src.id == dist.id || e.dist.id == dist.id)

  def activate   = { circle.fill = Color.LightCoral; Some(this) }
  def unactivate = { circle.fill = Color.LightBlue;  None }

  def delete() {
    edges foreach (_.delete(this))
    edges.clear()
    println(s"delete node [id: ${this.id}]")
  }
}

case class Edge(src: Node, dist: Node, init_weight: Int = 0) {
  val id = s"edge-${java.util.UUID.randomUUID.toString}"

  src.edges  += this
  dist.edges += this

  println(s"add edge [id: ${this.id}] [from: ${src.id}] [to: ${dist.id}]")

  val line = new Line

  line.startX <==> src.circle.centerX
  line.startY <==> src.circle.centerY

  line.endX <==> dist.circle.centerX
  line.endY <==> dist.circle.centerY

  val spinner = new Spinner(0, Int.MaxValue, init_weight)

  spinner.layoutX <== (line.startX + line.endX - spinner.width) / DoubleProperty(2.0)
  spinner.layoutY <== (line.startY + line.endY) / DoubleProperty(2.0)

  def weight: Int = spinner.value.value

  def delete(node: Node) {
    distOf(node).edges -= this
    println(s"delete edge [id: ${this.id}]")
  }

  def activate   = { line.stroke = Color.RoyalBlue; line.strokeWidth = 10.0; this }
  def unactivate = { line.stroke = Color.Black; line.strokeWidth = 1.0; this }

  def distOf(node: Node) = if (this.src != node) this.src else this.dist
}

case class Animation(prevStk: Stack[Edge], nextStk: Stack[Edge] = Stack[Edge]()) {

  def prev {
    if (!prevStk.isEmpty) {
      nextStk push prevStk.pop.unactivate
    }
  }

  def next {
    if (!nextStk.isEmpty) {
      prevStk push nextStk.pop.activate
    }
  }

}
