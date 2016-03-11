package graph

import annotation.tailrec
import collection.mutable.{Stack, Queue, Map, Set}

import scalafx.collections.ObservableBuffer

import graph.types._

object Algorithm {
  // 実行の開始などをログとして表示するためのヘルパー関数
  def messaging(title: String)(proc: Stack[Edge] => Unit) = {
    val stack = Stack[Edge]()
    println(s"${title} start")
    proc(stack)
    println(s"${title} end")
    Animation(stack)
  }

  // DFSの実装 inner_dfsが具体的な実装
  def dfs(node: Node) = messaging("DFS")(inner_dfs(node))
  private def inner_dfs(node: Node, visited: ObservableBuffer[String] = new ObservableBuffer[String])(stack: Stack[Edge]) {
    visited += node.id
    node.activate

    node.edges.sortBy(_.distOf(node).label) foreach { e =>
      val dist = e.distOf(node)

      if (!visited.contains(dist.id)) {
        println(s"DFS activate Edge [id: ${e.id}] Node [id: ${dist.id}]")
        e.activate
        stack push e
        inner_dfs(dist, visited)(stack)
      }
    }
  }
  
  // BFSの実装 inner_bfsが具体的な実装
  def bfs(node: Node) = messaging("BFS")(inner_bfs(Queue(node)))
  @tailrec
  private def inner_bfs(queue: Queue[Node], visited: ObservableBuffer[String] = new ObservableBuffer[String])(stack: Stack[Edge]) {
    if (queue.isEmpty) return

    val node = queue.dequeue

    visited += node.id
    node.activate

    node.edges.sortBy(_.distOf(node).label) foreach { e =>
      val dist = e.distOf(node)

      if (!visited.contains(dist.id)) {
        visited += dist.id
        println(s"BFS activate Edge [id: ${e.id}] Node [id: ${dist.id}]")
        stack push e.activate
        queue enqueue dist
      }
    }

    inner_bfs(queue, visited)(stack)
  }

  // dijkstraの実装 inner_dijkstraが具体的な実装
  def dijkstra(start: Node, goal: Node, nodes: Seq[Node]) = messaging("Dijkstra") { stack =>
    val (length, prevs) = (Map(start.id -> 0), Map[String, Edge]())
    val rev_stack = Stack[Edge]()
    var node: Option[Node] = goal.activate

    inner_dijkstra(Set(nodes: _*), length, prevs)(stack)

    while (node != Some(start) && node != None) {
      val eop = node flatMap (prevs get _.id)
      node = eop flatMap (e => node map (e.distOf(_)))
      eop foreach { e =>
        rev_stack push e.activate
        node foreach (_.activate)
        println(s"Dijkstra activate Edge [id: ${e.id}] Node [id: ${node.map(_.id).getOrElse("None")}]")
      }
    }

    start.activate

    rev_stack foreach (stack push _)
  }
  @tailrec
  private def inner_dijkstra(nodes: Set[Node], length: Map[String, Int], prevs: Map[String, Edge])(stack: Stack[Edge]) {
    if (nodes.isEmpty) return

    val min = nodes.minBy(n => length.getOrElse(n.id, Int.MaxValue))
    nodes -= min

    min.edges foreach { e =>
      val dist = e.distOf(min)
      val leng = length(min.id) + e.weight
      if (length.getOrElse(dist.id, Int.MaxValue) > leng) {
        length(dist.id) = leng
        prevs(dist.id)  = e
      }
    }

    inner_dijkstra(nodes, length, prevs)(stack)
  }

  // kruskalの実装 Union-Find木を用いている
  def kruskal(nodes: Seq[Node], edges: ObservableBuffer[Edge]) = messaging("Kruskal") { stack =>
    val uf = UnionFind[Node](nodes)

    edges.toSeq.sortBy(_.weight) foreach { e =>
      if (!uf.isEq(e.src, e.dist)) {
        uf union (e.src, e.dist)
        stack push e.activate
        e.src.activate
        e.dist.activate
        println(s"Kruskal activate Edge [id: ${e.id}]")
      }
    }
  }

  case class UnionFind[T](seq: Seq[T]) {
    private[this] val par: Map[T, T] = Map[T,T](seq.map(elem => elem -> elem): _*)

    @tailrec
    private[this] def find(t: T, acc: Seq[T] = Nil): T = {
      if (par(t) == t) {
        acc foreach { par(_) = t }
        t
      } else {
        val p = par(t)
        find(p, p +: acc)
      }
    }

    def union(a: T, b: T) { if (find(a) != find(b)) par(find(a)) = find(b) }
    def isEq(a: T, b: T) = find(a) == find(b)
  }
}
