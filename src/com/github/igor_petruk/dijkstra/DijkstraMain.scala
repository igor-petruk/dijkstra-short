package com.github.igor_petruk.dijkstra

import annotation.tailrec

case class Path(points:List[Int], length:Int)

class Dijkstra(graphInput:List[(Int,Int,Int)]){
  val graph = {
    val withoutEmpty = graphInput.view
      .map(x=>List(x,(x._2,x._1,x._3))).flatten
      .groupBy(_._1)
      .map(x=>(x._1,x._2.map(q=>(q._2,q._3)).toMap))
    val defaults = (0 to withoutEmpty.keys.max).map(x=>(x,Map[Int,Int]())).toMap
    defaults++withoutEmpty
  }

  def find(from:Int, to:Int):Option[Path]={
    val distance = (0 to graph.keys.max).map(x=>if (x==from) 0 else Int.MaxValue).toArray
    val pathArray = (0 to graph.keys.max).map(x=>if (x==from) from else -1).toArray

    @tailrec
    def iterate(visited:Set[Int]):Boolean={
      if (!graph.keys.forall(visited.contains(_))){
        val smallest = distance.view.zipWithIndex.filter(item=> !visited.contains(item._2)).minBy(_._1)

        if (smallest._1 == Int.MaxValue)
           return false

        val newVisited = visited + smallest._2
        for (edge <- graph(smallest._2)){
          val newDistance = smallest._1+edge._2
          if (distance(edge._1)>newDistance){
            distance(edge._1) = newDistance
            pathArray(edge._1) = smallest._2
          }
        }
        iterate(newVisited)
      } else {
        true // visited all
      }
    }
    if (iterate(Set.empty)){
      val path = {
        def pathStream(pt:Int):Stream[Int] = pt #:: pathStream(pathArray(pt))
        from :: pathStream(to).takeWhile(_!=from).toList.reverse
      }
      Some(Path(path, distance(to)))
    }else{
      None
    }
  }
}

object DijkstraMain {

  def main(argv:Array[String]){
    val path = new Dijkstra(List(
      (0,1,7),
      (0,5,14),
      (0,2,9),
      (1,2,10),
      (1,3,15),
      (5,4,9),
      (4,3,6),
      (2,3,11),
      (2,5,2)
    )).find(0,4)

    path match {
      case Some(way @ Path(List(0,2,5,4),20)) => println("Found way %s".format(way))
      case other => sys.error("Failed, found %s".format(other))
    }
  }

}
