package com.joecordingley.mu
//
//import monocle.{Iso, Lens, Traversal}
//import org.typelevel.discipline.Predicate
//
//import scalaz.std.list._
//
///**
//  * Created by joe on 01/07/17.
//  */
//object Util {
//
//  def setListIso[A]= Iso[Set[A],List[A]](_.toList)(_.toSet)
//  def listTraversal[A]:Traversal[List[A],A] = Traversal.fromTraverse[List,A]
//  def setTraversal[A]:Traversal[Set[A],A] = setListIso composeTraversal listTraversal
//  def mapListIso[A,B]:Iso[Map[A,B],List[(A,B)]]=Iso[Map[A,B],List[(A,B)]](_.toList)(_.toMap)
//  def mapTraversal[A,B]:Traversal[Map[A,B],(A,B)] = mapListIso composeTraversal listTraversal
//  def mapLens[A,B](a:A):Lens[Map[A,B],B] = Lens[Map[A,B],B](_(a))(b => m => m.updated(a,b))
//  def mapFilter[A,B](predicate:((A,B))=>Boolean):Lens[Map[A,B],Map[A,B]]=
//    Lens[Map[A,B],Map[A,B]](_.filter(predicate))(m => _.filterNot(predicate) ++ m)
////  def collectionFilter[A,S<:Seq[A]](predicate:(A) => Boolean):Lens[S,S] = Lens[S,S](_.filter(predicate))(s2 => _.filterNot(predicate)++s2)
//  def mapKeys[A,B](s:Set[A]):Traversal[Map[A,B],(A,B)]= mapFilter[A,B]{case(a,_) =>s(a)} composeTraversal mapTraversal
//
//}

object Util {
  def orderingFromOrderings[A](orderings:List[Ordering[A]]):Ordering[A] = new Ordering[A]{
    override def compare(x:A,y:A) = orderings.toStream.map(_.compare(x,y)).find(_!=0).getOrElse(0)
  }
  implicit class ListOps[A](l:List[A]) {
    def outrightFirst(implicit o:Ordering[A]):Option[A] = l.sorted match {
      case List(a1) => Some(a1)
      case a1 :: a2 :: _ if o.compare(a1,a2) < 0 => Some(a1)
      case _ => None
    }
  }


}
object SingleElementSet {
  def unapply[A](s:Set[A]):Option[A] = if (s.size ==1) Some(s.head) else None
}
