package be.adamv.cz2

import munit.FunSuite

import scala.util.Left


class KnowledgeGraphTest extends FunSuite:
  import ExprExamples.{$, _1, _2}
  test("aunt") {
    val parent = Var(1000)
    val male = Var(1001)
    val female = Var(1002)
    val child = Var(2000)

    val people = RangeStorage.highPos[String]()
    import people.v

    var family = ExprMap[Int](
      Expr(parent, v"Tom", v"Bob") -> 1,
      Expr(parent, v"Pam", v"Bob") -> 2,
      Expr(parent, v"Tom", v"Liz") -> 3,
      Expr(parent, v"Bob", v"Ann") -> 4,
      Expr(parent, v"Bob", v"Pat") -> 5,
      Expr(parent, v"Pat", v"Jim") -> 6,
      Expr(female, v"Pam") -> 7,
      Expr(male, v"Tom") -> 8,
      Expr(male, v"Bob") -> 9,
      Expr(female, v"Liz") -> 10,
      Expr(female, v"Pat") -> 11,
      Expr(female, v"Ann") -> 12,
      Expr(male, v"Jim") -> 13,
    )

    // all big O results here are worst case

    // family |= family.subst((parent $x $y), (child $y $x))
    family = family.union(family.transformMatches(Expr(parent, $, $), Expr(child, _2, _1)))

    // (match &self (parent $y Bob) $y)
    // &self.compileTransform([2 parent; 0 $y; 0 Bob], [0 $y])
    // assert([0 $y] <= [2 parent; 0 $y; 0 Bob])
    // cache.transformMatches([2 =; 2 parent; 0 $y; 0 Bob; 0 $rhs], $rhs) = [2 child; 0 Bob; 0 $y]
    // getAt([2 child; 0 Bob;]

    // parents(x) = family[App, App, child, x]
    // O(1)
    for (person, name) <- people.indexToValue do try
      val Left(em) = family.getAt(List(None, None, Some(child.leftMost), Some(person))) : @unchecked
      println(s"parents of $name : ${em.vars.keysIterator.map(x => people.get(x.toInt).get).mkString(", ")}")
    catch case e: RuntimeException => println(s"parents of $name not in knowledge base")

    // mother(x) = family[App, App, child, x] intersect family[App, female]
    // O(min(family[App, App, child, x], family[App, female]))
    for (person, name) <- people.indexToValue do try
      val Left(em) = family.getAt(List(None, None, Some(child.leftMost), Some(person))) : @unchecked
      val Left(em_) = family.getAt(List(None, Some(female.leftMost))) : @unchecked
      val female_parents = em.intersection(em_.asInstanceOf)
      println(s"mothers of $name : ${female_parents.em.vars.keysIterator.map(x => people.get(x.toInt).get).mkString(", ")}")
    catch case e: RuntimeException => println(s"mother of $name not in knowledge base")

    // sister(x) = (family[parent] intersect (family[App, App, child, x] @ family[App, female])).tail - x
    // O(family[App, App, child, x] * family[App, female])
    // sister(x) = (family[App, App, child, x] @ \y -> family[App, App, parent, y]) intersect family[App, female] - x
    // O(family[App, App, child, x] * family[App, App, parent, -])
    // sister(x) = (family[App, female] @ \y -> family[App, App, child, y]) intersect family[App, App, child, x] - x
    // O(family[App, female] * family[App, App, parent, -])

    // sister(x) = (family[App, App, parent] restrict family[App, App, child, x]).tail intersect family[App, female] - x
    // O(C1 + min(C1, family[App, female]))
    //   where C1 = family[App, App, child, x] * family[App, App, parent, ., -]
    // note you can play around with tail and intersect family[App, female]
    for (person, name) <- people.indexToValue do try
      val Left(em) = family.getAt(List(None, None, Some(parent.leftMost))) : @unchecked
      val Left(em_) = family.getAt(List(None, None, Some(child.leftMost), Some(person))) : @unchecked
      val intermediate = em.intersectionWith((x, _) => x)(em_.asInstanceOf)
      val flattened = ExprMapEngine[Int].drophead(intermediate.asInstanceOf)
      val Left(females) = family.getAt(List(None, Some(female.leftMost)))  : @unchecked
      val result = flattened.intersection(ExprMap(females).asInstanceOf)
      result.remove(Var(person))
      println(s"sisters of $name : ${result.em.vars.keysIterator.map(x => people.get(x.toInt).get).mkString(", ")}")
    catch case e: RuntimeException => println(s"sisters of $name not in knowledge base")

    // aunt(x) = family[App, App, child, x] @ \y -> ((family[App, App, child, y] @ \z -> family[App, App, parent, z]) intersect family[App, female] - y)
    // O(family[App, App, child, x] * family[App, App, child, -] * family[App, App, parent, -])

    // aunt(x) = family[App, App, child, x] @ \y -> ((family[App, App, parent] restrict family[App, App, child, y]).tail intersect family[App, female] - y)
    // O(C1 + min(C1, family[App, female]))
    //   where C1 = family[App, App, child, x] * family[App, App, parent, ., -] +
    //              family[App, App, child, -] * family[App, App, parent, ., -]
    for (person, name) <- people.indexToValue do try
      val Left(females) = family.getAt(List(None, Some(female.leftMost))) : @unchecked
      val Left(children) = family.getAt(List(None, None, Some(parent.leftMost))) : @unchecked
      val Left(parents) = family.getAt(List(None, None, Some(child.leftMost))) : @unchecked
      val Left(person_parents) = family.getAt(List(None, None, Some(child.leftMost), Some(person))) : @unchecked
      val intermediate = parents.intersectionWith((x, _) => x)(person_parents.asInstanceOf)
      val person_grandparents = ExprMapEngine[Int].drophead(intermediate.asInstanceOf)
      val intermediate_ = ExprMap(children).intersectionWith((x, _) => x)(person_grandparents.asInstanceOf)
      val person_parent_siblings = ExprMapEngine[ExprMap[Int]].drophead(intermediate_.asInstanceOf)
        .subtract(ExprMap(person_parents))
      val person_aunts = person_parent_siblings.intersection(ExprMap(females).asInstanceOf)
      println(s"Aunts of $name : ${person_aunts.em.vars.keysIterator.map(x => people.get(x.toInt).get).mkString(", ")}")
    catch case e: RuntimeException => println(s"aunt of $name not in knowledge base")

    // pred(x) = fix(predr)(family[App, App, child, x])
    // predr(cs) = cs union (family[App, App, child] restrict cs).tail
    // O(family[App, App, child, -, -])
    for (person, name) <- people.indexToValue do try
      val Left(parents) = family.getAt(List(None, None, Some(child.leftMost))) : @unchecked
      var Left(pred) = family.getAt(List(None, None, Some(child.leftMost), Some(person))) : @unchecked
      var oldest = pred
      while (oldest ne null) && oldest.nonEmpty do
        pred = pred.union(oldest.asInstanceOf)
        oldest = ExprMapEngine[Int].drophead(
          parents.intersectionWith((x, _) => x)(oldest.asInstanceOf).asInstanceOf).em
      println(s"Predecessors of $name : ${pred.vars.keysIterator.map(x => people.get(x.toInt).get).mkString(", ")}")
    catch case e: RuntimeException => println(s"predecessors of $name not in knowledge base")
  }
