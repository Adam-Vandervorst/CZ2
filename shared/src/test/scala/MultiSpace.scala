package be.adamv.cz2

import munit.FunSuite


class MultiSpaceTest extends FunSuite:
  import ExprExamples.{`=`, _1, _2, _3, $}

  val top = RangeStorage.highPos()
  val ns = top.storeInRange(Set("space"))
  import ns.v
  val spaces = top.inRange[ExprMap[_]](4)

  val metaSpace = ExprMap[Int]()
  val metaSpaceRef = spaces.addV(metaSpace)
  metaSpace.update(Expr(v"space", metaSpaceRef), 1)

  val librarySpace = ExprMap[Int]()
  val librarySpaceRef = spaces.addV(librarySpace)
  metaSpace.update(Expr(v"space", librarySpaceRef), 2)

  val knowledgeSpace = ExprMap[Int]()
  val knowledgeSpaceRef = spaces.addV(knowledgeSpace)
  metaSpace.update(Expr(v"space", knowledgeSpaceRef), 3)

  val computeSpace = ExprMap[Int]()
  val computeSpaceRef = spaces.addV(computeSpace)
  metaSpace.update(Expr(v"space", computeSpaceRef), 4)

