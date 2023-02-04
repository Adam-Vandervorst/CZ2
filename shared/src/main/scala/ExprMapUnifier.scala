package be.adamv.cz2


sealed abstract class UnificationFailure(msg: String) extends RuntimeException(msg)

case class ConflictingVars(pos: scala.collection.Set[Long]) extends UnificationFailure(s"Multiple vars: ${pos}")

case class ConflictingVarAndApp(pos: Long, app: ExprMap[ExprMap[_]]) extends UnificationFailure(s"var: ${pos} and ${app.prettyStructuredSet()}")

object ExprMapUnifier:
  def sim[V](em: EM[V], bindings: Seq[Int] = Nil): String =
    val pos = em.vars.view.filterKeys(_ > 0)
    val zero = em.vars.view.filterKeys(_ == 0)
    val neg = em.vars.view.filterKeys(_ < 0)
    if pos.sizeIs > 1 then throw ConflictingVars(pos.keySet)
    if pos.nonEmpty && em.apps.nonEmpty then throw ConflictingVarAndApp(pos.keys.head, em.apps.asInstanceOf)
    if neg.isEmpty then
      if pos.isEmpty then
        if zero.isEmpty then
          if em.apps.isEmpty then "empty"
          else s"app ${em.apps.prettyStructuredSet(false)}"
        else
          if em.apps.isEmpty then "registering any"
          else s"setting ${bindings.length} to ${em.apps.prettyStructuredSet(false)}"
      else
        if zero.isEmpty then s"skipping over constant ${pos.keys.head}"
        else s"setting ${bindings.length} to ${pos.keys.head}"
    else
      if pos.isEmpty then
        if zero.isEmpty then
          if em.apps.isEmpty then s"unify bindings ${neg.keySet}"
          else s"unify bindings ${neg.keySet} and app ${em.apps.prettyStructuredSet(false)}"
        else
          if em.apps.isEmpty then s"equate ${bindings.length} to ${neg.keySet}"
          else s"equate ${bindings.length} to ${neg.keySet} and ${em.apps.prettyStructuredSet(false)}"
      else
        if zero.isEmpty then
          s"setting bindings ${neg.keySet} to ${pos.keys.head}"
        else
          s"setting ${bindings.length} and ${neg.keySet} to ${pos.keys.head}"
