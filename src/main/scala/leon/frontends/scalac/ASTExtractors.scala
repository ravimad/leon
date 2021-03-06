/* Copyright 2009-2014 EPFL, Lausanne */

package leon
package frontends.scalac

import scala.tools.nsc._

/** Contains extractors to pull-out interesting parts of the Scala ASTs. */
trait ASTExtractors {
  val global: Global

  import global._
  import global.definitions._

  def classFromName(str: String) = {
    rootMirror.getClassByName(newTypeName(str))
  }

  def objectFromName(str: String) = {
    rootMirror.getClassByName(newTermName(str))
  }


  protected lazy val tuple2Sym          = classFromName("scala.Tuple2")
  protected lazy val tuple3Sym          = classFromName("scala.Tuple3")
  protected lazy val tuple4Sym          = classFromName("scala.Tuple4")
  protected lazy val tuple5Sym          = classFromName("scala.Tuple5")
  protected lazy val mapSym             = classFromName("scala.collection.immutable.Map")
  protected lazy val setSym             = classFromName("scala.collection.immutable.Set")
  protected lazy val optionClassSym     = classFromName("scala.Option")
  protected lazy val arraySym           = classFromName("scala.Array")
  protected lazy val someClassSym       = classFromName("scala.Some")
  protected lazy val function1TraitSym  = classFromName("scala.Function1")
  protected lazy val byNameSym          = classFromName("scala.<byname>")

  def isTuple2(sym : Symbol) : Boolean = sym == tuple2Sym
  def isTuple3(sym : Symbol) : Boolean = sym == tuple3Sym
  def isTuple4(sym : Symbol) : Boolean = sym == tuple4Sym
  def isTuple5(sym : Symbol) : Boolean = sym == tuple5Sym

  def isByNameSym(sym : Symbol) : Boolean = getResolvedTypeSym(sym) == byNameSym

  // Resolve type aliases
  def getResolvedTypeSym(sym: Symbol): Symbol = {
    if (sym.isAliasType) {
      getResolvedTypeSym(sym.tpe.resultType.typeSymbol)
    } else {
      sym
    }
  }

  def isSetTraitSym(sym : Symbol) : Boolean = {
    getResolvedTypeSym(sym) == setSym
  }

  def isMapTraitSym(sym : Symbol) : Boolean = {
    getResolvedTypeSym(sym) == mapSym
  }

  def isMultisetTraitSym(sym : Symbol) : Boolean = {
    sym == multisetTraitSym
  }

  def isOptionClassSym(sym : Symbol) : Boolean = {
    sym == optionClassSym || sym == someClassSym
  }

  def isFunction1TraitSym(sym : Symbol) : Boolean = {
    sym == function1TraitSym
  }


  protected lazy val multisetTraitSym  = try {
      classFromName("scala.collection.immutable.Multiset")
    } catch {
      case e: Throwable =>
        null
    }

  def isArrayClassSym(sym: Symbol): Boolean = sym == arraySym

  object ExtractorHelpers {
    object ExIdNamed {
      def unapply(id: Ident): Option[String] = Some(id.toString)
    }

    object ExHasType {
      def unapply(tr: Tree): Option[(Tree, Symbol)] = Some((tr, tr.tpe.typeSymbol))
    }

    object ExNamed {
      def unapply(name: Name): Option[String] = Some(name.toString)
    }

    object ExSymbol {
      def unapplySeq(t: Tree): Option[Seq[String]] = {
        Some(t.symbol.fullName.toString.split('.').toSeq)
      }
    }

    object ExSelected {
      def unapplySeq(select: Select): Option[Seq[String]] = select match {
        case Select(This(scalaName), name) =>
          Some(Seq(scalaName.toString, name.toString))

        case Select(from: Select, name) =>
          unapplySeq(from).map(prefix => prefix :+ name.toString)

        case Select(from: Ident, name) =>
          Some(Seq(from.toString, name.toString))

        case _ =>
          None
      }
    }
  }

  object StructuralExtractors {
    import ExtractorHelpers._

    /**
     * Extracts the templates from the ensuring expression
     */
    object ExTemplateExpression {
      def unapply(tree: Apply): Option[(Tree, List[Symbol], Tree)] = {        
        tree match {
          case Apply(Select(Apply(ExSelected("leon", "lang", "invariantLang","package","any2Template"), postcondTree :: Nil), ExNamed("template")),
            (Function(vardefs, templateBody)) :: Nil) => {
            //for each vardefs extract the term name
            val varnames = vardefs.map(_.symbol)
            //println("postcond: "+postcondTree + "params: "+varnames+" templateBody: "+templateBody)
            Some(postcondTree, varnames, templateBody)
          }
          case _ => None
        }
      }
    }
    
    /**
     * Extracts time variable from the templates and postconditions
     */
    object ExTimeVariable {
      def unapply(tree: Select) : Boolean = tree match {
        case ExSelected("leon", "lang", "invariantLang","package", "time") =>
          true
        case _ => false
      }
    }
    
    /**
     * Extracts depth variable from the templates and postconditions
     */
    object ExDepthVariable {
      def unapply(tree: Select) : Boolean = tree match {
        case ExSelected("leon", "lang", "invariantLang","package","depth") =>
          true
        case _ => false
      }
    }
    
    /**
     * Extracts nondet constructs
     */
    object ExNondetExpression {
      def unapply(tree: TypeApply) : Option[Type] = tree match {
        case a @ TypeApply(ExSelected("leon", "lang", "invariantLang", "nondet"), List(tpe)) =>{
          //println("Found nondet, type: "+tpe.tpe)
          Some(tpe.tpe)
        }
        case _ =>
          None
      }
    }

    object ExEnsuredExpression {
      /** Extracts the 'ensuring' contract from an expression. */
      def unapply(tree: Apply): Option[(Tree,Symbol,Tree)] = tree match {
        case Apply(Select(Apply(TypeApply(ExSelected("scala", "Predef", "any2Ensuring"), TypeTree() :: Nil),
              body :: Nil), ExNamed("ensuring")),
          (Function((vd @ ValDef(_, _, _, EmptyTree)) :: Nil, contractBody)) :: Nil)
          => Some((body, vd.symbol, contractBody))
        case _ => None
      }
    }

    object ExHoldsExpression {
      def unapply(tree: Select) : Option[Tree] = tree match {
        case Select(Apply(ExSymbol("leon", "lang", "any2IsValid"), realExpr :: Nil), ExNamed("holds")) =>
            Some(realExpr)
        case _ => None
       }
    }

    object ExRequiredExpression {
      /** Extracts the 'require' contract from an expression (only if it's the
       * first call in the block). */
      def unapply(tree: Block): Option[(Tree,Tree)] = tree match {
        case Block(Apply(ExSelected("scala", "Predef", "require"), contractBody :: Nil) :: rest, body) =>
          if(rest.isEmpty)
            Some((body,contractBody))
          else
            Some((Block(rest,body),contractBody))
        case _ => None
      }
    }


    object ExObjectDef {
      /** Matches an object with no type parameters, and regardless of its
       * visibility. Does not match on the automatically generated companion
       * objects of case classes (or any synthetic class). */
      def unapply(cd: ClassDef): Option[(String,Template)] = cd match {
        case ClassDef(_, name, tparams, impl) if ((cd.symbol.isModuleClass || cd.symbol.isPackage) && tparams.isEmpty && !cd.symbol.isSynthetic) => {
          Some((name.toString, impl))
        }
        case _ => None
      }
    }

    object ExAbstractClass {
      /** Matches an abstract class or a trait with no type parameters, no
       * constrctor args (in the case of a class), no implementation details,
       * no abstract members. */
      def unapply(cd: ClassDef): Option[(String, Symbol, Template)] = cd match {
        // abstract class
        case ClassDef(_, name, tparams, impl) if (cd.symbol.isAbstractClass) => Some((name.toString, cd.symbol, impl))

        case _ => None
      }
    }

    object ExCaseClass {
      def unapply(cd: ClassDef): Option[(String,Symbol,Seq[(String,Tree)], Template)] = cd match {
        case ClassDef(_, name, tparams, impl) if (cd.symbol.isCase && !cd.symbol.isAbstractClass && impl.body.size >= 8) => {
          val constructor: DefDef = impl.children.find(child => child match {
            case ExConstructorDef() => true
            case _ => false
          }).get.asInstanceOf[DefDef]

          val args = constructor.vparamss.flatten.map(vd => (vd.name.toString, vd.tpt))

          Some((name.toString, cd.symbol, args, impl))
        }
        case _ => None
      }
    }

    object ExCaseObject {
      def unapply(s: Select): Option[Symbol] = {
        if (s.tpe.typeSymbol.isModuleClass) {
          Some(s.tpe.typeSymbol)
        } else {
          None
        }
      }
    }

    object ExCaseClassSyntheticJunk {
      def unapply(cd: ClassDef): Boolean = cd match {
        case ClassDef(_, _, _, _) if (cd.symbol.isSynthetic) => true
        case _ => false
      }
    }

    object ExConstructorDef {
      def unapply(dd: DefDef): Boolean = dd match {
        case DefDef(_, name, tparams, vparamss, tpt, rhs) if(name == nme.CONSTRUCTOR && tparams.isEmpty) => true
        case _ => false
      }
    }

    object ExMainFunctionDef {
      def unapply(dd: DefDef): Boolean = dd match {
        case DefDef(_, name, tparams, vparamss, tpt, rhs) if(name.toString == "main" && tparams.isEmpty && vparamss.size == 1 && vparamss(0).size == 1) => {
          true
        }
        case _ => false
      }
    }

    object ExFunctionDef {
      /** Matches a function with a single list of arguments, no type
       * parameters and regardless of its visibility. */
      def unapply(dd: DefDef): Option[(Symbol, Seq[Symbol], Seq[ValDef], Type, Tree)] = dd match {
        case DefDef(_, name, tparams, vparamss, tpt, rhs) if(name != nme.CONSTRUCTOR) =>
          Some((dd.symbol, tparams.map(_.symbol), vparamss.flatten, tpt.tpe, rhs))
        case _ => None
      }
    }

  }

  object ExpressionExtractors {
    import ExtractorHelpers._

    object ExEpsilonExpression {
      def unapply(tree: Apply) : Option[(Tree, Symbol, Tree)] = tree match {
        case Apply(
              TypeApply(ExSymbol("leon", "lang", "xlang", "epsilon"), typeTree :: Nil),
              Function((vd @ ValDef(_, _, _, EmptyTree)) :: Nil, predicateBody) :: Nil) =>
            Some((typeTree, vd.symbol, predicateBody))
        case _ => None
      }
    }

    object ExWaypointExpression {
      def unapply(tree: Apply) : Option[(Tree, Tree, Tree)] = tree match {
        case Apply(
              TypeApply(ExSymbol("leon", "lang", "xlang", "waypoint"), typeTree :: Nil),
              List(i, expr)) =>
            Some((typeTree, i, expr))
        case _ => None
      }
    }

    object ExErrorExpression {
      def unapply(tree: Apply) : Option[(String, Tree)] = tree match {
        case a @ Apply(TypeApply(ExSymbol("leon", "lang", "error"), List(tpe)), List(lit : Literal)) =>
          Some((lit.value.stringValue, tpe))
        case _ =>
          None
      }
    }

    object ExHoleExpression {
      def unapply(tree: Tree) : Option[(Tree, List[Tree], List[Tree])] = tree match {
        case a @ Apply(Apply(TypeApply(s @ ExSymbol("leon", "lang", "synthesis", "$qmark"), List(tpt)), args1), args2)  =>
            Some((tpt, args1, args2))
        case a @ Apply(TypeApply(s @ ExSymbol("leon", "lang", "synthesis", "$qmark$qmark$qmark"), List(tpt)), List(o)) =>
            Some((tpt, Nil, List(o)))
        case _ => None
      }
    }

    object ExChooseExpression {
      def unapply(tree: Apply) : Option[(List[(Tree, Symbol)], Tree, Tree, Tree)] = tree match {
        case a @ Apply(
              TypeApply(s @ ExSymbol("leon", "lang", "synthesis", "choose"), types),
              Function(vds, predicateBody) :: Nil) =>
            Some(((types zip vds.map(_.symbol)).toList, a, predicateBody, s))
        case _ => None
      }
    }

    object ExArrayUpdated {
      def unapply(tree: Apply): Option[(Tree,Tree,Tree)] = tree match {
        case Apply(
              Apply(TypeApply(Select(Apply(ExSelected("scala", "Predef", s), Seq(lhs)), n), _), Seq(index, value)),
              List(Apply(_, _))) if (s.toString contains "Array") && (n.toString == "updated") => Some((lhs, index, value))
        case _ => None
      }
    }


    object ExValDef {
      /** Extracts val's in the head of blocks. */
      def unapply(tree: ValDef): Option[(Symbol,Tree,Tree)] = tree match {
        case vd @ ValDef(mods, _, tpt, rhs) if !mods.isMutable => Some((vd.symbol, tpt, rhs))
        case _ => None
      }
    }
    object ExVarDef {
      /** Extracts var's in the head of blocks. */
      def unapply(tree: ValDef): Option[(Symbol,Tree,Tree)] = tree match {
        case vd @ ValDef(mods, _, tpt, rhs) if mods.isMutable => Some((vd.symbol, tpt, rhs))
        case _ => None
      }
    }

    object ExAssign {
      def unapply(tree: Assign): Option[(Symbol,Tree)] = tree match {
        case Assign(id@Ident(_), rhs) => Some((id.symbol, rhs))
        case _ => None
      }
    }

    object ExWhile {
      def unapply(tree: LabelDef): Option[(Tree,Tree)] = tree match {
        case (label@LabelDef(
                _, _, If(cond, Block(body, jump@Apply(_, _)), unit@ExUnitLiteral())))
              if label.symbol == jump.symbol && unit.symbol == null => Some((cond, Block(body, unit)))
        case _ => None
      }
    }

    object ExWhileWithInvariant {
      def unapply(tree: Apply): Option[(Tree, Tree, Tree)] = tree match {
        case Apply(
          Select(
            Apply(while2invariant, List(ExWhile(cond, body))),
            invariantSym),
          List(invariant)) if invariantSym.toString == "invariant" => Some((cond, body, invariant))
        case _ => None
      }
    }

    object ExArrayLiteral {
      def unapply(tree: Apply): Option[(Type, Seq[Tree])] = tree match {
        case Apply(ExSelected("scala", "Array", "apply"), args) =>
          tree.tpe match {
            case TypeRef(_, _, List(t1)) =>
              Some((t1, args))
            case _ =>
              None
          }
        case Apply(Apply(TypeApply(ExSelected("scala", "Array", "apply"), List(tpt)), args), ctags) =>
          Some((tpt.tpe, args))

        case _ =>
          None
      }
    }

    object ExTuple {
      def unapply(tree: Apply): Option[(Seq[Type], Seq[Tree])] = tree match {
        case Apply(
          Select(New(tupleType), _),
          List(e1, e2)
        ) if tupleType.symbol == tuple2Sym => tupleType.tpe match {
            case TypeRef(_, sym, List(t1, t2)) => Some((Seq(t1, t2), Seq(e1, e2)))
            case _ => None
          }

        case Apply(
          Select(New(tupleType), _),
          List(e1, e2, e3)
        ) if tupleType.symbol == tuple3Sym => tupleType.tpe match {
            case TypeRef(_, sym, List(t1, t2, t3)) => Some((Seq(t1, t2, t3), Seq(e1, e2, e3)))
            case _ => None
          }
        case Apply(
          Select(New(tupleType), _),
          List(e1, e2, e3, e4)
        ) if tupleType.symbol == tuple4Sym => tupleType.tpe match {
            case TypeRef(_, sym, List(t1, t2, t3, t4)) => Some((Seq(t1, t2, t3, t4), Seq(e1, e2, e3, e4)))
            case _ => None
          }
        case Apply(
          Select(New(tupleType), _),
          List(e1, e2, e3, e4, e5)
        ) if tupleType.symbol == tuple5Sym => tupleType.tpe match {
            case TypeRef(_, sym, List(t1, t2, t3, t4, t5)) => Some((Seq(t1, t2, t3, t4, t5), Seq(e1, e2, e3, e4, e5)))
            case _ => None
          }
        // Match e1 -> e2
        case Apply(TypeApply(Select(Apply(TypeApply(ExSelected("scala", "Predef", "any2ArrowAssoc"), List(tpeFrom)), List(from)), ExNamed("$minus$greater")), List(tpeTo)), List(to)) =>

          Some((Seq(tpeFrom.tpe, tpeTo.tpe), Seq(from, to)))
        case _ => None
      }
    }

    object ExLocally {
      def unapply(tree: Apply) : Option[Tree] = tree match {
        case Apply(TypeApply(ExSelected("scala", "Predef", "locally"), _), List(body)) =>
          Some(body)

        case _ =>
          None
      }
    }

    object ExTupleExtract {
      def unapply(tree: Select) : Option[(Tree,Int)] = tree match {
        case Select(lhs, n) => {
          val methodName = n.toString
          if(methodName.head == '_') {
            val indexString = methodName.tail
            try {
              val index = indexString.toInt
              if(index > 0) {
                Some((lhs, index))
              } else None
            } catch {
              case t: Throwable =>
                None
            }
          } else None
        }
        case _ => None
      }
    }

    object ExIfThenElse {
      def unapply(tree: If): Option[(Tree,Tree,Tree)] = tree match {
        case If(t1,t2,t3) => Some((t1,t2,t3))
        case _ => None
      }
    }

    object ExBooleanLiteral {
      def unapply(tree: Literal): Option[Boolean] = tree match {
        case Literal(Constant(true)) => Some(true)
        case Literal(Constant(false)) => Some(false)
        case _ => None
      }
    }

    object ExInt32Literal {
      def unapply(tree: Literal): Option[Int] = tree match {
        case Literal(c @ Constant(i)) if c.tpe == IntClass.tpe => Some(c.intValue)
        case _ => None
      }
    }

    object ExUnitLiteral {
      def unapply(tree: Literal): Boolean = tree match {
        case Literal(c @ Constant(_)) if c.tpe == UnitClass.tpe => true
        case _ => false
      }
    }

    object ExSomeConstruction {
      def unapply(tree: Apply) : Option[(Type,Tree)] = tree match {
        case Apply(s @ Select(New(tpt), n), arg) if (arg.size == 1 && n == nme.CONSTRUCTOR && tpt.symbol.name.toString == "Some") => tpt.tpe match {
          case TypeRef(_, sym, tpe :: Nil) => Some((tpe, arg(0)))
          case _ => None
        }
        case _ => None
      }
    }

    object ExCaseClassConstruction {
      def unapply(tree: Apply): Option[(Tree,Seq[Tree])] = tree match {
        case Apply(s @ Select(New(tpt), n), args) if (n == nme.CONSTRUCTOR) => {
          Some((tpt, args))
        }
        case _ => None
      }
    }

    object ExIdentifier {
      def unapply(tree: Ident): Option[(Symbol,Tree)] = tree match {
        case i: Ident => Some((i.symbol, i))
        case _ => None
      }
    }

    object ExTyped {
      def unapply(tree : Typed): Option[(Tree,Tree)] = tree match {
        case Typed(e,t) => Some((e,t))
        case _ => None
      }
    }

    object ExIntIdentifier {
      def unapply(tree: Ident): Option[String] = tree match {
        case i: Ident if i.symbol.tpe == IntClass.tpe => Some(i.symbol.name.toString)
        case _ => None
      }
    }

    object ExAnd {
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(s @ Select(lhs, _), List(rhs)) if (s.symbol == Boolean_and) =>
          Some((lhs,rhs))
        case _ => None
      }
    }
  
    object ExOr {
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(s @ Select(lhs, _), List(rhs)) if (s.symbol == Boolean_or) =>
          Some((lhs,rhs))
        case _ => None
      }
    }
  
    object ExNot {
      def unapply(tree: Select): Option[Tree] = tree match {
        case Select(t, n) if (n == nme.UNARY_!) => Some(t)
        case _ => None
      }
    }
  
    object ExEquals {
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(Select(lhs, n), List(rhs)) if (n == nme.EQ) => Some((lhs,rhs))
        case _ => None
      }
    }
  
    object ExNotEquals {
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(Select(lhs, n), List(rhs)) if (n == nme.NE) => Some((lhs,rhs))
        case _ => None
      }
    }
  
    object ExLessThan {
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(Select(lhs, n), List(rhs)) if (n == nme.LT) => Some((lhs,rhs))
        case _ => None
      }
    }
  
    object ExLessEqThan {
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(Select(lhs, n), List(rhs)) if (n == nme.LE) => Some((lhs,rhs))
        case _ => None
      }
    }
  
    object ExGreaterThan {
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(Select(lhs, n), List(rhs)) if (n == nme.GT) => Some((lhs,rhs))
        case _ => None
      }
    }
  
    object ExGreaterEqThan {
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(Select(lhs, n), List(rhs)) if (n == nme.GE) => Some((lhs,rhs))
        case _ => None
      }
    }

    object ExUMinus {
      def unapply(tree: Select): Option[Tree] = tree match {
        case Select(t, n) if (n == nme.UNARY_-) => Some(t)
        case _ => None
      }
    }

    object ExPlus {
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(Select(lhs, n), List(rhs)) if (n == nme.ADD) => Some((lhs,rhs))
        case _ => None
      }
    }

    object ExMinus {
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(Select(lhs, n), List(rhs)) if (n == nme.SUB) => Some((lhs,rhs))
        case _ => None
      }
    }

    object ExTimes {
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(Select(lhs, n), List(rhs)) if (n == nme.MUL) => Some((lhs,rhs))
        case _ => None
      }
    }

    object ExDiv {
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(Select(lhs, n), List(rhs)) if (n == nme.DIV) => Some((lhs,rhs))
        case _ => None
      }
    }

    object ExMod {
      def unapply(tree: Apply): Option[(Tree,Tree)] = tree match {
        case Apply(Select(lhs, n), List(rhs)) if (n == nme.MOD) => Some((lhs,rhs))
        case _ => None
      }
    }

    object ExPatternMatching {
      def unapply(tree: Match): Option[(Tree,List[CaseDef])] =
        if(tree != null) Some((tree.selector, tree.cases)) else None
    }

    object ExIsInstanceOf {
      def unapply(tree: TypeApply) : Option[(Tree, Tree)] = tree match {
        case TypeApply(Select(t, isInstanceOfName), typeTree :: Nil) if isInstanceOfName.toString == "isInstanceOf" => Some((typeTree, t))
        case _ => None
      }
    }

    object ExEmptySet {
      def unapply(tree: TypeApply): Option[Tree] = tree match {
        case TypeApply(
          Select(
            Select(
              Select(
                Select(Ident(s), collectionName),
                immutableName),
              setName),
            emptyName),  theTypeTree :: Nil) if (
            collectionName.toString == "collection" && immutableName.toString == "immutable" && setName.toString == "Set" && emptyName.toString == "empty"
          ) => Some(theTypeTree)
        case TypeApply(Select(Select(Select(This(scalaName), predefName), setname), applyName), theTypeTree :: Nil)  if ("scala".equals(scalaName.toString) && "Predef".equals(predefName.toString) && "empty".equals(applyName.toString)) => Some(theTypeTree)
        case _ => None
      }
    }

    object ExEmptyMultiset {
      def unapply(tree: TypeApply): Option[Tree] = tree match {
        case TypeApply(
          Select(
            Select(
              Select(
                Select(Ident(s), collectionName),
                immutableName),
              setName),
            emptyName),  theTypeTree :: Nil) if (
            collectionName.toString == "collection" && immutableName.toString == "immutable" && setName.toString == "Multiset" && emptyName.toString == "empty"
          ) => Some(theTypeTree)
        case _ => None
      }
    }

    object ExLiteralMap {
      def unapply(tree: Apply): Option[(Tree, Tree, Seq[Tree])] = tree match {
        case Apply(TypeApply(ExSelected("scala", "Predef", "Map", "apply"), fromTypeTree :: toTypeTree :: Nil), args) =>
          Some((fromTypeTree, toTypeTree, args))
        case _ =>
          None
      }
    }
    object ExEmptyMap {
      def unapply(tree: TypeApply): Option[(Tree, Tree)] = tree match {
        case TypeApply(ExSelected("scala", "collection", "immutable", "Map", "empty"), fromTypeTree :: toTypeTree :: Nil) =>
          Some((fromTypeTree, toTypeTree))
        case TypeApply(ExSelected("scala", "Predef", "Map", "empty"), fromTypeTree :: toTypeTree :: Nil) =>
          Some((fromTypeTree, toTypeTree))
        case _ =>
          None
      }
    }

    object ExFiniteSet {
      def unapply(tree: Apply): Option[(Tree,List[Tree])] = tree match {
        case Apply(TypeApply(Select(Select(Select(Select(Ident(s), collectionName), immutableName), setName), applyName), theTypeTree :: Nil), args) if (collectionName.toString == "collection" && immutableName.toString == "immutable" && setName.toString == "Set" && applyName.toString == "apply") => Some((theTypeTree, args))
        case Apply(TypeApply(Select(Select(Select(This(scalaName), predefName), setName), applyName), theTypeTree :: Nil), args) if ("scala".equals(scalaName.toString) && "Predef".equals(predefName.toString) && setName.toString == "Set" && "apply".equals(applyName.toString)) => Some((theTypeTree, args))
        case _ => None
      }
    }

    object ExFiniteMultiset {
      def unapply(tree: Apply): Option[(Tree,List[Tree])] = tree match {
        case Apply(
          TypeApply(
            Select(
              Select(
                Select(
                  Select(Ident(s), collectionName),
                  immutableName),
                setName),
              emptyName),  theTypeTree :: Nil), args) if (
              collectionName.toString == "collection" && immutableName.toString == "immutable" && setName.toString == "Multiset" && emptyName.toString == "apply"
            )=> Some(theTypeTree, args)
        case _ => None
      }
    }

    object ExParameterLessCall {
      def unapply(tree: Tree): Option[(Tree, Symbol, Seq[Tree])] = tree match {
        case s @ Select(t, _) =>
          Some((t, s.symbol, Nil))

        case TypeApply(s @ Select(t, _), tps) =>
          Some((t, s.symbol, tps))

        case TypeApply(i: Ident, tps) =>
          Some((i, i.symbol, tps))

        case _ =>
          None
      }
    }

    object ExCall { 
      def unapply(tree: Tree): Option[(Tree, Symbol, Seq[Tree], Seq[Tree])] = tree match {
        // foo / foo[T]
        case ExParameterLessCall(t, s, tps) =>
          Some((t, s, tps, Nil))

        // foo(args)
        case Apply(i: Ident, args) =>
          Some((i, i.symbol, Nil, args))

        // foo(args1)(args2)
        case Apply(Apply(i: Ident, args1), args2) =>
          Some((i, i.symbol, Nil, args1 ++ args2))

        // foo[T](args)
        case Apply(ExParameterLessCall(t, s, tps), args) =>
          Some((t, s, tps, args))

        // foo[T](args1)(args2)
        case Apply(Apply(ExParameterLessCall(t, s, tps), args1), args2) =>
          Some((t, s, tps, args1 ++ args2))

        case _ => None
      }
    }

    object ExUpdate {
      def unapply(tree: Apply): Option[(Tree, Tree, Tree)] = tree match {
        case Apply(
              s @ Select(lhs, update),
              index :: newValue :: Nil) if(s.symbol.fullName.endsWith("Array.update")) => 
            Some((lhs, index, newValue))
        case _ => None
      }
    }

    object ExArrayFill {
      def unapply(tree: Apply): Option[(Tree, Tree, Tree)] = tree match {
        case Apply(
               Apply(
                 Apply(
                   TypeApply(ExSelected("scala", "Array", "fill"), baseType :: Nil),
                   length :: Nil
                 ),
                 defaultValue :: Nil
               ),
               manifest
             ) =>
            Some((baseType, length, defaultValue))
        case _ => None
      }
    }

  }
}
