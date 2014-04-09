/* Copyright 2009-2014 EPFL, Lausanne */

package leon
package codegen

import purescala.Common._
import purescala.Definitions._
import purescala.Trees._
import purescala.TypeTrees._
import utils._

import cafebabe._
import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import cafebabe.ClassFileTypes._
import cafebabe.Defaults.constructorName
import cafebabe.Flags._

trait CodeGeneration {
  self: CompilationUnit =>

  case class Locals(vars: Map[Identifier, Int]) {
    def varToLocal(v: Identifier): Option[Int] = vars.get(v)

    def withVars(newVars: Map[Identifier, Int]) = {
      Locals(vars ++ newVars)
    }

    def withVar(nv: (Identifier, Int)) = {
      Locals(vars + nv)
    }
  }

  object NoLocals extends Locals(Map())

  private[codegen] val BoxedIntClass             = "java/lang/Integer"
  private[codegen] val BoxedBoolClass            = "java/lang/Boolean"
  private[codegen] val BoxedArrayClass           = "leon/codegen/runtime/ArrayBox"

  private[codegen] val TupleClass                = "leon/codegen/runtime/Tuple"
  private[codegen] val SetClass                  = "leon/codegen/runtime/Set"
  private[codegen] val MapClass                  = "leon/codegen/runtime/Map"
  private[codegen] val CaseClassClass            = "leon/codegen/runtime/CaseClass"
  private[codegen] val ErrorClass                = "leon/codegen/runtime/LeonCodeGenRuntimeException"
  private[codegen] val ImpossibleEvaluationClass = "leon/codegen/runtime/LeonCodeGenEvaluationException"
  private[codegen] val HashingClass              = "leon/codegen/runtime/LeonCodeGenRuntimeHashing"
  private[codegen] val ChooseEntryPointClass     = "leon/codegen/runtime/ChooseEntryPoint"
  private[codegen] val MonitorClass              = "leon/codegen/runtime/LeonCodeGenRuntimeMonitor"

  def idToSafeJVMName(id: Identifier) = id.uniqueName.replaceAll("\\.", "\\$")
  def defToJVMName(d : Definition) : String = "Leon$CodeGen$" + idToSafeJVMName(d.id)

  def typeToJVM(tpe : TypeTree) : String = tpe match {
    case Int32Type => "I"

    case BooleanType => "Z"

    case UnitType => "Z"

    case c : ClassType =>
      leonClassToJVMInfo(c.classDef).map { case (n, _) => "L" + n + ";" }.getOrElse("Unsupported class " + c.id)

    case _ : TupleType =>
      "L" + TupleClass + ";"

    case _ : SetType =>
      "L" + SetClass + ";"

    case _ : MapType =>
      "L" + MapClass + ";"

    case ArrayType(base) =>
      "[" + typeToJVM(base)

    case TypeParameter(_) =>
      "Ljava/lang/Object;"

    case _ => throw CompilationException("Unsupported type : " + tpe)
  }

  // Assumes the CodeHandler has never received any bytecode.
  // Generates method body, and freezes the handler at the end.
  def compileFunDef(funDef : FunDef, ch : CodeHandler) {
    val newMapping = if (params.requireMonitor) {
        funDef.params.map(_.id).zipWithIndex.toMap.mapValues(_ + 1)
      } else {
        funDef.params.map(_.id).zipWithIndex.toMap
      }

    val body = funDef.body.getOrElse(throw CompilationException("Can't compile a FunDef without body: "+funDef.id.name))

    val bodyWithPre = if(funDef.hasPrecondition && params.checkContracts) {
      IfExpr(funDef.precondition.get, body, Error("Precondition failed"))
    } else {
      body
    }

    val bodyWithPost = if(funDef.hasPostcondition && params.checkContracts) {
      val Some((id, post)) = funDef.postcondition
      Let(id, bodyWithPre, IfExpr(post, Variable(id), Error("Postcondition failed")) )
    } else {
      bodyWithPre
    }

    val exprToCompile = purescala.TreeOps.matchToIfThenElse(bodyWithPost)

    if (params.recordInvocations) {
      ch << ALoad(0) << InvokeVirtual(MonitorClass, "onInvoke", "()V")
    }

    mkExpr(exprToCompile, ch)(Locals(newMapping))

    funDef.returnType match {
      case Int32Type | BooleanType | UnitType =>
        ch << IRETURN

      case _ : ClassType | _ : TupleType | _ : SetType | _ : MapType | _ : ArrayType | _: TypeParameter =>
        ch << ARETURN

      case other =>
        throw CompilationException("Unsupported return type : " + other.getClass)
    }

    ch.freeze
  }

  private[codegen] def mkExpr(e: Expr, ch: CodeHandler, canDelegateToMkBranch: Boolean = true)(implicit locals: Locals) {
    e match {
      case Variable(id) =>
        val slot = slotFor(id)
        val instr = id.getType match {
          case Int32Type | BooleanType | UnitType => ILoad(slot)
          case _ => ALoad(slot)
        }
        ch << instr

      case Let(i,d,b) =>
        mkExpr(d, ch)
        val slot = ch.getFreshVar
        val instr = i.getType match {
          case Int32Type | BooleanType | UnitType => IStore(slot)
          case _ => AStore(slot)
        }
        ch << instr
        mkExpr(b, ch)(locals.withVar(i -> slot))

      case LetTuple(is,d,b) =>
        mkExpr(d, ch) // the tuple
        var count = 0
        val withSlots = is.map(i => (i, ch.getFreshVar))
        for((i,s) <- withSlots) {
          ch << DUP
          ch << Ldc(count)
          ch << InvokeVirtual(TupleClass, "get", "(I)Ljava/lang/Object;")
          mkUnbox(i.getType, ch)
          val instr = i.getType match {
            case Int32Type | BooleanType | UnitType => IStore(s)
            case _ => AStore(s)
          }
          ch << instr
          count += 1
        }
        ch << POP
        mkExpr(b, ch)(locals.withVars(withSlots.toMap))

      case IntLiteral(v) =>
        ch << Ldc(v)

      case BooleanLiteral(v) =>
        ch << Ldc(if(v) 1 else 0)

      case UnitLiteral() =>
        ch << Ldc(1)

      // Case classes
      case CaseClass(cct, as) =>
        val (ccName, ccApplySig) = leonClassToJVMInfo(cct.classDef).getOrElse {
          throw CompilationException("Unknown class : " + cct.id)
        }
        ch << New(ccName) << DUP
        for((a, vd) <- as zip cct.classDef.fields) {
          vd.tpe match {
            case TypeParameter(_) =>
              mkBoxedExpr(a, ch)
            case _ =>
              mkExpr(a, ch)
          }
        }
        ch << InvokeSpecial(ccName, constructorName, ccApplySig)

      case CaseClassInstanceOf(cct, e) =>
        val (ccName, _) = leonClassToJVMInfo(cct.classDef).getOrElse {
          throw CompilationException("Unknown class : " + cct.id)
        }
        mkExpr(e, ch)
        ch << InstanceOf(ccName)

      case CaseClassSelector(cct, e, sid) =>
        mkExpr(e, ch)
        val (ccName, _) = leonClassToJVMInfo(cct.classDef).getOrElse {
          throw CompilationException("Unknown class : " + cct.id)
        }
        ch << CheckCast(ccName)
        instrumentedGetField(ch, cct, sid)

      // Tuples (note that instanceOf checks are in mkBranch)
      case Tuple(es) =>
        ch << New(TupleClass) << DUP
        ch << Ldc(es.size)
        ch << NewArray("java/lang/Object")
        for((e,i) <- es.zipWithIndex) {
          ch << DUP
          ch << Ldc(i)
          mkBoxedExpr(e, ch)
          ch << AASTORE
        }
        ch << InvokeSpecial(TupleClass, constructorName, "([Ljava/lang/Object;)V")

      case TupleSelect(t, i) =>
        val TupleType(bs) = t.getType
        mkExpr(t,ch)
        ch << Ldc(i - 1)
        ch << InvokeVirtual(TupleClass, "get", "(I)Ljava/lang/Object;")
        mkUnbox(bs(i - 1), ch)

      // Sets
      case FiniteSet(es) =>
        ch << DefaultNew(SetClass)
        for(e <- es) {
          ch << DUP
          mkBoxedExpr(e, ch)
          ch << InvokeVirtual(SetClass, "add", "(Ljava/lang/Object;)V")
        }

      case ElementOfSet(e, s) =>
        mkExpr(s, ch)
        mkBoxedExpr(e, ch)
        ch << InvokeVirtual(SetClass, "contains", "(Ljava/lang/Object;)Z")

      case SetCardinality(s) =>
        mkExpr(s, ch)
        ch << InvokeVirtual(SetClass, "size", "()I")

      case SubsetOf(s1, s2) =>
        mkExpr(s1, ch)
        mkExpr(s2, ch)
        ch << InvokeVirtual(SetClass, "subsetOf", "(L%s;)Z".format(SetClass))

      case SetIntersection(s1, s2) =>
        mkExpr(s1, ch)
        mkExpr(s2, ch)
        ch << InvokeVirtual(SetClass, "intersect", "(L%s;)L%s;".format(SetClass,SetClass))

      case SetUnion(s1, s2) =>
        mkExpr(s1, ch)
        mkExpr(s2, ch)
        ch << InvokeVirtual(SetClass, "union", "(L%s;)L%s;".format(SetClass,SetClass))

      case SetDifference(s1, s2) =>
        mkExpr(s1, ch)
        mkExpr(s2, ch)
        ch << InvokeVirtual(SetClass, "minus", "(L%s;)L%s;".format(SetClass,SetClass))

      // Maps
      case FiniteMap(ss) =>
        ch << DefaultNew(MapClass)
        for((f,t) <- ss) {
          ch << DUP
          mkBoxedExpr(f, ch)
          mkBoxedExpr(t, ch)
          ch << InvokeVirtual(MapClass, "add", "(Ljava/lang/Object;Ljava/lang/Object;)V")
        }

      case MapGet(m, k) =>
        val MapType(_, tt) = m.getType
        mkExpr(m, ch)
        mkBoxedExpr(k, ch)
        ch << InvokeVirtual(MapClass, "get", "(Ljava/lang/Object;)Ljava/lang/Object;")
        mkUnbox(tt, ch)

      case MapIsDefinedAt(m, k) =>
        mkExpr(m, ch)
        mkBoxedExpr(k, ch)
        ch << InvokeVirtual(MapClass, "isDefinedAt", "(Ljava/lang/Object;)Z")

      case MapUnion(m1, m2) =>
        mkExpr(m1, ch)
        mkExpr(m2, ch)
        ch << InvokeVirtual(MapClass, "union", "(L%s;)L%s;".format(MapClass,MapClass))

      // Branching
      case IfExpr(c, t, e) =>
        val tl = ch.getFreshLabel("then")
        val el = ch.getFreshLabel("else")
        val al = ch.getFreshLabel("after")
        mkBranch(c, tl, el, ch)
        ch << Label(tl)
        mkExpr(t, ch)
        ch << Goto(al) << Label(el)
        mkExpr(e, ch)
        ch << Label(al)

      case FunctionInvocation(tfd, as) =>
        val (cn, mn, ms) = leonFunDefToJVMInfo(tfd.fd).getOrElse {
          throw CompilationException("Unknown method : " + tfd.id)
        }

        if (params.requireMonitor) {
          ch << ALoad(0)
        }

        for((a, vd) <- as zip tfd.fd.params) {
          vd.tpe match {
            case TypeParameter(_) =>
              mkBoxedExpr(a, ch)
            case _ =>
              mkExpr(a, ch)
          }
        }

        ch << InvokeStatic(cn, mn, ms)

        (tfd.fd.returnType, tfd.returnType) match {
          case (TypeParameter(_), tpe)  =>
            mkUnbox(tpe, ch)
          case _ =>
        }

      // Arithmetic
      case Plus(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << IADD

      case Minus(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << ISUB

      case Times(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << IMUL

      case Division(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << IDIV

      case Modulo(l, r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << IREM

      case UMinus(e) =>
        mkExpr(e, ch)
        ch << INEG

      case ArrayLength(a) =>
        mkExpr(a, ch)
        ch << ARRAYLENGTH

      case as @ ArraySelect(a,i) =>
        mkExpr(a, ch)
        mkExpr(i, ch)
        ch << (as.getType match {
          case Untyped => throw CompilationException("Cannot compile untyped array access.")
          case Int32Type => IALOAD
          case BooleanType => BALOAD
          case _ => AALOAD
        })

      case au @ ArrayUpdated(a, i, v) =>
        mkExpr(a, ch)
        ch << DUP
        ch << ARRAYLENGTH
        val storeInstr = a.getType match {
          case ArrayType(Int32Type) => ch << NewArray.primitive("T_INT"); IASTORE
          case ArrayType(BooleanType) => ch << NewArray.primitive("T_BOOLEAN"); BASTORE
          case ArrayType(other) => ch << NewArray(typeToJVM(other)); AASTORE
          case other => throw CompilationException("Cannot compile finite array expression whose type is %s.".format(other))
        } 
        //srcArrary and targetArray is on the stack
        ch << DUP_X1 //insert targetArray under srcArray
        ch << Ldc(0) << SWAP //srcArray, 0, targetArray
        ch << DUP << ARRAYLENGTH //targetArray, length on stack
        ch << Ldc(0) << SWAP //final arguments: src, 0, target, 0, length
        ch << InvokeStatic("java/lang/System", "arraycopy", "(Ljava/lang/Object;ILjava/lang/Object;II)V")

        //targetArray remains on the stack
        ch << DUP
        mkExpr(i, ch)
        mkExpr(v, ch)
        ch << storeInstr
        //returns targetArray

      case a @ FiniteArray(es) =>
        ch << Ldc(es.size)
        val storeInstr = a.getType match {
          case ArrayType(Int32Type) => ch << NewArray.primitive("T_INT"); IASTORE
          case ArrayType(BooleanType) => ch << NewArray.primitive("T_BOOLEAN"); BASTORE
          case ArrayType(other) => ch << NewArray(typeToJVM(other)); AASTORE
          case other => throw CompilationException("Cannot compile finite array expression whose type is %s.".format(other))
        }
        for((e,i) <- es.zipWithIndex) {
          ch << DUP << Ldc(i)
          mkExpr(e, ch) 
          ch << storeInstr
        }

      // Misc and boolean tests
      case Error(desc) =>
        ch << New(ErrorClass) << DUP
        ch << Ldc(desc)
        ch << InvokeSpecial(ErrorClass, constructorName, "(Ljava/lang/String;)V")
        ch << ATHROW

      case hole @ Hole(oracle) =>
        mkExpr(OracleTraverser(oracle, hole.getType, program).value, ch)

      case choose @ Choose(_, _) =>
        val prob = synthesis.Problem.fromChoose(choose)

        val id = runtime.ChooseEntryPoint.register(prob, this);
        ch << Ldc(id)


        ch << Ldc(prob.as.size)
        ch << NewArray("java/lang/Object")

        for ((id, i) <- prob.as.zipWithIndex) {
          ch << DUP
          ch << Ldc(i)
          mkExpr(Variable(id), ch)
          mkBox(id.getType, ch)
          ch << AASTORE
        }

        ch << InvokeStatic(ChooseEntryPointClass, "invoke", "(I[Ljava/lang/Object;)Ljava/lang/Object;")

        mkUnbox(choose.getType, ch)

      case b if b.getType == BooleanType && canDelegateToMkBranch =>
        val fl = ch.getFreshLabel("boolfalse")
        val al = ch.getFreshLabel("boolafter")
        ch << Ldc(1)
        mkBranch(b, al, fl, ch, canDelegateToMkExpr = false)
        ch << Label(fl) << POP << Ldc(0) << Label(al)

      case _ => throw CompilationException("Unsupported expr. : " + e) 
    }
  }

  // Leaves on the stack a value equal to `e`, always of a type compatible with java.lang.Object.
  private[codegen] def mkBoxedExpr(e: Expr, ch: CodeHandler)(implicit locals: Locals) {
    e.getType match {
      case Int32Type =>
        ch << New(BoxedIntClass) << DUP
        mkExpr(e, ch)
        ch << InvokeSpecial(BoxedIntClass, constructorName, "(I)V")

      case BooleanType | UnitType =>
        ch << New(BoxedBoolClass) << DUP
        mkExpr(e, ch)
        ch << InvokeSpecial(BoxedBoolClass, constructorName, "(Z)V")

      case at @ ArrayType(et) =>
        ch << New(BoxedArrayClass) << DUP
        mkExpr(e, ch)
        ch << InvokeSpecial(BoxedArrayClass, constructorName, "(%s)V".format(typeToJVM(at)))

      case _ =>
        mkExpr(e, ch)
    }
  }

  // Assumes the top of the stack contains of value of the right type, and makes it
  // compatible with java.lang.Object.
  private[codegen] def mkBox(tpe: TypeTree, ch: CodeHandler)(implicit locals: Locals) {
    tpe match {
      case Int32Type =>
        ch << New(BoxedIntClass) << DUP_X1 << SWAP << InvokeSpecial(BoxedIntClass, constructorName, "(I)V")

      case BooleanType | UnitType =>
        ch << New(BoxedBoolClass) << DUP_X1 << SWAP << InvokeSpecial(BoxedBoolClass, constructorName, "(Z)V")

      case at @ ArrayType(et) =>
        ch << New(BoxedArrayClass) << DUP_X1 << SWAP << InvokeSpecial(BoxedArrayClass, constructorName, "(%s)V".format(typeToJVM(at)))
      case _ =>
    }
  }

  // Assumes that the top of the stack contains a value that should be of type `tpe`, and unboxes it to the right (JVM) type.
  private[codegen] def mkUnbox(tpe: TypeTree, ch: CodeHandler)(implicit locals: Locals) {
    tpe match {
      case Int32Type =>
        ch << CheckCast(BoxedIntClass) << InvokeVirtual(BoxedIntClass, "intValue", "()I")

      case BooleanType | UnitType =>
        ch << CheckCast(BoxedBoolClass) << InvokeVirtual(BoxedBoolClass, "booleanValue", "()Z")

      case ct : ClassType =>
        val (cn, _) = leonClassToJVMInfo(ct.classDef).getOrElse {
          throw new CompilationException("Unsupported class type : " + ct)
        }
        ch << CheckCast(cn)

      case tt : TupleType =>
        ch << CheckCast(TupleClass)

      case st : SetType =>
        ch << CheckCast(SetClass)

      case mt : MapType =>
        ch << CheckCast(MapClass)

      case tp : TypeParameter =>

      case tp : ArrayType =>
        ch << CheckCast(BoxedArrayClass) << InvokeVirtual(BoxedArrayClass, "arrayValue", "()%s".format(typeToJVM(tp)))
        ch << CheckCast(typeToJVM(tp))

      case _ =>
        throw new CompilationException("Unsupported type in unboxing : " + tpe)
    }
  }

  private[codegen] def mkBranch(cond: Expr, thenn: String, elze: String, ch: CodeHandler, canDelegateToMkExpr: Boolean = true)(implicit locals: Locals) {
    cond match {
      case BooleanLiteral(true) =>
        ch << Goto(thenn)

      case BooleanLiteral(false) =>
        ch << Goto(elze)

      case And(es) =>
        val fl = ch.getFreshLabel("andnext")
        mkBranch(es.head, fl, elze, ch)
        ch << Label(fl)
        mkBranch(And(es.tail), thenn, elze, ch)

      case Or(es) =>
        val fl = ch.getFreshLabel("ornext")
        mkBranch(es.head, thenn, fl, ch)
        ch << Label(fl)
        mkBranch(Or(es.tail), thenn, elze, ch) 

      case Implies(l, r) =>
        mkBranch(Or(Not(l), r), thenn, elze, ch)

      case Not(c) =>
        mkBranch(c, elze, thenn, ch)

      case Variable(b) =>
        ch << ILoad(slotFor(b)) << IfEq(elze) << Goto(thenn)

      case Equals(l,r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        l.getType match {
          case Int32Type | BooleanType | UnitType =>
            ch << If_ICmpEq(thenn) << Goto(elze)

          case _ =>
            ch << InvokeVirtual("java/lang/Object", "equals", "(Ljava/lang/Object;)Z")
            ch << IfEq(elze) << Goto(thenn)
        }

      case Iff(l,r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << If_ICmpEq(thenn) << Goto(elze)

      case LessThan(l,r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << If_ICmpLt(thenn) << Goto(elze) 

      case GreaterThan(l,r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << If_ICmpGt(thenn) << Goto(elze) 

      case LessEquals(l,r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << If_ICmpLe(thenn) << Goto(elze) 

      case GreaterEquals(l,r) =>
        mkExpr(l, ch)
        mkExpr(r, ch)
        ch << If_ICmpGe(thenn) << Goto(elze) 

      case other if canDelegateToMkExpr =>
        mkExpr(other, ch, canDelegateToMkBranch = false)
        ch << IfEq(elze) << Goto(thenn)

      case other => throw CompilationException("Unsupported branching expr. : " + other) 
    }
  }

  private[codegen] def slotFor(id: Identifier)(implicit locals: Locals) : Int = {
    locals.varToLocal(id).getOrElse {
      throw CompilationException("Unknown variable: " + id)
    }
  }

  def compileAbstractClassDef(acd : AbstractClassDef) {
    val cName = defToJVMName(acd)

    val cf  = classes(acd)

    cf.setFlags((
      CLASS_ACC_SUPER |
      CLASS_ACC_PUBLIC |
      CLASS_ACC_ABSTRACT
    ).asInstanceOf[U2])

    cf.addInterface(CaseClassClass)

    cf.addDefaultConstructor
  }

  /**
   * Instrument read operations
   */
  val instrumentedField = "__read"

  def instrumentedGetField(ch: CodeHandler, cct: CaseClassType, id: Identifier)(implicit locals: Locals): Unit = {
    val ccd = cct.classDef

    ccd.fields.zipWithIndex.find(_._1.id == id) match {
      case Some((f, i)) =>
        val expType = cct.fields(i).tpe

        val cName = defToJVMName(ccd)
        if (params.doInstrument) {
          ch << DUP << DUP
          ch << GetField(cName, instrumentedField, "I")
          ch << Ldc(1)
          ch << Ldc(i)
          ch << ISHL
          ch << IOR
          ch << PutField(cName, instrumentedField, "I")
        }
        ch << GetField(cName, f.id.name, typeToJVM(f.tpe))

        f.tpe match {
          case TypeParameter(_) =>
            mkUnbox(expType, ch)
          case _ =>
        }
      case None =>
        throw CompilationException("Unknown field: "+ccd.id.name+"."+id)
    }
  }

  def compileCaseClassDef(ccd: CaseClassDef) {

    val cName = defToJVMName(ccd)
    val pName = ccd.parent.map(parent => defToJVMName(parent.classDef))
    val cct = CaseClassType(ccd, ccd.tparams.map(_.tp))

    val cf = classes(ccd)

    cf.setFlags((
      CLASS_ACC_SUPER |
      CLASS_ACC_PUBLIC |
      CLASS_ACC_FINAL
    ).asInstanceOf[U2])

    if(ccd.parent.isEmpty) {
      cf.addInterface(CaseClassClass)
    }

    val namesTypes = ccd.fields.map { vd => (vd.id.name, typeToJVM(vd.tpe)) }

    // definition of the constructor
    if(!params.doInstrument && ccd.fields.isEmpty) {
      cf.addDefaultConstructor
    } else {
      for((nme, jvmt) <- namesTypes) {
        val fh = cf.addField(jvmt, nme)
        fh.setFlags((
          FIELD_ACC_PUBLIC |
          FIELD_ACC_FINAL
        ).asInstanceOf[U2])
      }

      if (params.doInstrument) {
        val fh = cf.addField("I", instrumentedField)
        fh.setFlags(FIELD_ACC_PUBLIC)
      }

      val cch = cf.addConstructor(namesTypes.map(_._2).toList).codeHandler

      cch << ALoad(0)
      cch << InvokeSpecial(pName.getOrElse("java/lang/Object"), constructorName, "()V")

      if (params.doInstrument) {
        cch << ALoad(0)
        cch << Ldc(0)
        cch << PutField(cName, instrumentedField, "I")
      }

      var c = 1
      for((nme, jvmt) <- namesTypes) {
        cch << ALoad(0)
        cch << (jvmt match {
          case "I" | "Z" => ILoad(c)
          case _ => ALoad(c)
        })
        cch << PutField(cName, nme, jvmt)
        c += 1
      }
      cch << RETURN
      cch.freeze
    }

    locally {
      val pnm = cf.addMethod("I", "__getRead")
      pnm.setFlags((
        METHOD_ACC_PUBLIC |
        METHOD_ACC_FINAL
      ).asInstanceOf[U2])

      val pnch = pnm.codeHandler

      pnch << ALoad(0) << GetField(cName, instrumentedField, "I") << IRETURN

      pnch.freeze
    }

    locally {
      val pnm = cf.addMethod("Ljava/lang/String;", "productName")
      pnm.setFlags((
        METHOD_ACC_PUBLIC |
        METHOD_ACC_FINAL
      ).asInstanceOf[U2])

      val pnch = pnm.codeHandler

      pnch << Ldc(cName) << ARETURN

      pnch.freeze
    }

    locally {
      val pem = cf.addMethod("[Ljava/lang/Object;", "productElements")
      pem.setFlags((
        METHOD_ACC_PUBLIC |
        METHOD_ACC_FINAL
      ).asInstanceOf[U2])

      val pech = pem.codeHandler

      pech << Ldc(ccd.fields.size)
      pech << NewArray("java/lang/Object")

      for ((f, i) <- ccd.fields.zipWithIndex) {
        pech << DUP
        pech << Ldc(i)
        pech << ALoad(0)
        instrumentedGetField(pech, cct, f.id)(NoLocals)
        mkBox(f.tpe, pech)(NoLocals)
        pech << AASTORE
      }

      pech << ARETURN
      pech.freeze
    }

    // definition of equals
    locally {
      val emh = cf.addMethod("Z", "equals", "Ljava/lang/Object;")
      emh.setFlags((
        METHOD_ACC_PUBLIC |
        METHOD_ACC_FINAL
      ).asInstanceOf[U2])

      val ech = emh.codeHandler

      val notRefEq = ech.getFreshLabel("notrefeq")
      val notEq = ech.getFreshLabel("noteq")
      val castSlot = ech.getFreshVar

      // If references are equal, trees are equal.
      ech << ALoad(0) << ALoad(1) << If_ACmpNe(notRefEq) << Ldc(1) << IRETURN << Label(notRefEq)

      // We check the type (this also checks against null)....
      ech << ALoad(1) << InstanceOf(cName) << IfEq(notEq)

      // ...finally, we compare fields one by one, shortcircuiting on disequalities.
      if(!ccd.fields.isEmpty) {
        ech << ALoad(1) << CheckCast(cName) << AStore(castSlot)

        for(vd <- ccd.fields) {
          ech << ALoad(0)
          instrumentedGetField(ech, cct, vd.id)(NoLocals)
          ech << ALoad(castSlot)
          instrumentedGetField(ech, cct, vd.id)(NoLocals)

          typeToJVM(vd.id.getType) match {
            case "I" | "Z" =>
              ech << If_ICmpNe(notEq)

            case ot =>
              ech << InvokeVirtual("java/lang/Object", "equals", "(Ljava/lang/Object;)Z") << IfEq(notEq)
          }
        }
      } 

      ech << Ldc(1) << IRETURN << Label(notEq) << Ldc(0) << IRETURN
      ech.freeze
    }

    // definition of hashcode
    locally {
      val hashFieldName = "$leon$hashCode"
      cf.addField("I", hashFieldName).setFlags((FIELD_ACC_PRIVATE).asInstanceOf[U2])
      val hmh = cf.addMethod("I", "hashCode", "")
      hmh.setFlags((
        METHOD_ACC_PUBLIC |
        METHOD_ACC_FINAL
      ).asInstanceOf[U2])

      val hch = hmh.codeHandler
      
      val wasNotCached = hch.getFreshLabel("wasNotCached")

      hch << ALoad(0) << GetField(cName, hashFieldName, "I") << DUP
      hch << IfEq(wasNotCached)
      hch << IRETURN
      hch << Label(wasNotCached) << POP
      hch << ALoad(0) << InvokeVirtual(cName, "productElements", "()[Ljava/lang/Object;")
      hch << ALoad(0) << InvokeVirtual(cName, "productName", "()Ljava/lang/String;")
      hch << InvokeVirtual("java/lang/String", "hashCode", "()I")
      hch << InvokeStatic(HashingClass, "seqHash", "([Ljava/lang/Object;I)I") << DUP
      hch << ALoad(0) << SWAP << PutField(cName, hashFieldName, "I") 
      hch << IRETURN
      
      hch.freeze
    }

  }
}
