sealed trait FuzzyClassOperation

case class Class(
                  name: String,
                  superClass: Option[Class] = None,
                  methods: List[Method] = Nil,
                  vars: List[ClassVar] = Nil
                ) extends FuzzyClassOperation

case class ClassVar(name: String, varType: VarType) extends FuzzyClassOperation
case class Method(name: String, params: List[Parameter], body: FuzzyOperation) extends FuzzyClassOperation
case class Parameter(name: String, paramType: String)
case class CreateNew(clazz: Class, instanceName: String) extends FuzzyExpression {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = this
}

sealed trait VarType
case object StringType extends VarType
case object DoubleType extends VarType
case object SetType extends VarType

sealed trait FuzzyExpression {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression
}

sealed trait FuzzyOperation extends FuzzyExpression
sealed trait FuzzyGateOperation extends FuzzyOperation
sealed trait FuzzySetExpression extends FuzzyExpression

sealed trait FuzzySetOperation extends FuzzySetExpression with FuzzyOperation

sealed trait FuzzyValue extends FuzzyExpression

case class FuzzyNumber(value: Double) extends FuzzyValue {
  override def equals(obj: Any): Boolean = {
    obj match {
      case FuzzyNumber(v) => Math.abs(value - v) < 1e-6
      case _ => false
    }
  }
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = this
}

case class FuzzyString(value: String) extends FuzzyValue {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = this
}





case class Input(name: String) extends FuzzyGateOperation {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    env.get(name) match {
      case Some(expr) if expr != this && !expr.isInstanceOf[Input] => expr.partialEvaluate(env)
      case Some(expr) => expr
      case None => this
    }
  }
}
case class ADD(a: FuzzyExpression, b: FuzzyExpression) extends FuzzyGateOperation {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    val left = a.partialEvaluate(env)
    val right = b.partialEvaluate(env)
    (left, right) match {
      case (FuzzyNumber(v1), FuzzyNumber(v2)) => FuzzyNumber(math.min(v1 + v2, 1.0))
      case _ => ADD(left, right)
    }
  }
}

case class MULT(a: FuzzyExpression, b: FuzzyExpression) extends FuzzyGateOperation {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    val left = a.partialEvaluate(env)
    val right = b.partialEvaluate(env)
    (left, right) match {
      case (FuzzyNumber(v1), FuzzyNumber(v2)) =>
        FuzzyNumber(v1 * v2)
      case _ => MULT(left, right)
    }
  }
}

case class XOR(a: FuzzyExpression, b: FuzzyExpression) extends FuzzyGateOperation {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    val left = a.partialEvaluate(env)
    val right = b.partialEvaluate(env)
    (left, right) match {
      case (FuzzyNumber(v1), FuzzyNumber(v2)) => FuzzyNumber(math.abs(v1 - v2))
      case _ => XOR(left, right)
    }
  }
}

case class IFTRUE(condition: FuzzyExpression, thenExec: THENEXECUTE, elseRun: ELSERUN) extends FuzzyExpression {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    val condEvaluated = condition.partialEvaluate(env)
    val thenEvaluated = thenExec.partialEvaluate(env).asInstanceOf[THENEXECUTE]
    val elseEvaluated = elseRun.partialEvaluate(env).asInstanceOf[ELSERUN]
    IFTRUE(condEvaluated, thenEvaluated, elseEvaluated)
  }
}

case class GREATER_EQUAL(a: FuzzyExpression, b: FuzzyExpression) extends FuzzyExpression {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    val left = a.partialEvaluate(env)
    val right = b.partialEvaluate(env)
    (left, right) match {
      case (FuzzyNumber(v1), FuzzyNumber(v2)) => FuzzyBoolean(v1 >= v2)
      case _ => GREATER_EQUAL(left, right)
    }
  }
}

case class THENEXECUTE(expr: FuzzyExpression) extends FuzzyExpression {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    val evaluatedExpr = expr.partialEvaluate(env)
    THENEXECUTE(evaluatedExpr)
  }
}

case class ELSERUN(expr: FuzzyExpression) extends FuzzyExpression {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    val evaluatedExpr = expr.partialEvaluate(env)
    ELSERUN(evaluatedExpr)
  }
}

case class GREATER_EQUAL_SET(a: FuzzySetExpression, b: FuzzySetExpression) extends FuzzyExpression {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    val left = a.partialEvaluate(env)
    val right = b.partialEvaluate(env)
    (left, right) match {
      case (FuzzySetValue(setA), FuzzySetValue(setB)) =>
        val cardinalityA = setA.elements.map(_.value).sum
        val cardinalityB = setB.elements.map(_.value).sum
        FuzzyBoolean(cardinalityA >= cardinalityB)
      case _ =>
        GREATER_EQUAL_SET(left.asInstanceOf[FuzzySetExpression], right.asInstanceOf[FuzzySetExpression])
    }
  }
}

case class FuzzyBoolean(value: Boolean) extends FuzzyValue {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = this
}

case class Assign(variable: Variable, expr: FuzzyExpression) extends FuzzyExpression {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    val evaluatedExpr = expr.partialEvaluate(env)
    Assign(variable, evaluatedExpr)
  }
}

case class Variable(name: String) extends FuzzyOperation {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    env.getOrElse(name, this)
  }
}

case class InvokeMethod(instance: FuzzyExpression, methodName: String, args: List[(String, FuzzyExpression)]) extends FuzzyExpression {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    val evaluatedInstance = instance.partialEvaluate(env)
    val evaluatedArgs = args.map { case (name, expr) => (name, expr.partialEvaluate(env)) }.toMap
    evaluatedInstance match {
      case CreateNew(clazz, instanceName) =>
        val methodOption = clazz.methods.find(_.name == methodName)
        methodOption match {
          case Some(method) =>
            val methodEnv = evaluatedArgs ++ env
            val instanceVars = clazz.vars.map { classVar =>
              val varName = s"${instanceName}.${classVar.name}"
              (classVar.name, env.getOrElse(varName, Variable(varName)))
            }.toMap
            val methodEnvWithInstance = methodEnv ++ instanceVars
            val evaluatedMethodBody = method.body.partialEvaluate(methodEnvWithInstance)
            evaluatedMethodBody
          case None => throw new IllegalArgumentException(s"Method $methodName not found in class ${clazz.name}")
        }
      case _ => throw new IllegalArgumentException(s"Cannot invoke method on non-class instance: $evaluatedInstance")
    }
  }
}

case class FuzzyGate(name: String, operation: FuzzyExpression)

object FuzzyOperations {
  def round(value: Double, precision: Int = 2): Double = {
    val scale = Math.pow(10, precision)
    Math.round(value * scale) / scale
  }

  def add(a: Double, b: Double): Double = Math.min(1.0, round(a + b))
  def mult(a: Double, b: Double): Double = round(a * b)
  def xor(a: Double, b: Double): Double = round(Math.abs(a - b))
}

object FuzzyGateEvaluator {
  def evaluateExpression(expr: FuzzyExpression, env: Map[String, FuzzyExpression]): (FuzzyExpression, Map[String, FuzzyExpression]) = expr match {
    case FuzzyNumber(value) => (FuzzyNumber(value), env)
    case Variable(name) =>
      val parts = name.split("\\.")
      if (parts.length == 2) {
        val instanceName = parts(0)
        val variableName = parts(1)
        val fullVarName = s"$instanceName.$variableName"
        val value = env.getOrElse(fullVarName, Variable(fullVarName))
        (value, env)
      } else {
        val value = env.getOrElse(name, Variable(name))
        (value, env)
      }
    case ADD(a, b) =>
      val (left, env1) = evaluateExpression(a, env)
      val (right, env2) = evaluateExpression(b, env1)
      (left, right) match {
        case (FuzzyNumber(v1), FuzzyNumber(v2)) =>
          (FuzzyNumber(FuzzyOperations.add(v1, v2)), env2)
        case _ =>
          (ADD(left, right), env2)
      }

    case MULT(a, b) =>
      val (left, env1) = evaluateExpression(a, env)
      val (right, env2) = evaluateExpression(b, env1)
      (left, right) match {
        case (FuzzyNumber(v1), FuzzyNumber(v2)) =>
          (FuzzyNumber(FuzzyOperations.mult(v1, v2)), env2)
        case _ =>
          (MULT(left, right), env2)
      }

    case XOR(a, b) =>
      val (left, env1) = evaluateExpression(a, env)
      val (right, env2) = evaluateExpression(b, env1)
      (left, right) match {
        case (FuzzyNumber(v1), FuzzyNumber(v2)) =>
          (FuzzyNumber(FuzzyOperations.xor(v1, v2)), env2)
        case _ =>
          (XOR(left, right), env2)
      }
    case GREATER_EQUAL(a, b) =>
      val (left, env1) = evaluateExpression(a, env)
      val (right, env2) = evaluateExpression(b, env1)
      (left, right) match {
        case (FuzzyNumber(v1), FuzzyNumber(v2)) => (FuzzyBoolean(v1 >= v2), env2)
        case _ => (GREATER_EQUAL(left, right), env2)
      }
    case GREATER_EQUAL_SET(a, b) =>
      val (left, env1) = evaluateSetExpression(a, env)
      val (right, env2) = evaluateSetExpression(b, env1)
      (left, right) match {
        case (FuzzySetValue(setA), FuzzySetValue(setB)) =>
          val cardinalityA = setA.elements.map(_.value).sum
          val cardinalityB = setB.elements.map(_.value).sum
          (FuzzyBoolean(cardinalityA >= cardinalityB), env2)
        case _ =>
          (GREATER_EQUAL_SET(left.asInstanceOf[FuzzySetExpression], right.asInstanceOf[FuzzySetExpression]), env2)
      }
    case IFTRUE(condition, thenExec, elseRun) =>
      val (condResult, env1) = evaluateExpression(condition, env)
      condResult match {
        case FuzzyBoolean(true) =>
          evaluateExpression(thenExec.expr, env1)
        case FuzzyBoolean(false) =>
          evaluateExpression(elseRun.expr, env1)
        case _ =>
          (IFTRUE(condResult, thenExec, elseRun), env1)
      }
    case Assign(variable, expr) =>
      val (evaluatedExpr, env1) = evaluateExpression(expr, env)
      val updatedEnv = env1 + (variable.name -> evaluatedExpr)
      (evaluatedExpr, updatedEnv)
    case THENEXECUTE(expr) =>
      evaluateExpression(expr, env)
    case ELSERUN(expr) =>
      evaluateExpression(expr, env)
    case InvokeMethod(instanceExpr, methodName, args) =>
      val (evaluatedInstance, env1) = evaluateExpression(instanceExpr, env)
      evaluatedInstance match {
        case instance: CreateNew =>
          val instanceName = instance.instanceName
          // Evaluate arguments
          val (evaluatedArgs, env2) = args.foldLeft((List.empty[(String, FuzzyExpression)], env1)) {
            case ((argList, envAcc), (argName, argExpr)) =>
              val (evaluatedArg, envNext) = evaluateExpression(argExpr, envAcc)
              (argList :+ (argName, evaluatedArg), envNext)
          }
          val method = findMethod(instance.clazz, methodName).getOrElse(
            throw new IllegalArgumentException(s"Method $methodName not found in class ${instance.clazz.name}")
          )
          val methodEnv = env2 ++ evaluatedArgs.toMap
          val instanceVars = instance.clazz.vars.map { classVar =>
            val varName = s"${instanceName}.${classVar.name}"
            (classVar.name, env2.getOrElse(varName, Variable(varName)))
          }.toMap
          val methodEnvWithInstance = methodEnv ++ instanceVars
          val (result, methodEnvUpdated) = evaluateExpression(method.body, methodEnvWithInstance)
          val updatedInstanceVars = methodEnvUpdated.collect {
            case (varName, value) if instanceVars.contains(varName) =>
              val instanceVarName = s"${instanceName}.${varName}"
              (instanceVarName, value)
          }
          val updatedEnv = env2 ++ updatedInstanceVars
          (result, updatedEnv)
        case _ =>
          throw new IllegalArgumentException(s"Cannot invoke method on non-class instance: $evaluatedInstance")
      }
    case setExpr: FuzzySetExpression =>
      evaluateSetExpression(setExpr, env)
    case Input(name) =>
      env.get(name) match {
        case Some(value) => (value, env)
        case None => (Input(name), env)
      }
    case _ => (expr, env)
  }

  def evaluate(gate: FuzzyGate, inputs: Map[String, FuzzyExpression]): FuzzyExpression = {
    def evalOperation(op: FuzzyExpression): FuzzyExpression = op match {
      case Input(name) =>
        inputs.getOrElse(name, Input(name))
      case FuzzyNumber(value) =>
        FuzzyNumber(value)
      case ADD(a, b) =>
        val left = evalOperation(a)
        val right = evalOperation(b)
        (left, right) match {
          case (FuzzyNumber(v1), FuzzyNumber(v2)) =>
            FuzzyNumber(FuzzyOperations.add(v1, v2))
          case _ => ADD(left, right)
        }
      case MULT(a, b) =>
        val left = evalOperation(a)
        val right = evalOperation(b)
        (left, right) match {
          case (FuzzyNumber(v1), FuzzyNumber(v2)) =>
            FuzzyNumber(FuzzyOperations.mult(v1, v2))
          case _ => MULT(left, right)
        }
      case XOR(a, b) =>
        val left = evalOperation(a)
        val right = evalOperation(b)
        (left, right) match {
          case (FuzzyNumber(v1), FuzzyNumber(v2)) =>
            FuzzyNumber(FuzzyOperations.xor(v1, v2))
          case _ => XOR(left, right)
        }
      // Add any additional cases if necessary
    }

    evalOperation(gate.operation)
  }

  def evaluateMethod(instance: CreateNew, methodName: String, args: Map[String, FuzzyExpression]): FuzzyExpression = {
    val clazz = instance.clazz
    val method = findMethod(clazz, methodName).getOrElse(throw new IllegalArgumentException(s"Method $methodName not found in class ${clazz.name}"))
    val scope = args
    evaluateMethodBody(method.body, scope)
  }

  private def findMethod(clazz: Class, methodName: String): Option[Method] = {
    clazz.methods.find(_.name == methodName).orElse {
      clazz.superClass.flatMap(findMethod(_, methodName))
    }
  }

  private def evaluateMethodBody(body: FuzzyExpression, scope: Map[String, FuzzyExpression]): FuzzyExpression = {
    body match {
      case op: FuzzyGateOperation =>
        evaluateGateOperation(op, scope)
      case _ =>
        throw new IllegalArgumentException("Unsupported operation in method body")
    }
  }

  private def evaluateGateOperation(op: FuzzyExpression, scope: Map[String, FuzzyExpression]): FuzzyExpression = op match {
    case Input(name) =>
      scope.getOrElse(name, Input(name))
    case FuzzyNumber(value) =>
      FuzzyNumber(value)
    case ADD(a, b) =>
      val left = evaluateGateOperation(a, scope)
      val right = evaluateGateOperation(b, scope)
      (left, right) match {
        case (FuzzyNumber(v1), FuzzyNumber(v2)) =>
          FuzzyNumber(FuzzyOperations.add(v1, v2))
        case _ => ADD(left, right)
      }
    case MULT(a, b) =>
      val left = evaluateGateOperation(a, scope)
      val right = evaluateGateOperation(b, scope)
      (left, right) match {
        case (FuzzyNumber(v1), FuzzyNumber(v2)) =>
          FuzzyNumber(FuzzyOperations.mult(v1, v2))
        case _ => MULT(left, right)
      }
    case XOR(a, b) =>
      val left = evaluateGateOperation(a, scope)
      val right = evaluateGateOperation(b, scope)
      (left, right) match {
        case (FuzzyNumber(v1), FuzzyNumber(v2)) =>
          FuzzyNumber(FuzzyOperations.xor(v1, v2))
        case _ => XOR(left, right)
      }
  }


  def evaluateSetExpression(expr: FuzzySetExpression, env: Map[String, FuzzyExpression]): (FuzzyExpression, Map[String, FuzzyExpression]) = expr match {
    case FuzzySetValue(set) =>
      (FuzzySetValue(set), env)

    case SetInput(name) =>
      env.get(name) match {
        case Some(setExpr: FuzzySetExpression) => evaluateSetExpression(setExpr, env)
        case _ => (SetInput(name), env)
      }

    case AddSets(a, b) =>
      val (left, env1) = evaluateSetExpression(a, env)
      val (right, env2) = evaluateSetExpression(b, env1)
      (left, right) match {
        case (FuzzySetValue(s1), FuzzySetValue(s2)) =>
          (FuzzySetValue(FuzzySetOperations.add(s1, s2)), env2)
        case _ =>
          (AddSets(left.asInstanceOf[FuzzySetExpression], right.asInstanceOf[FuzzySetExpression]), env2)
      }

    case MultSets(a, b) =>
      val (left, env1) = evaluateSetExpression(a, env)
      val (right, env2) = evaluateSetExpression(b, env1)
      (left, right) match {
        case (FuzzySetValue(s1), FuzzySetValue(s2)) =>
          (FuzzySetValue(FuzzySetOperations.mult(s1, s2)), env2)
        case _ =>
          (MultSets(left.asInstanceOf[FuzzySetExpression], right.asInstanceOf[FuzzySetExpression]), env2)
      }

    case XorSets(a, b) =>
      val (left, env1) = evaluateSetExpression(a, env)
      val (right, env2) = evaluateSetExpression(b, env1)
      (left, right) match {
        case (FuzzySetValue(s1), FuzzySetValue(s2)) =>
          (FuzzySetValue(FuzzySetOperations.xor(s1, s2)), env2)
        case _ =>
          (XorSets(left.asInstanceOf[FuzzySetExpression], right.asInstanceOf[FuzzySetExpression]), env2)
      }

    case Union(a, b) =>
      val (left, env1) = evaluateSetExpression(a, env)
      val (right, env2) = evaluateSetExpression(b, env1)
      (left, right) match {
        case (FuzzySetValue(s1), FuzzySetValue(s2)) =>
          (FuzzySetValue(FuzzySetOperations.union(s1, s2)), env2)
        case _ =>
          (Union(left.asInstanceOf[FuzzySetExpression], right.asInstanceOf[FuzzySetExpression]), env2)
      }

    case Intersection(a, b) =>
      val (left, env1) = evaluateSetExpression(a, env)
      val (right, env2) = evaluateSetExpression(b, env1)
      (left, right) match {
        case (FuzzySetValue(s1), FuzzySetValue(s2)) =>
          (FuzzySetValue(FuzzySetOperations.intersection(s1, s2)), env2)
        case _ =>
          (Intersection(left.asInstanceOf[FuzzySetExpression], right.asInstanceOf[FuzzySetExpression]), env2)
      }

    case Complement(a) =>
      val (evaluatedSet, env1) = evaluateSetExpression(a, env)
      evaluatedSet match {
        case FuzzySetValue(s1) =>
          (FuzzySetValue(FuzzySetOperations.complement(s1)), env1)
        case _ =>
          (Complement(evaluatedSet.asInstanceOf[FuzzySetExpression]), env1)
      }


    case AlphaCut(a, alphaExpr) =>
      val (evaluatedSet, env1) = evaluateSetExpression(a, env)
      val (evaluatedAlpha, env2) = evaluateExpression(alphaExpr, env1)
      (evaluatedSet, evaluatedAlpha) match {
        case (FuzzySetValue(s1), FuzzyNumber(alphaValue)) =>
          val elements = FuzzySetOperations.alphaCut(s1, alphaValue)
          val resultSet = FuzzySet(s"${s1.name}_ALPHACUT_$alphaValue", elements.map(Element(_, alphaValue)))
          (FuzzySetValue(resultSet), env2)
        case _ =>
          (AlphaCut(evaluatedSet.asInstanceOf[FuzzySetExpression], evaluatedAlpha), env2)
      }
  }

}

case class FuzzySetValue(set: FuzzySet) extends FuzzyValue with FuzzySetExpression {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = this
}

case class SetInput(name: String) extends FuzzySetOperation {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    env.get(name) match {
      case Some(setExpr: FuzzySetExpression) => setExpr.partialEvaluate(env)
      case _ => this
    }
  }
}
case class AddSets(setA: FuzzySetExpression, setB: FuzzySetExpression) extends FuzzySetOperation {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    val left = setA.partialEvaluate(env)
    val right = setB.partialEvaluate(env)
    (left, right) match {
      case (FuzzySetValue(s1), FuzzySetValue(s2)) =>
        FuzzySetValue(FuzzySetOperations.add(s1, s2))
      case _ =>
        AddSets(left.asInstanceOf[FuzzySetExpression], right.asInstanceOf[FuzzySetExpression])
    }
  }
}
case class MultSets(setA: FuzzySetExpression, setB: FuzzySetExpression) extends FuzzySetOperation {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    val left = setA.partialEvaluate(env)
    val right = setB.partialEvaluate(env)
    (left, right) match {
      case (FuzzySetValue(s1), FuzzySetValue(s2)) =>
        FuzzySetValue(FuzzySetOperations.mult(s1, s2))
      case _ =>
        MultSets(left.asInstanceOf[FuzzySetExpression], right.asInstanceOf[FuzzySetExpression])
    }
  }
}
case class XorSets(setA: FuzzySetExpression, setB: FuzzySetExpression) extends FuzzySetOperation {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    val left = setA.partialEvaluate(env)
    val right = setB.partialEvaluate(env)
    (left, right) match {
      case (FuzzySetValue(s1), FuzzySetValue(s2)) =>
        FuzzySetValue(FuzzySetOperations.xor(s1, s2))
      case _ =>
        XorSets(left.asInstanceOf[FuzzySetExpression], right.asInstanceOf[FuzzySetExpression])
    }
  }
}
case class AlphaCut(setA: FuzzySetExpression, alpha: FuzzyExpression) extends FuzzySetExpression {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    val evaluatedSet = setA.partialEvaluate(env)
    val evaluatedAlpha = alpha.partialEvaluate(env)
    (evaluatedSet, evaluatedAlpha) match {
      case (FuzzySetValue(s1), FuzzyNumber(a)) =>
        val elements = FuzzySetOperations.alphaCut(s1, a)
        FuzzySetValue(FuzzySet(s"${s1.name}_ALPHACUT_$a", elements.map(Element(_, a))))
      case _ =>
        AlphaCut(evaluatedSet.asInstanceOf[FuzzySetExpression], evaluatedAlpha)
    }
  }
}

case class Union(setA: FuzzySetExpression, setB: FuzzySetExpression) extends FuzzySetOperation {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    val left = setA.partialEvaluate(env)
    val right = setB.partialEvaluate(env)
    (left, right) match {
      case (FuzzySetValue(s1), FuzzySetValue(s2)) =>
        FuzzySetValue(FuzzySetOperations.union(s1, s2))
      case _ =>
        Union(left.asInstanceOf[FuzzySetExpression], right.asInstanceOf[FuzzySetExpression])
    }
  }
}
case class Intersection(setA: FuzzySetExpression, setB: FuzzySetExpression) extends FuzzySetOperation {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    val left = setA.partialEvaluate(env)
    val right = setB.partialEvaluate(env)
    (left, right) match {
      case (FuzzySetValue(s1), FuzzySetValue(s2)) =>
        FuzzySetValue(FuzzySetOperations.intersection(s1, s2))
      case _ =>
        Intersection(left.asInstanceOf[FuzzySetExpression], right.asInstanceOf[FuzzySetExpression])
    }
  }
}
case class Complement(setA: FuzzySetExpression) extends FuzzySetOperation {
  def partialEvaluate(env: Map[String, FuzzyExpression]): FuzzyExpression = {
    val evaluatedSet = setA.partialEvaluate(env)
    evaluatedSet match {
      case FuzzySetValue(s1) =>
        FuzzySetValue(FuzzySetOperations.complement(s1))
      case _ =>
        Complement(evaluatedSet.asInstanceOf[FuzzySetExpression])
    }
  }
}

// Set Operations
case class Element(name: String, value: Double)

case class FuzzySet(name: String, elements: List[Element]) {
  def eval(): List[Element] = elements
}

object FuzzySetOperations {

  def round(value: Double, precision: Int = 2): Double = {
    val scale = Math.pow(10, precision)
    Math.round(value * scale) / scale
  }

  def elementMap(set: FuzzySet): Map[String, Double] = {
    set.elements.map(e => e.name -> e.value).toMap
  }

  def union(setA: FuzzySet, setB: FuzzySet): FuzzySet = {
    val mapA = elementMap(setA)
    val mapB = elementMap(setB)

    val allKeys = mapA.keySet ++ mapB.keySet
    val newElements = allKeys.map { key =>
      val valueA = mapA.getOrElse(key, 0.0)
      val valueB = mapB.getOrElse(key, 0.0)
      Element(key, round(Math.max(valueA, valueB)))
    }.toList

    FuzzySet(s"${setA.name}_UNION_${setB.name}", newElements)
  }

  def intersection(setA: FuzzySet, setB: FuzzySet): FuzzySet = {
    val mapA = elementMap(setA)
    val mapB = elementMap(setB)

    val allKeys = mapA.keySet ++ mapB.keySet
    val newElements = allKeys.map { key =>
      val valueA = mapA.getOrElse(key, 0.0)
      val valueB = mapB.getOrElse(key, 0.0)
      Element(key, round(Math.min(valueA, valueB)))
    }.toList

    FuzzySet(s"${setA.name}_INTERSECTION_${setB.name}", newElements)
  }

  def complement(setA: FuzzySet): FuzzySet = {
    val newElements = setA.elements.map {
      case Element(nameA, valueA) =>
        Element(nameA, round(1 - valueA))
    }
    FuzzySet(s"${setA.name}_COMPLEMENT", newElements)
  }

  def add(setA: FuzzySet, setB: FuzzySet): FuzzySet = {
    val mapA = elementMap(setA)
    val mapB = elementMap(setB)

    val allKeys = mapA.keySet ++ mapB.keySet
    val newElements = allKeys.map { key =>
      val valueA = mapA.getOrElse(key, 0.0)
      val valueB = mapB.getOrElse(key, 0.0)
      Element(key, round(Math.min(1.0, valueA + valueB)))
    }.toList

    FuzzySet(s"${setA.name}_ADD_${setB.name}", newElements)
  }

  def mult(setA: FuzzySet, setB: FuzzySet): FuzzySet = {
    val mapA = elementMap(setA)
    val mapB = elementMap(setB)

    val allKeys = mapA.keySet ++ mapB.keySet
    val newElements = allKeys.map { key =>
      val valueA = mapA.getOrElse(key, 0.0)
      val valueB = mapB.getOrElse(key, 0.0)
      Element(key, round(valueA * valueB))
    }.toList

    FuzzySet(s"${setA.name}_MULT_${setB.name}", newElements)
  }

  def xor(setA: FuzzySet, setB: FuzzySet): FuzzySet = {
    val mapA = elementMap(setA)
    val mapB = elementMap(setB)

    val allKeys = mapA.keySet ++ mapB.keySet
    val newElements = allKeys.map { key =>
      val valueA = mapA.getOrElse(key, 0.0)
      val valueB = mapB.getOrElse(key, 0.0)
      Element(key, round(Math.max(valueA, valueB) - Math.min(valueA, valueB)))
    }.toList

    FuzzySet(s"${setA.name}_XOR_${setB.name}", newElements)
  }

  def alphaCut(setA: FuzzySet, alpha: Double): List[String] = {
    setA.elements.collect {
      case Element(nameA, valueA) if valueA >= alpha => nameA
    }
  }
}

case class FuzzySetClass(
                          name: String,
                          superClass: Option[Class] = None,
                          methods: List[Method] = Nil,
                          vars: List[ClassVar] = Nil
                        ) extends FuzzyClassOperation

case class FuzzySetVar(name: String, varType: VarType, elements: List[Element]) extends FuzzyClassOperation


import scala.collection.mutable

object FuzzyLogicDSL {
  val gates: mutable.Map[String, FuzzyGate] = mutable.Map()
  val inputScope: mutable.Map[String, mutable.Map[String, FuzzyExpression]] = mutable.Map()
  val instanceScope: mutable.Map[String, mutable.Map[String, FuzzyExpression]] = mutable.Map()
  val classInstances: mutable.Map[String, CreateNew] = mutable.Map()

  def AssignGate(gate: FuzzyGate): Unit = {
    gates += (gate.name -> gate)
  }

  def AssignInput(gateName: String, input: Input, value: FuzzyExpression): Unit = {
    val currentScope = inputScope.getOrElseUpdate(gateName, mutable.Map())
    currentScope += (input.name -> value)
  }

  def Scope(gate: FuzzyGate, inputAssignment: (Input, Double)): Unit = {
    AssignInput(gate.name, inputAssignment._1, FuzzyNumber(inputAssignment._2))
  }

  def ScopeInstance(instance: CreateNew, variable: String, value: FuzzyExpression): Unit = {
    val instanceName = instance.clazz.name
    val currentScope = instanceScope.getOrElseUpdate(instanceName, mutable.Map())
    currentScope += (variable -> value)
  }

  def TestGate(gateName: String, expectedResult: Double): Boolean = {
    val gate = gates.getOrElse(gateName, throw new IllegalArgumentException(s"Gate $gateName not found"))
    val inputValues = inputScope.getOrElse(gateName, throw new IllegalArgumentException(s"Input values for $gateName not found"))
    val result = FuzzyGateEvaluator.evaluate(gate, inputValues.toMap)
    result match {
      case FuzzyNumber(value) => value == expectedResult
      case _ => false
    }
  }

  def CreateInstance(clazz: Class, instanceName: String): CreateNew = {
    val instance = CreateNew(clazz, instanceName)
    classInstances += (instanceName -> instance)
    instance
  }

  def Invoke(instance: CreateNew, methodName: String, argNames: List[String]): FuzzyExpression = {
    val args: List[(String, FuzzyExpression)] = argNames.map { argName =>
      (argName, Input(argName))
    }
    InvokeMethod(instance, methodName, args)
  }
}

object Main extends App {
  val setA = FuzzySet("SetA", List(Element("x", 0.5), Element("y", 0.7)))
  val setB = FuzzySet("SetB", List(Element("x", 0.6), Element("y", 0.4)))

  val env = Map(
    "A" -> FuzzySetValue(setA),
    "B" -> FuzzySetValue(setB)
  )

  // Create assignment using GREATER_EQUAL_SET and IFTRUE
  val assignment = IFTRUE(
    GREATER_EQUAL_SET(SetInput("A"), SetInput("B")),
    THENEXECUTE(
      Assign(Variable("resultSet"), Union(SetInput("A"), SetInput("B")))
    ),
    ELSERUN(
      Assign(Variable("resultSet"), Intersection(SetInput("A"), SetInput("B")))
    )
  )

  // Evaluate the assignment
  val (result, finalEnv) = FuzzyGateEvaluator.evaluateExpression(assignment, env)

  // Get the resultSet from the environment
  finalEnv.get("resultSet") match {
    case Some(fsv: FuzzySetValue) =>
      val resultSet = fsv.set
      println(s"Resulting Set: ${resultSet.name}")
      resultSet.elements.foreach { e =>
        println(s"${e.name}: ${e.value}")
      }
    case Some(value) =>
      println(s"resultSet is not a FuzzySetValue, it is: $value")
    case None =>
      println("resultSet not found in finalEnv")
  }
}
