import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import FuzzyLogicDSL._

class FuzzyLogicDSLTest extends AnyFlatSpec with Matchers {

  behavior of "FuzzyLogicDSL"

  it should "correctly evaluate ADD operation in a class method" in {
    val baseClass = Class(
      name = "Base",
      methods = List(
        Method(
          name = "addMethod",
          params = List(Parameter("p1", "double"), Parameter("p2", "double")),
          body = ADD(Input("p1"), Input("p2"))
        )
      ),
      vars = List(ClassVar("v1", DoubleType))
    )
    val instance = CreateInstance(baseClass, "instance1")

    val resultExpr = Invoke(instance, methodName = "addMethod", argNames = List("p1", "p2"))
    val env = Map("p1" -> FuzzyNumber(0.4), "p2" -> FuzzyNumber(0.5))
    val (evaluatedResult, _) = FuzzyGateEvaluator.evaluateExpression(resultExpr, env)
    evaluatedResult shouldEqual FuzzyNumber(0.9)
  }

  it should "correctly evaluate XOR operation in a class method" in {
    val baseClass = Class(
      name = "Base",
      methods = List(
        Method(
          name = "xorMethod",
          params = List(Parameter("p1", "double"), Parameter("p2", "double")),
          body = XOR(Input("p1"), Input("p2"))
        )
      ),
      vars = List(ClassVar("v1", DoubleType))
    )
    val instance = CreateInstance(baseClass, "instance1")

    val methodExpr = Invoke(instance, methodName = "xorMethod", argNames = List("p1", "p2"))
    val env = Map("p1" -> FuzzyNumber(0.7), "p2" -> FuzzyNumber(0.4))
    val (evaluatedResult, _) = FuzzyGateEvaluator.evaluateExpression(methodExpr, env)
    evaluatedResult shouldEqual FuzzyNumber(0.3)
  }

  it should "partially evaluate an expression with undefined variables" in {
    val expr = ADD(FuzzyNumber(0.3), Input("x"))
    val partiallyEvaluatedExpr = expr.partialEvaluate(Map.empty)
    partiallyEvaluatedExpr shouldEqual ADD(FuzzyNumber(0.3), Input("x"))
  }

  it should "fully evaluate expressions when all variables are defined" in {
    val expr = MULT(FuzzyNumber(0.5), ADD(FuzzyNumber(0.3), Input("x")))
    val env = Map("x" -> FuzzyNumber(0.4))
    val fullyEvaluatedExpr = expr.partialEvaluate(env)
    fullyEvaluatedExpr shouldEqual FuzzyNumber(0.35)
  }

  it should "partially and fully evaluate a class method" in {
    val baseClass = Class(
      name = "Base",
      methods = List(
        Method(
          name = "computeMethod",
          params = List(Parameter("p1", "double"), Parameter("p2", "double")),
          body = MULT(FuzzyNumber(0.5), ADD(Input("p1"), Input("p2")))
        )
      ),
      vars = List(ClassVar("v1", DoubleType))
    )
    val instance = CreateInstance(baseClass, "instance1")

    ScopeInstance(instance, "p1", FuzzyNumber(0.4))
    val methodExpr = Invoke(instance, methodName = "computeMethod", argNames = List("p1", "p2"))
    val partiallyEvaluatedMethod = methodExpr.partialEvaluate(Map("p1" -> FuzzyNumber(0.4)))
    partiallyEvaluatedMethod shouldEqual MULT(FuzzyNumber(0.5), ADD(FuzzyNumber(0.4), Input("p2")))

    ScopeInstance(instance, "p2", FuzzyNumber(0.3))
    val (result, _) = FuzzyGateEvaluator.evaluateExpression(methodExpr, Map("p1" -> FuzzyNumber(0.4), "p2" -> FuzzyNumber(0.3)))
    result shouldEqual FuzzyNumber(0.35)
  }

  it should "correctly handle class inheritance and method overriding" in {
    val baseClass = Class(
      name = "BaseClass",
      methods = List(
        Method(
          name = "computeMethod",
          params = List(Parameter("p1", "double"), Parameter("p2", "double")),
          body = ADD(Input("p1"), Input("p2"))
        )
      ),
      vars = List(ClassVar("v1", DoubleType))
    )

    val derivedClass = Class(
      name = "DerivedClass",
      superClass = Some(baseClass),
      methods = List(
        Method(
          name = "computeMethod",
          params = List(Parameter("p1", "double"), Parameter("p2", "double")),
          body = MULT(Input("p1"), Input("p2"))
        )
      ),
      vars = List(ClassVar("v2", DoubleType))
    )

    val instance = CreateInstance(derivedClass, "instance1")

    val methodExpr = Invoke(instance, methodName = "computeMethod", argNames = List("p1", "p2"))
    val env = Map("p1" -> FuzzyNumber(0.6), "p2" -> FuzzyNumber(0.5))
    val (evaluatedResult, _) = FuzzyGateEvaluator.evaluateExpression(methodExpr, env)
    evaluatedResult shouldEqual FuzzyNumber(0.3) // 0.6 * 0.5 = 0.3

    val derivedClassWithoutOverride = Class(
      name = "DerivedClassWithoutOverride",
      superClass = Some(baseClass),
      methods = List(),
      vars = List(ClassVar("v2", DoubleType))
    )

    val instance2 = CreateInstance(derivedClassWithoutOverride, "instance2")
    val methodExpr2 = Invoke(instance2, methodName = "computeMethod", argNames = List("p1", "p2"))
    val env2 = Map("p1" -> FuzzyNumber(0.6), "p2" -> FuzzyNumber(0.5))
    val (evaluatedResult2, _) = FuzzyGateEvaluator.evaluateExpression(methodExpr2, env2)
    evaluatedResult2 shouldEqual FuzzyNumber(1.0) // 0.6 + 0.5 = 1.1, but max is 1.0
  }

  it should "correctly evaluate GREATER_EQUAL, IFTRUE, THENEXECUTE, and ELSERUN with complex expressions" in {
    val expr = IFTRUE(
      GREATER_EQUAL(Input("p1"), Input("p2")),
      THENEXECUTE(
        ADD(Input("p1"), FuzzyNumber(0.1))
      ),
      ELSERUN(
        MULT(Input("p2"), FuzzyNumber(0.5))
      )
    )

    // Case 1: p1 >= p2
    val env1 = Map("p1" -> FuzzyNumber(0.7), "p2" -> FuzzyNumber(0.5))
    val (result1, _) = FuzzyGateEvaluator.evaluateExpression(expr, env1)
    result1 shouldEqual FuzzyNumber(0.8) // 0.7 + 0.1 = 0.8

    // Case 2: p1 < p2
    val env2 = Map("p1" -> FuzzyNumber(0.4), "p2" -> FuzzyNumber(0.5))
    val (result2, _) = FuzzyGateEvaluator.evaluateExpression(expr, env2)
    result2 shouldEqual FuzzyNumber(0.25) // 0.5 * 0.5 = 0.25
  }

  behavior of "FuzzySet operations for evaluations"

  it should "partially and fully evaluate set operations" in {
    val setA = FuzzySet("SetA", List(Element("x", 0.5), Element("y", 0.7)))
    val setB = FuzzySet("SetB", List(Element("x", 0.6), Element("y", 0.4)))
    val setC = FuzzySet("SetC", List(Element("x", 0.8), Element("y", 0.2)))

    val env = Map(
      "A" -> FuzzySetValue(setA),
      "B" -> FuzzySetValue(setB)
    )

    val expr = Union(SetInput("A"), Intersection(SetInput("B"), SetInput("C")))

    val partiallyEvaluatedExpr = expr.partialEvaluate(env)
    partiallyEvaluatedExpr shouldBe Union(FuzzySetValue(setA), Intersection(FuzzySetValue(setB), SetInput("C")))

    val envFull = env + ("C" -> FuzzySetValue(setC))
    val fullyEvaluatedExpr = partiallyEvaluatedExpr.partialEvaluate(envFull)
    fullyEvaluatedExpr match {
      case FuzzySetValue(resultSet) =>
        resultSet.name shouldBe "SetA_UNION_SetB_INTERSECTION_SetC"
        resultSet.elements should contain allOf(
          Element("x", 0.6),
          Element("y", 0.7)
        )
      case _ =>
        fail("Expected a FuzzySetValue")
    }
  }

  it should "correctly evaluate ADD operation on fuzzy sets in a class method" in {
    val baseSetClass = Class(
      name = "BaseSet",
      methods = List(
        Method(
          name = "addMethod",
          params = List(Parameter("setA", "set"), Parameter("setB", "set")),
          body = AddSets(SetInput("setA"), SetInput("setB"))
        )
      )
    )

    val instance = CreateInstance(baseSetClass, "instance1")

    val env = Map(
      "setA" -> FuzzySetValue(FuzzySet("A", List(Element("x1", 0.2), Element("x2", 0.7)))),
      "setB" -> FuzzySetValue(FuzzySet("B", List(Element("x1", 0.5), Element("x3", 0.4))))
    )

    val methodExpr = Invoke(instance, methodName = "addMethod", argNames = List("setA", "setB"))
    val (result, _) = FuzzyGateEvaluator.evaluateExpression(methodExpr, env)

    result match {
      case FuzzySetValue(set) =>
        set.elements should contain allOf(
          Element("x1", 0.7), // min(1.0, 0.2 + 0.5)
          Element("x2", 0.7),
          Element("x3", 0.4)
        )
      case _ => fail("Expected a FuzzySetValue")
    }
  }

  it should "correctly evaluate MULT operation on fuzzy sets in a class method" in {
    val baseSetClass = Class(
      name = "BaseSet",
      methods = List(
        Method(
          name = "multMethod",
          params = List(Parameter("setA", "set"), Parameter("setB", "set")),
          body = MultSets(SetInput("setA"), SetInput("setB"))
        )
      )
    )

    val instance = CreateInstance(baseSetClass, "instance1")

    val env = Map(
      "setA" -> FuzzySetValue(FuzzySet("A", List(Element("x1", 0.6), Element("x2", 0.8)))),
      "setB" -> FuzzySetValue(FuzzySet("B", List(Element("x1", 0.5), Element("x3", 0.4))))
    )

    val methodExpr = Invoke(instance, methodName = "multMethod", argNames = List("setA", "setB"))
    val (result, _) = FuzzyGateEvaluator.evaluateExpression(methodExpr, env)

    result match {
      case FuzzySetValue(set) =>
        set.elements should contain allOf(
          Element("x1", 0.3), // 0.6 * 0.5
          Element("x2", 0.0), // 0.8 * 0.0
          Element("x3", 0.0) // 0.0 * 0.4
        )
      case _ => fail("Expected a FuzzySetValue")
    }
  }

  it should "correctly evaluate XOR operation on fuzzy sets in a class method" in {
    val baseSetClass = Class(
      name = "BaseSet",
      methods = List(
        Method(
          name = "xorMethod",
          params = List(Parameter("setA", "set"), Parameter("setB", "set")),
          body = XorSets(SetInput("setA"), SetInput("setB"))
        )
      )
    )

    val instance = CreateInstance(baseSetClass, "instance1")

    val env = Map(
      "setA" -> FuzzySetValue(FuzzySet("A", List(Element("x1", 0.7), Element("x2", 0.3)))),
      "setB" -> FuzzySetValue(FuzzySet("B", List(Element("x1", 0.4), Element("x3", 0.5))))
    )

    val methodExpr = Invoke(instance, methodName = "xorMethod", argNames = List("setA", "setB"))
    val (result, _) = FuzzyGateEvaluator.evaluateExpression(methodExpr, env)

    result match {
      case FuzzySetValue(set) =>
        set.elements should contain allOf(
          Element("x1", 0.3), // |0.7 - 0.4|
          Element("x2", 0.3), // |0.3 - 0.0|
          Element("x3", 0.5) // |0.0 - 0.5|
        )
      case _ => fail("Expected a FuzzySetValue")
    }
  }

  it should "correctly evaluate complement operation on a fuzzy set in a class method" in {
    val baseSetClass = Class(
      name = "BaseSet",
      methods = List(
        Method(
          name = "complementMethod",
          params = List(Parameter("setA", "set")),
          body = Complement(SetInput("setA"))
        )
      )
    )

    val instance = CreateInstance(baseSetClass, "instance1")

    val env = Map(
      "setA" -> FuzzySetValue(FuzzySet("A", List(Element("x1", 0.2), Element("x2", 0.8))))
    )

    val methodExpr = Invoke(instance, methodName = "complementMethod", argNames = List("setA"))
    val (result, _) = FuzzyGateEvaluator.evaluateExpression(methodExpr, env)

    result match {
      case FuzzySetValue(set) =>
        set.elements should contain allOf(
          Element("x1", 0.8), // 1 - 0.2
          Element("x2", 0.2) // 1 - 0.8
        )
      case _ => fail("Expected a FuzzySetValue")
    }
  }

  it should "correctly evaluate union operation on fuzzy sets in a class method" in {
    val baseSetClass = Class(
      name = "BaseSet",
      methods = List(
        Method(
          name = "unionMethod",
          params = List(Parameter("setA", "set"), Parameter("setB", "set")),
          body = Union(SetInput("setA"), SetInput("setB"))
        )
      )
    )

    val instance = CreateInstance(baseSetClass, "instance1")

    val env = Map(
      "setA" -> FuzzySetValue(FuzzySet("A", List(Element("x1", 0.2), Element("x2", 0.7)))),
      "setB" -> FuzzySetValue(FuzzySet("B", List(Element("x1", 0.6), Element("x3", 0.5))))
    )

    val methodExpr = Invoke(instance, methodName = "unionMethod", argNames = List("setA", "setB"))
    val (result, _) = FuzzyGateEvaluator.evaluateExpression(methodExpr, env)

    result match {
      case FuzzySetValue(set) =>
        set.elements should contain allOf(
          Element("x1", 0.6), // max(0.2, 0.6)
          Element("x2", 0.7),
          Element("x3", 0.5)
        )
      case _ => fail("Expected a FuzzySetValue")
    }
  }

  it should "correctly evaluate intersection operation on fuzzy sets in a class method" in {
    val baseSetClass = Class(
      name = "BaseSet",
      methods = List(
        Method(
          name = "intersectionMethod",
          params = List(Parameter("setA", "set"), Parameter("setB", "set")),
          body = Intersection(SetInput("setA"), SetInput("setB"))
        )
      )
    )

    val instance = CreateInstance(baseSetClass, "instance1")

    val env = Map(
      "setA" -> FuzzySetValue(FuzzySet("A", List(Element("x1", 0.5), Element("x2", 0.7)))),
      "setB" -> FuzzySetValue(FuzzySet("B", List(Element("x1", 0.3), Element("x3", 0.8))))
    )

    val methodExpr = Invoke(instance, methodName = "intersectionMethod", argNames = List("setA", "setB"))
    val (result, _) = FuzzyGateEvaluator.evaluateExpression(methodExpr, env)

    result match {
      case FuzzySetValue(set) =>
        set.elements should contain allOf(
          Element("x1", 0.3), // min(0.5, 0.3)
          Element("x2", 0.0), // min(0.7, 0.0)
          Element("x3", 0.0) // min(0.0, 0.8)
        )
      case _ => fail("Expected a FuzzySetValue")
    }
  }

  it should "correctly evaluate inherited fuzzy set method in derived class" in {
    val baseSetClass = Class(
      name = "BaseSet",
      methods = List(
        Method(
          name = "unionMethod",
          params = List(Parameter("setA", "set"), Parameter("setB", "set")),
          body = Union(SetInput("setA"), SetInput("setB"))
        )
      )
    )

    val derivedSetClass = Class(
      name = "DerivedSet",
      superClass = Some(baseSetClass),
      methods = List(
        Method(
          name = "intersectionMethod",
          params = List(Parameter("setA", "set"), Parameter("setB", "set")),
          body = Intersection(SetInput("setA"), SetInput("setB"))
        )
      )
    )

    val instance = CreateInstance(derivedSetClass, "instance1")

    val env = Map(
      "setA" -> FuzzySetValue(FuzzySet("A", List(Element("x1", 0.4), Element("x2", 0.6)))),
      "setB" -> FuzzySetValue(FuzzySet("B", List(Element("x1", 0.5), Element("x3", 0.7))))
    )

    // Invoke inherited unionMethod
    val unionExpr = Invoke(instance, methodName = "unionMethod", argNames = List("setA", "setB"))
    val (unionResult, _) = FuzzyGateEvaluator.evaluateExpression(unionExpr, env)

    unionResult match {
      case FuzzySetValue(set) =>
        set.elements should contain allOf(
          Element("x1", 0.5), // max(0.4, 0.5)
          Element("x2", 0.6),
          Element("x3", 0.7)
        )
      case _ => fail("Expected a FuzzySetValue")
    }

    // Invoke intersectionMethod from DerivedSet
    val intersectionExpr = Invoke(instance, methodName = "intersectionMethod", argNames = List("setA", "setB"))
    val (intersectionResult, _) = FuzzyGateEvaluator.evaluateExpression(intersectionExpr, env)

    intersectionResult match {
      case FuzzySetValue(set) =>
        set.elements should contain allOf(
          Element("x1", 0.4), // min(0.4, 0.5)
          Element("x2", 0.0),
          Element("x3", 0.0)
        )
      case _ => fail("Expected a FuzzySetValue")
    }
  }

  it should "correctly evaluate GREATER_EQUAL_SET, IFTRUE, THENEXECUTE, and ELSERUN with set operations" in {
    val setA = FuzzySet("SetA", List(Element("x", 0.5), Element("y", 0.7)))
    val setB = FuzzySet("SetB", List(Element("x", 0.6), Element("y", 0.4)))
    val setC = FuzzySet("SetC", List(Element("x", 0.8), Element("y", 0.2)))
    val setD = FuzzySet("SetD", List(Element("x", 0.3), Element("y", 0.9)))

    val env = Map(
      "A" -> FuzzySetValue(setA),
      "B" -> FuzzySetValue(setB),
      "C" -> FuzzySetValue(setC),
      "D" -> FuzzySetValue(setD)
    )

    val expr = IFTRUE(
      GREATER_EQUAL_SET(SetInput("A"), SetInput("B")),
      THENEXECUTE(
        Assign(Variable("resultSet"), Union(SetInput("A"), SetInput("C")))
      ),
      ELSERUN(
        Assign(Variable("resultSet"), Intersection(SetInput("B"), SetInput("D")))
      )
    )

    val (result, finalEnv) = FuzzyGateEvaluator.evaluateExpression(expr, env)

    finalEnv.get("resultSet") match {
      case Some(FuzzySetValue(resultSet)) =>
        if (setA.elements.map(_.value).sum >= setB.elements.map(_.value).sum) {
          resultSet.name shouldBe "SetA_UNION_SetC"
        } else {
          resultSet.name shouldBe "SetB_INTERSECTION_SetD"
        }
      case _ =>
        fail("resultSet not found or not a FuzzySetValue")
    }
  }

  it should "correctly handle class inheritance with set operations and test all set operations" in {
    val superClass = Class(
      name = "SuperClass",
      methods = List(
        Method(
          name = "combineSets",
          params = List(Parameter("setX", "set"), Parameter("setY", "set")),
          body = AddSets(SetInput("setX"), SetInput("setY"))
        )
      ),
      vars = List(ClassVar("v1", SetType))
    )

    val subClass = Class(
      name = "SubClass",
      superClass = Some(superClass),
      methods = List(
        Method(
          name = "combineSets",
          params = List(Parameter("setX", "set"), Parameter("setY", "set")),
          body = MultSets(SetInput("setX"), SetInput("setY"))
        ),
        Method(
          name = "complementSet",
          params = List(Parameter("setZ", "set")),
          body = Complement(SetInput("setZ"))
        )
      ),
      vars = List(ClassVar("v2", SetType))
    )

    val setX = FuzzySet("SetX", List(Element("a", 0.4), Element("b", 0.6)))
    val setY = FuzzySet("SetY", List(Element("a", 0.5), Element("b", 0.3)))
    val setZ = FuzzySet("SetZ", List(Element("a", 0.7), Element("b", 0.2)))

    val env = Map(
      "setX" -> FuzzySetValue(setX),
      "setY" -> FuzzySetValue(setY),
      "setZ" -> FuzzySetValue(setZ)
    )

    val instance = CreateInstance(subClass, "instance1")

    val combineExpr = Invoke(instance, methodName = "combineSets", argNames = List("setX", "setY"))
    val (combinedResult, _) = FuzzyGateEvaluator.evaluateExpression(combineExpr, env)

    combinedResult match {
      case FuzzySetValue(resultSet) =>
        resultSet.name shouldBe "SetX_MULT_SetY"
        resultSet.elements should contain allOf(
          Element("a", 0.2), // 0.4 * 0.5
          Element("b", 0.18) // 0.6 * 0.3
        )
      case _ =>
        fail("Expected a FuzzySetValue")
    }

    val complementExpr = Invoke(instance, methodName = "complementSet", argNames = List("setZ"))
    val (complementResult, _) = FuzzyGateEvaluator.evaluateExpression(complementExpr, env)

    complementResult match {
      case FuzzySetValue(resultSet) =>
        resultSet.name shouldBe "SetZ_COMPLEMENT"
        resultSet.elements should contain allOf(
          Element("a", 0.3), // 1 - 0.7
          Element("b", 0.8) // 1 - 0.2
        )
      case _ =>
        fail("Expected a FuzzySetValue")
    }

    val subClassNoOverride = Class(
      name = "SubClassNoOverride",
      superClass = Some(superClass),
      methods = List(), // No override
      vars = List(ClassVar("v2", SetType))
    )

    val instance2 = CreateInstance(subClassNoOverride, "instance2")

    // Invoke combineSets method (should use AddSets from SuperClass)
    val combineExpr2 = Invoke(instance2, methodName = "combineSets", argNames = List("setX", "setY"))
    val (combinedResult2, _) = FuzzyGateEvaluator.evaluateExpression(combineExpr2, env)

    combinedResult2 match {
      case FuzzySetValue(resultSet) =>
        resultSet.name shouldBe "SetX_ADD_SetY"
        resultSet.elements should contain allOf(
          Element("a", 0.9), // min(1.0, 0.4 + 0.5)
          Element("b", 0.9) // min(1.0, 0.6 + 0.3)
        )
      case _ =>
        fail("Expected a FuzzySetValue")
    }
  }
}