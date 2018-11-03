package optimization.linear

import org.scalatest.FunSuite

class DrillingProblemTest extends FunSuite {

  test("DrillingProblem.solve") {
    val pointA = new Point(1, 3)
    val pointB = new Point(2, 4)
    val pointC = new Point(3, 1)

    val input = Array(pointA, pointB, pointC)
    val expected = Array(pointC, pointB, pointA)

    assert(DrillingProblem.solve(input) === expected)
  }

  test("DrillingProblem.calculateDistance") {
    val from = new Point(1, 1)
    val to = new Point(2, 3)

    assert(DrillingProblem.calculateDistance(from, to) === 2)
  }

  test("DrillingProblem.getSmallestElementOfComplement") {
    val A = Array(2, 3, 4, 1)
    val B = Array(2, 4)

    assert(DrillingProblem.getSmallestElementOfComplement(A, B) === 1)
  }

  test("DrillingProblem.initialize") {
    val expected = Array(1, 2, 3, 4, 5)

    assert(DrillingProblem.initialize(5) === expected)
  }

  test("DrillingProblem.generate") {
    val expected = Array(6, 7)

    assert(DrillingProblem.generate(6, 7) === expected)
  }

  test("DrillingProblem.calculateK") {
    val expected = Array(1, 2, 3, 4, 5, 6)

    assert(DrillingProblem.calculateK(expected, 5, 6) === 6)
  }

  test("DrillingProblem.calculateTotalDistance") {
    val points = Array(new Point(1, 2), new Point(1, 3), new Point(1, 4))
    val permutations = Array(1, 2, 3)

    assert(DrillingProblem.calculateTotalDistance(points, permutations) === 2)
  }

  test("DrillingProblem.rearrange") {
    val pointA = new Point(1, 3)
    val pointB = new Point(2, 4)
    val pointC = new Point(3, 1)

    val points = Array(pointA, pointB, pointC)
    val permutations = Array(2, 3, 1)

    assert(DrillingProblem.rearrange(points, permutations) === Array(pointB, pointC, pointA))
  }
}
