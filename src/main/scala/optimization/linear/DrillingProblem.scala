package optimization.linear

object DrillingProblem {

  def solve(points: Array[Point]): Array[Point] = {
    val permutations = initialize(points.length)
    var smallestDistancePermutation = initialize(points.length)
    var i = points.length - 1
    val n = points.length
    while (i >= 1) {
      val k = calculateK(permutations, i, n)
      if (k <= n) {
        permutations.update(i - 1, k)
        if (i == n && calculateTotalDistance(points, permutations) < calculateTotalDistance(points, smallestDistancePermutation)) {
          smallestDistancePermutation = permutations
        }
        if (i < n) {
          permutations.update(i, 0)
          i = i + 1
        }
      }
      if (k == n + 1) {
        i = i - 1
      }
    }

    rearrange(points, smallestDistancePermutation)
  }

  def rearrange(points: Array[Point], smallestDistancePermutation: Array[Int]): Array[Point] = {
    val result = new Array[Point](points.length)

    for (i <- 0 until smallestDistancePermutation.length) {
      result.update(i, points(smallestDistancePermutation(i) - 1))
    }

    result
  }

  def calculateK(permutations: Array[Int], i: Int, n: Int): Int = {
    val A = generate(permutations(i - 1) + 1, n + 1)
    val B = permutations.slice(0, i - 1);
    getSmallestElementOfComplement(A, B)
  }

  def generate(from: Int, to: Int): Array[Int] = {
    val length = to - from + 1;
    val result = new Array[Int](length)

    for (i <- 0 until length) {
      result.update(i, from + i)
    }
    result
  }

  def initialize(length: Int): Array[Int] = {
    val permutations = new Array[Int](length)

    for (i <- 0 until length) {
      permutations.update(i, i + 1)
    }

    permutations
  }

  def calculateTotalDistance(points: Array[Point], permutations: Array[Int]): Int = {
    var sum: Int = 0;
    for (i <- 0 until permutations.length - 1) {
      sum = sum + calculateDistance(points(permutations(i) - 1), points(permutations(i + 1) - 1))
    }
    sum
  }

  def calculateDistance(from: Point, to: Point): Int = {
    math.max(math.abs(from.x - to.x), math.abs(from.y - to.y))
  }

  def getSmallestElementOfComplement(A: Array[Int], B: Array[Int]): Int = {
    A.filter(elem => !B.contains(elem)).sorted.apply(0)
  }

}
