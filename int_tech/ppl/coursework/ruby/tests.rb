#!/usr/bin/env ruby

require "./fracomp"
require "test/unit"

# For the test case definitions the ruby documentation under
# http://www.ruby-doc.org/stdlib-1.9.3/libdoc/test/unit/rdoc/
# and the wiki book under
# https://en.wikibooks.org/wiki/Ruby_Programming/Unit_testing
# where used as resources.

class TestFraction < Test::Unit::TestCase

  def test_numeric_constructors
    # test the == method before all other tests
    assert_equal     (Dvn::Fraction.new 1), (Dvn::Fraction.new 1)
    assert_not_equal (Dvn::Fraction.new 1), (Dvn::Fraction.new 2)

    # same object is built independent of constructor
    assert_equal (Dvn::Fraction.new 1, 1  ), (Dvn::Fraction.new  1    )
    assert_equal (Dvn::Fraction.new "1/1" ), (Dvn::Fraction.new  1    )
    assert_equal (Dvn::Fraction.new "1/1" ), (Dvn::Fraction.new  1,  1)
    assert_equal (Dvn::Fraction.new "1/2" ), (Dvn::Fraction.new  1,  2)
    assert_equal (Dvn::Fraction.new "-1/2"), (Dvn::Fraction.new -1,  2)
    assert_equal (Dvn::Fraction.new "1/-2"), (Dvn::Fraction.new  1, -2)

    # bad constructor arguments raise exceptions
    assert_raise (ArgumentError) { Dvn::Fraction.new      1.6,   2 }
    assert_raise (ArgumentError) { Dvn::Fraction.new        1, 2.2 }
    assert_raise (ArgumentError) { Dvn::Fraction.new "string"      }
    assert_raise (ArgumentError) { Dvn::Fraction.new "string",   1 }
    assert_raise (ArgumentError) { Dvn::Fraction.new        1, "2" }

    # second param is ignored when first param is a string
    assert_not_equal (Dvn::Fraction.new "1/2", 1), (Dvn::Fraction.new 1   )
    assert_equal     (Dvn::Fraction.new "1/2", 1), (Dvn::Fraction.new 1, 2)
    assert_equal     (Dvn::Fraction.new   "2", 3), (Dvn::Fraction.new 2, 1)
  end

  def test_string_constructor
    # spaces can be present
    assert_equal (Dvn::Fraction.new "1/2"), (Dvn::Fraction.new "1 / 2"  )
    assert_equal (Dvn::Fraction.new "1/2"), (Dvn::Fraction.new " 1 / 2 ")
    assert_equal (Dvn::Fraction.new "1/2"), (Dvn::Fraction.new "1/ 2 "  )
    assert_equal (Dvn::Fraction.new "2/1"), (Dvn::Fraction.new "2"      )
    assert_equal (Dvn::Fraction.new "2/1"), (Dvn::Fraction.new " 2 "    )
    assert_equal (Dvn::Fraction.new "2"  ), (Dvn::Fraction.new " 2/1"   )
    assert_equal (Dvn::Fraction.new "2"  ), (Dvn::Fraction.new "2 /1"   )

    # negative string fractions
    assert_equal (Dvn::Fraction.new "-2"),  (Dvn::Fraction.new -2, 1   )
    assert_equal (Dvn::Fraction.new "-2"),  (Dvn::Fraction.new "2 / -1")

    # badly constructed string raises an exception,
    # minus must be ussed as an unary operator:
    # "- 2" is invalid to follow ruby's convention where 0 == "- 2".to_i
    assert_raise (ArgumentError) { Dvn::Fraction.new "-   2" }
    assert_raise (ArgumentError) { Dvn::Fraction.new "1/- 2" }
  end

  def test_normalisation  # (and validation)
    # gcd processing
    assert_equal (Dvn::Fraction.new "1/2"), (Dvn::Fraction.new "2/4" )
    assert_equal (Dvn::Fraction.new "7/3"), (Dvn::Fraction.new "21/9")

    # negative numbers are normalised as well
    assert_equal (Dvn::Fraction.new -1   ), (Dvn::Fraction.new 10, -10)
    assert_equal (Dvn::Fraction.new -7, 3), (Dvn::Fraction.new "21/-9")
    assert_equal (Dvn::Fraction.new -0   ), (Dvn::Fraction.new 0      )

    # cannot divide by zero, exceptions will raise
    assert_raise          (ZeroDivisionError) { Dvn::Fraction.new "1/0" }
    assert_raise          (ZeroDivisionError) { Dvn::Fraction.new 1, 0  }
    assert_raise          (ZeroDivisionError) { Dvn::Fraction.new 0, 0  }
    assert_nothing_raised (ZeroDivisionError) { Dvn::Fraction.new 0, 1  }
  end

  def test_representations
    assert_equal (Dvn::Fraction.new -7, 3).to_s,    "-7/3"
    assert_equal (Dvn::Fraction.new -7, 3).inspect, "fraction: -7/3"
  end

  def test_comparisons
    # between fractions
    assert_operator (Dvn::Fraction.new "1/2"), '==', (Dvn::Fraction.new "1/2")
    assert_operator (Dvn::Fraction.new "1/2"),  '>', (Dvn::Fraction.new "1/3")
    assert_operator (Dvn::Fraction.new "1/2"), '>=', (Dvn::Fraction.new "1/2")
    assert_operator (Dvn::Fraction.new "1/2"), '>=', (Dvn::Fraction.new "1/3")
    assert_operator (Dvn::Fraction.new "1/3"),  '<', (Dvn::Fraction.new "1/2")
    assert_operator (Dvn::Fraction.new "1/2"), '<=', (Dvn::Fraction.new "1/2")
    assert_operator (Dvn::Fraction.new "1/3"), '<=', (Dvn::Fraction.new "1/2")

    # against normal integers
    assert_operator (Dvn::Fraction.new     2), '==', 2
    assert_operator (Dvn::Fraction.new     2),  '>', 1
    assert_operator (Dvn::Fraction.new     2), '>=', 1
    assert_operator (Dvn::Fraction.new "3/2"), '>=', 1
    assert_operator (Dvn::Fraction.new     2), '>=', 2
    assert_operator (Dvn::Fraction.new     1),  '<', 2
    assert_operator (Dvn::Fraction.new     1), '<=', 2
    assert_operator (Dvn::Fraction.new "1/2"), '<=', 2
    assert_operator (Dvn::Fraction.new     1), '<=', 1

    # test coercion (with Fraction object on the right side of the operator)
    assert_operator 2, '==', (Dvn::Fraction.new 2)
    assert_operator 2,  '>', (Dvn::Fraction.new 1)
    assert_operator 2, '>=', (Dvn::Fraction.new 1)
    assert_operator 1,  '<', (Dvn::Fraction.new 2)
    assert_operator 1, '<=', (Dvn::Fraction.new 2)
  end

  def test_arithmetic
    # against integers
    assert_equal (Dvn::Fraction.new "1/2") + 1, (Dvn::Fraction.new "3/2")
    assert_equal (Dvn::Fraction.new "3/2") - 1, (Dvn::Fraction.new "1/2")
    assert_equal (Dvn::Fraction.new "3/2") * 3, (Dvn::Fraction.new "9/2")
    assert_equal (Dvn::Fraction.new "3/2") / 3, (Dvn::Fraction.new "1/2")

    # test coercion (with Fraction object on the right side of the operator)
    assert_equal 1 + (Dvn::Fraction.new "1/2"), (Dvn::Fraction.new  "3/2")
    assert_equal 1 - (Dvn::Fraction.new "3/2"), (Dvn::Fraction.new "-1/2")
    assert_equal 3 * (Dvn::Fraction.new "3/2"), (Dvn::Fraction.new  "9/2")
    assert_equal 3 / (Dvn::Fraction.new "3/2"), (Dvn::Fraction.new      2)

    # against fractions
    assert_equal (Dvn::Fraction.new "1/2") + (Dvn::Fraction.new "1/2"), 1
    assert_equal (Dvn::Fraction.new "3/2") - (Dvn::Fraction.new "1/2"), 1
    assert_equal (Dvn::Fraction.new "2/3") * (Dvn::Fraction.new "3/2"), 1
    assert_equal (Dvn::Fraction.new "3/2") / (Dvn::Fraction.new "3/2"), 1

    # division by zero
    assert_raise          (ZeroDivisionError) \
		{ (Dvn::Fraction.new "1/2") / (Dvn::Fraction.new "0/1") }
    assert_raise          (ZeroDivisionError) \
		{ (Dvn::Fraction.new "1/2") / 0                         }
    assert_nothing_raised (ZeroDivisionError) \
		{ (Dvn::Fraction.new "0/1") / (Dvn::Fraction.new "1/2") }
    assert_nothing_raised (ZeroDivisionError) \
		{                         0 / (Dvn::Fraction.new "1/2") }
  end

  def test_value
    assert_equal (Dvn::Fraction.new "-1/2").abs,     (Dvn::Fraction.new " 1/2")
    assert_equal (Dvn::Fraction.new "-1/2").negate,  (Dvn::Fraction.new " 1/2")
    assert_equal (Dvn::Fraction.new " 1/2").negate,  (Dvn::Fraction.new "-1/2")
    assert_equal (Dvn::Fraction.new " 1/2").inverse, (Dvn::Fraction.new "2"   )
    assert_equal (Dvn::Fraction.new " 3/2").inverse, (Dvn::Fraction.new "2/3" )
  end

end

class TestComplex < Test::Unit::TestCase

  def test_equality
    # approximate equality between float values
    assert           (Dvn::Complex.new  1).approximatelyEquals ( 1 + 10**-7)
    assert_equal     (Dvn::Complex.new  1),                    ( 1 + 10**-7)
    assert_not_equal (Dvn::Complex.new  1),                    ( 1 + 10**-5)
    assert_equal     (Dvn::Complex.new -6),                    (-6 + 10**-7)
    assert_not_equal (Dvn::Complex.new -6),                    (-6 + 10**-5)
  end

  def test_numeric_constructors
    # test the equality between two Complex objects
    assert_equal     (Dvn::Complex.new 1), (Dvn::Complex.new 1)
    assert_not_equal (Dvn::Complex.new 1), (Dvn::Complex.new 2)

    # same object is built independent of constructor
    assert_equal (Dvn::Complex.new 1,   0    ), (Dvn::Complex.new   1      )
    assert_equal (Dvn::Complex.new 1.3, 0    ), (Dvn::Complex.new 1.3      )
    assert_equal (Dvn::Complex.new "1+1i"    ), (Dvn::Complex.new   1,    1)
    assert_equal (Dvn::Complex.new "1.2-4.3i"), (Dvn::Complex.new 1.2, -4.3)
    assert_equal (Dvn::Complex.new "3i"      ), (Dvn::Complex.new   0,    3)
    assert_equal (Dvn::Complex.new "0i"      ), (Dvn::Complex.new   0      )
    assert_equal (Dvn::Complex.new 0.0       ), (Dvn::Complex.new   0,    0)

    # bad constructor arguments raise exceptions
    assert_raise (ArgumentError) { Dvn::Complex.new       "",          2 }
    assert_raise (ArgumentError) { Dvn::Complex.new        1,  "not num" }
    assert_raise (ArgumentError) { Dvn::Complex.new        1,        "2" }
    assert_raise (ArgumentError) { Dvn::Complex.new "string"             }
    assert_raise (ArgumentError) { Dvn::Complex.new "string",          1 }

    # second param is ignored when first param is a string
    assert_not_equal (Dvn::Complex.new    "2", 1), (Dvn::Complex.new 1   )
    assert_equal     (Dvn::Complex.new "2+1i", 1), (Dvn::Complex.new 2, 1)
    assert_equal     (Dvn::Complex.new    "2", 3), (Dvn::Complex.new 2, 0)
  end

  def test_string_constructor
    # negative real or imaginary parts
    assert_equal (Dvn::Complex.new "-2+1i"),  (Dvn::Complex.new -2,  1)
    assert_equal (Dvn::Complex.new "-2i"  ),  (Dvn::Complex.new  0, -2)

    # spaces can be present
    assert_equal (Dvn::Complex.new " 2+3i"    ), (Dvn::Complex.new  2,  3)
    assert_equal (Dvn::Complex.new " 2-3I"    ), (Dvn::Complex.new  2, -3)
    assert_equal (Dvn::Complex.new "-2-3i"    ), (Dvn::Complex.new -2, -3)
    assert_equal (Dvn::Complex.new "-2 + 3i"  ), (Dvn::Complex.new -2,  3)
    assert_equal (Dvn::Complex.new " -2 + 3I "), (Dvn::Complex.new -2,  3)
    assert_equal (Dvn::Complex.new " 2+3i"    ), (Dvn::Complex.new  2,  3)
    assert_equal (Dvn::Complex.new "2+3i "    ), (Dvn::Complex.new  2,  3)

    # badly constructed string raises an exception
    assert_raise (ArgumentError) { Dvn::Complex.new "-   1"  }
    assert_raise (ArgumentError) { Dvn::Complex.new "3i + 2" }
    assert_raise (ArgumentError) { Dvn::Complex.new "-3i-2"  }
  end

  def test_representations
    # if there's no real part only the imaginary is printed, and vice-versa
    assert_equal (Dvn::Complex.new  6, -3).to_s,    "6.0-3.0i"
    assert_equal (Dvn::Complex.new  6,  3).to_s,    "6.0+3.0i"
    assert_equal (Dvn::Complex.new -6,  3).to_s,    "-6.0+3.0i"
    assert_equal (Dvn::Complex.new  0,  3).to_s,    "3.0i"
    assert_equal (Dvn::Complex.new  0, -3).to_s,    "-3.0i"
    assert_equal (Dvn::Complex.new  6,  0).to_s,    "6.0"
    assert_equal (Dvn::Complex.new -6,  0).to_s,    "-6.0"
    assert_equal (Dvn::Complex.new  0,  0).to_s,    "0.0"
    assert_equal (Dvn::Complex.new  6, -3).inspect, "complex number: 6.0-3.0i"
    assert_equal (Dvn::Complex.new -6,  3).inspect, "complex number: -6.0+3.0i"
    assert_equal (Dvn::Complex.new -6, -3).inspect, "complex number: -6.0-3.0i"
    assert_equal (Dvn::Complex.new  0,  3).inspect, "complex number: 3.0i"
    assert_equal (Dvn::Complex.new  6,  0).inspect, "complex number: 6.0"
    assert_equal (Dvn::Complex.new  0,  0).inspect, "complex number: 0.0"
  end

  def test_comparisons
    # between complex numbers
    assert_operator (Dvn::Complex.new  1, 2), '==', (Dvn::Complex.new  1, 2)
    assert_operator (Dvn::Complex.new  0   ), '==', (Dvn::Complex.new  0   )
    assert_operator (Dvn::Complex.new  2, 1),  '>', (Dvn::Complex.new  2   )
    assert_operator (Dvn::Complex.new  2, 1), '>=', (Dvn::Complex.new  2   )
    assert_operator (Dvn::Complex.new  2, 1), '>=', (Dvn::Complex.new  2, 1)
    assert_operator (Dvn::Complex.new -3   ),  '<', (Dvn::Complex.new -3, 1)
    assert_operator (Dvn::Complex.new -3   ), '<=', (Dvn::Complex.new -3, 1)
    assert_operator (Dvn::Complex.new -3, 1), '<=', (Dvn::Complex.new -3, 1)

    # against normal integers
    assert_operator (Dvn::Complex.new 2   ), '==', 2
    assert_operator (Dvn::Complex.new 0   ), '==', 0
    assert_operator (Dvn::Complex.new 2   ),  '>', 1
    assert_operator (Dvn::Complex.new 2   ), '>=', 1
    assert_operator (Dvn::Complex.new 2   ), '>=', 2
    assert_operator (Dvn::Complex.new 2, 1), '>=', 2
    assert_operator (Dvn::Complex.new 1   ),  '<', 2
    assert_operator (Dvn::Complex.new 1   ), '<=', 2
    assert_operator (Dvn::Complex.new 1   ), '<=', 1
    assert_operator (Dvn::Complex.new 1, 1), '<=', 2

    # test coercion (with Complex object on the right side of the operator)
    assert_operator 2, '==', (Dvn::Complex.new 2)
    assert_operator 2,  '>', (Dvn::Complex.new 1)
    assert_operator 2, '>=', (Dvn::Complex.new 1)
    assert_operator 1,  '<', (Dvn::Complex.new 2)
    assert_operator 1, '<=', (Dvn::Complex.new 2)
  end

  def test_arithmetic
    # against integers
    assert_equal (Dvn::Complex.new 1.3, 3) + 1, (Dvn::Complex.new 2.3, 3)
    assert_equal (Dvn::Complex.new  -2, 1) - 1, (Dvn::Complex.new  -3, 1)
    assert_equal (Dvn::Complex.new   2, 1) * 3, (Dvn::Complex.new   6, 3)
    assert_equal (Dvn::Complex.new  -6, 3) / 3, (Dvn::Complex.new  -2, 1)

    # against other complex numbers
    assert_equal (Dvn::Complex.new 0.5,   1) + (Dvn::Complex.new 0.5,  -1), 1
    assert_equal (Dvn::Complex.new   2, 0.6) - (Dvn::Complex.new   1, 0.6), 1
    assert_equal (Dvn::Complex.new  -2,   1) * (Dvn::Complex.new  -2,  -1), 5
    assert_equal (Dvn::Complex.new  12,  -6) / (Dvn::Complex.new   2,  -1), 6

    # test coercion (with Complex object on the right side of the operator)
    assert_equal 1 + (Dvn::Complex.new 1.3, 3), (Dvn::Complex.new 2.3,  3)
    assert_equal 1 - (Dvn::Complex.new  -2, 1), (Dvn::Complex.new   3, -1)
    assert_equal 3 * (Dvn::Complex.new   2, 1), (Dvn::Complex.new   6,  3)
    assert_equal 6 / (Dvn::Complex.new  -3, 3), (Dvn::Complex.new  -1, -1)

    # division by zero
    assert_raise          (ZeroDivisionError) \
		{ (Dvn::Complex.new 1, 1) / (Dvn::Complex.new 0, 0) }
    assert_raise          (ZeroDivisionError) \
		{ (Dvn::Complex.new 1, 1) / 0                         }
    assert_nothing_raised (ZeroDivisionError) \
		{ (Dvn::Complex.new 0, 0) / (Dvn::Complex.new 0, 1) }
    assert_nothing_raised (ZeroDivisionError) \
		{                       0 / (Dvn::Complex.new 1, 1) }
  end

  def test_value
    # absolute value is the distance to 0+0i
    assert_equal  (Dvn::Complex.new  2, -1).abs, (Dvn::Complex.new -1, 2).abs
    assert_equal  (Dvn::Complex.new  2,  1).abs, (Dvn::Complex.new  2, 1).abs
    assert_equal  (Dvn::Complex.new  2, -1).abs         \
	       , ((Dvn::Complex.new -1,  2).distance 0)
    assert_equal  (Dvn::Complex.new  2,  1).abs         \
	       , ((Dvn::Complex.new  2,  1).distance 0)
    assert_equal ((Dvn::Complex.new  2, -1).distance 3) \
	       , ((Dvn::Complex.new  2,  1).distance 3)

    # negate and conjugate (very simple functions: simple tests)
    assert_equal (Dvn::Complex.new  1,  2).negate,    (Dvn::Complex.new -1, -2)
    assert_equal (Dvn::Complex.new  2, -1).negate,    (Dvn::Complex.new -2,  1)
    assert_equal (Dvn::Complex.new  2, -1).conjugate, (Dvn::Complex.new  2,  1)
    assert_equal (Dvn::Complex.new -2,  1).conjugate, (Dvn::Complex.new -2, -1)
  end

end

