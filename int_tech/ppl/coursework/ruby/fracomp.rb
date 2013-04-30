#!/usr/bin/env ruby

# As this is an exercise, in place of using the Comparable mixin
# to implement the comparison operators (==, <, <=, >, >=) I have
# implemented the operators ==, < and > manualy and used the following
# mixin to provide <= and >= in both Fraction and Complex classes.
module GtLtOrEqual
  def >= obj
    self > obj or self == obj
  end

  def <= obj
    self < obj or self == obj
  end
end

# this greater common divisor implementation handles zeros
# but not negative numbers, use abs before calling it
def gcd a, b
  if 0 == b
    a
  else
    gcd b, a % b
  end
end

# namespace Dvn: dual valued numbers
module Dvn
  class Fraction
    attr_reader :num, :den
    include GtLtOrEqual

    def initialize param, den=1
      if param.is_a? String  # "3 / -4", " -3", "-1/2" are all valid
	fract = /^\s*(-?\d+)\s*\/\s*(-?\d+)\s*$/.match param
	integ = /^\s*(-?\d+)\s*$/.match param
	if not fract.nil?
	  num = (fract.values_at 1).first.to_i
	  den = (fract.values_at 2).first.to_i
	elsif not integ.nil?
	  num = (integ.values_at 1).first.to_i
	  den = 1
	else
	  raise ArgumentError.new "Fraction.new: " \
				  "First parameter must be a fraction string"
	end
      elsif param.is_a? Fixnum and den.is_a? Fixnum
	num = param
      else
	raise ArgumentError.new "Fraction.new: " \
				"Parameters must be representable " \
				"as integer numbers"
      end
      @num = num
      @den = den
      self.validate
    end

    # provides the simplest fraction representation, handles 0/1
    def normalize
      nd_gcd = gcd @num.abs, @den.abs
      @num = @num / nd_gcd
      @den = @den / nd_gcd
    end

    # handles n/0 and negative denominators
    def validate
      if 0 == @den
	raise ZeroDivisionError.new \
	      "Fraction.validate: Denominator cannot be zero"
      end
      if 0 > @den
	@num = -@num
	@den = -@den
      end
      self.normalize
    end

    def to_s
      "#@num/#@den"
    end

    def inspect
      "fraction: " +  self.to_s
    end

    # In ruby coerce is used to allow operators word both ways around objects
    # that can be transformed into fractions or complex numbers.  Based on the
    # article http://c2.com/cgi/wiki?RubyCoerce by ElizabethWiethoff.
    def coerce number
      return (Fraction.new number), self
    end

    def == frac
      unless frac.is_a? Fraction
	frac = Fraction.new frac
      end
      @num == frac.num and @den == frac.den
    end

    def > frac
      unless frac.is_a? Fraction
	frac = Fraction.new frac
      end
      @num * frac.den > @den * frac.num
    end

    def < frac
      unless frac.is_a? Fraction
	frac = Fraction.new frac
      end
      frac > self and not self == frac
    end

    def + frac
      unless frac.is_a? Fraction
	frac = Fraction.new frac
      end
      Fraction.new (@num * frac.den + @den * frac.num), (@den * frac.den)
    end

    def - frac
      unless frac.is_a? Fraction
	frac = Fraction.new frac
      end
      Fraction.new (@num * frac.den - @den * frac.num), (@den * frac.den)
    end

    def * frac
      unless frac.is_a? Fraction
	frac = Fraction.new frac
      end
      Fraction.new (@num * frac.num), (@den * frac.den)
    end

    def / frac  # division by zero is handled by validate in the new object
      unless frac.is_a? Fraction
	frac = Fraction.new frac
      end
      Fraction.new (@num * frac.den), (@den * frac.num)
    end

    def abs
      if 0 > @num
	self.negate  # negate already returns a new object
      else
	Fraction.new @num, @den
      end
    end

    def negate
      Fraction.new -@num, @den
    end

    def inverse  # division by zero handled in validate
      Fraction.new @den, @num
    end

  end

  class Complex
    attr_reader :real, :imgn
    include GtLtOrEqual

    def initialize param, imgn=0.0
      if param.is_a? String  # "-3.2i", "2", "1.2 -3i" are all valid
	comp = /^\s*(-?[\d\.]+)\s*([+-])\s*([\d\.]+)[iI]\s*$/.match param
	rnum = /^\s*(-?[\d\.]+)\s*$/.match param
	inum = /^\s*(-?[\d\.]+)[iI]\s*$/.match param
	if not comp.nil?
	  real = (comp.values_at 1).first.to_f
	  imgn = ((comp.values_at 2).first + (comp.values_at 3).first).to_f
	elsif not rnum.nil?
	  real = (rnum.values_at 1).first.to_f
	  imgn = 0.0
	elsif not inum.nil?
	  real = 0.0
	  imgn = (inum.values_at 1).first.to_f
	else
	  raise ArgumentError.new "Complex.new: " \
				  "Parameter must be in 2+3i form"
	end
      elsif param.is_a? Numeric and imgn.is_a? Numeric
	real = param
      else
	raise ArgumentError.new "Complex.new: " \
				"Parameters must form a complex number"
      end
      @real = real.to_f
      @imgn = imgn.to_f
    end

    def to_s
      real = imgn = sign = i = nil
      if 0 != @imgn
	imgn = @imgn
	i = 'i'
      end
      if 0 != @real
	real = @real
      end
      if real and 0 < @imgn
	sign = '+'
      end
      str = "#{real}#{sign}#{imgn}#{i}"
      if '' == str
	"0.0"
      else
	str
      end
    end

    def inspect
      "complex number: " + self.to_s
    end

    # In ruby coerce is used to allow operators work both ways around objects
    # that can be transformed into fractions or complex numbers.  Based on the
    # article http://c2.com/cgi/wiki?RubyCoerce by ElizabethWiethoff.
    def coerce number
      return (Complex.new number), self
    end

    # distance between two complex numbers
    def distance comp
      unless comp.is_a? Complex
	comp = Complex.new comp
      end
      Math.sqrt (@real - comp.real)**2 + (@imgn - comp.imgn)**2
    end

    # Compare is the values are closer to each other than the distance
    # of the bigger value to 0+0i times 10^-6.  The comparison must be done
    # using >= and not > as otherwise 0+0i != 0+0i.
    def approximatelyEquals comp
      unless comp.is_a? Complex
	comp = Complex.new comp
      end
      if self > comp
	bigger = self
      else
	bigger = comp
      end
      (bigger.distance 0) * 0.000001 >= (self.distance comp)
    end

    # assert_equal from test/unit needs this
    def == comp
      self.approximatelyEquals comp
    end

    def > comp
      unless comp.is_a? Complex
	comp = Complex.new comp
      end
      (self.distance 0) > (comp.distance 0)
    end

    def < comp
      unless comp.is_a? Complex
	comp = Complex.new comp
      end
      (self.distance 0) < (comp.distance 0)
    end

    def + comp
      unless comp.is_a? Complex
	comp = Complex.new comp
      end
      Complex.new (@real + comp.real), (@imgn + comp.imgn)
    end

    def - comp
      unless comp.is_a? Complex
	comp = Complex.new comp
      end
      Complex.new (@real - comp.real), (@imgn - comp.imgn)
    end

    def * comp
      unless comp.is_a? Complex
	comp = Complex.new comp
      end
      Complex.new (@real * comp.real - @imgn * comp.imgn) \
		, (@real * comp.imgn + @imgn * comp.real)
    end

    def / comp
      unless comp.is_a? Complex
	comp = Complex.new comp
      end
      if 0 == comp.real and 0 == comp.imgn
	raise ZeroDivisionError.new \
	      "Complex: Denominator cannot be zero (0+0i)"
      end
      Complex.new ((@real*comp.real + @imgn*comp.imgn) /
					(comp.real**2 + comp.imgn**2)) \
		, ((@imgn*comp.real - @real*comp.imgn) /
					(comp.real**2 + comp.imgn**2))
    end

    def abs
      self.distance 0
    end

    def negate
      Complex.new -@real, -@imgn
    end

    def conjugate
      Complex.new @real, -@imgn
    end

  end

end

