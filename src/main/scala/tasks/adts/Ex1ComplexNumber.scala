package tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:
    case class ComplexNumber(re: Double, im: Double)

    // Change assignment below: should probably define a case class and use it?
    type Complex = ComplexNumber

    def complex(re: Double, im: Double): Complex = ComplexNumber(re, im)

    extension (complex: Complex)
      def re(): Double = complex match
        case ComplexNumber(re, _) => re

      def im(): Double = complex match
        case ComplexNumber(_, im) => im

      def sum(other: Complex): Complex = complex match
        case ComplexNumber(re, im) => ComplexNumber(re + other.re, im + other.im)

      def subtract(other: Complex): Complex = complex match
        case ComplexNumber(re, im) => ComplexNumber(re - other.re, im - other.im)

      def asString(): String = complex match
        case ComplexNumber(re, im) =>
          (if im == 0 || re != 0 then re.toString else "") +
            (if im > 0 && re != 0 then " + " else if re == 0 || im == 0 then "" else " - ") +
            (if im > 0 then im.toString + "i"
            else if im < 0 && re != 0 then math.abs(im) + "i"
            else if re == 0 && im != 0 then im + "i"
            else "")