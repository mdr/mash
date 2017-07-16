package com.github.mdr.mash.inference

import java.time.{ Instant, LocalDate }

import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.os.{ PathClass, PermissionsSectionClass }
import com.github.mdr.mash.ns.time.{ DateClass, DateTimeClass, DaysClass }
import com.github.mdr.mash.runtime._
import org.scalatest.{ FlatSpec, Matchers }

import scala.collection.immutable.ListMap

class ValueTypeDetectorTest extends FlatSpec with Matchers {

  "ValueTypeDetector" should "detect simple types" in {
    ValueTypeDetector.getType(MashBoolean.True) shouldEqual Type.Instance(BooleanClass)
    ValueTypeDetector.getType(MashBoolean.False) shouldEqual Type.Instance(BooleanClass)
    ValueTypeDetector.getType(MashUnit) shouldEqual Type.Instance(UnitClass)
    ValueTypeDetector.getType(MashNull) shouldEqual Type.Instance(NullClass)
    ValueTypeDetector.getType(MashWrapped(Instant.now)) shouldEqual Type.Instance(DateTimeClass)
    ValueTypeDetector.getType(MashWrapped(LocalDate.now)) shouldEqual Type.Instance(DateClass)
    ValueTypeDetector.getType(MashString("foo")) shouldEqual Type.Instance(StringClass)
    ValueTypeDetector.getType(MashString("foo", Some(PathClass))) shouldEqual (StringClass taggedWith PathClass)
    ValueTypeDetector.getType(MashNumber(1)) shouldEqual Type.Instance(NumberClass)
    ValueTypeDetector.getType(MashNumber(1, Some(DaysClass))) shouldEqual (NumberClass taggedWith DaysClass)
  }

  "ValueTypeDetector" should "detect object types in a classless object" in {
    ValueTypeDetector.getType(MashObject.of(Seq("foo" -> MashBoolean.True))) shouldEqual
      Type.Object(ListMap("foo" -> BooleanClass))
  }

  "ValueTypeDetector" should "detect object types in an object with a class" in {
    import PermissionsSectionClass.Fields._
    val obj = MashObject.of(
      fields = ListMap(
        CanRead -> MashBoolean.True,
        CanWrite -> MashBoolean.True,
        CanExecute -> MashBoolean.True),
      PermissionsSectionClass)
    ValueTypeDetector.getType(obj) shouldEqual Type.Instance(PermissionsSectionClass)
  }
}