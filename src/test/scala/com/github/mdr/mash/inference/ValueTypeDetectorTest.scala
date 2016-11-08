package com.github.mdr.mash.inference

import java.time.Instant
import java.time.LocalDate

import scala.collection.immutable.ListMap

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.github.mdr.mash.ns.core._
import com.github.mdr.mash.ns.os.PathClass
import com.github.mdr.mash.ns.os.PermissionsSectionClass
import com.github.mdr.mash.ns.time.DateTimeClass
import com.github.mdr.mash.ns.time.DaysClass
import com.github.mdr.mash.ns.time.LocalDateClass
import com.github.mdr.mash.runtime._

class ValueTypeDetectorTest extends FlatSpec with Matchers {

  "ValueTypeDetector" should "detect simple types" in {
    ValueTypeDetector.getType(MashBoolean.True) shouldEqual Type.Instance(BooleanClass)
    ValueTypeDetector.getType(MashBoolean.False) shouldEqual Type.Instance(BooleanClass)
    ValueTypeDetector.getType(MashUnit) shouldEqual Type.Instance(UnitClass)
    ValueTypeDetector.getType(MashNull) shouldEqual Type.Instance(NullClass)
    ValueTypeDetector.getType(MashWrapped(Instant.now)) shouldEqual Type.Instance(DateTimeClass)
    ValueTypeDetector.getType(MashWrapped(LocalDate.now)) shouldEqual Type.Instance(LocalDateClass)
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