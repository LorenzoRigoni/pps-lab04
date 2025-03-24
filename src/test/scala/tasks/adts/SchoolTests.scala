package tasks.adts

import org.junit.*
import org.junit.Assert.*
import tasks.adts.SchoolModel.*

class SchoolTests:
  val schoolModel: SchoolModule = BasicSchoolModule

  import schoolModel.*
  import u03.extensionmethods.Sequences.*
  import Sequence.*

  val emptySchool: School = schoolModel.emptySchool

  @Test def testEmptySchool(): Unit =
    assertEquals(Nil(), emptySchool.teachers)
    assertEquals(Nil(), emptySchool.courses)
    assertFalse(emptySchool.hasTeacher("John"))
    assertFalse(emptySchool.hasCourse("Math"))

  @Test def testAddTeachersAndCourses(): Unit =
    val teacherJohn = teacher("John")
    val mathCourse = course("Math")
    val notEmptySchool = emptySchool.setTeacherToCourse(teacherJohn, mathCourse)
    assertEquals(Cons("John", Nil()), notEmptySchool.teachers)
    assertEquals(Cons("Math", Nil()), notEmptySchool.courses)
    assertTrue(notEmptySchool.hasTeacher("John"))
    assertTrue(notEmptySchool.hasCourse("Math"))
    assertFalse(notEmptySchool.hasCourse("Italian"))

  @Test def testAddCourseToExistingTeacher(): Unit =
    val teacherJohn = teacher("John")
    val mathCourse = course("Math")
    val firstSchool = emptySchool.setTeacherToCourse(teacherJohn, mathCourse)
    val italian = course("Italian")
    val secondSchool = firstSchool.setTeacherToCourse(teacherJohn, italian)
    assertEquals(Cons("John", Nil()), secondSchool.teachers)
    assertEquals(Cons("Math", Cons("Italian", Nil())), secondSchool.courses)
    assertTrue(secondSchool.hasTeacher("John"))
    assertTrue(secondSchool.hasCourse("Math"))
    assertTrue(secondSchool.hasCourse("Italian"))
    assertEquals(Cons("Math", Cons("Italian", Nil())), secondSchool.coursesOfATeacher(teacher("John")))