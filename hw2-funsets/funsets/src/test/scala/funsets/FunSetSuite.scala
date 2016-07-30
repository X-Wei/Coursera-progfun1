package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * This class is a test suite for the methods in object FunSets. To run
  * the test suite, you can either:
  * - run the "test" command in the SBT console
  * - right-click the file in eclipse and chose "Run As" - "JUnit Test"
  */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
    * Link to the scaladoc - very clear and detailed tutorial of FunSuite
    *
    * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
    *
    * Operators
    * - test
    * - ignore
    * - pending
    */

  /**
    * Tests are written using the "test" operator and the "assert" method.
    */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
    * For ScalaTest tests, there exists a special equality operator "===" that
    * can be used inside "assert". If the assertion fails, the two values will
    * be printed in the error message. Otherwise, when using "==", the test
    * error message will only say "assertion failed", without showing the values.
    *
    * Try it out! Change the values so that the assertion fails, and look at the
    * error message.
    */
  test("adding ints") {
    assert(1 + 2 === 3)
  }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
    * When writing tests, one would often like to re-use certain values for multiple
    * tests. For instance, we would like to create an Int-set and have multiple test
    * about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we can
    * store it in the test class using a val:
    *
    * val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes? Then
    * the test methods are not even executed, because creating an instance of the
    * test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    *
    */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val all:Set = x => true
    val s1234:Set = n => n<5 && n>0
  }

  /**
    * This test is currently disabled (by using "ignore") because the method
    * "singletonSet" is not yet implemented and the test would fail.
    *
    * Once you finish your implementation of "singletonSet", exchange the
    * function "ignore" by "test".
    */
  test("singletonSet(1) contains 1") {

    /**
      * We create a new instance of the "TestSets" trait, this gives us access
      * to the values "s1" to "s3".
      */
    new TestSets {
      /**
        * The string argument of "assert" is a message that is printed in case
        * the test fails. This helps identifying which assertion failed.
        */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection contains only elements in both set") {
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      val s = intersect(s12, s23)
      assert(!contains(s, 1), "{1 2}^{2 3} doesn't contain 1")
      assert(!contains(s, 3), "{1 2}^{2 3} doesn't contain 3")
      assert(contains(s, 2), "{1 2}^{2 3} contains 2")
    }
  }

  test("diff(s12, s23) = {1}") {
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      val s = diff(s12, s23)
      assert(!contains(s, 2), "{1 2}-{2 3} doesn't contain 2")
      assert(!contains(s, 3), "{1 2}-{2 3} doesn't contain 3")
      assert(contains(s, 1), "{1 2}-{2 3} contains 1")
    }
  }

  test("filter(s123, elem is odd) = {1,3}") {
    new TestSets {
      val s12 = union(s1, s2)
      val s123 = union(s12, s3)
      val s = filter(s123, x => x % 2 == 1)
      assert(!contains(s, 2), "filter(s123, x odd) doesn't contain 2"+FunSets.toString(s))
      assert(contains(s, 3), "filter(s123, x odd) contains 3")
      assert(contains(s, 1), "filter(s123, x odd) contains 1")
    }
  }
  test("test forall") {
    new TestSets {
      assert(forall(all, x=>true), "forall(all, x=>true) is true")
      assert(!forall(all, x=> x%2==1), "forall(all, x odd) is false")
    }
  }

  test("test exists") {
    new TestSets {
      assert(!exists(all, x=>false), "!exists(all, x=>false)")
      assert(exists(all, x=> x%2==1), "exists(all, x=> x%2==1)")
      assert(exists(all, x => x===666), "exists(all, x => x===666)")
      assert(exists(s1234, x => x===4), "exists(all, x => x===666)")
    }
  }

  test("test map"){
    new TestSets {
      val s = map(s1234, x => x*x)
      assert(contains(s,1),"1 is in map(s1234, x=>x*x)")
      assert(!contains(s,2),"2 is not in map(s1234, x=>x*x) "+FunSets.toString(s))
      assert(!contains(s,3),"3 is not in map(s1234, x=>x*x) ")
      assert(contains(s,4),"4 is in map(s1234, x=>x*x) ")
      assert(contains(s,9),"9 is in map(s1234, x=>x*x) ")
      assert(contains(s,16),"16 is in map(s1234, x=>x*x) ")
    }
  }


}
