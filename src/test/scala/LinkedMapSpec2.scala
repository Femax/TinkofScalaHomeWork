import org.scalatest._

class LinkedMapSpec2 extends FlatSpec {

  val linkedMap = LinkedMap.apply(("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5))


  "A LinkedMap" should "create LinkedMap with updated values" in {
    val updatedLinkedMap = linkedMap.update("2", 3)
    assert(updatedLinkedMap.apply("1").get == 1)
    assert(updatedLinkedMap.apply("2").get == 3)
    println(updatedLinkedMap)
    val updatedLinkedMap3 = linkedMap.update("7", 7).update("6", 6)
    assert(updatedLinkedMap3.apply("7").get == 7)
    assert(updatedLinkedMap3.apply("6").get == 6)
    println(updatedLinkedMap3)

    val linkedMap3 = LinkedMap((2147483647,""), (2147483647,""))
    val updated4 = linkedMap3.update(0, "")
    println(updated4)

  }

  "A LinkedMap" should "should contain 1, 2, 3" in {
    print(linkedMap)
    assert(linkedMap.contains("1"))
    assert(linkedMap.contains("2"))
    assert(linkedMap.contains("3"))
  }

  "A LinkedMap" should "should == after reverse to reversed" in {
    val lm = LinkedMap(("1", 1), ("2", 2), ("3", 3), ("4", 4))
    val lmreversed = LinkedMap(("4", 4), ("3", 3), ("2", 2), ("1", 1))
    assert(lm.reverse == lmreversed)
  }

  "A LinkedMap" should "should delete all elemets" in {
    val updatedLinkedMap = linkedMap.delete("2")
    assert(updatedLinkedMap.apply("1").getOrElse(0) == 1)
    assert(updatedLinkedMap.apply("3").getOrElse(0) == 3)
    assert(updatedLinkedMap.apply("2").isEmpty)
    val updatedLinkedMap1 = updatedLinkedMap.delete("1").delete("1")
    println(updatedLinkedMap1)
    assert(updatedLinkedMap1.apply("1").isEmpty)
    assert(updatedLinkedMap1.apply("2").isEmpty)
    assert(updatedLinkedMap1.apply("3").getOrElse(0) == 3)
    val updatedLinkedMap2 = updatedLinkedMap1.delete("3").delete("4").delete("5")
    assert(updatedLinkedMap2.isEmpty)
  }

  "A LinkedMap" should "should ++ ('4',4),('5',5)" in {
    val updatedLinkedMap = linkedMap.++(LinkedMap(("4", 4), ("5", 5)))
    assert(updatedLinkedMap.apply("4").getOrElse(0) == 4)
    assert(updatedLinkedMap.apply("5").getOrElse(0) == 5)
    val updatedLinkedMap2 = linkedMap.++(LinkedMap(("6", 6), ("7", 7)))
    assert(updatedLinkedMap2.apply("6").getOrElse(0) == 6)
    assert(updatedLinkedMap2.apply("7").getOrElse(0) == 7)
    val updatedLinkedMap3 = linkedMap.++(LinkedMap(("4", 66), ("5", 77)))
    println(updatedLinkedMap3)
    assert(updatedLinkedMap3.apply("4").getOrElse(0) == 4)
    assert(updatedLinkedMap3.apply("5").getOrElse(0) == 5)
  }

  "A LinkedMap" should "map mutate to (String, String)" in {
    val updatedLinkedMap = linkedMap.mapValues((value: Int) => value.toString)
    println(updatedLinkedMap)
    assert(updatedLinkedMap.apply("3").getOrElse(0) == "3")
  }

  "A LinkedMap" should s"map mutate to (key,#value+#key) (String, String)" in {
    val updatedLinkedMap = linkedMap.mapWithKey((key: String, value: Int) => value.toString + key)
    assert(updatedLinkedMap.apply("3").getOrElse(0) == "33")
  }

  "A LinkedMap" should s"map should do stringBuffer" in {
    val stringBuffer = new StringBuffer()
    linkedMap.foreach[Unit](pair => {
      stringBuffer append pair._1 append pair._2
    })
    assert(stringBuffer.toString == "1122334455")
  }


}
