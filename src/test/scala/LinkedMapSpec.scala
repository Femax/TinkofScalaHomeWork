import org.scalatest._

class LinkedMapSpec extends FlatSpec {

  val linkedMap = LinkedMap.apply(("1", 1), ("2", 2), ("3", 3))


  "A LinkedMap" should "create LinkedMap with updated values" in {
    val updatedLinkedMap = linkedMap.update("2", 3).update("1", 3)

    assert(updatedLinkedMap.apply("1").get == 3)
    assert(updatedLinkedMap.apply("2").get == 3)
  }

  "A LinkedMap" should "should contain 1, 2, 3" in {
    assert(linkedMap.contains("1"))
    assert(linkedMap.contains("2"))
    assert(linkedMap.contains("3"))
  }

  "A LinkedMap" should "should reverse" in {
    val updatedLinkedMap = linkedMap.reverse
    assert(updatedLinkedMap.linkedList.getFirst._2 == 3)
  }

  "A LinkedMap" should "should delete all elemets" in {
    val updatedLinkedMap = linkedMap.delete("2").delete("1").delete("3")
    assert(updatedLinkedMap.isEmpty)
  }

  "A LinkedMap" should "should ++ ('4',4),('5',5)" in {
    val updatedLinkedMap = linkedMap.++(LinkedMap.apply(("4", 4), ("5", 5)))
    assert(updatedLinkedMap.apply("4").getOrElse(0) == 4)
    assert(updatedLinkedMap.apply("5").getOrElse(0) == 5)
  }

  "A LinkedMap" should "map mutate to (String, String)" in {
    val updatedLinkedMap = linkedMap.mapValues((value: Int) => value.toString)
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
    assert(stringBuffer.toString == "112233")
  }


}
