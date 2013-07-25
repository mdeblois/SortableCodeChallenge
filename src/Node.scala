import scala.collection.mutable.ListBuffer

class Node(xvalue: String, xcompareValue: String, xproduct: Option[Product], xchildren: Option[ListBuffer[Node]]) {
  var value: String = xvalue
  var compareValue: String = xcompareValue
  var children: Option[ListBuffer[Node]] = xchildren
  var product: Option[Product] = xproduct
}