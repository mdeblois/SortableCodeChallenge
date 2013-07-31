import scala.io.Source
import scala.util.parsing.json._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import java.io._
import scala.util.control.Breaks._
import EStatusCode._

object Parser {
  def ParseProducts(filename: String): (EStatusCode, List[Node]) = {
    // manufacturer list to create the tree
    var productTree: ListBuffer[Node] = ListBuffer[Node]()

    // need to read each line which is a JSON object representing a product
    for (line <- Source.fromFile(filename).getLines()) {
      var product: Product = new Product

      // Get JSON value and set it to the new product
      val json: Option[Any] = JSON.parseFull(line)
      val productMap: Map[String, String] = json.get.asInstanceOf[Map[String, String]]
      product.productName = productMap.getOrElse("product_name", "")
      product.manufacturer = productMap.getOrElse("manufacturer", "")
      product.family = productMap.getOrElse("family", "")
      product.model = productMap.getOrElse("model", "")
      product.announcedDate = productMap.getOrElse("announced-date", "")

      // clean manufacturer string and verify if it already exists in the list
      val manufacturerCompareValue = CleanString(product.manufacturer)
      var manufacturerNode: Node = {
        if (productTree.exists { _.compareValue.equals(manufacturerCompareValue) }) {
          productTree.find { _.compareValue.equals(manufacturerCompareValue) }.get
        } else {
          // return a new node and add it to the tree
          var newNode = new Node(product.manufacturer, manufacturerCompareValue, None, None)
          productTree.append(newNode)
          newNode
        }
      }

      // now we have the manufacturer node, so we need to check if the model 
      // node already exists by using the 'cleaned' model string
      val modelCompareValue = CleanString(product.model)
      if (manufacturerNode.children.isDefined && manufacturerNode.children.get.exists { _.compareValue.equals(modelCompareValue) }) {
        // model already exists, so now we need to add another level of children 
        // for families, but only if both models have a family defined
        // so first get current node and verify with current product
        var modelNode = manufacturerNode.children.get.find { _.compareValue.equals(modelCompareValue) }.get
        if (!modelNode.product.get.family.equals("") && !product.family.equals("")) {
          // now check if model node already has family children
          if (!modelNode.children.isDefined) {
            // this node does not already have children, so create it and remove
            // product from the node

            // create family node for found node
            var foundNodeFamilyNode = new Node(modelNode.product.get.family, CleanString(modelNode.product.get.family), modelNode.product, None)
            // create current product family node
            var currentProductFamilyNode = new Node(product.family, CleanString(product.family), Option(product), None)

            // create list and add both nodes to the model node
            modelNode.children = Option(ListBuffer[Node]())
            modelNode.children.get.append(foundNodeFamilyNode)
            modelNode.children.get.append(currentProductFamilyNode)

            // remove product from found model node
            modelNode.product = None
          } else {
            // children list already exists so if family does not exist
            // add it to the list
            val familyCompareValue = CleanString(product.family)
            if (!modelNode.children.get.exists(n => n.compareValue.equals(familyCompareValue))) {
              // create current product family node
              var currentProductFamilyNode = new Node(product.family, familyCompareValue, Option(product), None)

              modelNode.children.get.append(currentProductFamilyNode)
            }
          }
        }

      } else {
        // create new node for this model and append it to the manufacturer's children list
        var newNode = new Node(product.model, modelCompareValue, Option[Product](product), None)
        if (manufacturerNode.children.isEmpty) {
          // create list if not already
          manufacturerNode.children = Option(ListBuffer[Node]())
        }

        // add model to list
        manufacturerNode.children.get.append(newNode)
      }

    }

    (EStatusCode.Success, productTree.toList)
  }

  /// This method will take the listings, read one line at a time, 
  /// and compare the listing to the list of products and create a list
  /// of results to return
  def ParseListings(filename: String, productTree: List[Node]): (EStatusCode, Map[String, ListBuffer[Listing]]) = {
    // the result is a map of the product name to the listing
    var results = scala.collection.mutable.Map[String, ListBuffer[Listing]]();

    for (line <- Source.fromFile(filename).getLines()) {

      // if line is empty, go to the next one
      if (!line.equals("")) {
        var listing = new Listing

        // Get JSON value and set the values of the listing
        val json: Option[Any] = JSON.parseFull(line)
        val listingMap: Map[String, String] = json.get.asInstanceOf[Map[String, String]]
        listing.title = listingMap.getOrElse("title", "")
        listing.manufacturer = listingMap.getOrElse("manufacturer", "")
        listing.currency = listingMap.getOrElse("currency", "")
        listing.price = listingMap.getOrElse("price", "")

        // clean manufacturer and check if it matches
        val cleanManufacturer = CleanString(listing.manufacturer)

        // get list of matched manufacturer
        val manufacturers = productTree.filter(m => cleanManufacturer.contains(m.compareValue))

        // continue if only 1 manufacturer was found
        if (manufacturers.length == 1) {
          // clean and parse title
          val cleanListingTitle = CleanString(listing.title)
          val parsedListingTitle = ParseString(listing.title)
          // go through the list of models of this manufacturer and check if model matches
          var matchedNode: Option[Node] = None
          breakable {
            for (modelNode <- manufacturers(0).children.get.filter(DoesModelMatch(cleanListingTitle, parsedListingTitle, _))) {

              // the models in this body matched listing
              // check to see if there are multiple families for this model
              val compareNode: Option[Node] = {
                if (modelNode.children.isDefined) {
                  // there are multiple families so need to check if a family is specified
                  // in the listing title
                  var matchedFamilyNodes = modelNode.children.get.filter(n => cleanListingTitle.contains(n.compareValue))
                  if (matchedFamilyNodes.length == 1) {
                    // there's only one child so get it and set the compare
                    val familyNode = matchedFamilyNodes(0)
                    Option(new Node(modelNode.value, modelNode.compareValue, familyNode.product, None))
                  } else {
                    // don't set compare node since there are multiple families in the title
                    // and can't therefore be tied to one product
                    None
                  }
                } else {
                  // set compare node to modelNode copy
                  Option(new Node(modelNode.value, modelNode.compareValue, modelNode.product, None))
                }
              }

              // check to make sure a model was found before checking it against the matched model
              if (compareNode.isDefined) {
                if (matchedNode.isEmpty) {
                  // if matched model has not been set yet, set it with the compareNode
                  matchedNode = compareNode
                } else {
                  // node has been set, so check if name is at around the same location
                  // in the clean listing title and take the one with the longer name
                  val compareNodeIndex = cleanListingTitle.indexOf(compareNode.get.compareValue)
                  val matchedNodeIndex = cleanListingTitle.indexOf(matchedNode.get.compareValue)

                  if (compareNodeIndex == matchedNodeIndex
                    || (compareNodeIndex < matchedNodeIndex && matchedNodeIndex < (compareNodeIndex + compareNode.get.compareValue.length()))
                    || (matchedNodeIndex < compareNodeIndex && compareNodeIndex < (matchedNodeIndex + matchedNode.get.compareValue.length()))) {
                    // if the index is the same or the further one starts within the string of the earlier one, 
                    // then take the model node (product) that has the longer name
                    if (matchedNode.get.compareValue.length() < compareNode.get.compareValue.length()) {
                      matchedNode = compareNode
                    }
                  } else {
                    // the indices are too far apart and therefore must represent a different model
                    // if there are 2 models in one listing, it's not good so drop the current listing
                    // and go to the next
                    matchedNode = None
                    break
                  }
                }
              }
            }
          }

          // only if matched model is defined was there a product found to match 
          // the listing so add that to the result
          if (matchedNode.isDefined) {
            // make sure this product has an entry before adding to the list
            if (!results.contains(matchedNode.get.product.get.productName)) {
              results.put(matchedNode.get.product.get.productName, ListBuffer[Listing]())
            }

            results(matchedNode.get.product.get.productName).append(listing)
          } else {
            // not product found for this listing so log it
            Logger.log("There were no models found for this listing: " + line)
          }

        } else {
          // there were no or multiple manufacturers found
          // so just log it
          Logger.log("Manufacturers found for " + listing.manufacturer + ": " + manufacturers.length)
        }

      }
    }

    (EStatusCode.Success, results.toMap)
  }

  /// This method will return if the model name 
  def DoesModelMatch(title: String, parsedTitle: Array[String], modelNode: Node): Boolean = {
    // if model is not contained within the title, then 
    // return false right away
    if (!title.contains(modelNode.compareValue)) {
      return false
    }

    // go through the title's substring and try to match
    // subsequent substrings to model's name
    var lastIndex = -1
    var comparedLength = 0
    for (subStr <- parsedTitle) {
      if (subStr.length() > modelNode.compareValue.length()) {
        // if the sub string in title is greater than model, then
        // check if manufacturer and/or family can be prepended to the string
        // if still not equal, then it's definitely not this one, so just 
        // move on to the next sub string
        var familyList: ListBuffer[String] = ListBuffer()
        var manufacturer:String = ""
        if (modelNode.children.isDefined) {
          if(modelNode.children.get.length > 0){
            manufacturer = CleanString(modelNode.children.get(0).product.get.manufacturer)
          }
          // there are multiple families, so need to go through each
          for (familyNode <- modelNode.children.get) {
            familyList.append(familyNode.compareValue)
          }
        } else {
          // just add current family to family list
          familyList.append(CleanString(modelNode.product.get.family))
          manufacturer = CleanString(modelNode.product.get.manufacturer)
        }

        // now go through and and check if [manufacturer] and or [family] + [model] is equal
        // to the current title sub string
        for (family <- familyList) {
          if ((family + modelNode.compareValue).equals(subStr)
            || (manufacturer + family + modelNode.compareValue).equals(subStr)) {
            // so it was equal so return true
            return true
          }
        }

        // none was found so just reset
        lastIndex = -1
        comparedLength = 0
      } else if (subStr.length() == modelNode.compareValue.length()) {
        // just need to check if the two string are equal,
        // if yes, then this model matches, if not, need 
        // to reset
        if (subStr.equals(modelNode.compareValue)) {
          return true
        }

        lastIndex = -1
        comparedLength = 0
      } else { // subStr length less than model length
        // check if model contains this substring and record index
        val index = modelNode.compareValue.indexOf(subStr, comparedLength)
        if (index < 0) {
          // reset values
          lastIndex = -1
          comparedLength = 0
        } else if (index > -1 && lastIndex == -1) {
          // this is the first found string, so set the lastIndex
          lastIndex = index
          comparedLength = subStr.length()
        } else {
          comparedLength += subStr.length()
          if (modelNode.compareValue.length == comparedLength) {
            // model matches so get out
            return true
          }
        }
      }
    }

    false
  }

  /// this method will write the results to the specified file in JSON format
  def WriteResultsToFile(filename: String, results: Map[String, ListBuffer[Listing]]): Unit = {
    // create print writer
    val resultsWriter = new PrintWriter(new File(filename))

    for (product <- results.keys) {
      var listingsList = ListBuffer[JSONObject]()
      for (listing <- results(product)) {
        var listingMap = Map(("title", listing.title), ("manufacturer", listing.manufacturer), ("currency", listing.currency), ("price", listing.price))
        listingsList.append(JSONObject(listingMap))
      }
      // now we have the list of listings, create product map
      var productMap = Map[String, Any](("product_name", product), ("listings", JSONArray(listingsList.toList)))
      resultsWriter.write(JSONObject(productMap).toString() + "\n")
    }
    resultsWriter.flush()
    resultsWriter.close()
  }

  def CleanString(stringToClean: String): String = {
    stringToClean.replaceAll("""[ \\/_\-\.,\+]""", "").toLowerCase()
  }

  def ParseString(stringToParse: String): Array[String] = {
    stringToParse.toLowerCase().split("""[ \\/_\-\.,\+]""")
  }
}