object CodeChallenge {

  def main(args: Array[String]): Unit = {
    // first get the arguments passed in (if any)
    Console.println("parsing arguments")
    val productsFilename = if (args.length > 0) args(0) else "products.txt"
    val listingsFilename = if (args.length > 1) args(1) else "listings.txt"
    val resultsFilename = if (args.length > 2) args(2) else "results.txt"

    // read and parse the products
    Console.println("parsing products")
    val (productStatusCode, products) = Parser.ParseProducts(productsFilename)
    if(productStatusCode != EStatusCode.Success){
      Console.println("Error Parsing Products:"+EStatusCode.toString())
      exit(-1)
    }
    
    // read and parse the listings
    Console.println("parsing listings")
    val (listingsStatusCode, results) = Parser.ParseListings(listingsFilename, products)

    // write the results if successful
    if(listingsStatusCode != EStatusCode.Success){
      Console.println("Error Parsing Listings:" + EStatusCode.toString())
      exit(-1)
    }
    
    Console.println("writing results")
    Parser.WriteResultsToFile(resultsFilename, results)
    
  }

}