object CodeChallenge {

  def main(args: Array[String]): Unit = {
    // first get the arguments passed in (if any)
    Console.println("parsing arguments")
    val productsFilename = if (args.length > 0) args(0) else "products.txt"
    val listingsFilename = if (args.length > 1) args(1) else "listings.txt"
    val resultsFilename = if (args.length > 2) args(2) else "results.txt"

    // read and parse the products
    Console.println("parsing products")
    val products = Parser.ParseProducts(productsFilename)

    // read and parse the listings
    Console.println("parsing listings")
    val results = Parser.ParseListings(listingsFilename, products)

    // write the results
    Console.println("writing results")
    Parser.WriteResultsToFile(resultsFilename, results)
  }

}