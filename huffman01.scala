object Main {
    def encodeHuffman(data: String): (String, collection.mutable.Map[Char, String]) = {
        val numSymbols = data.length
        val numSymbolsD = numSymbols * 2
        val symbolToAmount = collection.mutable.Map[Char, Int]()
        val symbolToCode = collection.mutable.Map[Char, String]()
        val queue = collection.mutable.PriorityQueue[(Int, String)]()

        data.foreach( c => {symbolToAmount.update (c, 1 + {symbolToAmount.getOrElse(c, 0)})} )

        symbolToAmount.foreach(t => queue.enqueue((numSymbols - t._2, t._1.toString)))

        println(queue.clone.dequeueAll)

        while(queue.length > 1) {
          val first = queue.dequeue
          val second = queue.dequeue
          val newNode = (numSymbols - (numSymbolsD - (first._1 + second._1)), first._2 + second._2)

          first._2.foreach( c => {symbolToCode.update (c, '0' + {symbolToCode.getOrElse(c, "")})} )
          second._2.foreach( c => {symbolToCode.update (c, '1' + {symbolToCode.getOrElse(c, "")})} )

          println("Merging " + first + " with " + second + " -> " + newNode)
          queue.enqueue(newNode)
        }
        
        var encodedInput = ""
        data.foreach( c => encodedInput += symbolToCode.get(c).get )
        (encodedInput, symbolToCode)
    }

    def decodeHuffman(data: String, code: collection.mutable.Map[Char, String]): String = {
        var codeToSymbol = collection.mutable.Map[String, Char]()
        code.foreach( t => {codeToSymbol.update(t._2, t._1)} )

        var decodedOutput = ""
        var buffer = ""
        data.foreach( c => {
            buffer += c
            if( codeToSymbol.contains(buffer) ){
                decodedOutput += codeToSymbol.get(buffer).get
                buffer = ""
            }
        })
        decodedOutput
    }

    def main(args: Array[String]): Unit = {
        var encoded = encodeHuffman(args(0))
        println("Encoded: " + encoded._1)
        println("Decoded: " + decodeHuffman(encoded._1, encoded._2))
    }
}
