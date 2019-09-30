object Main {
    def encodeHuffman(data: String): (String, collection.mutable.Map[Char, String]) = {
        val numSymbols = data.length
        //map used to map a symbol to its codeword
        val symbolToCode = collection.mutable.Map[Char, String]().withDefaultValue("")
        //map used to map a symbol to its frequency in the input (to create probabilities)
        val symbolToAmount = collection.mutable.Map[Char, Int]().withDefaultValue(0)
        val queue = collection.mutable.PriorityQueue.empty[(Int, String)](implicitly[Ordering[(Int, String)]].reverse)

        //count symbols from input
        data.foreach( c => symbolToAmount(c) = 1 + symbolToAmount.apply(c) )

        //construct priority queue
        symbolToAmount.foreach(t => queue.enqueue((t._2, t._1.toString)))
        println(queue.clone.dequeueAll)

        while(queue.length > 1) {
          val leftNode = queue.dequeue
          val rightNode = queue.dequeue
          val newNode = (leftNode._1 + rightNode._1, leftNode._2 + rightNode._2)

          //construct codewords for symbols
          //prepend 0 for each symbol from the left node and 1 for each symbol of the right node
          leftNode._2.foreach( c => symbolToCode(c) = "0" + symbolToCode.apply(c) )
          rightNode._2.foreach( c => symbolToCode(c) = "1" + symbolToCode.apply(c) )

          println("Merging " + leftNode + " with " + rightNode + " -> " + newNode)
          queue.enqueue(newNode)
        }

        var encodedInput = ""
        data.foreach( c => encodedInput += symbolToCode.get(c).get )
        (encodedInput, symbolToCode)
    }

    def decodeHuffman(data: String, code: collection.mutable.Map[Char, String]): String = {
        //build map from codeword to symbol using the code map
        var codeToSymbol = collection.mutable.Map[String, Char]()
        code.foreach( t => codeToSymbol(t._2) = t._1 )

        var decodedOutput = ""
        var buffer = ""
        data.foreach( c => {
            //add encoded character to buffer
            buffer += c
            if( codeToSymbol.contains(buffer) ){
                //only if buffer contains a valid codeword:
                //add correspondent symbol to decoded output
                decodedOutput += codeToSymbol.get(buffer).get
                //and clear buffer to build the next codeword
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
