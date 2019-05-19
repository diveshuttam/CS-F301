package pplAssignment

//Author: Divesh Uttamchandani
//While declaring val's, I have ommited the type to avoid clutter as the lines are already long
//Type is inferred from the return type of the function/value

object F2016A7TS0045P{
  //length of a list
  def length[A](l:List[A]):Int={
    if(l.isEmpty)
     0
    else
      1+length(l.tail)
  }

  //generic map function
  def myMap[A,B](l:List[A], f:A=>B): List[B]={
    if(l.isEmpty)
      List[B]()
    else
      f(l.head)::myMap(l.tail,f)
  }

  //generic Reduce function
  def myReduce[A,B](l:List[A], f:(A,B)=>B, c:B): B={
    if(l.isEmpty)
      c
    else
      f(l.head, myReduce(l.tail,f,c))
  }

  //drop numEle from front of l
  def dropFront[A](l:List[A], numEle:Int):List[A]={
    if(l.isEmpty || numEle<=0)
      l
    else
      dropFront(l.tail,numEle-1)
  }

  //get numEle from front of l
  def getFront[A](l:List[A], numEle:Int):List[A]={
    if(l.isEmpty || numEle<=0)
      List[A]()
    else
      l.head::getFront(l.tail,numEle-1)
  }

  //get 2d matrix from row_min to row_max and col_min to col_max(all inclusive)
  //1 based indexing followed
  def get2Dmatrix[A](l:List[List[A]], row_min:Int, row_max:Int, col_min:Int, col_max:Int):List[List[A]]={
    def getCols(l1:List[A]):List[A]={
      getFront(dropFront(l1,col_min-1), col_max-col_min+1)
    }
    myMap(getFront(dropFront(l,row_min-1), row_max-row_min+1),getCols) 
  }

  //Dot product of two 2d matrices
  def dotProduct(matrix_1:List[List[Double]], matrix_2:List[List[Double]]):Double = {
    def rowDotProduct(matrix_1:List[Double],matrix_2:List[Double]):Double = {
      if(matrix_1.isEmpty || matrix_2.isEmpty)
        0
      else
        matrix_1.head*matrix_2.head + rowDotProduct(matrix_1.tail, matrix_2.tail)
    }

    if(matrix_1.isEmpty || matrix_2.isEmpty)
      0
    else
      rowDotProduct(matrix_1.head, matrix_2.head) + dotProduct(matrix_1.tail, matrix_2.tail)
  }

  def convolute(Image:List[List[Double]], Kernel : List[List[Double]], imageSize: List[Int], kernelSize: List[Int]):List[List[Double]] = {
      val kernelRows = kernelSize.head
      val kernelCols = kernelSize.tail.head
      val imageRows = imageSize.head
      val imageCols = imageSize.tail.head
      if(imageRows>=kernelRows && imageCols>=kernelCols){
        val temp1 = dotProduct(get2Dmatrix(Image, 1, kernelRows, 1, kernelCols), Kernel)
        val temp2 = convolute(get2Dmatrix(Image,1,kernelRows,2,imageCols),Kernel, List(kernelRows, imageCols-1),kernelSize).head
        val temp3 = convolute(Image.tail, Kernel, List(imageRows-1, imageCols),kernelSize)
        if(temp3.head.isEmpty)
          List(temp1::temp2)
        else
          (temp1::temp2)::temp3
      }
      else
      {
        List(List[Double]())
      }
  }

  def activationLayer(activationFunc:Double => Double, Image:List[List[Double]]):List[List[Double]]={
    def activationFunc2d(l:List[Double]):List[Double]={
      myMap(l,activationFunc)
    }
    myMap(Image, activationFunc2d)
  }

  def singlePooling(poolingFunc:List[Double] => Double, Image: List[List[Double]], K:Int):List[Double]={
    def myConcat(l1:List[Double], l2:List[Double]):List[Double]={
      l1:::l2
    }
    val imageRows = length(Image)
    val imageCols = length(Image.head)
    def poolingHelper(poolingFunc:List[Double] => Double, Image: List[List[Double]], K:Int, imageRows:Int, imageCols:Int):List[Double]={
      if(imageRows==0 || imageCols==0)
        List[Double]()
      else
      {
        val temp1=poolingFunc(myReduce(get2Dmatrix(Image,1,K,1,K),myConcat,List[Double]()))
        val temp2=poolingHelper(poolingFunc, get2Dmatrix(Image,1,K,K+1,imageCols), K, K, imageCols-K)
        val temp3=poolingHelper(poolingFunc, get2Dmatrix(Image,K+1,imageRows,1,imageCols), K, imageRows-K, imageCols)
        (temp1::temp2):::temp3
      }
    }
    poolingHelper(poolingFunc, Image, K, imageRows, imageCols)
  }
  
  def poolingLayer(poolingFunc: List[Double] => Double, Image: List[List[Double]], K:Int):List[List[Double]]={
    if(Image.isEmpty || Image.head.isEmpty)
    {
      List(List[Double]())
    }
    else{
      val temp1=singlePooling(poolingFunc, getFront(Image,K), K)
      val temp2=poolingLayer(poolingFunc, dropFront(Image,K), K)
      if(temp2.isEmpty || temp2.head.isEmpty)
      {
        List(temp1)
      }
      else{
        temp1::temp2
      }
    }
  }

  def mixedLayer(Image:List[List[Double]], Kernel:List[List[Double]], imageSize: List[Int], kernelSize: List[Int], activationFunc:Double=>Double, poolingFunc: List[Double] => Double, K:Int):List[List[Double]]={
    val temp1 = convolute(Image, Kernel, imageSize, kernelSize)
    val temp2 = activationLayer(activationFunc, temp1)
    val temp3 = poolingLayer(poolingFunc, temp2, K)
    temp3
  }

  def normalise(Image:List[List[Double]]):List[List[Int]]={
    if(Image.isEmpty){
      List[List[Int]]()
    }
    else{
      //find maximum
      def findMaxNum(a:Double, b:Double):Double={
        if(a>b)
          a
        else
          b
      }

      def findMaxInArr(l:List[Double]):Double={
        val init = l.head
        myReduce(l, findMaxNum, init)
      }

      val maxMap = myMap(Image, findMaxInArr)
      val max = myReduce(maxMap, findMaxNum, maxMap.head)

      // find minimum
      def findMinNum(a:Double, b:Double):Double={
        if(a<b)
          a
        else
          b
      }

      def findMinInArr(l:List[Double]):Double={
        val init = l.head
        myReduce(l, findMinNum, init)
      }

      val minMap = myMap(Image, findMinInArr)
      val min = myReduce(minMap, findMinNum, maxMap.head)
      
      def normalise2d(l:List[Double]):List[Int]={
        def normalise1d(a:Double):Int={
          (((a-min)/(max-min))*255).round.toInt
        }
        myMap(l,normalise1d)
      }
      
      myMap(Image, normalise2d)
    }
  }

  def ReLu(a:Double):Double={
    if(a>0)
      a
    else
      0
  }

  def LeakyReLu(a:Double):Double = {
    if(a > 0)
      a
    else
      0.5*a
  }

  def maxPooling(list:List[Double]):Double={
    def findMaxNum(a:Double, b:Double):Double={
        if(a>b)
          a
        else
          b
      }
    myReduce(list,findMaxNum,list.head)
  }

  def addMatrix(matrix_1:List[List[Double]], matrix_2:List[List[Double]]):List[List[Double]] = {
    def rowAdd(matrix_1:List[Double],matrix_2:List[Double]):List[Double] = {
      if(matrix_1.isEmpty || matrix_2.isEmpty)
        List[Double]()
      else
        matrix_1.head+matrix_2.head::rowAdd(matrix_1.tail, matrix_2.tail)
    }

    if(matrix_1.isEmpty || matrix_2.isEmpty)
      List(List())
    else
    {
      val temp1=rowAdd(matrix_1.head, matrix_2.head)
      val temp2=addMatrix(matrix_1.tail, matrix_2.tail)
      if(temp2.head.isEmpty)
      {
        List(temp1)
      }
      else{
        temp1::temp2
      }
    }
  }

  def avgPooling(list:List[Double]):Double = {
    def sum(a:Double, b:Double):Double={
      a+b
    }
    myReduce(list, sum, 0.0)/length(list)
  }

  def assembly(Image:List[List[Double]], imageSize:List[Int], w1: Double, w2: Double,b:Double, Kernel1: List[List[Double]], kernelSize1:List[Int], Kernel2: List[List[Double]], kernelSize2:List[Int], Kernel3: List[List[Double]], kernelSize3:List[Int], Size:Int):List[List[Int]]={
    //top multiply acts to bind weight (like a decorator)
    def multiply(w:Double):(List[Double])=>List[Double]={
      def multiply_2d(l:List[Double]):List[Double]={
          def multiply1d(a:Double):Double={
            a*w
          }
          myMap(l,multiply1d)
      }
      multiply_2d
    }
    //bais is constant so no need of returning function
    def addBias(l:List[Double]):List[Double]={
        def add1d(a:Double):Double={
          a+b
        }
        myMap(l,add1d)
    }
    val temp1 = mixedLayer(Image, Kernel1, imageSize, kernelSize1, ReLu, avgPooling, Size)
    val temp2 = mixedLayer(Image, Kernel2, imageSize, kernelSize2, ReLu, avgPooling, Size)
    val temp3 = myMap(temp1,multiply(w1))
    val temp4 = myMap(temp2,multiply(w2))
    val temp5 = addMatrix(temp3, temp4)
    val temp6 = myMap(temp5, addBias)
    val temp6size = List(length(temp6), length(temp6.head))
    val temp7 = mixedLayer(temp6, Kernel3, temp6size, kernelSize3, LeakyReLu, maxPooling, Size)
    normalise(temp7)
  }
}
