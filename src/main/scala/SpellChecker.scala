object SpellChecker extends App{

  def alphabet: Seq[Char] = 'a' to 'z'

  val docString = io.Source.fromFile("big.txt").mkString
  val words: Seq[String] = s"[${alphabet.head}-${alphabet.last}]+".r.findAllIn(docString.toLowerCase).toSeq

  def deletes(word: Seq[Char]): Seq[Seq[Char]] =
    word.indices.map{idx => word.patch(idx, List(), 1)}

  def transposes(word: Seq[Char]): Seq[Seq[Char]] = {
    val splits: Seq[(Seq[Char], Seq[Char])] = word.indices map word.splitAt
    splits collect {case (a, b0 +: b1 +: b2) => a ++: b1 +: b0 +: b2}
  }

  def replaces(word: Seq[Char]): Seq[Seq[Char]] = {
    val splits: Seq[(Seq[Char], Seq[Char])] = word.indices map word.splitAt
    alphabet.flatMap { c => splits collect { case (a, b0 +: b1) => a ++: c +: b1 } }
  }

  def inserts(word: Seq[Char]): Seq[Seq[Char]] = {
    val splits: Seq[(Seq[Char], Seq[Char])] = word.indices map word.splitAt
    alphabet flatMap {c => splits map {case (a, b) => a ++: c +: b}}
  }

  def edits(word: Seq[Char]): Seq[Seq[Char]] = {
    val d = deletes(word)
    val t = transposes(word)
    val i: Seq[Seq[Char]] = inserts(word)
    val r = replaces(word)
    d ++ t ++ r ++ i
  }

  val count: Map[String, Int] =
    words.groupBy(x => x).mapValues(_.size) withDefaultValue(0)

  def P(word: String): Double =
    count.get(word.mkString).map{occurrences => occurrences.toDouble/words.size} getOrElse 0.0

  def oneDistance(word: Seq[Char]): Seq[Seq[Char]] = edits(word)
  def twoDistance(word: Seq[Char]): Seq[Seq[Char]] =
    for{
      e1 <- edits(word)
      e2 <- edits(e1)
    } yield e2

  def twoDistance1(words: Seq[Seq[Char]]) = words.flatMap{ seqChar =>
    oneDistance(seqChar)
  }


  def check( word: Seq[Char] ) = {
    var edits1: Seq[Seq[Char]] = Seq.empty[Seq[Char]]
    if(count.get(word.mkString).isDefined) word
    else {
      edits1 = oneDistance(word)
      val candidates1 = edits1.map{_.mkString}.filter(count.contains)
      if (candidates1.size > 0){
        (candidates1 maxBy P).mkString
      }
      else {
        val edits2 = twoDistance1(edits1)
        val candidates2 = edits2.map{_.mkString}.filter(count.contains)
        if (candidates2.size > 0){
          (candidates2 maxBy P).mkString
        }
        else
          word
      }
    }
  }


  val assertions =
    Map("speling" -> "spelling",
      "korrecter" -> "corrected",
      "corrected" -> "corrected",
      "bycycle" -> "bicycle",
      "inconvnient" -> "inconvenient",
      "arrainged" -> "arranged",
      "peotry" -> "poetry",
      "peotryy" -> "poetry",
      "word" -> "word",
      "quintessential" -> "quintessential"
    )


  assertions foreach {
    case (word, expected) =>
      val correction = check(word)
      println( s"${word} => ${correction}")
      assert(correction.toString == expected, s"correction of $word = $correction but did not equal $expected")
  }

  println(s"Tests pass ...\n")

  while(true){
    val word = scala.io.StdIn.readLine("Please enter a word to correct:\n")
    println(s"corrected is: ${check(word)}" )
  }

}