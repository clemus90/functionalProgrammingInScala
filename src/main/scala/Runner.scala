object Runner extends App{
  import chapter5.Stream

  println(Stream.fibs().take(9).toList)
}
