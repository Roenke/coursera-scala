package parallel

/**
  * Created by vitaly on 09.06.2016.
  */
object Parallel extends App {
  class a extends Thread {
    override def run() ={
      println("Hello")
    }
  }

  override def main(args: Array[String]) = {
    println("hello")
  }
}
