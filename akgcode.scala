import java.nio.file

import org.rogach.scallop._

class Conf(args: Seq[String]) extends ScallopConf(args) {
    val inputFilename = trailArg[String]()
    verify()
}
    
object AkGCodeApp extends App {
    val conf = new Conf(args)
    
    println(conf.inputFilename.get)
}
