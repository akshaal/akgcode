// Akshaal, 2017

import org.rogach.scallop.{ScallopConf, ScallopOption}
import org.rogach.scallop.exceptions.RequiredOptionNotFound
    
class Conf(args: Seq[String]) extends ScallopConf(args) {
    version("akgcode by Akshaal, 2017")
    banner("""Usage: akgcode [OPTION]... <input-filename>
             |Options:
             |""".stripMargin)
        
    val verbose: ScallopOption[Boolean] = opt(descr = "Show verbose messages")
    val inputFilename: ScallopOption[String] = trailArg(descr = "GCode file to process")
    
    override def onError(e: Throwable): Unit = e match {
        case RequiredOptionNotFound(optionName) =>
            System.err.println(s"\n\nRequired option not found: ${optionName}\nSee below for more info...\n\n")
            printHelp()
            System.exit(-1)
    }
        
    verify()
}

object AkGCodeApp extends App {
    val conf = new Conf(args)

    println((conf.inputFilename, conf.verbose))
}
