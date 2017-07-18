// Evgeny "Akshaal" Chukreev, 2017

import scala.util.Try
import org.apache.commons.io.IOUtils
import org.rogach.scallop.{ScallopConf, ScallopOption}
import org.rogach.scallop.exceptions.RequiredOptionNotFound
    
object AkScriptUtils {
    var verbose: Boolean = false
    
    def captureOutputInheritInput(cmd: String, args: String*): String = {
        IOUtils.toString(
            new ProcessBuilder((Seq(cmd) ++ args): _*)
                .redirectInput(ProcessBuilder.Redirect.INHERIT)
                .start()
                .getInputStream(),
            "utf8")
    }

    val isConsole: Boolean = System.console() != null
    
    def inConsoleOrElse[T](v: T, vElse: T): T = if (isConsole) v else vElse
    
    val termColumnsOpt: Option[Int] = Try { captureOutputInheritInput("stty", "size").split(" ")(1).trim.toInt }.toOption

    val termColumns: Int = inConsoleOrElse(termColumnsOpt, None).getOrElse(78)

    val RESET_COLOR: String = inConsoleOrElse("\u001b[0m", "")
    val ERROR_COLOR: String = inConsoleOrElse("\u001b[91m", "")
    val INFO_COLOR: String = inConsoleOrElse("\u001b[92m", "")
    val DEBUG_COLOR: String = inConsoleOrElse("\u001b[93m", "")
    val GRAPH_COLOR: String = inConsoleOrElse("\u001b[94m", "")
    val STRESS_COLOR: String = inConsoleOrElse("\u001b[95m", "")

    def ERROR(s: String): String = ERROR_COLOR + s + RESET_COLOR
    def INFO(s: String): String = INFO_COLOR + s + RESET_COLOR
    def DEBUG(s: String): String = DEBUG_COLOR + s + RESET_COLOR
    def GRAPH(s: String): String = GRAPH_COLOR + s + RESET_COLOR
    def STRESS(s: String): String = STRESS_COLOR + s + RESET_COLOR

    val TERM_SEPLINE = "=" * termColumns

    def print_sep(): Unit = System.err.println(GRAPH(TERM_SEPLINE))

    def print_info(vals: Any*): Unit = System.err.println(GRAPH(":::: ") + INFO(vals.mkString("").replace(RESET_COLOR, INFO_COLOR)))
    def print_error(vals: Any*): Unit = System.err.println(GRAPH("!!!! ") + ERROR(vals.mkString("").replace(RESET_COLOR, ERROR_COLOR)))
    
    def print_debug(vals: Any*): Unit = {
        if (verbose) {
            System.err.println(GRAPH(":::: ... ") + DEBUG(vals.mkString("").replace(RESET_COLOR, DEBUG_COLOR)))
        }
    }
}
    
import AkScriptUtils._
    
class Conf(args: Seq[String]) extends ScallopConf(args) {
    version("akgcode by Akshaal, 2017")
    banner("""Usage: akgcode [OPTION]... <input-filename>
             |Options:
             |""".stripMargin)
        
    val verbose: ScallopOption[Boolean] = opt(descr = "Show verbose messages")
    val inputFilename: ScallopOption[String] = trailArg(descr = "GCode file to process")
    
    override def onError(e: Throwable): Unit = e match {
        case RequiredOptionNotFound(optionName) =>
            print_error(s"\n\nRequired option not found: ${STRESS(optionName)}\nSee below for more info...\n\n")
            printHelp()
            println()
            System.exit(-1)
    }
        
    verify()
}
    
object AkGCodeApp extends App {
    val conf = new Conf(args)

    print_sep()
    
    println((termColumns))
    println((conf.inputFilename, conf.verbose))
}
