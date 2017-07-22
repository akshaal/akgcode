// Evgeny "Akshaal" Chukreev, 2017

import scala.util.Try
import scala.util.control.NonFatal
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
    
    def ERROR(s: Any): String = ERROR_COLOR + s + RESET_COLOR
    def INFO(s: Any): String = INFO_COLOR + s + RESET_COLOR
    def DEBUG(s: Any): String = DEBUG_COLOR + s + RESET_COLOR
    def GRAPH(s: Any): String = GRAPH_COLOR + s + RESET_COLOR
    def STRESS(s: Any): String = STRESS_COLOR + s + RESET_COLOR
        
    val TERM_SEPLINE = "=" * termColumns
    
    def printSep(): Unit = System.err.println(GRAPH(TERM_SEPLINE))
        
    def printInfo(vals: Any*): Unit = System.err.println(GRAPH(":::: ") + INFO(vals.mkString("").replace(RESET_COLOR, INFO_COLOR)))
    def printError(vals: Any*): Unit = System.err.println(GRAPH("!!!! ") + ERROR(vals.mkString("").replace(RESET_COLOR, ERROR_COLOR)))
    
    def printDebug(vals: Any*): Unit = {
        if (verbose) {
            System.err.println(GRAPH(":::: ... ") + DEBUG(vals.mkString("").replace(RESET_COLOR, DEBUG_COLOR)))
        }
    }
}
 
case class Borked(msg: String, exc: Throwable = null) extends RuntimeException(msg, exc)
    
object Borked {
    def inCtx[T](ctxMsg: => String)(code: => T): T = {
        try code catch {
            case NonFatal(exc) =>
                if (exc.getMessage == null) throw new Borked(ctxMsg, exc)
                else throw new Borked(ctxMsg + ": " + exc.getMessage, exc)
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
            printError(s"\n\nRequired option not found: ${STRESS(optionName)}\nSee below for more info...\n\n")
            printHelp()
            println()
            System.exit(-1)
            
        case exc => throw exc
    }
        
    verify()
}
 
sealed abstract class MoveMode
object MoveMode {
    case object Abs extends MoveMode
    case object Rel extends MoveMode
}
   
case class Pos(value: Int) extends AnyVal {
    def isUndefined: Boolean = this == Pos.undefined
    def isDefined: Boolean = !this.isUndefined
    def isZero: Boolean = this == Pos.zero
    def isNotZero: Boolean = !this.isZero
    
    private def ensureBothDefined(other: Pos): Unit = {
        if (other.isUndefined) throw Borked("Other pos is undefined!")
        else if (this.isUndefined) throw Borked("This pos is undefined!")
    }
    
    def >(other: Pos): Boolean = { ensureBothDefined(other); value > other.value }
    def >=(other: Pos): Boolean = { ensureBothDefined(other); value >= other.value }
    def <=(other: Pos): Boolean = { ensureBothDefined(other); value <= other.value }
    def <(other: Pos): Boolean = { ensureBothDefined(other); value < other.value }
    def ===(other: Pos): Boolean = { ensureBothDefined(other); value == other.value }
    def +(other: Pos): Pos = { ensureBothDefined(other); Pos(value + other.value) }
    def -(other: Pos): Pos = { ensureBothDefined(other); Pos(value - other.value) }
    
    def or(other: Pos): Pos = if (this.isDefined) this else other
    
    def addIfDefined(other: Pos): Pos = if (other.isDefined) this + other else this
 
    def move(moveMode: MoveMode, other: Pos): Pos =
        moveMode match {
            case MoveMode.Rel => this addIfDefined other
            case MoveMode.Abs => other or this
        }
   
    def toDouble: Double = value.toDouble / 1000
}
    
object Pos {
    val SCALE = 1000
    
    val undefined = Pos(-100000)
    val zero = Pos(0)
    
    def fromDouble(d: Double): Pos = Pos(Math.round((d * SCALE).toFloat))
}
    
case class Delta(dx: Pos, dy: Pos, dz: Pos, de: Pos) {
    def isXyzMove: Boolean = dx.isNotZero || dy.isNotZero || dz.isNotZero
    def isExtrusion: Boolean = de > Pos.zero
    def isRetraction: Boolean = de < Pos.zero
}
        
object XYZE {
    val undefined = XYZE()
}
    
case class XYZE(x: Pos = Pos.undefined, y: Pos = Pos.undefined, z: Pos = Pos.undefined, e: Pos = Pos.undefined) {
    def isDefined: Boolean = x.isDefined && y.isDefined && z.isDefined && e.isDefined
    def isAtHome: Boolean = x.isZero && y.isZero && z.isZero
    
    def move(moveMode: MoveMode, other: XYZE): XYZE =
        XYZE(
            x = Borked.inCtx("x")(x.move(moveMode, other.x)),
            y = Borked.inCtx("y")(y.move(moveMode, other.y)),
            z = Borked.inCtx("z")(z.move(moveMode, other.z)),
            e = Borked.inCtx("e")(e.move(moveMode, other.e))
        )
    
    def -(other: XYZE): Delta = Delta(dx = x - other.x, dy = y - other.y, dz = z - other.z, de = e - other.e)
}

class GCodeFile(val filename: String) {
    printInfo("Reading GCode file: ", STRESS(filename))

    val lines: Vector[String] = scala.io.Source.fromFile("test.gcode").getLines.toVector
    printInfo("... done reading gcode file, it contains ", STRESS(lines.size), " lines")
}

class ParsedLine(lineWithIndex: (String, Int)) {
    val (line, index): (String, Int) = lineWithIndex
    val trimmedLine: String = line.split(";")(0).trim

    val tokens: Seq[String] = trimmedLine.split(" ").map(_.trim).filter(_ != "")
    val cmd: String = if (tokens.isEmpty) "" else tokens(0)
    val args: Seq[String] = tokens drop 1
    
    def isNotEmpty = trimmedLine != ""
}   

class Xyzes(gcodeFile: GCodeFile) {
    printInfo("Searching for xyze states...")

    private var xyze = XYZE()
    private var moveModeOpt: Option[MoveMode] = None
    private var ignoredCmds = 0

    for (parsedLine <- gcodeFile.lines.zipWithIndex.map(new ParsedLine(_)) if parsedLine.isNotEmpty) {
        Borked.inCtx(s"Line ${parsedLine.index}: ${parsedLine.cmd}") {
            parsedLine.cmd.toUpperCase match {
                case "M190" | "M104" | "M109" | "G21" | "M82" | "M107" | "M117" | "M205" | "M140" | "M106" | "M84" =>
                    ignoredCmds += 1

                case "G90" =>
                    moveModeOpt = Some(MoveMode.Abs)

                case "G91" =>
                    moveModeOpt = Some(MoveMode.Rel)

                case "G28" =>
                    if (parsedLine.args.isEmpty) {
                        xyze = xyze.copy(x = Pos.zero, y = Pos.zero, z = Pos.zero)
                    } else {
                        for (arg <- parsedLine.args) {
                            Borked.inCtx(arg) {
                                arg match {
                                    case "X" | "X0" =>  xyze = xyze.copy(x = Pos.zero)
                                    case "Y" | "Y0" =>  xyze = xyze.copy(y = Pos.zero)
                                    case "Z" | "Z0" =>  xyze = xyze.copy(z = Pos.zero)
                                    case arg => Borked("Uknown argument!")
                                }
                            }
                        }
                    }

/*        elif code == "G1" or code == "G01" or code == "G0" or code == "G00":
            # Move
            pargs = parse_args()

            for arg in pargs:
                if arg.startswith("X"):
                    self.pos = self.pos.update(self.__mode, dx = float(arg[1:]))
                elif arg.startswith("Y"):
                    self.pos = self.pos.update(self.__mode, dy = float(arg[1:]))
                elif arg.startswith("Z"):
                    self.pos = self.pos.update(self.__mode, dz = float(arg[1:]))
                elif arg.startswith("E"):
                    self.pos = self.pos.update(self.__mode, de = float(arg[1:]))
                elif arg.startswith("F"):
                    pass
                else:
                    complain_parse(arg)
*/
                    
                case cmd =>
                    throw Borked("Uknown command!")
            }
        }
    }

    printInfo("... commands ignored: ", STRESS(ignoredCmds))
}

object AkGCodeApp extends App {
    printSep()
    
    val conf = new Conf(args)
    val gcodeFile = new GCodeFile(conf.inputFilename.toOption.get)
    val xyzes = new Xyzes(gcodeFile)
}
    
