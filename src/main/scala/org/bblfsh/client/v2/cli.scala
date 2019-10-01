package org.bblfsh.client.v2.cli

import scopt.OParser
import gopkg.in.bblfsh.sdk.v2.protocol.driver.Mode
import java.io.File
import scala.io.Source
import org.bblfsh.client.v2._, BblfshClient._

case class Endpoint(host: String, port: Int)

case class CLIOptions(
  endpoint: Endpoint = Endpoint("localhost", 9432),
  language: String = "",
  query: String = "",
  mode: Mode = Mode.DEFAULT_MODE,
  out: UastFormat = UastBinary,
  file: File
)

object CLIOptions {
  val builder = OParser.builder[CLIOptions]
  import builder._

  private def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }
  }

  private def parseEndpoint(s: String): Option[Endpoint] = {
    val index = s.indexOf(':')
    val strPort = s.slice(index, s.length)
    toInt(strPort) match {
      case Some(port) =>
        val host = s.slice(0, index)
        Some(Endpoint(host, port))
      case _ =>
        None
    }
  }

  /** Allows to read UastFormat from the command line */
  implicit val modeRead: scopt.Read[UastFormat] =
    scopt.Read.reads {
      case "yaml" => UastYaml
      case "binary" => UastBinary
    }

  /** Allows to read a UAST Mode from the command line */
  implicit val outRead: scopt.Read[Mode] =
    scopt.Read.reads {
      case "semantic" => Mode.SEMANTIC
      case "annotated" => Mode.ANNOTATED
      case "native" => Mode.NATIVE
    }

  /** Contains the parsing information for the CLI arguments */
  val parser = {
    OParser.sequence(
      programName("<bblfsh-client-assembly-*.jar | sbt run>"),
      head("bblfsh-cli", "v2"),

      opt[String]('a', "host")
        .valueName("<address:port>")
        .action((a, opts) =>
          parseEndpoint(a) match {
            case Some(e) =>
              opts.copy(endpoint = e)
            case _ =>
              opts
          }
        )
        .text("Babelfish endpoint address")
        .validate{ a =>
          parseEndpoint(a) match {
            case Some(endpoint) => success
            case _ => failure("--host must be address:port")
          }
        },
      opt[String]('l', "language")
        .valueName("<python | c++ | java | php | ...>")
        .action((l, opts) => opts.copy(language = l))
        .text("language to parse (default: auto)"),
      opt[String]('q', "query")
        .valueName("<XPath query>")
        .action((q, opts) => opts.copy(query = q))
        .text("XPath query applied to the resulting UAST"),
      opt[Mode]('m', "mode")
        .valueName("<semantic | annotated | native>")
        .action((m, opts) => opts.copy(mode = m))
        .text("UAST transformation mode: semantic, annotated, native (default: semantic)")
        .validate(m =>
          if (Seq(Mode.SEMANTIC, Mode.ANNOTATED, Mode.NATIVE).contains(m))
            success
          else
            failure("--mode must be one of semantic, annotated, native")
        ),
      arg[File]("<file>")
        .unbounded()
        .required()
        .action((f, opts) => opts.copy(file = f))
        .text("file to parse")
        .validate(f =>
          if (f.isFile)
            success
          else
            failure(s"file $f does not exist")
        ),
      help("help").text("prints this usage text")
    )
  }

  def getFrom(args: Seq[String]): Option[CLIOptions] =
    OParser.parse(parser, args, CLIOptions(file = null))
}

object ScalaClientCLI extends App {
  CLIOptions.getFrom(args) match {
    case Some(opts) =>
      val endpoint = opts.endpoint
      val client = BblfshClient(endpoint.host, endpoint.port)
      val file = opts.file
      val name = file.toString
      val content = Source.fromFile(name).getLines.mkString("\n")
      val query = opts.query
      val mode = opts.mode
      val lang = opts.language
      val fmt = opts.out
      val resp = client.parseWithOptions(name, content, lang, BblfshClient.DEFAULT_TIMEOUT_SEC, mode)
      val ctx = resp.uast.decode(fmt)
      if (!query.isEmpty) {
        val it = ctx.filter(query)
        val root = it.next.load()
        println(root)
      } else {
        println(ctx.root.load())
      }
    case _ =>
      System.exit(-1)
  }
}
