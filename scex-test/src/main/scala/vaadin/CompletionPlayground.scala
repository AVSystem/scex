package vaadin

import com.avsystem.commons.misc.TypeString
import com.avsystem.scex.compiler.ScexSettings
import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler.Member
import com.avsystem.scex.japi.XmlFriendlyJavaScexCompiler
import com.avsystem.scex.presentation.SymbolAttributes
import com.avsystem.scex.util.PredefinedAccessSpecs.basicOperations
import com.avsystem.scex.util.SimpleContext
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.{ExpressionProfile, NamedSource}
import com.vaadin.event.FieldEvents.{TextChangeEvent, TextChangeListener}
import com.vaadin.terminal.gwt.server.AbstractApplicationServlet
import com.vaadin.ui.{Label, TextField, Window}

import javax.servlet.http.HttpServletRequest
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.session.SessionHandler
import org.eclipse.jetty.servlet.{ServletContextHandler, ServletHolder}

import scala.annotation.nowarn

@nowarn("msg=a pure expression does nothing in statement position")
object CompletionPlayground {

  val settings = new ScexSettings

  lazy val compiler = new XmlFriendlyJavaScexCompiler(settings)

  class SampleServlet extends AbstractApplicationServlet {
    def getNewApplication(request: HttpServletRequest) = new SampleApplication

    def getApplicationClass = classOf[SampleApplication]
  }

  class SampleApplication extends com.vaadin.Application {
    def init(): Unit = try {
      val window = new Window
      setMainWindow(window)

      val acl = {
        import com.avsystem.scex.validation.SymbolValidator._
        basicOperations ++ allow {
          on { dyn: Dyn =>
            dyn.all.members
          }

          on { inter: Intermediate =>
            inter.all.members
          }

          on { interDyn: InterDyn =>
            interDyn.all.members
          }

          on { jl: JavaLol =>
            jl.all.members
          }

          on { l: List[Int] =>
            l.filter _
            l.all.membersNamed.map
          }

          on { a: Array[_] =>
            a.as[Array[Any]].exists(_: Any => Boolean)
          }

          on { r: JavaRoot =>
            r.all.members
          }

          on { r: Root =>
            r.all.members
            r.dyn
          }
        }
      }

      val header =
        """
          |import com.avsystem.scex.util.TypesafeEquals._
        """.stripMargin

      val utils =
        """
          |val utilStuff = "dafuq"
        """.stripMargin

      val profile = new ExpressionProfile("test", SyntaxValidator.SimpleExpressions, SymbolValidator(acl),
        SymbolAttributes(Nil), header, NamedSource("test", utils))

      def memberRepr(member: Member) =
        s"${member.name}${member.params.map(_.map(p => s"${p.name}: ${p.tpe}-${p.tpe.erasure}").mkString("(", ", ", ")")).mkString}: " +
          s"${member.returnType}-${member.returnType.erasure} - ${member.documentation}"

      val completer = compiler.getCompleter[SimpleContext[Root], String](profile, variableTypes =
        Map("someInt" -> TypeString[Int], "intList" -> TypeString[List[Int]]))
      val scopeMembers = completer.getScopeCompletion.members.filterNot(_.flags.iimplicit).map(memberRepr).mkString("\n")

      val textField = new TextField
      textField.setWidth("100%")

      val label = new Label
      label.setContentMode(Label.CONTENT_PREFORMATTED)

      textField.addListener(new TextChangeListener {
        def textChange(event: TextChangeEvent): Unit = {
          val pos = event.getCursorPosition - 1
          val completion = completer.getTypeCompletion(event.getText + "}}}", pos)
          val errors = completer.getErrors(event.getText).mkString("\n")
          val members = completion.members.filterNot(_.flags.iimplicit).map(memberRepr).mkString("\n")
          val parsedTree = completer.parse(event.getText)
          val typedPrefix = completion.typedPrefixTree
          val parsedPrefix = parsedTree.locate(typedPrefix.attachments.position)
          label.setValue(s"POSITION: $pos\nPARSED:\n${parsedTree.pretty(withPositions = true, withTypes = true)}\nERRORS:\n$errors\nPPREFIX:\n${parsedPrefix.pretty(withPositions = true, withTypes = true)}\n" +
            s"TPREFIX:\n${typedPrefix.pretty(withPositions = true, withTypes = true)}\nCOMPLETION:\n$members\nSCOPE COMPLETION:\n$scopeMembers")
        }
      })

      window.addComponent(textField)
      window.addComponent(label)
    } catch {
      case e: Exception =>
        e.printStackTrace()
    }
  }

  def main(args: Array[String]): Unit = {
    val server = new Server(9090)

    val handler = new ServletContextHandler
    handler.addServlet(new ServletHolder(new SampleServlet), "/*")
    handler.setSessionHandler(new SessionHandler)

    server.setHandler(handler)

    server.start()
    server.join()
  }
}
