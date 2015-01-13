package vaadin

import java.{lang => jl, util => ju}
import javax.servlet.http.HttpServletRequest

import com.avsystem.scex.util.{PredefinedAttributes, PredefinedAccessSpecs, SimpleContext}
import PredefinedAccessSpecs.{basicOperations, javaCollectionExtensions}
import com.avsystem.scex.compiler.ScexSettings
import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler.Member
import com.avsystem.scex.japi.XmlFriendlyJavaScexCompiler
import com.avsystem.scex.presentation.{Attributes, SymbolAttributes}
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.{ExpressionProfile, NamedSource}
import com.vaadin.event.FieldEvents.{TextChangeEvent, TextChangeListener}
import com.vaadin.terminal.gwt.server.AbstractApplicationServlet
import com.vaadin.ui.{Label, TextField, Window}
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.session.SessionHandler
import org.eclipse.jetty.servlet.{ServletContextHandler, ServletHolder}

object CompletionPlayground {

  val settings = new ScexSettings

  lazy val compiler = new XmlFriendlyJavaScexCompiler(settings)

  class SampleServlet extends AbstractApplicationServlet {
    def getNewApplication(request: HttpServletRequest) = new SampleApplication

    def getApplicationClass = classOf[SampleApplication]
  }

  class SampleApplication extends com.vaadin.Application {
    def init(): Unit = {
      val window = new Window
      setMainWindow(window)

      val acl = {
        import com.avsystem.scex.validation.SymbolValidator._
        basicOperations ++ javaCollectionExtensions ++ allow {
          Dyn.selectDynamic _

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
      val attrList = PredefinedAttributes.basicOperations ++ PredefinedAttributes.javaCollectionExtensions

      val header =
        """
          |import com.avsystem.scex.util.TypesafeEquals._
          |import com.avsystem.scex.util.JavaCollectionExtensions._
        """.stripMargin

      val utils =
        """
          |val utilStuff = "dafuq"
        """.stripMargin

      val profile = new ExpressionProfile("test", SyntaxValidator.SimpleExpressions, SymbolValidator(acl),
        SymbolAttributes(attrList), header, NamedSource("test", utils))

      def memberRepr(member: Member) =
        s"${member.name}${member.params.map(_.map(p => s"${p.name}: ${p.tpe}-${p.tpe.erasure}").mkString("(", ", ", ")")).mkString}: ${member.returnType}-${member.returnType.erasure} - ${member.documentation}"

      val completer = compiler.getCompleter[SimpleContext[JavaRoot], String](profile)
      val scopeMembers = completer.getScopeCompletion.members.filterNot(_.flags.iimplicit).map(memberRepr).mkString("\n")

      println("SCOPE MEMBERS:")
      completer.getScopeCompletion.members.foreach(println)

      val textField = new TextField
      textField.setWidth("100%")

      val label = new Label
      label.setContentMode(Label.CONTENT_PREFORMATTED)

      textField.addListener(new TextChangeListener {
        def textChange(event: TextChangeEvent): Unit = {
          val completion = completer.getTypeCompletion(event.getText + "}}}", event.getCursorPosition - 1)
          val errors = completer.getErrors(event.getText).mkString("\n")
          val members = completion.members.filterNot(_.flags.iimplicit).map(memberRepr).mkString("\n")
          val parsedTree = completer.parse(event.getText)
          val typedPrefix = completion.typedPrefixTree
          val parsedPrefix = parsedTree.locate(typedPrefix.attachments.position)
          label.setValue(s"PARSED:\n${parsedTree.pretty(true, true)}\nERRORS:\n$errors\nPPREFIX:\n${parsedPrefix.pretty(true, true)}\n" +
            s"TPREFIX:\n${typedPrefix.pretty(true, true)}\nCOMPLETION:\n$members\nSCOPE COMPLETION:\n$scopeMembers")

          println("TYPE MEMBERS:")
          completion.members.foreach(println)
        }
      })

      window.addComponent(textField)
      window.addComponent(label)
    }
  }

  def main(args: Array[String]) {
    val server = new Server(9090)

    val handler = new ServletContextHandler
    handler.addServlet(new ServletHolder(new SampleServlet), "/*")
    handler.setSessionHandler(new SessionHandler)

    server.setHandler(handler)

    server.start()
    server.join()
  }
}
