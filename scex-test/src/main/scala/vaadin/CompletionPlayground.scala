package vaadin

import com.avsystem.scex.compiler.ScexCompilerConfig
import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler.Member
import com.avsystem.scex.japi.XmlFriendlyJavaScexCompiler
import com.avsystem.scex.util.SimpleContext
import com.avsystem.scex.validation.{SymbolValidator, SyntaxValidator}
import com.avsystem.scex.{PredefinedAccessSpecs, ExpressionProfile}
import com.vaadin.event.FieldEvents.{TextChangeEvent, TextChangeListener}
import com.vaadin.terminal.gwt.server.AbstractApplicationServlet
import com.vaadin.ui.{Label, TextField, Window}
import java.{util => ju, lang => jl}
import javax.servlet.http.HttpServletRequest
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.session.SessionHandler
import org.eclipse.jetty.servlet.{ServletHolder, ServletContextHandler}

object CompletionPlayground {

  lazy val compiler = new XmlFriendlyJavaScexCompiler(new ScexCompilerConfig)

  class SampleServlet extends AbstractApplicationServlet {
    def getNewApplication(request: HttpServletRequest) = new SampleApplication

    def getApplicationClass = classOf[SampleApplication]
  }

  class SampleApplication extends com.vaadin.Application {
    def init(): Unit = {
      val window = new Window
      setMainWindow(window)

      import SymbolValidator._
      val acl = PredefinedAccessSpecs.basicOperations ++ allow {
        Dyn.selectDynamic _

        on { r: Root =>
          r.all.members
          r.dyn
        }
      }

      val profile = new ExpressionProfile(SyntaxValidator.SimpleExpressions, SymbolValidator(acl),
        "import com.avsystem.scex.util.TypesafeEquals._", "")

      def memberRepr(member: Member) =
        s"${member.name}${member.params.map(_.map(p => s"${p.name}: ${p.tpe}").mkString("(", ", ", ")")).mkString}: ${member.tpe}"

      val completer = compiler.getCompleter[SimpleContext[Root], String](profile)
      val scopeMembers = completer.getScopeCompletion.members.filterNot(_.iimplicit).map(memberRepr).mkString("\n")

      val textField = new TextField
      textField.setWidth("100%")

      val label = new Label
      label.setContentMode(Label.CONTENT_RAW)

      textField.addListener(new TextChangeListener {
        def textChange(event: TextChangeEvent): Unit = {
          val completion = completer.getTypeCompletion(event.getText, event.getCursorPosition - 1)
          val errors = completer.getErrors(event.getText).mkString("\n")
          val members = completion.members.filterNot(_.iimplicit).map(memberRepr).mkString("\n")
          label.setValue(s"ERRORS:\n$errors\nCOMPLETION:\n$members\nSCOPE COMPLETION:\n$scopeMembers".replaceAllLiterally("\n", "<br/>"))
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
