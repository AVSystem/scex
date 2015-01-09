import com.avsystem.scex.ExpressionContext;
import com.avsystem.scex.ExpressionProfile;
import com.avsystem.scex.NamedSource;
import com.avsystem.scex.compiler.ScexSettings;
import com.avsystem.scex.compiler.presentation.ScexPresentationCompiler;
import com.avsystem.scex.japi.DefaultJavaScexCompiler;
import com.avsystem.scex.japi.JavaScexCompiler;
import com.avsystem.scex.presentation.Attributes;
import com.avsystem.scex.presentation.SymbolAttributes;
import com.avsystem.scex.symboldsl.SymbolInfo;
import com.avsystem.scex.validation.SymbolValidator;
import com.avsystem.scex.validation.SyntaxValidator;
import scala.collection.JavaConversions;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class ScexJavaTest {
    public static void main(String[] args) throws Exception {
        DefaultJavaScexCompiler compiler = new DefaultJavaScexCompiler(new ScexSettings());

        SyntaxValidator syntaxValidator = compiler.compileSyntaxValidator(NamedSource.apply("test", readResource("/syntaxValidator.scala")));
        SymbolValidator symbolValidator = compiler.compileSymbolValidator(NamedSource.apply("test", readResource("/symbolValidator.scala")));
        SymbolAttributes symbolAttributes = compiler.compileSymbolAttributes(NamedSource.apply("test", readResource("/symbolAttributes.scala")));

        for (SymbolInfo<Attributes> info : JavaConversions.seqAsJavaList(symbolAttributes.infoList())) {
            System.out.println(info);
        }

        ExpressionProfile profile = new ExpressionProfile("test", syntaxValidator, symbolValidator,
                symbolAttributes, "", NamedSource.apply("test", ""));

        Class<ExpressionContext<?, ?>> aecClass = (Class) ExpressionContext.class;
        ScexPresentationCompiler.Completer ctx = compiler.buildCompleter().contextType(aecClass)
                .resultType(String.class).profile(profile).template(false).get();

        System.out.println("FUUUUUUU");

        long start = System.currentTimeMillis();
        for (int i = 0; i < 3000; i++) {
            ctx.getTypeCompletion("new JavaCostam(\"asdfasdaasd\").toSrslyWtf", 10);
            if (i % 100 == 0) {
                System.out.println(i);
            }
        }
        System.out.println("FINISHED");
        long duration = System.currentTimeMillis() - start;
        System.out.println(3000000.0 / duration);
    }

    private static String readResource(String resource) throws IOException {
        BufferedReader rd = new BufferedReader(new InputStreamReader(ScexJavaTest.class.getResourceAsStream(resource)));
        StringBuilder sb = new StringBuilder();
        String line;
        while ((line = rd.readLine()) != null) {
            sb.append(line);
            sb.append("\n");
        }
        return sb.toString();
    }
}
