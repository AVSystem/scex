import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import com.avsystem.scex.Expression;
import com.avsystem.scex.compiler.ExpressionProfile;
import com.avsystem.scex.compiler.JavaScexCompiler;
import com.avsystem.scex.compiler.JavaScexCompiler$;
import com.avsystem.scex.compiler.ScexCompilerConfig;
import com.avsystem.scex.validation.SymbolValidator;
import com.avsystem.scex.validation.SyntaxValidator;

public class ScexJavaTest {
    public static void main(String[] args) throws Exception {
        JavaScexCompiler compiler = JavaScexCompiler$.MODULE$.apply(new ScexCompilerConfig());

        SyntaxValidator syntaxValidator = compiler.compileSyntaxValidator(readResource("/syntaxValidator.scala"));
        SymbolValidator symbolValidator = compiler.compileSymbolValidator(readResource("/symbolValidator.scala"));

        ExpressionProfile profile = new ExpressionProfile(syntaxValidator, symbolValidator, "", "");

        JavaScexCompiler.JavaInteractiveContext ctx = compiler.getInteractiveContext(profile, void.class, String.class);
        System.out.println("FUUUUUUU");

        long start = System.currentTimeMillis();
        for (int i = 0; i < 3000; i++) {
            ctx.getTypeCompletionForJava("new JavaCostam(\"asdfasdaasd\").toSrslyWtf {{", 1);
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
