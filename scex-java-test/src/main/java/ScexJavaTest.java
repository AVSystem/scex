import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import com.avsystem.scex.ExpressionContext;
import com.avsystem.scex.compiler.ExpressionProfile;
import com.avsystem.scex.compiler.ScexCompilerConfig;
import com.avsystem.scex.japi.DefaultJavaScexCompiler;
import com.avsystem.scex.japi.JavaScexCompiler;
import com.avsystem.scex.validation.SymbolValidator;
import com.avsystem.scex.validation.SyntaxValidator;

public class ScexJavaTest {
    public static void main(String[] args) throws Exception {
        DefaultJavaScexCompiler compiler = new DefaultJavaScexCompiler(new ScexCompilerConfig());

        SyntaxValidator syntaxValidator = compiler.compileSyntaxValidator(readResource("/syntaxValidator.scala"));
        SymbolValidator symbolValidator = compiler.compileSymbolValidator(readResource("/symbolValidator.scala"));

        ExpressionProfile profile = new ExpressionProfile(syntaxValidator, symbolValidator, "", "");

        Class<ExpressionContext<?, ?>> aecClass = (Class) ExpressionContext.class;
        JavaScexCompiler.JavaInteractiveContext ctx = compiler.getJavaInteractiveContext(profile, aecClass, String.class);
        System.out.println("FUUUUUUU");

        long start = System.currentTimeMillis();
        for (int i = 0; i < 3000; i++) {
            ctx.getTypeCompletion("new JavaCostam(\"asdfasdaasd\").toSrslyWtf {{", 1);
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
