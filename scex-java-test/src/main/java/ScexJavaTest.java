import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import com.avsystem.scex.Expression;
import com.avsystem.scex.compiler.ExpressionCompiler;
import com.avsystem.scex.compiler.ExpressionCompilerConfig;
import com.avsystem.scex.compiler.ExpressionProfile;
import com.avsystem.scex.validation.SymbolValidator;
import com.avsystem.scex.validation.SyntaxValidator;

public class ScexJavaTest {
    public static void main(String[] args) throws Exception {
        ExpressionCompiler compiler = new ExpressionCompiler(new ExpressionCompilerConfig());

        SyntaxValidator syntaxValidator = compiler.compileSyntaxValidator(readResource("/syntaxValidator.scala"));
        SymbolValidator symbolValidator = compiler.compileSymbolValidator(readResource("/symbolValidator.scala"));

        ExpressionProfile profile = new ExpressionProfile(syntaxValidator, symbolValidator, "");

        Expression<Void, String> expression = compiler.getCompiledExpression(profile, "new JavaCostam(\"dafuq\").toString", void.class, String.class);
        System.out.println(expression.apply(null));
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
