import com.avsystem.scex.ExpressionCompiler;
import scala.Function1;

public class JavaLol {
    public int fuu = 5;

    private int lol;

    public int getLol() {
        return lol;
    }

    public void setLol(int lol) {
        this.lol = lol;
    }

    public static void main(String[] args) {
        ExpressionCompiler compiler = new ExpressionCompiler();
        Function1<Object, String> expr = compiler.getCompiledExpression(null, "${hashCode}", Object.class, String.class);
        expr.apply(new Object());
    }
}
