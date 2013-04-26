import com.avsystem.scex.Expression;
import com.avsystem.scex.ExpressionCompiler;

public class JavaLol {
    public int fuu = 5;

    private int lol;

    public int getLol() {
        return lol;
    }

    public void setLol(int lol) {
        this.lol = lol;
    }

    public boolean isFoo() {
        return true;
    }

    public static void main(String[] args) throws Exception {
        ExpressionCompiler compiler = new ExpressionCompiler();
        Expression<Object, String> expr = compiler.getCompiledStringExpression(null, "${1+2+3}", Object.class);
        expr.apply(new Object());
    }
}
