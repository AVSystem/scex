import com.avsystem.scex.Expression;
import com.avsystem.scex.compiler.ScexCompiler;

public class JavaLol {
    public static class StaticLol {

    }

    public class InnerLol {

    }

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
        ScexCompiler compiler = new ScexCompiler(null);
        Expression<String, Object> expr = compiler.getCompiledExpression(null, null, String.class, Object.class);
    }
}
