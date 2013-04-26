import com.avsystem.scex.Expression;
import com.avsystem.scex.ExpressionCompiler;
import com.avsystem.scex.TypeConverters;

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
        System.out.println(TypeConverters.javaTypeAsScalaType(int.class));
    }
}
