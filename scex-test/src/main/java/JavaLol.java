import com.avsystem.scex.compiler.TypeConverter;
import com.avsystem.scex.compiler.TypeTag;

import java.util.List;
import java.util.Map;

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
        System.out.println(TypeConverter.javaTypeAsScalaType((new TypeTag<Map<?, ? super List>>() {
        })));
    }
}
