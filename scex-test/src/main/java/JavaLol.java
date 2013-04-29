import com.avsystem.scex.TypeConverters;
import com.avsystem.scex.TypeTag;

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
        System.out.println(TypeConverters.javaTypeAsScalaType((new TypeTag<Map<?, ? super List>>() {
        })));
    }
}
