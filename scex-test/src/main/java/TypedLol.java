import java.util.List;

public class TypedLol<T extends TypedLol<T>> {
    public static class Stuff<E> {

    }

    public class Dafuq<F extends List<? super String>> {
        public F getStuff() {
            return null;
        }
    }

}
