package com.avsystem.scex.compiler;

import java.util.HashMap;
import java.util.Map;

public class ParameterizedClass<T> {
    public static class StaticInnerGeneric<A extends Cloneable> {
        public class DeeplyInnerGeneric<B> {
            public String fjeld = "[fjeld]";

            public Map<A, B> getSampleMap() {
                return new HashMap<>();
            }

            public boolean isAwesome() {
                return true;
            }

            public boolean getAwesomeness() {
                return true;
            }

            public String handleStuff(String stuff) {
                return "[" + stuff + " handled]";
            }
        }
    }
}
