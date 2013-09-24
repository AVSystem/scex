package com.avsystem.scex.compiler;

public class JavaRoot {
    public String getProperty() {
        return "property";
    }

    public boolean isExtraordinary() {
        return true;
    }

    public Boolean isExtraordinarilyBoxed() {
        return false;
    }

    public double field = 42.42;

    public int twice(int i) {
        return i * 2;
    }

    public void overriddenMethod() {
    }
}
