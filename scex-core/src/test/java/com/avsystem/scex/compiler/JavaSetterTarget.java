package com.avsystem.scex.compiler;

/**
 * Created: 28-11-2013
 * Author: ghik
 */
public class JavaSetterTarget {
    public int field = 0;

    private int beanprop;

    public int getBeanprop() {
        return beanprop;
    }

    public void setBeanprop(int beanprop) {
        this.beanprop = beanprop;
    }

    public JavaSetterTarget self() {
        return this;
    }
}
