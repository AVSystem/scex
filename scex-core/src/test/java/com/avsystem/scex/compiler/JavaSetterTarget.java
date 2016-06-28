package com.avsystem.scex.compiler;

/**
 * Created: 28-11-2013
 * Author: ghik
 */
public class JavaSetterTarget {
    public int field = 0;

    private int beanprop;
    private Boolean awesome;

    public int getBeanprop() {
        return beanprop;
    }

    public void setBeanprop(int beanprop) {
        this.beanprop = beanprop;
    }

    public Boolean isAwesome() {
        return awesome;
    }

    public void setAwesome(Boolean awesome) {
        this.awesome = awesome;
    }

    public JavaSetterTarget self() {
        return this;
    }
}
