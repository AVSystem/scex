package com.avsystem.scex.compiler.overriding;

/**
 * Author: ghik
 * Created: 22/01/15.
 */
public interface Base extends DynamicGetter {
    @Override
    public Base get(String property);
}
