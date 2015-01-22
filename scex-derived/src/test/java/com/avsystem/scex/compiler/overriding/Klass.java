package com.avsystem.scex.compiler.overriding;

/**
 * Author: ghik
 * Created: 22/01/15.
 */
public class Klass implements Base {
    @Override
    public Klass get(String property) {
        return this;
    }
}
