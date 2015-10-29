package com.avsystem.scex.compiler.overriding;

/**
 * Author: ghik
 * Created: 29/10/15.
 */
public class Specialized extends GenericBase<String> {
    @Override
    public String getThat() {
        return "that";
    }
}
