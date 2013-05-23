package com.avsystem.scex.compiler;

public class ParameterizedClass<T> {
    public static class StaticInnerGeneric<A extends Cloneable> {
        public class DeeplyInnerGeneric<B> {
        }
    }
}
