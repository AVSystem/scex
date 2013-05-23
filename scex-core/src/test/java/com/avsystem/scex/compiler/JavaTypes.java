package com.avsystem.scex.compiler;

import java.lang.reflect.Type;

import com.avsystem.scex.TypeTag;

public class JavaTypes {
    public static Type comparableOfString() {
        return new TypeTag<Comparable<String>>() {
        }.underlyingType();
    }

    public static Type comparableOfWildcard() {
        return new TypeTag<Comparable<?>>() {
        }.underlyingType();
    }

    public static Type complexParameterizedType() {
        return new TypeTag<ParameterizedClass.StaticInnerGeneric<?>.DeeplyInnerGeneric<?>>() {
        }.underlyingType();
    }

    public static Type partiallyWildcardedParameterizedType() {
        return new TypeTag<ParameterizedClass.StaticInnerGeneric<Cloneable>.DeeplyInnerGeneric<?>>() {
        }.underlyingType();
    }
}
