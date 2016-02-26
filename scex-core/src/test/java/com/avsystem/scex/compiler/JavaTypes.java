package com.avsystem.scex.compiler;

import com.google.common.reflect.TypeToken;

import java.lang.reflect.Type;

public class JavaTypes {
    public static Type comparableOfString() {
        return new TypeToken<Comparable<String>>() {
        }.getType();
    }

    public static Type comparableOfWildcard() {
        return new TypeToken<Comparable<?>>() {
        }.getType();
    }

    public static Type complexParameterizedType() {
        return new TypeToken<ParameterizedClass.StaticInnerGeneric<?>.DeeplyInnerGeneric<?>>() {
        }.getType();
    }

    public static Type partiallyWildcardedParameterizedType() {
        return new TypeToken<ParameterizedClass.StaticInnerGeneric<Cloneable>.DeeplyInnerGeneric<?>>() {
        }.getType();
    }
}
