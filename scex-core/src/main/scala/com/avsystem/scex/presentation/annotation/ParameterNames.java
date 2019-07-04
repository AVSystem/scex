package com.avsystem.scex.presentation.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Adds information about parameter names of Java methods.
 *
 * @deprecated you should use <tt>-parameters</tt> compiler option for Java instead
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
@Deprecated
public @interface ParameterNames {
    String[] value();
}
