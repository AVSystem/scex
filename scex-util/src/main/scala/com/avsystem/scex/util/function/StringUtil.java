package com.avsystem.scex.util.function;

import java.util.Collection;

public interface StringUtil {
    String concat(String... parts);

    boolean contains(String list, String item);

    boolean stringContains(String source, String item);

    double extract(String string);

    String regexFind(String value, String pattern);

    String regexFindGroup(String value, String pattern);

    boolean regexMatches(String value, String pattern);

    String regexReplace(String value, String pattern, String replacement);

    String regexReplaceAll(String value, String pattern, String replacement);

    String slice(String item, int from);

    String slice(String item, int from, int to, boolean dot);

    String stripToAlphanumeric(String source, String replacement);

    String removeEnd(String source, String end);

    String removeStart(String source, String start);

    String removeTRRoot(String source);

    String random(int length);

    String leftPad(String str, int size, String padStr);

    String rightPad(String str, int size, String padStr);

    String subString(String str, int from, int to);

    String[] split(String str, String separator);

    String trimToEmpty(String str);

    String replace(String str, String find, String replacement);

    String join(Collection<String> list, String separator);
}
