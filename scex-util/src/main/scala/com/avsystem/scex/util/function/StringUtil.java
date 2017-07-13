package com.avsystem.scex.util.function;

import com.avsystem.scex.presentation.annotation.ParameterNames;

import java.util.Collection;

public interface StringUtil {
    @ParameterNames("parts")
    String concat(String... parts);

    @ParameterNames({ "list", "item" })
    boolean contains(String list, String item);

    @ParameterNames({ "source", "item" })
    boolean stringContains(String source, String item);

    @ParameterNames("string")
    double extract(String string);

    @ParameterNames({ "value", "pattern" })
    String regexFind(String value, String pattern);

    @ParameterNames({ "value", "pattern" })
    String regexFindGroup(String value, String pattern);

    @ParameterNames({ "value", "pattern" })
    boolean regexMatches(String value, String pattern);

    @ParameterNames({ "value", "pattern", "replacement" })
    String regexReplace(String value, String pattern, String replacement);

    @ParameterNames({ "value", "pattern", "replacement" })
    String regexReplaceAll(String value, String pattern, String replacement);

    @ParameterNames({ "item", "from" })
    String slice(String item, int from);

    @ParameterNames({ "item", "from", "to", "dot" })
    String slice(String item, int from, int to, boolean dot);

    @ParameterNames({ "source", "replacement" })
    String stripToAlphanumeric(String source, String replacement);

    @ParameterNames({ "source", "end" })
    String removeEnd(String source, String end);

    @ParameterNames({ "source", "start" })
    String removeStart(String source, String start);

    @ParameterNames("source")
    String removeTRRoot(String source);

    @ParameterNames("length")
    String random(int length);

    @ParameterNames({ "str", "size", "padStr" })
    String leftPad(String str, int size, String padStr);

    @ParameterNames({ "str", "size", "padStr" })
    String rightPad(String str, int size, String padStr);

    @ParameterNames({ "str", "from", "to" })
    String subString(String str, int from, int to);

    @ParameterNames({ "str", "separator" })
    String[] split(String str, String separator);

    @ParameterNames({ "str" })
    String trimToEmpty(String str);

    @ParameterNames({ "str", "find", "replacement" })
    String replace(String str, String find, String replacement);

    @ParameterNames({ "list", "separator" })
    String join(Collection<String> list, String separator);
}
