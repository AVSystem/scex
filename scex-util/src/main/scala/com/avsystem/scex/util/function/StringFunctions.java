package com.avsystem.scex.util.function;

import org.apache.commons.lang3.StringUtils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class StringFunctions {
    private static final String NON_ALPHANUERIC_REGEX = "[^A-Za-z0-9]";
    private static final Pattern PATTERN;

    static {
        final String Digits = "(\\p{Digit}+)";
        final String HexDigits = "(\\p{XDigit}+)";
        // an exponent is 'e' or 'E' followed by an optionally
        // signed decimal integer.
        final String Exp = "[eE][+-]?" + Digits;
        final String fpRegex = ("[\\x00-\\x20]*" + // Optional leading "whitespace"
                "[+-]?(" + // Optional sign character
                "NaN|" + // "NaN" string
                "Infinity|" + // "Infinity" string

                // A decimal floating-point string representing a finite positive
                // number without a leading sign has at most five basic pieces:
                // Digits . Digits ExponentPart FloatTypeSuffix
                //
                // Since this method allows integer-only strings as input
                // in addition to strings of floating-point literals, the
                // two sub-patterns below are simplifications of the grammar
                // productions from the Java Language Specification, 2nd
                // edition, section 3.10.2.

                // Digits ._opt Digits_opt ExponentPart_opt FloatTypeSuffix_opt
                "(((" + Digits + "(\\.)?(" + Digits + "?)(" + Exp + ")?)|" +

                // . Digits ExponentPart_opt FloatTypeSuffix_opt
                "(\\.(" + Digits + ")(" + Exp + ")?)|" +

                // Hexadecimal strings
                "((" +
                // 0[xX] HexDigits ._opt BinaryExponent FloatTypeSuffix_opt
                "(0[xX]" + HexDigits + "(\\.)?)|" +

                // 0[xX] HexDigits_opt . HexDigits BinaryExponent FloatTypeSuffix_opt
                "(0[xX]" + HexDigits + "?(\\.)" + HexDigits + ")" +

                ")[pP][+-]?" + Digits + "))" + "[fFdD]?))" + "[\\x00-\\x20]*");// Optional trailing "whitespace"
        PATTERN = Pattern.compile(fpRegex);
    }

    public static String concat(String... parts) {
        return StringUtils.join(parts);
    }

    public static boolean contains(String list, String item) {
        String[] parts = StringUtils.split(list, ",");
        for (String part : parts) {
            if (part.trim().equalsIgnoreCase(item.trim())) {
                return true;
            }
        }
        return false;
    }

    public static double extract(String string) {
        if (string == null) {
            return 0;
        }
        Matcher matcher = PATTERN.matcher(string.replaceAll("\\,", "\\."));
        if (matcher.find()) {
            return Double.parseDouble(matcher.group());
        } else {
            return 0;
        }
    }

    public static Double extractStrict(String string) {
        if (string == null) {
            return null;
        }
        Matcher matcher = PATTERN.matcher(string.replaceAll("\\,", "\\."));
        if (matcher.find()) {
            double value = Double.parseDouble(matcher.group());
            if (Double.isNaN(value) || Double.isInfinite(value)) {
                return null;
            }
            return value;
        } else {
            return null;
        }
    }

    public static String regexFind(String value, String pattern) {
        Matcher matcher = Pattern.compile(pattern).matcher(value);
        if (matcher.find()) {
            return matcher.group();
        } else {
            return null;
        }
    }

    public static String regexFindGroup(String value, String pattern) {
        Matcher matcher = Pattern.compile(pattern).matcher(value);
        if (matcher.find()) {
            return matcher.group(1);
        } else {
            return null;
        }
    }

    public static boolean regexMatches(String value, String pattern) {
        return value.matches(pattern);
    }

    public static String regexReplace(String value, String pattern, String replacement) {
        return value.replaceFirst(pattern, replacement);
    }

    public static String regexReplaceAll(String value, String pattern, String replacement) {
        return value.replaceAll(pattern, replacement);
    }

    public static String slice(String item, int from) {
        if (item == null) {
            return null;
        }

        String splits[] = item.split("\\.");
        StringBuilder sb = new StringBuilder();
        for (int i = from; i < splits.length; i++) {
            sb.append(splits[i]);
            sb.append(".");
        }
        String result = sb.toString();
        if (!item.endsWith(".") && result.endsWith(".")) {
            result = result.substring(0, result.length() - 1);
        }
        return result;
    }

    public static String slice(String item, int from, int to, boolean dot) {
        if (item == null) {
            return null;
        }
        String splits[] = item.split("\\.");
        StringBuilder sb = new StringBuilder();
        to = Math.min(to, splits.length);
        for (int i = from; i < to; i++) {
            sb.append(splits[i]);
            sb.append(".");
        }
        String result = sb.toString();
        if (!dot && result.endsWith(".")) {
            result = result.substring(0, result.length() - 1);
        }
        return result;
    }

    public static String stripToAlphanumeric(String source, String replacement) {
        return source.replaceAll(NON_ALPHANUERIC_REGEX, replacement);
    }

    public static String leftPad(String str, int size, String padStr) {
        return StringUtils.leftPad(str, size, padStr);
    }

    public static String rightPad(String str, int size, String padStr) {
        return StringUtils.rightPad(str, size, padStr);
    }
}
