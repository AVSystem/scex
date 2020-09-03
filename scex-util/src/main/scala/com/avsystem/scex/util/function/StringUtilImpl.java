package com.avsystem.scex.util.function;

import org.apache.commons.codec.digest.HmacAlgorithms;
import org.apache.commons.codec.digest.HmacUtils;
import org.apache.commons.lang3.RandomStringUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.Collection;

public class StringUtilImpl implements StringUtil {
    public static final StringUtilImpl INSTANCE = new StringUtilImpl();

    private StringUtilImpl() {
    }

    @Override
    public String concat(String... parts) {
        return StringFunctions.concat(parts);
    }

    @Override
    public boolean contains(String list, String item) {
        return StringFunctions.contains(list, item);
    }

    @Override
    public double extract(String string) {
        return StringFunctions.extract(string);
    }

    @Override
    public String regexFind(String value, String pattern) {
        return StringFunctions.regexFind(value, pattern);
    }

    @Override
    public String regexFindGroup(String value, String pattern) {
        return StringFunctions.regexFindGroup(value, pattern);
    }

    @Override
    public boolean regexMatches(String value, String pattern) {
        return StringFunctions.regexMatches(value, pattern);
    }

    @Override
    public String regexReplace(String value, String pattern, String replacement) {
        return StringFunctions.regexReplace(value, pattern, replacement);
    }

    @Override
    public String regexReplaceAll(String value, String pattern, String replacement) {
        return StringFunctions.regexReplaceAll(value, pattern, replacement);
    }

    @Override
    public String slice(String item, int from) {
        return StringFunctions.slice(item, from);
    }

    @Override
    public String slice(String item, int from, int to, boolean dot) {
        return StringFunctions.slice(item, from, to, dot);
    }

    @Override
    public String stripToAlphanumeric(String source, String replacement) {
        return StringFunctions.stripToAlphanumeric(source, replacement);
    }

    /**
     * Remove end for string if exist
     *
     * @param source source string
     * @param end    string to remove from the end of source
     * @return string with removed end
     */
    @Override
    public String removeEnd(String source, String end) {
        if (source != null && end != null) {
            return source.endsWith(end) ? source.substring(0, source.length() - end.length()) : source;
        }
        return null;
    }

    /**
     * Remove start from string if exist
     *
     * @param source source string
     * @param start  string to remove from the start of source
     * @return string with removed end
     */
    @Override
    public String removeStart(String source, String start) {
        if (source != null && start != null) {
            return source.startsWith(start) ? source.substring(start.length(), source.length()) : source;
        }
        return null;
    }

    @Override
    public String removeTRRoot(String source) {
        if (source != null) {
            if (source.startsWith("InternetGatewayDevice.")) {
                source = source.replaceFirst("InternetGatewayDevice.", "");
            } else if (source.startsWith("Device.")) {
                source = source.replaceFirst("Device.", "");
            }
        }
        return source;
    }

    @Override
    public String random(int length) {
        return RandomStringUtils.randomAlphanumeric(length);
    }

    /**
     * Left pad a String with a specified String.
     *
     * @param str    the String to pad out, may be null
     * @param size   the size to pad to
     * @param padStr the String to pad with, null or empty treated as single space
     * @return right padded String
     */
    @Override
    public String leftPad(String str, int size, String padStr) {
        return StringFunctions.leftPad(str, size, padStr);
    }

    /**
     * Right pad a String with a specified String.
     *
     * @param str    the String to pad out, may be null
     * @param size   the size to pad to
     * @param padStr the String to pad with, null or empty treated as single space
     * @return left padded String
     */
    @Override
    public String rightPad(String str, int size, String padStr) {
        return StringFunctions.rightPad(str, size, padStr);
    }

    @Override
    public String subString(String str, int from, int to) {
        return StringUtils.substring(str, from, to);
    }

    @Override
    public String[] split(String str, String separator) {
        return StringUtils.split(str, separator);
    }

    @Override
    public String trimToEmpty(String str) {
        return StringUtils.trimToEmpty(str);
    }

    @Override
    public String replace(String str, String find, String replacement) {
        return StringUtils.replace(str, find, replacement);
    }

    @Override
    public String join(Collection<String> list, String separator) {
        return StringUtils.join(list, separator);
    }

    @Override
    public boolean stringContains(String source, String item) {
        return StringUtils.contains(source, item);
    }

    @Override
    public String hmacMD5(String str, String key) {
        return new HmacUtils(HmacAlgorithms.HMAC_MD5, key).hmacHex(str);
    }
}
