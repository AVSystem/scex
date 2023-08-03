package com.avsystem.scex.util.function;

import java.util.Collection;
import com.avsystem.scex.presentation.annotation.Documentation;

public interface StringUtil {
    @Documentation("Returns concatenation of the provided comma-separated parts into a new string.")
    String concat(String... parts);

    @Documentation("Returns true if the `list` of comma-separated values contains the string provided as the `item` argument.")
    boolean contains(String list, String item);

    @Documentation("Returns true if the `source` string contains the value provided as `item`.")
    boolean stringContains(String source, String item);

    @Documentation("Returns the first number in the provided string as a double value type. Returns 0 if string is null or does not include a number.")
    double extract(String string);

    @Documentation("Returns a string which matches the regular expression `pattern`. Returns null if there is no match.")
    String regexFind(String value, String pattern);

    @Documentation("Returns a group of characters which matches the regular expression `pattern`. Returns null if there is no match.")
    String regexFindGroup(String value, String pattern);

    @Documentation("Returns true if the string `value` matches the regular expression `pattern`.")
    boolean regexMatches(String value, String pattern);

    @Documentation("Returns a string with first part of the `value` string matching the regular expression `pattern` replaced with the `replacement` argument.")
    String regexReplace(String value, String pattern, String replacement);

    @Documentation("Returns a string with all parts of the `value` string matching the regular expression `pattern` replaced with the `replacement` argument.")
    String regexReplaceAll(String value, String pattern, String replacement);

    @Documentation("Returns all parts of a dot-separated string following the part indicated by `from` value, e.g. string.slice('a.b.c.d.', 2) returns 'c.d.'")
    String slice(String item, int from);

    @Documentation("Returns all parts of a dot-separated string following the part indicated by `from` value. `Dot` boolean value determines if a dot is added to the output, e.g. string.slice('a.b.c.d.', 2, false) returns 'c.d'")
    String slice(String item, int from, int to, boolean dot);

    @Documentation("Returns the `source` string with all non-alphanumeric characters replaced with the `replacement` string.")
    String stripToAlphanumeric(String source, String replacement);

    @Documentation("Returns the `source` string with it last characters indicated by the `end` string removed.")
    String removeEnd(String source, String end);

    @Documentation("Returns the `source` string with it first characters indicated by the `end` string removed.")
    String removeStart(String source, String start);

    @Documentation("Returns the `source` string with `InternetGatewayDevice.` or `Device.` parts removed.")
    String removeTRRoot(String source);

    @Documentation("Returns a random alphanumeric string. The `length` argument indicates the number of characters.")
    String random(int length);

    @Documentation("Prepends the `str` string with `padStr` (repeated or trimmed if needed). Number of characters in the output corresponds to `size`.")
    String leftPad(String str, int size, String padStr);

    @Documentation("Appends `padStr` (repeated or trimmed if needed) to the `str` string. Number of characters in the output corresponds to `size`.")
    String rightPad(String str, int size, String padStr);

    @Documentation("Returns a substring of `str`. The substring begins at the specified `from` index and extends to the character at the `to` index.")
    String subString(String str, int from, int to);

    @Documentation("Returns an array of strings split from `str` with the given `separator`.")
    String[] split(String str, String separator);

    @Documentation("Returns the `str` string with whitespaces removed.")
    String trimToEmpty(String str);

    @Documentation("Replaces all occurrences of `find` with `replacement` in the `string`.")
    String replace(String str, String find, String replacement);

    @Documentation("Returns a string which is a concatenation of the `list` elements using the provided `separator`.")
    String join(Collection<String> list, String separator);

    @Documentation("Calculates the HMAC MD5 digest from the `str` string, using the provided `key`. The output is a 32 character hexadecimal string.")
    String hmacMD5(String str, String key);
}
