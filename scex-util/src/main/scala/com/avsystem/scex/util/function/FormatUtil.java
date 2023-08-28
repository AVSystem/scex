package com.avsystem.scex.util.function;

import java.util.Collection;
import com.avsystem.scex.presentation.annotation.Documentation;

public interface FormatUtil {
    Number asNumber(String value);

    @Documentation("Returns true if the provided `value` contains only numeric values.")
    boolean isNumeric(String value);

    @Documentation("Transforms the provided `value` into a common scale. `siCompliance` set to `true` means a decimal " +
            "base is used (1000), otherwise binary (1024). `unitName` is appended to the output.")
    String normalizeValue(long value, boolean siCompliance, String unitName);

    @Documentation("Transforms the provided `value` into a common scale. `siCompliance` set to `true` means a decimal " +
            "base is used (1000), otherwise binary (1024). Select unit via `inputUnitIndex` integer value corresponding" +
            "to k, M, G, T, P, E. `unitName` is appended to the output.")
    String normalizeValue(long value, int inputUnitIndex, boolean siCompliance, String unitName);

    @Documentation("Returns normalized value for the provided number of `bytes`, e.g. `format.normalizeBytes(16777216)` returns 16.8 MB")
    String normalizeBytes(long bytes);

    @Documentation("Returns the numeric value of `seconds` as a string representation, e.g. `1000` as `16m 40s`.")
    String secondsToPeriod(long seconds);

    @Documentation("Returns the provided list of strings joined.")
    String join(Collection<String> collection);

    @Documentation("Returns the provided list of strings joined and using the selected `delimiter`.")
    String join(Collection<String> collection, String delimiter);

}
