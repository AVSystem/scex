package com.avsystem.scex.util.function;

import java.util.Collection;

public interface FormatUtil {
    Number asNumber(String value);

    boolean isNumeric(String value);

    String normalizeValue(long value, boolean siCompliance, String unitName);

    String normalizeValue(long value, int inputUnitIndex, boolean siCompliance, String unitName);

    String normalizeBytes(long bytes);

    String secondsToPeriod(long seconds);

    String join(Collection<String> collection);

    String join(Collection<String> collection, String delimiter);

}
