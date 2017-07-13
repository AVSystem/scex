package com.avsystem.scex.util.function;

import com.avsystem.scex.presentation.annotation.ParameterNames;

import java.util.Collection;

public interface FormatUtil {
    @ParameterNames("value")
    Number asNumber(String value);

    @ParameterNames("value")
    boolean isNumeric(String value);

    @ParameterNames({ "value", "siCompliance", "unitName" })
    String normalizeValue(long value, boolean siCompliance, String unitName);

    @ParameterNames({ "value", "inputUnitIndex", "siCompliance", "unitName" })
    String normalizeValue(long value, int inputUnitIndex, boolean siCompliance, String unitName);

    @ParameterNames("bytes")
    String normalizeBytes(long bytes);

    @ParameterNames("seconds")
    String secondsToPeriod(long seconds);

    @ParameterNames("collection")
    String join(Collection<String> collection);

    @ParameterNames({ "collection", "delimiter" })
    String join(Collection<String> collection, String delimiter);

}
