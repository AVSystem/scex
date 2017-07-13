package com.avsystem.scex.util.function;

import org.apache.commons.lang3.StringUtils;
import org.joda.time.Period;
import org.joda.time.PeriodType;
import org.joda.time.format.PeriodFormatter;
import org.joda.time.format.PeriodFormatterBuilder;

import java.math.BigDecimal;
import java.math.MathContext;
import java.util.Collection;
import java.util.Date;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class FormatUtilImpl implements FormatUtil {
    public static final FormatUtilImpl INSTANCE = new FormatUtilImpl();

    private static final String NUMERIC_PATTERN = "((-|\\+)?[0-9]+(\\.[0-9]+)?)+";

    private static final Pattern numericPattern = Pattern.compile(NUMERIC_PATTERN);

    public static String getPeriodMessage(Date from) {
        Period period = new Period(from.getTime(), new Date().getTime());
        String periodMessage = INSTANCE.formatPeriod(period);
        return StringUtils.substringBefore(periodMessage, " ");
    }

    private FormatUtilImpl() {
    }

    private static final PeriodFormatter PERIOD_FORMATTER = new PeriodFormatterBuilder()
            .printZeroNever()
            .appendYears().appendSuffix("y ")
            .appendMonths().appendSuffix("M ")
            .appendDays().appendSuffix("d ")
            .appendHours().appendSuffix("h ")
            .printZeroAlways()
            .appendMinutes().appendSuffix("m ")
            .appendSeconds().appendSuffix("s ")
            .toFormatter();

    private static final String[] Q = new String[] { "", "k", "M", "G", "T", "P", "E" };
    private Long SI = 1000L;
    private Long BINARY = 1024L;
    private static final BigDecimal ZERO = new BigDecimal(0);

    @Override
    public Number asNumber(String value) {
        if (StringUtils.isBlank(value)) {
            return ZERO;
        }
        return new BigDecimal(value, MathContext.UNLIMITED);
    }

    @Override
    public String normalizeValue(long value, int inputUnitIndex, boolean siCompliance, String unitName) {
        long unit = siCompliance ? SI : BINARY;
        for (int i = 6 + inputUnitIndex; i > 0; i--) {
            double step = Math.pow(unit, i);
            if (value > step)
                return String.format("%3.1f %s%s%s", value / step, Q[i], siCompliance ? "" : "i", unitName);
        }

        return Long.toString(value) + " " + unitName;
    }

    @Override
    public String normalizeValue(long value, boolean siCompliance, String unitName) {
        return normalizeValue(value, 0, siCompliance, unitName);
    }

    @Override
    public String normalizeBytes(long bytes) {
        return normalizeValue(bytes, true, "B");
    }

    public String normalizeBytesWithDot(long bytes) {
        return normalizeBytes(bytes).replace(",", ".");
    }


    @Override
    public String secondsToPeriod(long seconds) {
        return formatPeriod(new Period(TimeUnit.SECONDS.toMillis(seconds)));
    }

    public String formatPeriod(Period period) {
        return PERIOD_FORMATTER.print(period.normalizedStandard(PeriodType.yearMonthDayTime()));
    }

    /**
     * Check if given value is numeric
     *
     * @param value value to check
     * @return true valid value is numeric, false otherwise
     */
    @Override
    public boolean isNumeric(String value) {
        if (value != null) {
            Matcher matcher = numericPattern.matcher(value);
            return matcher.matches();
        }
        return false;
    }

    @Override
    public String join(Collection<String> collection) {
        return join(collection, ",");
    }

    @Override
    public String join(Collection<String> collection, String delimiter) {
        return StringUtils.join(collection.iterator(), delimiter);
    }
}
