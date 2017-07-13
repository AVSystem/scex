package com.avsystem.scex.util.function;

import org.apache.commons.lang3.time.DateUtils;

import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

public class DateUtilImpl implements DateUtil {
    public static final DateUtil INSTANCE = new DateUtilImpl();

    private DateUtilImpl() {
    }

    @Override
    public Date now() {
        return new Date();
    }

    @Override
    public Date fromMillis(long millis) {
        return new Date(millis);
    }

    //ADD
    @Override
    public Date addMilliseconds(Date date, int milliseconds) {
        return DateUtils.addMilliseconds(date, milliseconds);
    }

    @Override
    public Date addSeconds(Date date, int seconds) {
        return DateUtils.addSeconds(date, seconds);
    }

    @Override
    public Date addMinutes(Date date, int minutes) {
        return DateUtils.addMinutes(date, minutes);
    }

    @Override
    public Date addHours(Date date, int hours) {
        return DateUtils.addHours(date, hours);
    }

    @Override
    public Date addDays(Date date, int days) {
        return DateUtils.addDays(date, days);
    }

    @Override
    public Date addWeeks(Date date, int weeks) {
        return DateUtils.addWeeks(date, weeks);
    }

    @Override
    public Date addMonths(Date date, int months) {
        return DateUtils.addMonths(date, months);
    }

    @Override
    public Date addYears(Date date, int years) {
        return DateUtils.addYears(date, years);
    }

    //SET
    @Override
    public Date setMilliseconds(Date date, int milliseconds) {
        return DateUtils.setMilliseconds(date, milliseconds);
    }

    @Override
    public Date setSeconds(Date date, int seconds) {
        return DateUtils.setSeconds(date, seconds);
    }

    @Override
    public Date setMinutes(Date date, int minutes) {
        return DateUtils.setMinutes(date, minutes);
    }

    @Override
    public Date setHours(Date date, int hours) {
        return DateUtils.setHours(date, hours);
    }

    @Override
    public Date setDays(Date date, int days) {
        return DateUtils.setDays(date, days);
    }

    @Override
    public Date setMonths(Date date, int months) {
        return DateUtils.setMonths(date, months);
    }

    @Override
    public Date setYears(Date date, int years) {
        return DateUtils.setYears(date, years);
    }

    //ROUND
    @Override
    public Date roundToSeconds(Date date) {
        return DateUtils.round(date, Calendar.SECOND);
    }

    @Override
    public Date roundToMinutes(Date date) {
        return DateUtils.round(date, Calendar.MINUTE);
    }

    @Override
    public Date roundToHours(Date date) {
        return DateUtils.round(date, Calendar.HOUR);
    }

    @Override
    public Date roundToDay(Date date) {
        return DateUtils.round(date, Calendar.DAY_OF_MONTH);
    }

    @Override
    public Date roundToMonth(Date date) {
        return DateUtils.round(date, Calendar.MONTH);
    }

    @Override
    public Date roundToYear(Date date) {
        return DateUtils.round(date, Calendar.YEAR);
    }

    //TRUNCATE
    @Override
    public Date truncateBySeconds(Date date) {
        return DateUtils.truncate(date, Calendar.SECOND);
    }

    @Override
    public Date truncateByMinutes(Date date) {
        return DateUtils.truncate(date, Calendar.MINUTE);
    }

    @Override
    public Date truncateByHours(Date date) {
        return DateUtils.truncate(date, Calendar.HOUR);
    }

    @Override
    public Date truncateByDay(Date date) {
        return DateUtils.truncate(date, Calendar.DAY_OF_MONTH);
    }

    @Override
    public Date truncateByMonth(Date date) {
        return DateUtils.truncate(date, Calendar.MONTH);
    }

    @Override
    public Date truncateByYear(Date date) {
        return DateUtils.truncate(date, Calendar.YEAR);
    }

    //COMPARE
    @Override
    public boolean isEqual(Date date1, Date date2) {
        return DateUtils.isSameInstant(date1, date2);
    }

    @Override
    public boolean isBefore(Date date1, Date date2) {
        return date1.before(date2);
    }

    @Override
    public boolean isAfter(Date date1, Date date2) {
        return date1.after(date2);
    }

    @Override
    public String format(Date date, String format) {
        SimpleDateFormat formatter = new SimpleDateFormat(format);
        return formatter.format(date);
    }

    @Override
    public String formatTimeRange(Double from, Double to) {
        String[] units = { "ms", "s", "m", "h", "d", "y" };
        Integer[] unitsDivider = { 1000, 60, 60, 24, 365, 1 };

        int unit = 0;
        while (unit < units.length - 1 && (from >= unitsDivider[unit] || to >= unitsDivider[unit])) {
            from /= unitsDivider[unit];
            to /= unitsDivider[unit];
            ++unit;
        }

        DecimalFormat format = new DecimalFormat("###.#");
        return String.format("%s-%s%s", format.format(from), format.format(to), units[unit]);
    }

    @Override
    public String formatTimeRange(Long from, Long to) {
        return formatTimeRange(from.doubleValue(), to.doubleValue());
    }

    @Override
    public String formatTimeRange(Date from, Date to) {
        return formatTimeRange(from.getTime(), to.getTime());
    }
}
