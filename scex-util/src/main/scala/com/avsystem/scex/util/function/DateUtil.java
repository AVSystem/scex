package com.avsystem.scex.util.function;

import java.util.Date;

public interface DateUtil {

    Date now();

    public abstract Date fromMillis(long millis);

    //ADD
    Date addMilliseconds(Date date, int milliseconds);

    Date addSeconds(Date date, int seconds);

    Date addMinutes(Date date, int minutes);

    Date addHours(Date date, int hours);

    Date addDays(Date date, int days);

    Date addWeeks(Date date, int weeks);

    Date addMonths(Date date, int months);

    Date addYears(Date date, int years);

    //SET
    Date setMilliseconds(Date date, int milliseconds);

    Date setSeconds(Date date, int seconds);

    Date setMinutes(Date date, int minutes);

    Date setHours(Date date, int hours);

    Date setDays(Date date, int days);

    Date setMonths(Date date, int months);

    Date setYears(Date date, int years);

    //ROUND
    Date roundToSeconds(Date date);

    Date roundToMinutes(Date date);

    Date roundToHours(Date date);

    Date roundToDay(Date date);

    Date roundToMonth(Date date);

    Date roundToYear(Date date);

    //TRUNCATE
    Date truncateBySeconds(Date date);

    Date truncateByMinutes(Date date);

    Date truncateByHours(Date date);

    Date truncateByDay(Date date);

    Date truncateByMonth(Date date);

    Date truncateByYear(Date date);

    //COMPARE
    boolean isEqual(Date date1, Date date2);

    boolean isBefore(Date date1, Date date2);

    boolean isAfter(Date date1, Date date2);

    //FORMAT
    String format(Date date, String format);

    String formatTimeRange(Double from, Double to);

    String formatTimeRange(Long from, Long to);

    String formatTimeRange(Date from, Date to);

}
