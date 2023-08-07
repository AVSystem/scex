package com.avsystem.scex.util.function;

import java.util.Date;
import com.avsystem.scex.presentation.annotation.Documentation;

public interface DateUtil {

    @Documentation("Returns current date in `YYYY.MM.DD hh:mm:ss` format.")
    Date now();

    @Documentation("Returns the provided value as a date in milliseconds since 1970.01.01 00:00:00.")
    public abstract Date fromMillis(long millis);

    //ADD
    @Documentation("Returns the `date` incremented by the provided amount of `milliseconds`.")
    Date addMilliseconds(Date date, int milliseconds);

    @Documentation("Returns the `date` incremented by the provided amount of `seconds`.")
    Date addSeconds(Date date, int seconds);

    @Documentation("Returns the `date` incremented by the provided amount of `minutes`.")
    Date addMinutes(Date date, int minutes);

    @Documentation("Returns the `date` incremented by the provided amount of `hours`.")
    Date addHours(Date date, int hours);

    @Documentation("Returns the `date` incremented by the provided amount of `days`.")
    Date addDays(Date date, int days);

    @Documentation("Returns the `date` incremented by the provided amount of `weeks`.")
    Date addWeeks(Date date, int weeks);

    @Documentation("Returns the `date` incremented by the provided amount of `months`.")
    Date addMonths(Date date, int months);

    @Documentation("Returns the `date` incremented by the provided amount of `years`.")
    Date addYears(Date date, int years);

    //SET
    @Documentation("Returns the `date` with `milliseconds` changed to the provided value.")
    Date setMilliseconds(Date date, int milliseconds);

    @Documentation("Returns the `date` with `seconds` changed to the provided value.")
    Date setSeconds(Date date, int seconds);

    @Documentation("Returns the `date` with `minutes` changed to the provided value.")
    Date setMinutes(Date date, int minutes);

    @Documentation("Returns the `date` with `hours` changed to the provided value.")
    Date setHours(Date date, int hours);

    @Documentation("Returns the `date` with `days` changed to the provided value.")
    Date setDays(Date date, int days);

    @Documentation("Returns the `date` with `months` changed to the provided value.")
    Date setMonths(Date date, int months);

    @Documentation("Returns the `date` with `years` changed to the provided value.")
    Date setYears(Date date, int years);

    //ROUND
    @Documentation("Returns the `date` rounded to the nearest second.")
    Date roundToSeconds(Date date);

    @Documentation("Returns the `date` rounded to the nearest minute.")
    Date roundToMinutes(Date date);

    @Documentation("Returns the `date` rounded to the nearest hour.")
    Date roundToHours(Date date);

    @Documentation("Returns the `date` rounded to the nearest day.")
    Date roundToDay(Date date);

    @Documentation("Returns the `date` rounded to the nearest month.")
    Date roundToMonth(Date date);

    @Documentation("Returns the `date` rounded to the nearest year.")
    Date roundToYear(Date date);

    //TRUNCATE
    @Documentation("Returns the `date` truncated by seconds.")
    Date truncateBySeconds(Date date);

    @Documentation("Returns the `date` truncated by minutes.")
    Date truncateByMinutes(Date date);

    @Documentation("Returns the `date` truncated by hours.")
    Date truncateByHours(Date date);

    @Documentation("Returns the `date` truncated by day.")
    Date truncateByDay(Date date);

    @Documentation("Returns the `date` truncated by month.")
    Date truncateByMonth(Date date);

    @Documentation("Returns the `date` truncated by year.")
    Date truncateByYear(Date date);

    //COMPARE
    @Documentation("Returns true if `date1` equals `date2`.")
    boolean isEqual(Date date1, Date date2);

    @Documentation("Returns true if `date1` is before `date2`.")
    boolean isBefore(Date date1, Date date2);

    @Documentation("Returns true if `date1` is after `date2`.")
    boolean isAfter(Date date1, Date date2);

    //FORMAT
    @Documentation("Returns the provided `date` in the indicated `format`.")
    String format(Date date, String format);

    @Documentation("Returns a time range between the two values to be provided in milliseconds.")
    String formatTimeRange(Double from, Double to);

    @Documentation("Returns a time range between the two values to be provided in milliseconds.")
    String formatTimeRange(Long from, Long to);

    @Documentation("Returns a time range between two dates.")
    String formatTimeRange(Date from, Date to);

}
