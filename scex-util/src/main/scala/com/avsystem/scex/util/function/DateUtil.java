package com.avsystem.scex.util.function;

import com.avsystem.scex.presentation.annotation.ParameterNames;

import java.util.Date;

public interface DateUtil {

    Date now();

    @ParameterNames("millis")
    public abstract Date fromMillis(long millis);

    //ADD
    @ParameterNames({ "date", "milliseconds" })
    Date addMilliseconds(Date date, int milliseconds);

    @ParameterNames({ "date", "seconds" })
    Date addSeconds(Date date, int seconds);

    @ParameterNames({ "date", "minutes" })
    Date addMinutes(Date date, int minutes);

    @ParameterNames({ "date", "hours" })
    Date addHours(Date date, int hours);

    @ParameterNames({ "date", "days" })
    Date addDays(Date date, int days);

    @ParameterNames({ "date", "weeks" })
    Date addWeeks(Date date, int weeks);

    @ParameterNames({ "date", "months" })
    Date addMonths(Date date, int months);

    @ParameterNames({ "date", "years" })
    Date addYears(Date date, int years);

    //SET
    @ParameterNames({ "date", "milliseconds" })
    Date setMilliseconds(Date date, int milliseconds);

    @ParameterNames({ "date", "seconds" })
    Date setSeconds(Date date, int seconds);

    @ParameterNames({ "date", "minutes" })
    Date setMinutes(Date date, int minutes);

    @ParameterNames({ "date", "hours" })
    Date setHours(Date date, int hours);

    @ParameterNames({ "date", "days" })
    Date setDays(Date date, int days);

    @ParameterNames({ "date", "months" })
    Date setMonths(Date date, int months);

    @ParameterNames({ "date", "years" })
    Date setYears(Date date, int years);

    //ROUND
    @ParameterNames("date")
    Date roundToSeconds(Date date);

    @ParameterNames("date")
    Date roundToMinutes(Date date);

    @ParameterNames("date")
    Date roundToHours(Date date);

    @ParameterNames("date")
    Date roundToDay(Date date);

    @ParameterNames("date")
    Date roundToMonth(Date date);

    @ParameterNames("date")
    Date roundToYear(Date date);

    //TRUNCATE
    @ParameterNames("date")
    Date truncateBySeconds(Date date);

    @ParameterNames("date")
    Date truncateByMinutes(Date date);

    @ParameterNames("date")
    Date truncateByHours(Date date);

    @ParameterNames("date")
    Date truncateByDay(Date date);

    @ParameterNames("date")
    Date truncateByMonth(Date date);

    @ParameterNames("date")
    Date truncateByYear(Date date);

    //COMPARE
    @ParameterNames({ "date1", "date2" })
    boolean isEqual(Date date1, Date date2);

    @ParameterNames({ "date1", "date2" })
    boolean isBefore(Date date1, Date date2);

    @ParameterNames({ "date1", "date2" })
    boolean isAfter(Date date1, Date date2);

    //FORMAT
    @ParameterNames({ "date", "format" })
    String format(Date date, String format);

    @ParameterNames({ "from", "to" })
    String formatTimeRange(Double from, Double to);

    @ParameterNames({ "from", "to" })
    String formatTimeRange(Long from, Long to);

    @ParameterNames({ "from", "to" })
    String formatTimeRange(Date from, Date to);

}
