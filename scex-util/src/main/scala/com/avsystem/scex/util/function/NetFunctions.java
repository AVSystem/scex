package com.avsystem.scex.util.function;

import com.google.common.net.InetAddresses;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.net.util.SubnetUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.InetAddress;
import java.util.Comparator;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class NetFunctions {
    private static final Logger LOGGER = LoggerFactory.getLogger(NetFunctions.class);

    private static final String MACADDRESS_PATTERN =
            "^(([0-9A-Fa-f][0-9A-Fa-f][-:]?){5}[0-9A-Fa-f]" +
                    "[0-9A-Fa-f])|(([0-9A-Fa-f][0-9A-Fa-f]" +
                    "[0-9A-Fa-f][0-9A-Fa-f].){2}[0-9A-Fa-f]" +
                    "[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f])$";

    private static final Pattern macPattern = Pattern.compile(MACADDRESS_PATTERN);

    public static class InetAddressComparator implements Comparator<InetAddress> {
        @Override
        public int compare(InetAddress adr1, InetAddress adr2) {
            byte[] ba1 = adr1.getAddress();
            byte[] ba2 = adr2.getAddress();

            // general ordering: ipv4 before ipv6
            if (ba1.length < ba2.length) return -1;
            if (ba1.length > ba2.length) return 1;

            // we have 2 ips of the same type, so we have to compare each byte
            for (int i = 0; i < ba1.length; i++) {
                int b1 = unsignedByteToInt(ba1[i]);
                int b2 = unsignedByteToInt(ba2[i]);
                if (b1 == b2)
                    continue;
                if (b1 < b2)
                    return -1;
                else
                    return 1;
            }
            return 0;
        }

        private int unsignedByteToInt(byte b) {
            return (int) b & 0xFF;
        }
    }

    /**
     * @param ip
     * @param subnetWithMask CIDR signature (e.g. 192.168.0.57/27)
     * @return
     */
    public static boolean isIpInSubnet(String ip, String subnetWithMask) {
        if (ip == null || subnetWithMask == null) {
            return false;
        }
        try {
            SubnetUtils.SubnetInfo subnetInfo = (new SubnetUtils(subnetWithMask)).getInfo();
            return subnetInfo.isInRange(ip);
        } catch (Exception ex) {
            LOGGER.info("Ip not in subnet", ex);
            return false;
        }
    }

    public static boolean isIpInSubnetWithMask(String ip, String subnet, String mask) {
        if (ip == null || subnet == null || mask == null) {
            return false;
        }
        try {
            SubnetUtils.SubnetInfo subnetInfo = (new SubnetUtils(subnet, mask)).getInfo();
            return subnetInfo.isInRange(ip);
        } catch (Exception ex) {
            LOGGER.info("Ip not in subnet", ex);
            return false;
        }
    }

    public static String stripMac(String mac) {
        if (mac == null) {
            return "";
        }
        mac = StringUtils.stripToEmpty(mac).toUpperCase(Locale.ENGLISH);
        return mac.replaceAll("[^0-9A-F]+", "");
    }

    /**
     * Validate ip address with regular expression
     *
     * @param ip ip address for validation
     * @return true valid ip address, false invalid ip address
     */
    public static boolean isIp(final String ip) {
        if (ip == null) {
            return false;
        }
        return InetAddresses.isInetAddress(ip);
    }

    /**
     * Validate comma-separated ip addresses with regular expression
     *
     * @param ip ip address for validation
     * @return true valid ip address, false invalid ip address
     */
    public static boolean isIps(final String ips) {
        if (ips == null) {
            return false;
        }

        if (ips != null) {
            for (String ip : ips.trim().split("\\s*,\\s*")) {
                if (!isIp(ip)) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Validate mac address with regular expression
     *
     * @param mac mac address for validation
     * @return true valid mac address, false invalid mac address
     */
    public static boolean isMac(final String mac) {
        if (mac == null) {
            return false;
        }
        Matcher matcher = macPattern.matcher(mac.replaceAll("\\s", ""));
        return matcher.matches();
    }
}