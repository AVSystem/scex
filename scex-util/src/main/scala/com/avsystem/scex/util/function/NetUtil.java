package com.avsystem.scex.util.function;

import com.avsystem.scex.presentation.annotation.Documentation;

public interface NetUtil {
    @Documentation("Returns true if the value of `ip` argument belongs to the subnet provided as the `subnetWithMask` " +
            "argument. Use CIDR notation for the `subnetWithMask` argument, e.g. `10.8.0.0/16`.")
    boolean isIpInSubnet(String ip, String subnetWithMask);

    @Documentation("Returns true if the value of 'ip' argument belongs to the subnet defined by the `subnet` and 'mask' arguments. Use dot-decimal notation for the arguments.")
    boolean isIpInSubnetWithMask(String ip, String subnet, String mask);

    @Documentation("Returns network address (host identifier) for the provided `ip` and `mask`. Use dot-decimal notation for the arguments.")
    String networkAddress(String ip, String mask);

    @Documentation("Returns network address (host identifier) for the provided `subnetWithMask`. Use CIDR notation for the argument, e.g. `192.168.10.5/24`.")
    String networkAddress(String subnetWithMask);

    @Documentation("Converts subnet mask in CIDR notation to dot-decimal notation.")
    String getMask(String cidr);

    @Documentation("Returns minimal host address based on the provided `ip` and `mask`. Use dot-decimal notation for the arguments.")
    String getMinAddress(String ip, String mask);

    @Documentation("Returns maximal host address based on the provided `ip` and `mask`. Use dot-decimal notation for the arguments.")
    String getMaxAddress(String ip, String mask);

    @Documentation("Returns the `mac` address provided as the argument with any separating characters removed and letters changed to uppercase.")
    String stripMac(String mac);

    @Documentation("Returns 0 if this address is equal to the one provided as the argument, -1 if less than, 1 if greater than.")
    Integer compareIp(String ip1, String ip2);

    @Documentation("Returns true if the provided string is a valid IP address.")
    boolean isIp(String ip);

    @Documentation("Returns true if the provided object is a valid IP address.")
    public boolean isIp(Object ip);

    @Documentation("Returns true if the all the provided IP addresses in a comma-separated list are valid.")
    boolean isIps(String ips);

    @Documentation("Returns true if the provided `mac` argument is valid hardware address.")
    boolean isMac(String mac);

    @Documentation("Converts network mask in CIDR notation to dot-decimal notation, e.g. `/24` to `255.255.255.0`.")
    String maskFromCidr(String mask);

    @Documentation("Converts network mask in dot-decimal notation to CIDR notation, e.g. `255.255.255.0` to `/24`.")
    String maskToCidr(String mask);

    @Documentation("Returns true if the provided hostname is valid against RFC 1035 and RFC 1123.")
    boolean isValidHostname(String host);

}