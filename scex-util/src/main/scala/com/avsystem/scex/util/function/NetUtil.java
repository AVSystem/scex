package com.avsystem.scex.util.function;

import com.avsystem.scex.presentation.annotation.Documentation;

public interface NetUtil {
    boolean isIpInSubnet(String ip, String subnetWithMask);

    boolean isIpInSubnetWithMask(String ip, String subnet, String mask);

    String networkAddress(String ip, String mask);

    String networkAddress(String subnetWithMask);

    @Documentation("return network mask from CIDR in /XX")
    String getMask(String cidr);

    @Documentation("return minimal host address for given network and mask")
    String getMinAddress(String ip, String mask);

    @Documentation("return maximal host address for given network and mask")
    String getMaxAddress(String ip, String mask);

    String stripMac(String mac);

    Integer compareIp(String ip1, String ip2);

    @Documentation("check if string is ipv4 address")
    boolean isIp(String ip);

    @Documentation("check if object is ipv4 address")
    public boolean isIp(Object ip);

    boolean isIps(String ips);

    boolean isMac(String mac);

}
