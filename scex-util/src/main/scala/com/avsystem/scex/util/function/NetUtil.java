package com.avsystem.scex.util.function;

import com.avsystem.scex.presentation.annotation.Documentation;
import com.avsystem.scex.presentation.annotation.ParameterNames;

public interface NetUtil {
    @ParameterNames({ "ip", "subnetWithMask" })
    boolean isIpInSubnet(String ip, String subnetWithMask);

    @ParameterNames({ "ip", "subnet", "mask" })
    boolean isIpInSubnetWithMask(String ip, String subnet, String mask);

    @ParameterNames({ "ip", "mask" })
    String networkAddress(String ip, String mask);

    @ParameterNames("mac")
    String stripMac(String mac);

    @ParameterNames({ "ip1", "ip2" })
    Integer compareIp(String ip1, String ip2);

    @ParameterNames("ip")
    @Documentation("check if string is ipv4 address")
    boolean isIp(String ip);

    @ParameterNames("ip")
    @Documentation("check if object is ipv4 address")
    public boolean isIp(Object ip);

    @ParameterNames("ips")
    boolean isIps(String ips);

    @ParameterNames("mac")
    boolean isMac(String mac);

}
