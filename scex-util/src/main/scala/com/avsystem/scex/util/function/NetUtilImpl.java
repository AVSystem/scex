package com.avsystem.scex.util.function;

import org.apache.commons.net.util.SubnetUtils;

import java.net.InetAddress;
import java.net.UnknownHostException;

public class NetUtilImpl implements NetUtil {
    public static final NetUtilImpl INSTANCE = new NetUtilImpl();

    private NetUtilImpl() {
    }

    protected boolean isIpAndMask(String ip, String mask) {
        return isIp(ip) && isIp(mask);
    }

    @Override
    public boolean isIpInSubnet(String ip, String subnetWithMask) {
        return NetFunctions.isIpInSubnet(ip, subnetWithMask);
    }

    @Override
    public boolean isIpInSubnetWithMask(String ip, String subnet, String mask) {
        return NetFunctions.isIpInSubnetWithMask(ip, subnet, mask);
    }

    @Override
    public String stripMac(String mac) {
        return NetFunctions.stripMac(mac);
    }

    @Override
    public boolean isIp(String ip) {
        return NetFunctions.isIp(ip);
    }

    @Override
    public boolean isIp(Object ip) {
        return isIp(String.valueOf(ip));
    }

    @Override
    public boolean isIps(String ips) {
        return NetFunctions.isIps(ips);
    }

    @Override
    public boolean isMac(String mac) {
        return NetFunctions.isMac(mac);
    }

    @Override
    public String networkAddress(String ip, String mask) {
        if (isIpAndMask(ip, mask)) {
            return new SubnetUtils(ip, mask).getInfo().getNetworkAddress();
        }
        return null;
    }

    @Override
    public String getMask(String cidr) {
        if (!cidr.startsWith("/"))
            cidr = "/" + cidr;
        if (cidr.matches("\\/\\d{1,2}")) {
            return new SubnetUtils("0.0.0.0" + cidr).getInfo().getNetmask();
        }
        return null;
    }

    @Override
    public String getMinAddress(String ip, String mask) {
        if (isIpAndMask(ip, mask)) {
            return new SubnetUtils(ip, mask).getInfo().getLowAddress();
        }
        return null;
    }

    @Override
    public String getMaxAddress(String ip, String mask) {
        if (isIpAndMask(ip, mask)) {
            return new SubnetUtils(ip, mask).getInfo().getHighAddress();
        }
        return null;
    }

    @Override
    public String networkAddress(String subnetWithMask) {
        return new SubnetUtils(subnetWithMask).getInfo().getHighAddress();
    }

    @Override
    public Integer compareIp(String ip1, String ip2) {
        try {
            InetAddress adr1 = InetAddress.getByName(ip1);
            InetAddress adr2 = InetAddress.getByName(ip2);
            NetFunctions.InetAddressComparator inetAddressComparator = new NetFunctions.InetAddressComparator();
            return inetAddressComparator.compare(adr1, adr2);
        } catch (UnknownHostException e) {
            return null;
        }
    }
}
