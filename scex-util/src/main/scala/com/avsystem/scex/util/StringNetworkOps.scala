package com.avsystem.scex.util

import java.net.{InetAddress, UnknownHostException}

import com.avsystem.scex.util.function.NetFunctions
import com.avsystem.scex.presentation.annotation.Documentation

final class StringNetworkOps(private val wrapped: String) extends AnyVal {

  @Documentation("Returns true if the string is an IPv4 address which belongs to the subnet provided as the `subnetWithMask` argument. Use CIDR notation for the argument, e.g. `10.8.0.0/16`.")
  def isIpInSubnet(subnetWithMask: String): Boolean =
    NetFunctions.isIpInSubnet(wrapped, subnetWithMask)

  @Documentation("Returns true if the string is an IPv4 address which belongs to the subnet defined by the `subnet` and 'mask' arguments. Use dot-decimal notation for the arguments.")
  def isIpInSubnetWithMask(subnet: String, mask: String): Boolean =
    NetFunctions.isIpInSubnetWithMask(wrapped, subnet, mask)

  @Documentation("Returns the MAC address with any separating characters removed and letters changed to uppercase.")
  def stripMac: String = NetFunctions.stripMac(wrapped)
  @Documentation("Returns true if the string is a valid IP address.")
  def isIp: Boolean = NetFunctions.isIp(wrapped)
  @Documentation("Returns true if the string is a comma-separated list of valid IP addresses.")
  def isIps: Boolean = NetFunctions.isIps(wrapped)
  @Documentation("Returns true if the string is a valid hardware address.")
  def isMac: Boolean = NetFunctions.isMac(wrapped)

  @Documentation("Returns 0 if the addresses provided are equal, -1 if `ip1` < `ip2` or 1 if `ip1` > `ip2`.")
  def compareAsIpTo(ip: String): Integer = try {
    val adr1 = InetAddress.getByName(wrapped)
    val adr2 = InetAddress.getByName(ip)
    val inetAddressComparator = new NetFunctions.InetAddressComparator
    inetAddressComparator.compare(adr1, adr2)
  } catch {
    case _: UnknownHostException => null
  }
}
