package com.avsystem.scex.util

import java.net.{InetAddress, UnknownHostException}

import com.avsystem.scex.util.function.NetFunctions

final class StringNetworkOps(private val wrapped: String) extends AnyVal {
  def isIpInSubnet(subnetWithMask: String): Boolean =
    NetFunctions.isIpInSubnet(wrapped, subnetWithMask)

  def isIpInSubnetWithMask(subnet: String, mask: String): Boolean =
    NetFunctions.isIpInSubnetWithMask(wrapped, subnet, mask)

  def stripMac: String = NetFunctions.stripMac(wrapped)
  def isIp: Boolean = NetFunctions.isIp(wrapped)
  def isIps: Boolean = NetFunctions.isIps(wrapped)
  def isMac: Boolean = NetFunctions.isMac(wrapped)

  def compareAsIpTo(ip: String): Integer = try {
    val adr1 = InetAddress.getByName(wrapped)
    val adr2 = InetAddress.getByName(ip)
    val inetAddressComparator = new NetFunctions.InetAddressComparator
    inetAddressComparator.compare(adr1, adr2)
  } catch {
    case _: UnknownHostException => null
  }
}
