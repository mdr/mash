package com.github.mdr.mash.ns.http

import java.security.SecureRandom
import java.security.cert.X509Certificate
import javax.net.ssl._

import org.apache.http.conn.ssl.X509HostnameVerifier

object InsecureSsl {

  object TrustAllTrustManager extends X509TrustManager {

    override def getAcceptedIssuers: Array[X509Certificate] = null

    override def checkClientTrusted(x509Certificates: Array[X509Certificate], s: String) {}

    override def checkServerTrusted(x509Certificates: Array[X509Certificate], s: String) {}

  }

  object TrustAllHostnameVerifier extends HostnameVerifier {
    override def verify(s: String, sslSession: SSLSession): Boolean = true
  }

  def makeInsecureSslContext(): SSLContext = {
    val sslContext = SSLContext.getInstance("TLS")
    sslContext.init(null, Array(TrustAllTrustManager), new SecureRandom)
    HttpsURLConnection.setDefaultHostnameVerifier(TrustAllHostnameVerifier)
    SSLContext.setDefault(sslContext)
    HttpsURLConnection.setDefaultSSLSocketFactory(sslContext.getSocketFactory)
    sslContext
  }

}