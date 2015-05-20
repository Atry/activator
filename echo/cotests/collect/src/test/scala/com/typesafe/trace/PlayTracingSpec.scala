/**
 *  Copyright (C) 2011-2013 Typesafe, Inc <http://typesafe.com>
 */

package com.typesafe.trace

import com.typesafe.trace.test.EchoCollectSpec
import scala.concurrent.duration._


class Play23Scala210NettyGetTracingSpec extends EchoCollectSpec {
  "Play Netty (GETs only)" must {
    "GET /get" in {
      eventCheck(expected = 56) {
        // allEvents.toSeq.sortBy(_.nanoTime).foreach(println)
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "GET /getWithSession" in {
      eventCheck(expected = 112) {
        // printTraces
        countTraces should be(2)
        countEventsOf[NettyHttpReceivedStart.type] should be(2)
        countEventsOf[NettyHttpReceivedEnd.type] should be(2)
        countEventsOf[NettyPlayReceivedStart.type] should be(2)
        countEventsOf[NettyPlayReceivedEnd.type] should be(2)
        countEventsOf[ActionResolved] should be(2)
        countEventsOf[ActionRouteRequest] should be(2)
        countEventsOf[ActionInvoked] should be(2)
        countEventsOf[ActionSimpleResult] should be(2)
        countEventsOf[NettyResponseHeader] should be(2)
        countEventsOf[NettyResponseBody] should be(2)
        countEventsOf[NettyReadBytes] should be(2)
        countEventsOf[ActionResultGenerationStart.type] should be(2)
        countEventsOf[ActionResultGenerationEnd.type] should be(2)
        eventsOf[ActionInvoked].exists(_.annotation.asInstanceOf[ActionInvoked].invocationInfo.session == Some(Map("key" -> "value"))) should be(true)
      }
    }
    "GET /get/sync/10" in {
      eventCheck(expected = 56, timeout = timeoutHandler.finiteTimeoutify(10.seconds)) {
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "GET /get/async/100 -- 100ms delay" in {
      eventCheck(expected = 56) {
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[ActionAsyncResult.type] should be(0) // ActionAsyncResult not valid for Play 2.3
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "GET /get/async_immediate -- no delay" in {
      eventCheck(expected = 56) {
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[ActionAsyncResult.type] should be(0) // ActionAsyncResult not valid for Play 2.3
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "GET /get/chunked/10" in {
      eventCheck(expected = 746, timeout = timeoutHandler.finiteTimeoutify(10.seconds)) {
        // printTraces
        // Unable to get stable data out of this
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionChunkedResult] should be(1)
        countEventsOf[IterateeFolded] should be(241)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyWriteChunk] should be(11)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "GET /get/large" in {
      eventCheck(expected = 56, timeout = timeoutHandler.finiteTimeoutify(10.seconds)) {
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[IterateeFolded] should be(15)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "GET /no-handler" in {
      eventCheck(expected = 56) {
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionHandlerNotFound] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[IterateeFolded] should be(15)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "GET /error" in {
      eventCheck(expected = 61, timeout = timeoutHandler.finiteTimeoutify(10.seconds)) {
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionError] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[IterateeFolded] should be(17)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "properly trace bad requests GET /get/sync/abcd" in {
      eventCheck(expected = 56) {
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionBadRequest] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[IterateeFolded] should be(15)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
  }
}

class Play23Scala210NettyPostTracingSpec extends EchoCollectSpec {
  "Play Netty (POSTs only)" must {
    "POST /post" in {
      eventCheck(expected = 213) {
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "POST /post (BIG - should be converted by Netty to a chunked POST)" in {
      eventOneOfCheck[Int](expectedOptions = Map(771 -> 9, 770 -> 9, 769 -> 9), timeout = timeoutHandler.finiteTimeoutify(10.seconds)) { readBytesCount ⇒
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(readBytesCount)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "(chunked) POST /post" in {
      eventOneOfCheck[Int](expectedOptions = Map(227 -> 2, 228 -> 3, 229 -> 4, 230 -> 5, 231 -> 6), timeout = timeoutHandler.finiteTimeoutify(10.seconds)) { readBytesCount ⇒
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(readBytesCount)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "(file upload) POST /uploadFile" in {
      eventOneOfCheck[Int](expectedOptions = Map(864 -> 5, 865 -> 6, 866 -> 7, 867 -> 8), timeout = timeoutHandler.finiteTimeoutify(10.seconds)) { readBytesCount ⇒
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(readBytesCount)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "BAD (file upload) POST /uploadFile" in {
      eventOneOfCheck[Int](expectedOptions = Map(77 -> 1, 78 -> 2, 79 -> 3, 80 -> 4, 81 -> 5, 82 -> 6), timeout = timeoutHandler.finiteTimeoutify(10.seconds)) { readBytesCount ⇒
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(readBytesCount)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
  }
}

class Play23Scala210IterateeTracingSpec extends EchoCollectSpec {
  "Iteratee tracing" must {
    "trace iteratees" in {
      eventCheck(expected = 24) {
        // printTraces()
        // countTraces should be(1)
        countEventsOf[IterateeFolded] should be(7)
      }
    }
    "create Iteratee instances with IterateeInfo" in {
      eventCheck(expected = 1) {
        // printTraces()
        // countTraces should be(1)
        countEventsOf[IterateeCreated] should be(1)
      }
    }
  }
}

class Play23Scala210NettySamplingTracingSpec extends EchoCollectSpec {
  "Play trace sampling" must {
    "produce a sample in-line with sampling rate" in {
      eventCheck(expected = 280) {
        // printTraces
        countTraces should be(5)
        countEventsOf[NettyPlayReceivedStart.type] should be(5)
        countEventsOf[NettyPlayReceivedEnd.type] should be(5)
        countEventsOf[ActionResolved] should be(5)
        countEventsOf[ActionRouteRequest] should be(5)
        countEventsOf[ActionInvoked] should be(5)
        countEventsOf[ActionSimpleResult] should be(5)
        countEventsOf[NettyResponseHeader] should be(5)
        countEventsOf[NettyResponseBody] should be(5)
        countEventsOf[ActionResultGenerationStart.type] should be(5)
        countEventsOf[ActionResultGenerationEnd.type] should be(5)
        countEventsOf[NettyHttpReceivedStart.type] should be(5)
        countEventsOf[NettyReadBytes] should be(5)
        countEventsOf[NettyHttpReceivedEnd.type] should be(5)
      }
    }
    "not produce a useful trace when tracing disabled for a URI" in {
      eventCheck(expected = 0) {
        // allEvents.toSeq.sortBy(_.nanoTime).foreach(println)
        // printTraces
        countTraces should be(0)
      }
    }
  }
}

class Play23Scala211NettyGetTracingSpec extends EchoCollectSpec {
  "Play Netty (GETs only)" must {
    "GET /get" in {
      eventCheck(expected = 56) {
        // allEvents.toSeq.sortBy(_.nanoTime).foreach(println)
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "GET /getWithSession" in {
      eventCheck(expected = 112) {
        // printTraces
        countTraces should be(2)
        countEventsOf[NettyHttpReceivedStart.type] should be(2)
        countEventsOf[NettyHttpReceivedEnd.type] should be(2)
        countEventsOf[NettyPlayReceivedStart.type] should be(2)
        countEventsOf[NettyPlayReceivedEnd.type] should be(2)
        countEventsOf[ActionResolved] should be(2)
        countEventsOf[ActionRouteRequest] should be(2)
        countEventsOf[ActionInvoked] should be(2)
        countEventsOf[ActionSimpleResult] should be(2)
        countEventsOf[NettyResponseHeader] should be(2)
        countEventsOf[NettyResponseBody] should be(2)
        countEventsOf[NettyReadBytes] should be(2)
        countEventsOf[ActionResultGenerationStart.type] should be(2)
        countEventsOf[ActionResultGenerationEnd.type] should be(2)
        eventsOf[ActionInvoked].exists(_.annotation.asInstanceOf[ActionInvoked].invocationInfo.session == Some(Map("key" -> "value"))) should be(true)
      }
    }
    "GET /get/sync/10" in {
      eventCheck(expected = 56, timeout = timeoutHandler.finiteTimeoutify(10.seconds)) {
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "GET /get/async/100 -- 100ms delay" in {
      eventCheck(expected = 56) {
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[ActionAsyncResult.type] should be(0) // ActionAsyncResult not valid for Play 2.3
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "GET /get/async_immediate -- no delay" in {
      eventCheck(expected = 56) {
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[ActionAsyncResult.type] should be(0) // ActionAsyncResult not valid for Play 2.3
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "GET /get/chunked/10" in {
      eventCheck(expected = 746, timeout = timeoutHandler.finiteTimeoutify(10.seconds)) {
        // printTraces
        // Unable to get stable data out of this
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionChunkedResult] should be(1)
        countEventsOf[IterateeFolded] should be(241)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyWriteChunk] should be(11)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "GET /get/large" in {
      eventCheck(expected = 56, timeout = timeoutHandler.finiteTimeoutify(10.seconds)) {
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[IterateeFolded] should be(15)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "GET /no-handler" in {
      eventCheck(expected = 56) {
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionHandlerNotFound] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[IterateeFolded] should be(15)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "GET /error" in {
      eventCheck(expected = 61, timeout = timeoutHandler.finiteTimeoutify(10.seconds)) {
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionError] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[IterateeFolded] should be(17)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "properly trace bad requests GET /get/sync/abcd" in {
      eventCheck(expected = 56) {
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionBadRequest] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[IterateeFolded] should be(15)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
  }
}

class Play23Scala211NettyPostTracingSpec extends EchoCollectSpec {
  "Play Netty (POSTs only)" must {
    "POST /post" in {
      eventCheck(expected = 213) {
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(1)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "POST /post (BIG - should be converted by Netty to a chunked POST)" in {
      eventCheck(expected = 771, timeout = timeoutHandler.finiteTimeoutify(10.seconds)) {
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(9)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "(chunked) POST /post" in {
      eventOneOfCheck[Int](expectedOptions = Map(227 -> 2, 228 -> 3, 229 -> 4, 230 -> 5, 231 -> 6), timeout = timeoutHandler.finiteTimeoutify(10.seconds)) { readBytesCount ⇒
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(readBytesCount)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "(file upload) POST /uploadFile" in {
      eventOneOfCheck[Int](expectedOptions = Map(864 -> 5, 865 -> 6, 866 -> 7, 867 -> 8), timeout = timeoutHandler.finiteTimeoutify(10.seconds)) { readBytesCount ⇒
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(readBytesCount)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
    "BAD (file upload) POST /uploadFile" in {
      eventOneOfCheck[Int](expectedOptions = Map(77 -> 1, 78 -> 2, 79 -> 3, 80 -> 4, 81 -> 5, 82 -> 6), timeout = timeoutHandler.finiteTimeoutify(10.seconds)) { readBytesCount ⇒
        // printTraces
        countTraces should be(1)
        countEventsOf[NettyHttpReceivedStart.type] should be(1)
        countEventsOf[NettyHttpReceivedEnd.type] should be(1)
        countEventsOf[NettyPlayReceivedStart.type] should be(1)
        countEventsOf[NettyPlayReceivedEnd.type] should be(1)
        countEventsOf[ActionResolved] should be(1)
        countEventsOf[ActionRouteRequest] should be(1)
        countEventsOf[ActionInvoked] should be(1)
        countEventsOf[ActionSimpleResult] should be(1)
        countEventsOf[NettyResponseHeader] should be(1)
        countEventsOf[NettyResponseBody] should be(1)
        countEventsOf[NettyReadBytes] should be(readBytesCount)
        countEventsOf[ActionResultGenerationStart.type] should be(1)
        countEventsOf[ActionResultGenerationEnd.type] should be(1)
      }
    }
  }
}

class Play23Scala211IterateeTracingSpec extends EchoCollectSpec {
  "Iteratee tracing" must {
    "trace iteratees" in {
      eventCheck(expected = 24) {
        // printTraces()
        // countTraces should be(1)
        countEventsOf[IterateeFolded] should be(7)
      }
    }
    "create Iteratee instances with IterateeInfo" in {
      eventCheck(expected = 1) {
        // printTraces()
        // countTraces should be(1)
        countEventsOf[IterateeCreated] should be(1)
      }
    }
  }
}

class Play23Scala211NettySamplingTracingSpec extends EchoCollectSpec {
  "Play trace sampling" must {
    "produce a sample in-line with sampling rate" in {
      eventCheck(expected = 280) {
        // printTraces
        countTraces should be(5)
        countEventsOf[NettyPlayReceivedStart.type] should be(5)
        countEventsOf[NettyPlayReceivedEnd.type] should be(5)
        countEventsOf[ActionResolved] should be(5)
        countEventsOf[ActionRouteRequest] should be(5)
        countEventsOf[ActionInvoked] should be(5)
        countEventsOf[ActionSimpleResult] should be(5)
        countEventsOf[NettyResponseHeader] should be(5)
        countEventsOf[NettyResponseBody] should be(5)
        countEventsOf[ActionResultGenerationStart.type] should be(5)
        countEventsOf[ActionResultGenerationEnd.type] should be(5)
        countEventsOf[NettyHttpReceivedStart.type] should be(5)
        countEventsOf[NettyReadBytes] should be(5)
        countEventsOf[NettyHttpReceivedEnd.type] should be(5)
      }
    }
    "not produce a useful trace when tracing disabled for a URI" in {
      eventCheck(expected = 0) {
        // allEvents.toSeq.sortBy(_.nanoTime).foreach(println)
        // printTraces
        countTraces should be(0)
      }
    }
  }
}
