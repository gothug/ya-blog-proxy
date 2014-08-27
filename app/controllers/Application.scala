package controllers

import play.api._
import play.api.mvc._
import play.api.libs.ws._
import play.api.Play.current
import play.api.Logger
import play.api.libs.json._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util._

import java.net.URI

object Application extends Controller {

  val yandex_api_url               = "http://blogs.yandex.ru/search.rss"
  val api_query_timeout            = 15 seconds
  val max_concurrent_http_requests = 2

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def search(keywords: List[String]) =
    Action {request =>
      {
        val requests = genApiRequests(keywords)

        val extractedLinks =
          executeRequestBatches(
            requests.grouped(max_concurrent_http_requests).toList
          ).flatten

        val domains = extractedLinks.toSet.toList.map(getDomainFromUrl)

        val domainStat = domains.groupBy(identity).mapValues(_.length)

        val response = Json.prettyPrint(Json.toJson(domainStat))

        Ok(response)
      }
    }

  type apiQueryResults = List[List[String]]

  def executeRequestBatches(
    requestBatches: List[List[WSRequestHolder]]
  ): apiQueryResults = {
    def executeAndAccumulateResult(
      requestBatches: List[List[WSRequestHolder]],
      links: apiQueryResults
    ): apiQueryResults =
      requestBatches match {
        case List()   => links
        case x :: xs  => executeAndAccumulateResult(xs,
                          executeRequestBatch(x) ::: links)
      }

    executeAndAccumulateResult(requestBatches, List())
  }

  def genApiRequests(keywords: List[String]) =
    keywords.map(kw => WS.url(yandex_api_url + "?text=" + kw))

  def executeRequestBatch(requestBatch: List[WSRequestHolder]) = {
    val batchRequestFutures =
      requestBatch.map(_.get.map{r => writeLog(r.status)
                                      parseRss(r.xml).toList})

    val batchRequestTryFutures = batchRequestFutures.map(futureToFutureTry)

    val requests = Future.sequence(batchRequestTryFutures)

    val parsedResults = requests.map(_.collect{case Success(x) => x})

    Await.result(parsedResults, api_query_timeout).toList
  }

  def parseRss(xml: scala.xml.Elem) =
    (xml \ "channel" \ "item" \ "link").map(l => l.text).take(10)

  def getDomainFromUrl(url: String) =
    URI.create(url).getHost.split('.').toList.reverse.take(2).reverse.
      mkString(".")

  def futureToFutureTry[T](f: Future[T]): Future[Try[T]] =
    f.map(Success(_)).recover({case e => Failure(e)})

  def writeLog(status: Int) = {
      val cur_time = System.currentTimeMillis / 1000
      val msg = List("processing response from get request",
        "response code: " + status,
        "timestamp: " + cur_time).mkString(", ")
      Logger.info(msg)
    }
}
