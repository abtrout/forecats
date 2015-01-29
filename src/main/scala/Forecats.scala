package io.forecats

import akka.actor.{Actor, ActorSystem, ActorLogging}
import akka.event.LoggingAdapter
import argonaut._, Argonaut._
import com.typesafe.config.Config
import scala.util.{Success, Failure}
import spray.routing._
import spray.http.MediaTypes.{`application/json` => JSON}
import spray.http.StatusCodes
import spray.http.HttpHeaders.RawHeader

class ForecatsActor(config: Config)(implicit system: ActorSystem) 
  extends Actor
  with ActorLogging
  with ForecatsService {

  def actorRefFactory = context
  implicit def executionContext = actorRefFactory.dispatcher

  implicit val weatherUtil = new WeatherLookup(config.getConfig("forecast"))
  implicit val catUtil = new CatLookup(config.getConfig("redis"))

  def receive = runRoute(
    get {
      respondWithHeader(RawHeader("Access-Control-Allow-Origin", "*")) {
        weatherRequest ~
        catRequest
      }
    }
  )
}

trait ForecatsService extends HttpService {

  import scala.concurrent.ExecutionContext.Implicits.global
  import DataTypes.Forecast

  def log: LoggingAdapter
  
  def validCoordinates(lat: Double, lng: Double) =
    (-90 <= lat && lat <= 90) && (-180 <= lng && lng <= 180)

  def weatherRequest(implicit weatherUtil: WeatherLookup) =
    path("weather" / DoubleNumber ~ "," ~ DoubleNumber) { (lat, lng) =>
      validate(validCoordinates(lat, lng), s"Invalid coordinates") {
        onComplete(weatherUtil.getWeather(lat, lng)) {
          case Success(forecast) => respondWithMediaType(JSON) {
            complete(forecast.asJson.toString)
          }
          case Failure(ex) =>
            log error s"failed to getWeather for coordinates ($lat, $lng): ${ex.getMessage}"
            complete(StatusCodes.InternalServerError)
        }
      }
    }

  def catRequest(implicit catUtil: CatLookup) =
    path("cats" / "random") {
      onComplete(catUtil.getRandom) {
        case Success(cat) => complete(cat)
        case Failure(ex) =>
          log error s"random cat query failed: ${ex.getMessage}"
          complete(StatusCodes.NotFound)
      }
    }
}
