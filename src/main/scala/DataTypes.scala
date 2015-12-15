package io.forecats

import spray.http.HttpResponse
import spray.httpx.unmarshalling.{FromResponseUnmarshaller, MalformedContent}
import argonaut._, Argonaut._

object DataTypes extends WeatherTypes {

  implicit def currentWeatherCodec =
    casecodec4(CurrentWeather.apply, CurrentWeather.unapply)(
      "icon",
      "summary",
      "temperature",
      "apparentTemperature")

  implicit def dailyWeatherCodec =
    casecodec7(DailyWeather.apply, DailyWeather.unapply)(
      "time",
      "icon",
      "summary",
      "temperatureMin",
      "temperatureMinTime",
      "temperatureMax",
      "temperatureMaxTime")

  implicit def hourlyWeatherCodec =
    casecodec4(HourlyWeather.apply, HourlyWeather.unapply)(
      "time",
      "icon",
      "summary",
      "temperature")
  
  case class Forecast(
    currently: CurrentWeather,
    hourly: List[HourlyWeather],
    daily: List[DailyWeather])

  implicit def forecastDecoder = DecodeJson[Forecast] { c =>
    def parseHourly(xs: List[HourlyWeather]) =
      xs.zipWithIndex.filter(_._2 % 4 == 0).map(_._1).take(5)

    for {
      currently <- (c --\ "currently").as[CurrentWeather]
      hourly <- (c --\ "hourly" --\ "data").as[List[HourlyWeather]]
      daily <- (c --\ "daily" --\ "data").as[List[DailyWeather]]
    } yield Forecast(currently, parseHourly(hourly), daily.take(5))
  }

  implicit def forecastEncoder = EncodeJson[Forecast](f =>
    ("currently" := f.currently) ->:
    ("hourly" := f.hourly) ->:
    ("daily" := f.daily) ->:
    jEmptyObject)

  implicit val forecastUnmarshaller = new FromResponseUnmarshaller[Forecast] {
    def apply(response: HttpResponse) = {
      Parse.decodeOption[Forecast](response.entity.asString) match {
        case Some(forecast) => Right(forecast)
        case None => Left(MalformedContent("Failed to unmarshal Forecast"))
      }
    }
  }
}

trait WeatherTypes {
  case class CurrentWeather(
    icon: Option[String],
    summary: Option[String],
    temperature: Option[Int],
    apparentTemperature: Option[Int]
  )

  case class HourlyWeather(
    time: Long,
    icon: Option[String],
    summary: Option[String],
    temperature: Option[Int]
  )

  case class DailyWeather(
    time: Long,
    icon: Option[String],
    summary: Option[String],
    temperatureMin: Option[Int],
    temperatureMinTime: Option[Long],
    temperatureMax: Option[Int],
    temperatureMaxTime: Option[Long]
  )
}
