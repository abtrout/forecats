package io.forecats

import spray.http.HttpResponse
import spray.httpx.unmarshalling.{FromResponseUnmarshaller, MalformedContent}
import argonaut._, Argonaut._

import scala.util.Try
import java.util.TimeZone

object DataTypes extends WeatherTypes {

  implicit def currentWeatherCodec =
    casecodec4(CurrentWeather.apply, CurrentWeather.unapply)(
      "icon",
      "summary",
      "temperature",
      "apparentTemperature"
    )

  implicit def dailyWeatherCodec =
    casecodec7(DailyWeather.apply, DailyWeather.unapply)(
      "time",
      "icon",
      "summary",
      "temperatureMin",
      "temperatureMinTime",
      "temperatureMax",
      "temperatureMaxTime"
    )

  implicit def hourlyWeatherCodec =
    casecodec4(HourlyWeather.apply, HourlyWeather.unapply)(
      "time",
      "icon",
      "summary",
      "temperature"
    )
  
  case class Forecast(
    offset: Long,
    currently: CurrentWeather,
    hourly: List[HourlyWeather],
    daily: List[DailyWeather]
  )

  implicit def forecastDecoder = DecodeJson[Forecast](c => {

    def parseHourly(xs: List[HourlyWeather]) =
      xs.zipWithIndex.filter(_._2 % 4 == 0).map(_._1).take(5)

    def offsetForTimezone(tz: String): Long =
      Try(TimeZone.getTimeZone(tz).getOffset(System.currentTimeMillis)).toOption.getOrElse(0).toLong

    for {
      timezone <- (c --\ "timezone").as[String]
      offset = offsetForTimezone(timezone)
      currently <- (c --\ "currently").as[CurrentWeather]
      hourly <- (c --\ "hourly" --\ "data").as[List[HourlyWeather]]
      daily <- (c --\ "daily" --\ "data").as[List[DailyWeather]]
    } yield Forecast(offset, currently, parseHourly(hourly), daily.take(5))
  })

  implicit def forecastEncoder = EncodeJson[Forecast](f =>
    ("currently" := f.currently) ->:
    ("hourly" := f.hourly) ->:
    ("daily" := f.daily) ->:
    ("offset" := f.offset) ->:
    jEmptyObject
  )

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
    icon: String,
    summary: String,
    temperature: Int,
    apparentTemperature: Int
  )

  case class HourlyWeather(
    time: Long,
    icon: String,
    summary: String,
    temperature: Int
  )

  case class DailyWeather(
    time: Long,
    icon: String,
    summary: String,
    temperatureMin: Int,
    temperatureMinTime: Long,
    temperatureMax: Int,
    temperatureMaxTime: Long
  )
}
