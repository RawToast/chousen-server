# chousen-server

[![CircleCI](https://circleci.com/gh/RawToast/chousen-server.svg?style=svg&circle-token=f705bd2bb3a6ac38bc7cb58e0d9964f7545c76a2)](https://circleci.com/gh/RawToast/chousen-server) [![Codacy Badge](https://api.codacy.com/project/badge/Grade/ad8c474ac887461fb3d1785cc76888bd)](https://www.codacy.com?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=RawToast/chousen-server&amp;utm_campaign=Badge_Grade)
[![codecov](https://codecov.io/gh/RawToast/chousen-server/branch/master/graph/badge.svg?token=7wvPNCJNvX)](https://codecov.io/gh/RawToast/chousen-server)

Simple rougelike game built as a simlpe website and an API using immutable data, type classes, optics, and recursion.

## Game Server

To start a local server on port 8080: sbt run

## Dev interface

The basic developer web interface will be available at http://localhost:8080

[Heroku Server](https://immense-bastion-74506.herokuapp.com)

## Libraries

Built in Scala with the following opensource libraries:

* [Http4s](https://github.com/http4s/http4s)
* [Cats](https://github.com/typelevel/cats)
* [Monocle](https://github.com/julien-truffaut/monocle)
* [Circe](https://github.com/circe/circe)
* [Twirl](https://github.com/playframework/twirl)
