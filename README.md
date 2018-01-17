# chousen-server

[![CircleCI](https://circleci.com/gh/RawToast/chousen-server.svg?style=svg&circle-token=f705bd2bb3a6ac38bc7cb58e0d9964f7545c76a2)](https://circleci.com/gh/RawToast/chousen-server) [![Codacy Badge](https://api.codacy.com/project/badge/Grade/ad8c474ac887461fb3d1785cc76888bd)](https://www.codacy.com?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=RawToast/chousen-server&amp;utm_campaign=Badge_Grade)
[![codecov](https://codecov.io/gh/RawToast/chousen-server/branch/master/graph/badge.svg?token=7wvPNCJNvX)](https://codecov.io/gh/RawToast/chousen-server)

Simple rougelike / card game built as a restful API using immutable data, optics, and lots of recursion.

I started working on this side project whilst learning Scala to explore various libraries and patterns. As a result,
this codebase is not in a consistent or tidy state.


## Game Server

To start a local server on port 8080: sbt run

## Dev interface

The Twirl based basic developer web interface was removed from this project, which now only contains the backend server. 
The [chousen-frontend](https://github.com/RawToast/chousen-frontend) project serves as an example frontend.

Latest development frontend on [Heroku](https://bit.ly/chousen)


## Libraries

Built in Scala with the following opensource libraries:

* [Http4s](https://github.com/http4s/http4s)
* [Cats](https://github.com/typelevel/cats)
* [Monocle](https://github.com/julien-truffaut/monocle)
* [Circe](https://github.com/circe/circe)
