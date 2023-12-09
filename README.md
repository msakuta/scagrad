# scagrad

An experimental autograd in Scala


## Overview

They say Scala is a language with both OOP and functional aspects and feels like Rust.
So let's give a try and implement autograd.

## Examples

* Sine wave ($\sin(x^2)$)

![Sine function plot](images/scagrad-sin.png)

* Gaussian

![Gaussian function plot](images/scagrad-gauss.png)

* Higher order autograd

![Higher order autograd plot](images/scagrad-higher.png)


## How to build and run

I don't want to contaminate my system with Scala and its build tools, so I used docker.
Nowadays even [Windows](https://docs.docker.com/desktop/install/windows-install/) can run Docker via WSL2, so you don't really need to install in a platform dependent manner.

Use [docker-compose.yml](docker-compose.yml) file to run the Scala server

```
docker-compose up -d
```

Exec a shell script inside the container

```
docker exec -it scagrad-scala-1 bash
```

Change directory to the working directory, build and run

```
cd /work
scalac src/main/scala/example/Hello.scala
scala scala src/main/scala/example/Hello.scala > output.txt
```

Or you could use [sbt](https://www.scala-sbt.org/)

```
# sbt
sbt:scagrad> run
```

### Other means

[Scastie](https://scastie.scala-lang.org/) is supposed to be a playground that you can try Scala on the web browser, but it's dead.
