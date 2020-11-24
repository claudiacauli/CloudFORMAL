FROM    maven:3.6.0-jdk-8

ENV SCALA_VERSION=2.13.1
ENV SBT_VERSION=1.2.7

COPY pom.xml /usr/local/CloudFORMAL/pom.xml
COPY src /usr/local/CloudFORMAL/src

WORKDIR /usr/local/CloudFORMAL

RUN wget http://www.scala-lang.org/files/archive/scala-${SCALA_VERSION}.deb
RUN wget http://dl.bintray.com/sbt/debian/sbt-${SBT_VERSION}.deb
RUN dpkg -i scala-${SCALA_VERSION}.deb
RUN dpkg -i sbt-${SBT_VERSION}.deb
RUN apt-get update
RUN apt-get install -y scala sbt
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* /*.deb

RUN mvn clean scala:compile
RUN mvn package

CMD ["java","-jar","target/ExecCloudFORMAL.jar"]
