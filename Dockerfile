FROM hseeberger/scala-sbt:11.0.10_1.5.2_2.13.6 AS build
ENV LTO_LOG_LEVEL="INFO"
ENV LTO_HEAP_SIZE="2g"

WORKDIR /usr/src

COPY . .

RUN test -f target/lto-public-all-*.jar || sbt packageAll -Dnetwork=mainnet


FROM openjdk:11-jre-slim
ENV LTO_LOG_LEVEL="INFO"
ENV LTO_HEAP_SIZE="2g"
ENV LTO_CONFIG_FILE="/lto/configs/lto-config.conf"

# Install python
RUN apt-get update -y && apt-get install -y python3 \
    python3-pip curl \
  && ln -s /usr/bin/python3 python \
  && pip3 install --upgrade pip

RUN pip3 install requests pyhocon pywaves==0.8.19 tqdm

COPY starter.py /lto-node/
COPY entrypoint.sh /lto-node/
COPY --from=build /usr/src/target/lto-public-all-*.jar /lto-node/lto-public-all.jar
COPY lto-*.conf /lto-node/

VOLUME /lto
EXPOSE 6869 6868 6863
ENTRYPOINT ["/lto-node/entrypoint.sh"]

