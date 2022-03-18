FROM hseeberger/scala-sbt:11.0.10_1.5.2_2.13.6 AS build
ENV LTO_LOG_LEVEL="INFO"
ENV LTO_HEAP_SIZE="2g"

WORKDIR /usr/src

COPY . .

RUN git status
RUN test -f target/lto-public-all-*.jar || sbt build

RUN test -f genesis.conf || cp genesis.example.conf genesis.conf
RUN java -cp target/lto-public-all-*.jar com.ltonetwork.GenesisBlockGenerator genesis.conf lto-custom.conf \
  && echo 'include "local.conf"' >> lto-custom.conf


FROM openjdk:11-jre-slim
ENV LTO_LOG_LEVEL="INFO"
ENV LTO_HEAP_SIZE="2g"
ENV LTO_CONFIG_FILE="/lto/configs/lto-config.conf"

RUN apt-get -qq update -y

# Install python
RUN apt-get -q install -y python3 python3-pip curl \
  && ln -s /usr/bin/python3 python \
  && pip3 install -q --upgrade pip
RUN pip3 install requests pyhocon pywaves==0.8.19 tqdm

# Setup cron job for fee vote
RUN apt-get -q install -y cron
RUN echo '0 * * * * python /lto-node/fee-vote.py /lto/fee-vote' > /etc/cron.d/fee-vote \
  && chmod 0644 /etc/cron.d/fee-vote \
  && crontab /etc/cron.d/fee-vote

COPY starter.py /lto-node/
COPY fee-vote.py /lto-node/
COPY entrypoint.sh /lto-node/
COPY --from=build /usr/src/target/lto-public-all-*.jar /lto-node/lto-public-all.jar
COPY lto-*.conf /lto-node/
COPY --from=build /usr/src/lto-custom.conf /lto-node/

VOLUME /lto
EXPOSE 6869 6868 6863
ENTRYPOINT ["/lto-node/entrypoint.sh"]

