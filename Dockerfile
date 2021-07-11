FROM migamake/stack-build-image:15.3 as server-build
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update
RUN apt-get install -y libgmp10 libc6
RUN mkdir -p /build2
WORKDIR /build2
ADD stack.yaml   ./
ADD package.yaml ./
ADD README.md    ./
ADD LICENSE      ./
ADD Setup.hs     ./
ADD src          ./src
ADD app          ./app
ADD test         ./test
RUN stack build myproj:exe:myproj-exe --copy-bins --local-bin-path=/usr/bin

FROM ubuntu:rolling AS server-deployment
ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8
RUN apt-get update; apt-get install -y libgmp10 libc6; apt-get upgrade -y; apt-get clean
COPY --from=server-build /usr/bin/myproj-exe /usr/bin/myproj-exe
RUN mkdir /workdir
WORKDIR /workdir
EXPOSE 8080
ENTRYPOINT ["/usr/bin/myproj-exe"]

