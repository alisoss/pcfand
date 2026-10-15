# FPC cross-compiler for x86_64-win64: compiler, RTL and crt (used by tools/win-build.sh)
FROM debian:bookworm
RUN apt-get update -qq \
 && DEBIAN_FRONTEND=noninteractive apt-get install -y -qq --no-install-recommends \
      fpc binutils-mingw-w64-x86-64 make curl ca-certificates \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /tmp
RUN curl -fsSL --retry 3 -o src.tar.gz \
      https://downloads.sourceforge.net/project/freepascal/Source/3.2.2/fpc-3.2.2.source.tar.gz \
 && tar xzf src.tar.gz && rm src.tar.gz
WORKDIR /tmp/fpc-3.2.2
# not crossall/install: their package stage needs __libc_csu_init, gone in glibc 2.34
RUN make compiler_cycle CPU_TARGET=x86_64 OS_TARGET=win64 BINUTILSPREFIX=x86_64-w64-mingw32- \
 && make rtl_all        CPU_TARGET=x86_64 OS_TARGET=win64 BINUTILSPREFIX=x86_64-w64-mingw32-
RUN mkdir -p /opt/fpcwin/bin /opt/fpcwin/units/x86_64-win64/rtl \
 && cp compiler/ppcrossx64 /opt/fpcwin/bin/ \
 && cp rtl/units/x86_64-win64/*.ppu rtl/units/x86_64-win64/*.o \
       rtl/units/x86_64-win64/*.a /opt/fpcwin/units/x86_64-win64/rtl/
RUN mkdir -p /opt/fpcwin/units/x86_64-win64/rtl-console \
 && /opt/fpcwin/bin/ppcrossx64 -Twin64 -Px86_64 -O2 -Xs -n \
      -Fu/opt/fpcwin/units/x86_64-win64/rtl \
      -Fi/tmp/fpc-3.2.2/packages/rtl-console/src/win \
      -Fi/tmp/fpc-3.2.2/packages/rtl-console/src/inc \
      -FE/opt/fpcwin/units/x86_64-win64/rtl-console \
      -FU/opt/fpcwin/units/x86_64-win64/rtl-console \
      /tmp/fpc-3.2.2/packages/rtl-console/src/win/crt.pp
ENV PATH=/opt/fpcwin/bin:$PATH
WORKDIR /work
