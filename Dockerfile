FROM rocker/tidyverse:3.3.1

RUN apt-get update \
&& apt-get install -y --no-install-recommends \
    bzip2 \
    libreadline-dev \
    libjpeg-dev \
    libpng-dev \
&& apt-get clean \
&& rm -rf /var/lib/apt/lists/*
RUN git clone https://github.com/sstephenson/rbenv.git /opt/rbenv \
&& git clone https://github.com/sstephenson/ruby-build.git /opt/rbenv/plugins/ruby-build \
&& /opt/rbenv/plugins/ruby-build/install.sh
ENV PATH /opt/rbenv/bin:/opt/rbenv/shims:$PATH
ENV RBENV_ROOT /opt/rbenv
RUN echo 'export RBENV_ROOT="/opt/rbenv"' >> /etc/profile \
&& echo 'export PATH="${RBENV_ROOT}/bin:${PATH}"' >> /etc/profile \
&& echo 'eval "$(rbenv init -)"' >> /etc/profile.d/rbenv.sh \
&& echo 'eval "$(rbenv init -)"' >> /etc/profile \
&& sh /etc/profile.d/rbenv.sh \
&& rbenv install 2.2.2 \
&& rbenv global 2.2.2 \
&& echo 'gem: --no-document' >> ~/.gemrc && cp ~/.gemrc /etc/gemrc && chmod uog+r /etc/gemrc \
&& gem update --system 2.7.8 \
&& gem source -a http://dream.misasa.okayama-u.ac.jp/rubygems/ \
&& gem install casteml

RUN install2.r --error \
    --deps TRUE \
    RcppRoll \
    jpeg \
    png \
&& installGithub.r \
    --deps TRUE \
    misasa/MedusaRClient

WORKDIR /tmp
COPY ./ chelyabinsk/
RUN R CMD build chelyabinsk
RUN R CMD INSTALL chelyabinsk_*.tar.gz
