FROM rocker/shiny:4.3.0

ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libpq-dev \
    libxml2-dev \
    cmake \ 
    && rm -rf /var/lib/apt/lists/*


RUN addgroup --system shiny \
    && adduser --system --home /home/app --ingroup shiny shiny

# Set the working directory to /home/app
WORKDIR /home/app

# Copy renv stuff
COPY --chown=shiny:shiny .Rprofile renv.lock .renvignore dependencies.R ./
COPY --chown=shiny:shiny renv/activate.R renv/

# Ensure the 'shiny' user has appropriate permissions to install packages
# TODO try to remove permissions at the end of the script
RUN chown -R shiny:shiny /home/app

# Install R dependencies
RUN sudo -u shiny Rscript -e 'renv::restore(clean=TRUE)'

# Copy app
COPY --chown=shiny:shiny app.R ./
COPY --chown=shiny:shiny config.yml ./
COPY --chown=shiny:shiny rhino.yml ./
COPY --chown=shiny:shiny app app/

USER shiny 

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp(host='0.0.0.0', port=3838)"]
