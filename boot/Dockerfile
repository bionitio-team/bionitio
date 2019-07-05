FROM python:3.7.3-stretch
# Install the bootstrap script into /app
WORKDIR /app
COPY bionitio-boot.sh .
# Run the bootstrap script in the /out directory, which ought to be mounted (-v) by the docker run command
WORKDIR /out
ENTRYPOINT ["/app/bionitio-boot.sh"]
