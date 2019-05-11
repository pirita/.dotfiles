#!/usr/bin/env bash
#
# Generates metals-emacs binary in `/usr/local/bin`

coursier=/tmp/coursier
metalsV="0.5.2"

# Get coursier latest version
curl -L -o $coursier https://git.io/coursier
chmod +x $coursier

# Generate `metals-emacs` executable
$coursier bootstrap \
  --java-opt -XX:+UseG1GC \
  --java-opt -XX:+UseStringDeduplication  \
  --java-opt -Xss4m \
  --java-opt -Xms1G \
  --java-opt -Xmx4G  \
  --java-opt -Dmetals.client=emacs \
  --java-opt -Dmetals.http=true \
  org.scalameta:metals_2.12:$metalsV \
  -r sonatype:releases \
  -o /usr/local/bin/metals-emacs -f
