FROM gitpod/workspace-full

# Install Haskell
RUN curl -sSL https://get.haskellstack.org/ | sh

# Install Node.js 16
RUN curl -fsSL https://deb.nodesource.com/setup_16.x | sudo -E bash -
RUN sudo apt-get install -y nodejs

# Install PostgreSQL
RUN sudo apt-get update && sudo apt-get install -y postgresql postgresql-contrib

# Start PostgreSQL service
RUN sudo service postgresql start

# Create a PostgreSQL user and database
RUN sudo -u postgres createuser -s gitpod
RUN sudo -u postgres createdb thrift_crowd_staking

# Install any other dependencies your project might need
# For example:
# RUN stack install servant servant-server
