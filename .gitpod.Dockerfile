FROM gitpod/workspace-full

# Install Haskell
RUN curl -sSL https://get.haskellstack.org/ | sh

# Install PostgreSQL
RUN sudo apt-get update && sudo apt-get install -y postgresql postgresql-contrib

# Start PostgreSQL service
RUN sudo service postgresql start

# Create a PostgreSQL user and database
RUN sudo -u postgres createuser -s gitpod
RUN sudo -u postgres createdb thrift_crowd_staking
