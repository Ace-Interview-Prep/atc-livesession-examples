{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.cabal-install
    pkgs.postgresql
    pkgs.zlib
  ];

  shellHook = ''
    echo "Setting up PostgreSQL..."

    # Ensure an unprivileged user initializes and runs PostgreSQL
    if [ "$USER" = "root" ]; then
      echo "You are running as root. Please use a regular user for nix-shell."
      exit 1
    fi

    # Initialize the database if not already done
    if [ ! -d ./pgdata ]; then
      echo "Initializing PostgreSQL data directory..."
      mkdir -p ./pgdata
      initdb -D ./pgdata
      mkdir -p ./pgdata/run   
    fi

    # Ensure the data directory is owned by the current user
    chown -R $(whoami) ./pgdata
    chmod -R $(whoami) ./pgdata/run

    # Start PostgreSQL on port 5432
    echo "Starting PostgreSQL on port 5432..."
    pg_ctl start -D ./pgdata -o "-p 5432 -k $(pwd)/pgdata/run" -l ./pgdata/postgresql.log

    # Wait for PostgreSQL to start
    echo "Waiting for PostgreSQL to be ready..."
    until pg_isready -h localhost -p 5432; do
      sleep 1
    done

    echo "Checking if role 'postgres' exists..."
    if ! psql -h localhost -p 5432 -U postgres -tAc "SELECT 1 FROM pg_roles WHERE rolname='postgres';" | grep -q 1; then
      echo "Creating role 'postgres'..."
      postgres --single -D ./pgdata template1 <<< "CREATE ROLE postgres WITH SUPERUSER LOGIN PASSWORD 'password';"
    fi

    echo "Checking if database 'dbserialization' exists..."
    if ! psql -h localhost -p 5432 -U postgres -tAc "SELECT 1 FROM pg_database WHERE datname='dbserialization';" | grep -q 1; then
      echo "Creating database 'dbserialization'..."
      psql -h localhost -p 5432 -U postgres -c "CREATE DATABASE dbserialization;"
    fi

    echo "PostgreSQL setup complete!"
  '';
}