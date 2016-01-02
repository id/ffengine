#!/bin/sh
cat db/roles.sql | sudo su - postgres -c "psql"
echo "localhost:5432:ffengine:ffengine:ffengine" >> ~/.pgpass
chmod 0600 ~/.pgpass
psql -d ffengine -h localhost -U ffengine -f db/schema.sql
psql -d ffengine -h localhost -U ffengine -f db/functions.sql

