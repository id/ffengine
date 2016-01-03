#!/bin/sh
psql -c "create role ffengine with login password 'ffengine'"; -U postgres
psql -c "create database ffengine owner ffengine;" -U postgres
psql -c "grant all on database ffengine to ffengine;" -U postgres
echo "localhost:5432:ffengine:ffengine:ffengine" >> ~/.pgpass
chmod 0600 ~/.pgpass
psql -d ffengine -h localhost -U ffengine -f schema.sql
psql -d ffengine -h localhost -U ffengine -f functions.sql

