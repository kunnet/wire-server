{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module V23 (migration) where

import Cassandra.Schema
import Text.RawString.QQ

migration :: Migration
migration = Migration 23 "Create team_billing table" $
    schema' [r|
        CREATE TABLE team_billing (
            team    uuid PRIMARY KEY,
            email   text
        ) WITH compaction = {'class': 'org.apache.cassandra.db.compaction.LeveledCompactionStrategy'}
            AND gc_grace_seconds = 864000;
        |]
