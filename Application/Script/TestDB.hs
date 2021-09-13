#!/usr/bin/env run-script
module Application.Script.TestDB where
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedStrings #-}

import Application.Helper.CanVersion
import Application.Helper.WorkflowProgress
import Application.Script.Prelude
import IHP.Log as Log

run :: Script
run = do
    cs0 @ContractState <- newRecord
    cs <- query @ContractState |> fetchOne 
    Log.info $  "huhu" ++ show cs