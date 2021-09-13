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
    ContractState :: cs <- query @ContractState |> fetchOne 
    putStrLn "huhu"