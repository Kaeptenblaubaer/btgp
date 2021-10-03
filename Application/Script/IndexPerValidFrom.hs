#!/usr/bin/env run-script
module Application.Script.IndexPerValidFrom where
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE OverloadedStrings #-}

import Application.Script.Prelude
import Application.Model.CanVersion
import Application.Model.Persistence.Partner
import IHP.Log as Log
import IHP.Pagination.Types as PT
-- 

run :: Script 
run = do
    today <- today
    now <- getCurrentTime
    countP <- countStatesByValidFromMaxTxn HistorytypePartner today now
    let o :: PT.Options = defaultPaginationOptions
        p :: PT.Pagination = Pagination
                {
                    currentPage = 1
                ,   totalItems = countP
                ,   pageSize = 3
                ,   window = windowSize o
                }
        showPage = \pag -> Log.info $ "Page " ++ show (currentPage pag)
    pStates :: ([PartnerState],pagination) <- selectStatesByValidFromMaxTxn HistorytypePartner today now o p
    showPage $ snd pStates
    forEach pStates \s -> do
        Log.info $ "pState= " ++ show s

    pStates :: ([PartnerState],pagination) <- selectStatesByValidFromMaxTxn HistorytypePartner today now o $ snd pStates
    showPage $ snd pStates
    forEach pStates \s -> do
        Log.info $ "pState= " ++ show s

    pStates :: ([PartnerState],pagination) <- selectStatesByValidFromMaxTxn HistorytypePartner today now o $ snd pStates
    showPage $ snd pStates
    forEach pStates \s -> do
        Log.info $ "pState= " ++ show s
    
    