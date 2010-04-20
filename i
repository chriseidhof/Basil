#/bin/sh
ghci Example3.hs -W -hide-all-packages -package base -package containers -package fclabels -package Takusen -package regular -package monads-fd -package transformers -package HDBC -package HDBC-sqlite3 -package template-haskell -ddump-splices
