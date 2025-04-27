module Vex.CLI.Color (red, blue, reset) where

red :: String
red = "\x1b[31m"

blue :: String
blue = "\x1b[34m"

reset :: String
reset = "\x1b[0m"
