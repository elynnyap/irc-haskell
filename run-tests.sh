#!/bin/bash

python3 -m pytest tests/ --chirc-exe=.cabal-sandbox/bin/chat-server-exe --randomize-ports --json=tests/report.json --html=tests/report.html 
