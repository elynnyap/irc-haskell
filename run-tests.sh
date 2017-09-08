#!/bin/bash

python3 -m pytest tests/chirc/tests/test_connection.py --chirc-exe=.cabal-sandbox/bin/chat-server-exe --randomize-ports --json=tests/report.json --html=tests/report.html 
