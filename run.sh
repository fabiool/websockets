#!/bin/bash
stack clean --full && \
stack build && \
stack exec websockets-exe -- -v --trace-dir=/home/fabiool/Projects/haskell/option-bench/tmp --host=earth.fabiool.eu --port=6455 -i 1 -m 5 -d 1000000 -w 5000000 -f 1
