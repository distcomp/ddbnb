#!/bin/bash
erl -pa ebin -detached -name `cat registry-node` -boot start_sasl \
    -s dcbc_app -config registry.config #-dcbc working_mode registry
