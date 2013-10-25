#!/bin/bash
ERL_LIBS=.. erl -name `cat registry-node` -boot start_sasl -s dcbc_app -dcbc working_mode registry
