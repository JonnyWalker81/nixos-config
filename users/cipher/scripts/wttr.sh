#!/usr/bin/env bash

curl -s "wttr.in/91106?format=j1" | jq '.current_condition[0].FeelsLikeF' | sed -e 's|["'\'']||g'
