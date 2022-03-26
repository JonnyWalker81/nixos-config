#!/usr/bin/env bash

# curl -s "wttr.in/91106?format=j1" | jq '.current_condition[0].FeelsLikeF' | sed -e 's|["'\'']||g'
# curl -s "wttr.in/91106?format=j1" | jq --r '.current_condition[0].FeelsLikeF'
curl -s "wttr.in/91106?format=j1" | jq -r '.current_condition[0] | "\(.FeelsLikeF)\u00B0F, \(.weatherDesc[0].value)"'
