#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

emacs --batch -Q \
	-l ert \
	-l "${ROOT_DIR}/tests/emacs/orglife-config-tests.el" \
	-f ert-run-tests-batch-and-exit
