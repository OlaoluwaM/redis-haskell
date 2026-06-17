#!/usr/bin/env bash
set -e

if [[ -n "$SOURCE_DATE_EPOCH" ]]; then
	BUILD_ID=$(date -u -d "@$SOURCE_DATE_EPOCH" +%s 2>/dev/null || date -u -r "$SOURCE_DATE_EPOCH" +%s 2>/dev/null || date -u +%s)
else
	BUILD_ID=$(uname -n)"-"$(date +%s)
fi

echo "$BUILD_ID"
