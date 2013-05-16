#!/usr/bin/env sh

s3cmd -c ~/.s3cfg.real-s3-personal-account sync -P _site/* s3://reiddraper.com
