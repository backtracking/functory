#!/bin/sh
./network_test.opt -stop -w & 
./network_test.opt -local

sleep 2
./network_test.opt -stop -w -test map & 
./network_test.opt -local -test map

sleep 2
./network_test.opt -stop -w -test map_local_fold & 
./network_test.opt -local -test map_local_fold

sleep 2
./network_test.opt -stop -w -test map_remote_fold & 
./network_test.opt -local -test map_remote_fold

sleep 2
./network_test.opt -stop -w -test map_fold_ac & 
./network_test.opt -local -test map_fold_ac

sleep 2
./network_test.opt -stop -w -test map_fold_a & 
./network_test.opt -local -test map_fold_a





