% Trains a neural network over the collected dataset from all frames.
% This dataset was tuned to contain the same number of normal patterns
% and abnormal patterns, such tunning prevent the bias of the network
% towards the most frequent (normal) patterns.

clear all;  % for GNU Octave it shall be 'clear -all'

load_files = 1:6;
[trns, dgtrns, tst, dgtst] = gload_data(load_files);
hdlst = giterate_nets(7, trns, dgtrns, tst, dgtst);

