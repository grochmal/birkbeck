% Script running the coursework experiments,
% all networks are created, trained and finally evaluated.

clear all;  % for GNU Octave it shall be 'clear -all'

load_files = 1:6;
[trns, dgtrns, tst, dgtst] = load_data(load_files);
run_files  = 1:6;  %  in case of few failed experiments change this
hdlst = iterate_nets(run_files, trns, dgtrns, tst, dgtst);

