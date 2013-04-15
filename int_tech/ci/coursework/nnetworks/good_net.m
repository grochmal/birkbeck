% Trains a neural network over the collected dataset from all frames.
% This dataset was tuned to contain the same number of normal patterns
% and abnormal patterns, such tunning prevent the bias of the network
% towards the most frequent (normal) patterns.

clear all;  % for GNU Octave it shall be 'clear -all'

function hdlst = iterate_nets(nfiles, trns, dgtrns, tst, dgtst)
    hdlst = 5:5:55;
    for h = hdlst
	fprintf('Run nets for frame %i with %i hidden nodes\n', nfiles, h);
	res = run_experiments(nfiles, h, trns, dgtrns, tst, dgtst);
    end
end

function [trns, dgtrns, tst, dgtst] = load_data(nfiles)
    tstfl  = [];
    trnfls = [];
    path   = '../third_party/';
    trnfls(:,:,7) = load(strcat(path, 'good_trn.norm.ssv'));
    for i = nfiles
	tstfl = [tstfl; load(strcat(path, int2str(i), 'tst.norm.ssv'))];
    end
    tst    = tstfl(:,1:16)';
    outtst = tstfl(:,17)';
    dgtst  = [];
    dgtst(1, find(outtst)  )  = 1;
    dgtst(2, find(outtst-1))  = 1;
    trns   = [];
    trns(:,:,7) = trnfls(:,1:16,7)';
    out         = trnfls(:,17,7)';
    dgtrns = [];
    dgtrns(1, find(out),   7) = 1;
    dgtrns(2, find(out-1), 7) = 1;
end

load_files = 1:6;
[trns, dgtrns, tst, dgtst] = load_data(load_files);
hdlst = iterate_nets(7, trns, dgtrns, tst, dgtst);

