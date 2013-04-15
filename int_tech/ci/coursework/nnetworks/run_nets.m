% Script running the coursework experiments,
% all networks are created, tranined and finally evaluated.

clear all;  % for GNU Octave it shall be 'clear -all'

function hdlst = iterate_nets(nfiles, trns, dgtrns, tst, dgtst)
    hdlst = 5:5:55;
    for i = nfiles
	for h = hdlst
	    fprintf('Run nets for frame %i with %i hidden nodes\n', i, h);
	    res = run_experiments(i, h, trns, dgtrns, tst, dgtst);
	end
    end
end

function [trns, dgtrns, tst, dgtst] = load_data(nfiles)
    tstfl  = [];
    trnfls = [];
    path   = '../third_party/';
    for i = nfiles
	trnfls(:,:,i) = load(strcat(path, int2str(i), 'trn.norm.ssv'));
	tstfl = [tstfl; load(strcat(path, int2str(i), 'tst.norm.ssv'))];
    end
    tst    = tstfl(:,1:16)';
    outtst = tstfl(:,17)';
    dgtst  = [];
    dgtst(1, find(outtst)  ) = 1;
    dgtst(2, find(outtst-1)) = 1;
    trns   = [];
    dgtrns = [];
    for i = nfiles
	trns(:,:,i) = trnfls(:,1:16,i)';
	out         = trnfls(:,17,i)';
	dgtrns(1, find(out),   i) = 1;
	dgtrns(2, find(out-1), i) = 1;
    end
end

load_files = 1:6;
[trns, dgtrns, tst, dgtst] = load_data(load_files);
run_files  = 1:6;  %  in case of few failed experiments change this
hdlst = iterate_nets(run_files, trns, dgtrns, tst, dgtst);

