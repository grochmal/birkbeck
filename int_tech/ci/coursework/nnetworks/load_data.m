% Get the data files

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

