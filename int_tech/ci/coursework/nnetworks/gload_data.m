% load_data version for good_net.m

function [trns, dgtrns, tst, dgtst] = gload_data(nfiles)
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

