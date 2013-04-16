% Script to quickly test configuration changes to the neural network

clear;
tm = cputime;
path   = '../third_party/';
trnfl  = load(strcat(path, 'good_trn.norm.ssv'));
trn    = trnfl(:, 1:16)';
trnout = trnfl(:, 17)';
dgtrn(1, find(trnout)  ) = 1;
dgtrn(2, find(trnout-1)) = 1;
tstfl  = load(strcat(path, '6tst.norm.ssv'));
tst    = tstfl(:, 1:16)';
tstout = tstfl(:, 17)';
dgtst(1, find(tstout)  ) = 1;
dgtst(2, find(tstout-1)) = 1;
net = feedforwardnet(25, 'trainrp');
net.layers{1}.transferFcn = 'logsig';
net.layers{2}.transferFcn = 'tansig';
net.performFcn            = 'msereg';
net.trainParam.epochs          = 12000;
net.trainParam.goal            = 1e-6;
net.trainParam.max_fail        = 120;
net.trainParam.showWindow      = false;
net.trainParam.showCommandLine = true;
net.trainParam.show            = 60;
netc = configure(net, trn, dgtrn);
neti = init(netc);
neti.inputs{1}.processParams{2}.ymin  =  0;
neti.outputs{2}.processParams{2}.ymin = -1;
[nett, tr]    = train(neti, trn, dgtrn);
epochs        = tr.num_epochs;
restrn        = nett(trn);
[ctrn, Cmtrn] = confusion(dgtrn, round(restrn));
errtrn        = sum(sum((dgtrn - restrn).^2));
restst        = nett(tst);
[ctst, Cmtst] = confusion(dgtst, round(restst));
errtst        = sum(sum((dgtst - restst).^2));
etm = cputime - tm;

