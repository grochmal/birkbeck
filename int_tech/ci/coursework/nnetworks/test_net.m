% Script to quickly test configuration changes to the neural network

clear;
path   = '../third_party/';
trnfl  = load(strcat(path, 'good_trn.norm.ssv'));
trn    = trnfl(:,1:16)';
trnout = trnfl(:, 17)';
dgtrn(1, find(trnout)  ) = 1;
dgtrn(2, find(trnout-1)) = 1;
tstfl  = load(strcat(path, '6tst.norm.ssv'));
tst    = tstfl(:,1:16)';
tstout = tstfl(:, 17)';
dgtst(1, find(tstout)  ) = 1;
dgtst(2, find(tstout-1)) = 1;
net = feedforwardnet(25, 'trainrp');
net.layers{1}.transferFcn = 'logsig';
net.layers{2}.transferFcn = 'logsig';
net.performFcn            = 'msereg';
net.trainParam.epochs          = 12000;
net.trainParam.goal            = 1e-6;
net.trainParam.max_fail        = 120;
net.trainParam.showWindow      = false;
net.trainParam.showCommandLine = true;
netc       = configure(net, trn, dgtrn);
neti       = init(netc);
[nett, tr] = train(neti, trn, dgtrn);
epochs        = tr.epochs;
restrn        = nett(trn);
[ctrn, Cmtrn] = confusion(dgtrn, round(restrn));
errtrn        = sum((dgtrn - restrn).^2);
restst        = nett(tst);
[ctst, Cmtst] = confusion(dgtst, round(restst));
errtst        = sum((dgtst - restst).^2);

