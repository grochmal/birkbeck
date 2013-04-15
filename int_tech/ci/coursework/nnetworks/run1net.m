% Package encapsulating the use of the NN Toolbox,
% this allows the code to be tested without the toolbox.

function [ epochs,         \
	   errtrn, errtst, \
	   ctrn,   ctst,   \
	   Cmtrn,  Cmtst   ] = run1net(nhidden, trn, dgtrn, tst, dgtst)
    net = feedforwardnet(nhidden, 'trainrp');
    net.layers{1}.transferFcn = 'logsig';
    net.layers{2}.transferFcn = 'logsig';
    net.performFcn            = 'msereg';
    net.trainParam.epochs          = 12000;
    net.trainParam.goal            = 1e-6;
    net.trainParam.max_fail        = 120;
    net.trainParam.showWindow      = false;
    net.trainParam.showCommandLine = false;
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
end

