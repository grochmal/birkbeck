% Package encapsulating the use of the NN Toolbox,
% this allows the code to be tested without the toolbox.

function [ epochs,         ...
	   errtrn, errtst, ...
	   ctrn,   ctst,   ...
	   Cmtrn,  Cmtst   ] = run1net(nhidden, trn, dgtrn, tst, dgtst)
    net = feedforwardnet(nhidden, 'trainrp');
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
    [nett, tr] = train(neti, trn, dgtrn);
    epochs        = tr.num_epochs;
    restrn        = nett(trn);
    [ctrn, Cmtrn] = confusion(dgtrn, round(restrn));
    errtrn        = sum(sum((dgtrn - restrn).^2));
    restst        = nett(tst);
    [ctst, Cmtst] = confusion(dgtst, round(restst));
    errtst        = sum(sum((dgtst - restst).^2));
end

