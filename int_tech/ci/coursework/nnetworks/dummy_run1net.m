% Package with a dummy NN Toolbox function,
% allows testing the code without the NN Toolbox.

function [ epochs,         ...
	   errtrn, errtst, ...
	   ctrn,   ctst,   ...
	   Cmtrn,  Cmtst   ] = dummy_run1net(nhidden, trn, dgtrn, tst, dgtst)
    epochs = 30   + 0.001*nhidden;
    errtrn = 0.03 + 0.001*nhidden;
    errtst = 0.04 + 0.001*nhidden;
    ctrn   = 0.03 + 0.001*nhidden;
    ctst   = 0.18 + 0.001*nhidden;
    Cmtrn  = [1 2; 3 4] + 0.001*nhidden;
    Cmtst  = [5 6; 7 8] + 0.001*nhidden;
end

