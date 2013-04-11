% Trains a neural network over the collected dataset from all frames.
% This dataset was tuned to contain the same number of nomal patterns
% and abnormal patterns, such tunning prevent the bias towards the most
% frequent (normal) patterns of the network.

trn_full = load('../third_party/goodtrn.norm.ssv');
trn = (trn_full(:, 1:16))';
trn_out = (trn_full(:, 17));
diag = zeros(2, 200);
diag(1, find(trn_out)) = 1;
diag(2, find(trn_out-1)) = 1;

net = feedforwardnet(45, 'trainrp');
net.layers{1}.transferFcn = 'logsig';
net.trainParam.epochs = 12000;
net.trainParam.goal = 1e-6;
net.trainParam.max_fail = 120;
%net.trainParam.showWindow = false;
net.trainParam.showCommandLine = false;
netc = configure(net, trn, diag);
neti = init(netc);
[nett, tr] = train(neti, trn, diag);

