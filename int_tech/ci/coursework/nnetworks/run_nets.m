% Script running the coursework experiments,
% all networks are created, tranined and finally evaluated.

trn1_full = load('../third_party/2trn.norm.ssv');
trn1 = (trn1_full(:, 1:16))';
trn1_out = (trn1_full(:,17));
diag = zeros(2,200);
diag(1, find(trn1_out)) = 1;
diag(2, find(trn1_out-1)) = 1;

net = feedforwardnet(45, 'trainrp');
net.layers{1}.transferFcn = 'logsig';
net.trainParam.epochs = 12000;
net.trainParam.goal = 1e-6;
net.trainParam.max_fail = 120;
net.trainParam.showWindow = false;
net.trainParam.showCommandLine = false;
netc = configure(net, trn1, diag);
neti = init(netc);
[nett, tr] = train(neti, trn1, diag);

% this must be changed to cmply wih the coursework
output = round(nett(trn1));
right = zeros(1, 300);
for i = 1:300
  if min(output(:,i) == diag(:,i))
    right(i) = 1;
  end
end

