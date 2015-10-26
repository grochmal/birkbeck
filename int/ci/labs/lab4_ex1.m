p = [-1 -1 2 2; 0 5 0 5];
t = [-1 -1 1 1];
net = newff(minmax(p), [3 1], {'tansig' 'purelin'}, 'traingdm');
net.trainParam.epochs = 30;
net.trainParam.lr = 0.3;
net.trainParam.mc = 0.6;
net = train(net, p, t);
y = sim(net, p);
[t; y]
