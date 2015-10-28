x = simplecluster_dataset;
net = selforgmap([8 8]);
net.trainParam.epochs = 2000
nett = train(net, x);
y = nett(x);
classes = vec2ind(y);
