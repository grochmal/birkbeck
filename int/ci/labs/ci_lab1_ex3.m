p = [.1 .8 .1 .9; .2 .9 .1 .8];
net = competlayer(2);
net.trainParam.epochs = 500;
nett = train(net, p);
neta = adapt(net, p);
%for i=1:500
%    neta = adapt(neta, p);
%end
uns_t = sim(nett, p);
uns_a = sim(neta, p);
uns_tc = vec2ind(uns_t);
uns_ac = vec2ind(uns_a);
