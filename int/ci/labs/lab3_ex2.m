%P = [-0.5 -0.5 0.3 -0.1 -0.8; -0.5 0.5 -0.5 1 0];  % impossible data set
P = [-0.5 -0.5 0.3 -0.1 -0.8; -0.5 0.2 -0.5 1 1];
T  = [1 1 0 0 0];
plotpv(P, T);
net = newp([-2 2; -2 2], 1);
plotpv(P, T);
linehandle = plotpc(net.IW{1}, net.b{1});
for a=1:25
   [net Y E] = adapt(net, P , T);
   linehandle = plotpc(net.IW{1}, net.b{1}, linehandle);
   drawnow;
end
