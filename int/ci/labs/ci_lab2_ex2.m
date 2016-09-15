X = [0 1; 0 1];
clusters = 8;
points = 10;
std_dev =0.05;
P = nngenc(X, clusters, points, std_dev);
plot(P(1,:), P(2,:), 'r+', 'color', 'yellow');
title('Input Vectors');
xlabel('p(1)');
ylabel('p(2)');
net = newc(X, clusters, 0.1);
w = net.IW{1};
hold on;
plot(w(:,1), w(:,2), 'ob', 'color', 'green');
net.trainParam.epochs = 7;
nett1 = train(net, P);
w = nett1.IW{1};
hold on;
plot(w(:,1), w(:,2), 'xb', 'color', 'red');
net.trainParam.epochs = 25;
nett2 = train(net, P);
w = nett2.IW{1};
hold on;
plot(w(:,1), w(:,2), 'xb', 'color', 'blue');