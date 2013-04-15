% Generic package to run a set of 100 networks,
% used in both run_nets.m and good_net.m

function res = run_experiments(nfile, hidden, trns, dgtrns, tst, dgtst)
    net_restrn = [];
    net_restst = [];
    for j = 1:100
	fprintf('    run net no. %i\n', j);
%	[ epochs,         \
%	  errtrn, errtst, \
%	  ctrn,   ctst,   \
%	  Cmtrn,  Cmtst   ] = run1net(hidden, trns(:,:,nfile), \
%				      dgtrns(:,:,nfile), tst, dgtst);
	[ epochs,         \
	  errtrn, errtst, \
	  ctrn,   ctst,   \
	  Cmtrn,  Cmtst   ] = dummy_run1net(hidden, trns(:,:,nfile), \
					    dgtrns(:,:,nfile), tst, dgtst);
	net_restrn = [net_restrn; Cmtrn(1,1) Cmtrn(1,2) Cmtrn(2,2) \
				  Cmtrn(2,1) ctrn epochs errtrn    ];
	net_restst = [net_restst; Cmtst(1,1) Cmtst(1,2) Cmtst(2,2) \
				  Cmtst(2,1) ctst       errtst     ];
    end
    save_results(net_restrn, 'trn', nfile, hidden);
    save_results(net_restst, 'tst', nfile, hidden);
    res = [net_restrn net_restst];
end

