% Generic output package for both run_nets.m and good_net.m

function path = save_results(results, type, filenum, hneurons)
    path     = '../results/';
    filename = sprintf('%s%s%i_%02i.out', path, type, filenum, hneurons);
    save(filename, '-ascii', 'results');
end

