% Row or Column proportions
function [y] = prop(varargin)
    p=inputParser;
    p.addRequired('x', @isnumeric);
    addParameter(p, 'dim', 2);
    addParameter(p, 'cum', false);
    parse(p, varargin{:});
    %
    [m, n] = size(p.Results.x);
    if(p.Results.dim==2)
        if(p.Results.cum)
            y = cumsum(p.Results.x, 2)./(sum(p.Results.x, 2)*ones([1, n]));
        else
            y = p.Results.x ./(sum(p.Results.x, 2)*ones([1, n]));
        end
    elseif(p.Results.dim==1)
        if(p.Results.cum)
            y = cumsum(p.Results.x, 1)./(ones([m, 1])*sum(p.Results.x, 1));
        else
            y = p.Results.x ./(ones([m, 1])*sum(p.Results.x, 1));
        end
    end
end