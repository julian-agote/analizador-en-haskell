function r = calcN(i,k,t,X)
%bspline oinarri funtzioa
%   Detailed explanation goes here
if (k>1) 
    r=(t-X(i))*calcN(i,k-1,t,X)/(X(i+k-1)-X(i))+((X(i+k)-t)*calcN(i+1,k-1,t,X))/(X(i+k)-X(i+1));
elseif (t>=X(i) && t<=X(i+1))
    r=1;
else
    r=0;
end    
end

