function N=bspline_oinarri_funtzio_per(k,n,t,inter)
X=0:n+k;
j=2;
N=zeros(n+1,k+1);
h=(n-k)/inter;
N(k,1)=1;
while (j<=k+1)
    for i=1:n
        N(i,j)=(((k+t*h)-X(i))*N(i,j-1))/(X(i+j-1)-X(i))+((X(j+i)-(k+t*h))*N(i+1,j-1))/(X(i+j)-X(i+1));
    end
    j=j+1;
end
end    