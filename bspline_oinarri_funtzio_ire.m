function N=bspline_oinarri_funtzio_ire(k,n,t,inter)
X=zeros(1,n+k);
for i=k+1:n
	X(i)=i-k;
end
for i=n+1:n+k
	X(i)=n-k+1;
end	
N=zeros(n+1,k);
h=(n-k+2)/inter;
i=1;
while(i<n+k)
	if((t>=X(i))&&(t<X(i+1)))
		N(i,1)=1;
		break
	end
	i=i+1;
end	
j=2;
while (j<=k)
    for i=1:n
        N(i,j)=calN(i,j,N,X,k,h,n,t);
    end
    j=j+1;
end
end    

function val=calN(l,z,N,X,k,h,n,t)
if (z==1)
    val=N(l,z);
else
	r1 = calN(l,z-1,N,X,k,h,n,t);
    if(l<n)
        r2 = calN(l+1,z-1,N,X,k,h,n,t);
    else
        r2 = 0;
    end;    
	val = 0;
    if(r1~=0) 
        val=(((t*h)-X(l))*r1)/(X(l+z-1)-X(l));
    end
	if(r2~=0)
        val=val+((X(z+l)-(t*h))*r2)/(X(l+z)-X(l+1));
    end
end
end	