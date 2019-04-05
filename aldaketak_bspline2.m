X = [1 1 0 1;2 3 0 1;4 3 0 1;3 1 0 1];
% X = X * [1 0 0 0;0 cos(90) sin(90) 0;0 -sin(90) cos(90) 0;0 0 0 1 ];
k=3;n=4;
B=[X(1,1) X(1,2) X(1,3);X(2,1) X(2,2) X(2,3);X(3,1) X(3,2) X(3,3);X(4,1) X(4,2) X(4,3)];
h=(n-k+2)/(2*n);P=zeros(length(0:h:n-k+2),3);
N=bspline_oinarri_funtzio_ire(k,n,0,8);
for i=1:n
	P(1,1)=P(1,1)+N(i,k)*B(i,1);
	P(1,2)=P(1,2)+N(i,k)*B(i,2);
	P(1,3)=P(1,3)+N(i,k)*B(i,3);
end
N=bspline_oinarri_funtzio_ire(k,n,1,8);
for i=1:n
	P(2,1)=P(2,1)+N(i,k)*B(i,1);
	P(2,2)=P(2,2)+N(i,k)*B(i,2);
	P(2,3)=P(2,3)+N(i,k)*B(i,3);
end
N=bspline_oinarri_funtzio_ire(k,n,2,8);
for i=1:n
	P(3,1)=P(3,1)+N(i,k)*B(i,1);
	P(3,2)=P(3,2)+N(i,k)*B(i,2);
	P(3,3)=P(3,3)+N(i,k)*B(i,3);
end
N=bspline_oinarri_funtzio_ire(k,n,3,8);
for i=1:n
	P(4,1)=P(4,1)+N(i,k)*B(i,1);
	P(4,2)=P(4,2)+N(i,k)*B(i,2);
	P(4,3)=P(4,3)+N(i,k)*B(i,3);
end
N=bspline_oinarri_funtzio_ire(k,n,4,8);
for i=1:n
	P(5,1)=P(5,1)+N(i,k)*B(i,1);
	P(5,2)=P(5,2)+N(i,k)*B(i,2);
	P(5,3)=P(5,3)+N(i,k)*B(i,3);
end
N=bspline_oinarri_funtzio_ire(k,n,5,8);
for i=1:n
	P(6,1)=P(6,1)+N(i,k)*B(i,1);
	P(6,2)=P(6,2)+N(i,k)*B(i,2);
	P(6,3)=P(6,3)+N(i,k)*B(i,3);
end
N=bspline_oinarri_funtzio_ire(k,n,6,8);
for i=1:n
	P(7,1)=P(7,1)+N(i,k)*B(i,1);
	P(7,2)=P(7,2)+N(i,k)*B(i,2);
	P(7,3)=P(7,3)+N(i,k)*B(i,3);
end
N=bspline_oinarri_funtzio_ire(k,n,7,8);
for i=1:n
	P(8,1)=P(8,1)+N(i,k)*B(i,1);
	P(8,2)=P(8,2)+N(i,k)*B(i,2);
	P(8,3)=P(8,3)+N(i,k)*B(i,3);
end
N=bspline_oinarri_funtzio_ire(k,n,8,8);
for i=1:n
	P(9,1)=P(9,1)+N(i,k)*B(i,1);
	P(9,2)=P(9,2)+N(i,k)*B(i,2);
	P(9,3)=P(9,3)+N(i,k)*B(i,3);
end
% subplot(1,2,1);
X1=[P(1,1) P(2,1)];Y1=[P(1,2) P(2,2)];Z1=[P(1,3) P(2,3)];line(X1,Y1,Z1,'Color','red','LineStyle','--');
X1=[P(2,1) P(3,1)];Y1=[P(2,2) P(3,2)];Z1=[P(2,3) P(3,3)];line(X1,Y1,Z1,'Color','red','LineStyle','--');
X1=[P(3,1) P(4,1)];Y1=[P(3,2) P(4,2)];Z1=[P(3,3) P(4,3)];line(X1,Y1,Z1,'Color','red','LineStyle','--');
X1=[P(4,1) P(5,1)];Y1=[P(4,2) P(5,2)];Z1=[P(4,3) P(5,3)];line(X1,Y1,Z1,'Color','red','LineStyle','--');
X1=[P(5,1) P(6,1)];Y1=[P(5,2) P(6,2)];Z1=[P(5,3) P(6,3)];line(X1,Y1,Z1,'Color','red','LineStyle','--');
X1=[P(6,1) P(7,1)];Y1=[P(6,2) P(7,2)];Z1=[P(6,3) P(7,3)];line(X1,Y1,Z1,'Color','red','LineStyle','--');
X1=[P(7,1) P(8,1)];Y1=[P(7,2) P(8,2)];Z1=[P(7,3) P(8,3)];line(X1,Y1,Z1,'Color','red','LineStyle','--');
X1=[P(8,1) P(9,1)];Y1=[P(8,2) P(9,2)];Z1=[P(8,3) P(9,3)];line(X1,Y1,Z1,'Color','red','LineStyle','--');
view(3);hold on;
% xlim([min(X(:,1))-1 max(X(:,1))+1]);ylim([min(X(:,2))-1 max(X(:,2))+1]);zlim([min(X(:,3))-1 max(X(:,3))+1]);xlabel('x');ylabel('y');zlabel('z');
XP = X;
for a=0:35
	XP = XP * [1 0 0 0;0 cos(10) sin(10) 0;0 -sin(10) cos(10) 0;0 0 0 1 ];
	k=3;n=4;
	B=[XP(1,1) XP(1,2) XP(1,3);XP(2,1) XP(2,2) XP(2,3);XP(3,1) XP(3,2) XP(3,3);XP(4,1) XP(4,2) XP(4,3)];
	h=(n-k+2)/(2*n);P=zeros(length(0:h:n-k+2),3);
	N=bspline_oinarri_funtzio_ire(k,n,0,8);
	for i=1:n
		P(1,1)=P(1,1)+N(i,k)*B(i,1);
		P(1,2)=P(1,2)+N(i,k)*B(i,2);
		P(1,3)=P(1,3)+N(i,k)*B(i,3);
	end
	N=bspline_oinarri_funtzio_ire(k,n,1,8);
	for i=1:n
		P(2,1)=P(2,1)+N(i,k)*B(i,1);
		P(2,2)=P(2,2)+N(i,k)*B(i,2);
		P(2,3)=P(2,3)+N(i,k)*B(i,3);
	end
	N=bspline_oinarri_funtzio_ire(k,n,2,8);
	for i=1:n
		P(3,1)=P(3,1)+N(i,k)*B(i,1);
		P(3,2)=P(3,2)+N(i,k)*B(i,2);
		P(3,3)=P(3,3)+N(i,k)*B(i,3);
	end
	N=bspline_oinarri_funtzio_ire(k,n,3,8);
	for i=1:n
		P(4,1)=P(4,1)+N(i,k)*B(i,1);
		P(4,2)=P(4,2)+N(i,k)*B(i,2);
		P(4,3)=P(4,3)+N(i,k)*B(i,3);
	end
	N=bspline_oinarri_funtzio_ire(k,n,4,8);
	for i=1:n
		P(5,1)=P(5,1)+N(i,k)*B(i,1);
		P(5,2)=P(5,2)+N(i,k)*B(i,2);
		P(5,3)=P(5,3)+N(i,k)*B(i,3);
	end
	N=bspline_oinarri_funtzio_ire(k,n,5,8);
	for i=1:n
		P(6,1)=P(6,1)+N(i,k)*B(i,1);
		P(6,2)=P(6,2)+N(i,k)*B(i,2);
		P(6,3)=P(6,3)+N(i,k)*B(i,3);
	end
	N=bspline_oinarri_funtzio_ire(k,n,6,8);
	for i=1:n
		P(7,1)=P(7,1)+N(i,k)*B(i,1);
		P(7,2)=P(7,2)+N(i,k)*B(i,2);
		P(7,3)=P(7,3)+N(i,k)*B(i,3);
	end
	N=bspline_oinarri_funtzio_ire(k,n,7,8);
	for i=1:n
		P(8,1)=P(8,1)+N(i,k)*B(i,1);
		P(8,2)=P(8,2)+N(i,k)*B(i,2);
		P(8,3)=P(8,3)+N(i,k)*B(i,3);
	end
	N=bspline_oinarri_funtzio_ire(k,n,8,8);
	for i=1:n
		P(9,1)=P(9,1)+N(i,k)*B(i,1);
		P(9,2)=P(9,2)+N(i,k)*B(i,2);
		P(9,3)=P(9,3)+N(i,k)*B(i,3);
	end
% 	subplot(1,2,2);
    X1=[P(1,1) P(2,1)];Y1=[P(1,2) P(2,2)];Z1=[P(1,3) P(2,3)];line(X1,Y1,Z1);
X1=[P(2,1) P(3,1)];Y1=[P(2,2) P(3,2)];Z1=[P(2,3) P(3,3)];line(X1,Y1,Z1);
X1=[P(3,1) P(4,1)];Y1=[P(3,2) P(4,2)];Z1=[P(3,3) P(4,3)];line(X1,Y1,Z1);
X1=[P(4,1) P(5,1)];Y1=[P(4,2) P(5,2)];Z1=[P(4,3) P(5,3)];line(X1,Y1,Z1);
X1=[P(5,1) P(6,1)];Y1=[P(5,2) P(6,2)];Z1=[P(5,3) P(6,3)];line(X1,Y1,Z1);
X1=[P(6,1) P(7,1)];Y1=[P(6,2) P(7,2)];Z1=[P(6,3) P(7,3)];line(X1,Y1,Z1);
X1=[P(7,1) P(8,1)];Y1=[P(7,2) P(8,2)];Z1=[P(7,3) P(8,3)];line(X1,Y1,Z1);
X1=[P(8,1) P(9,1)];Y1=[P(8,2) P(9,2)];Z1=[P(8,3) P(9,3)];line(X1,Y1,Z1);
% view(3);hold on;
% 	xlim([min(XP(:,1))-1 max(XP(:,1))+1]);ylim([min(XP(:,2))-1 max(XP(:,2))+1]);zlim([min(XP(:,3))-1 max(XP(:,3))+1]);xlabel('x');ylabel('y');zlabel('z');
end
XP = XP * [1 0 0 0;0 cos(10) sin(10) 0;0 -sin(10) cos(10) 0;0 0 0 1 ]
k=3;n=4;
B=[XP(1,1) XP(1,2) XP(1,3);XP(2,1) XP(2,2) XP(2,3);XP(3,1) XP(3,2) XP(3,3);XP(4,1) XP(4,2) XP(4,3)];
h=(n-k+2)/(2*n);P=zeros(length(0:h:n-k+2),3);
N=bspline_oinarri_funtzio_ire(k,n,0,8);
for i=1:n
	P(1,1)=P(1,1)+N(i,k)*B(i,1);
	P(1,2)=P(1,2)+N(i,k)*B(i,2);
	P(1,3)=P(1,3)+N(i,k)*B(i,3);
end
N=bspline_oinarri_funtzio_ire(k,n,1,8);
for i=1:n
	P(2,1)=P(2,1)+N(i,k)*B(i,1);
	P(2,2)=P(2,2)+N(i,k)*B(i,2);
	P(2,3)=P(2,3)+N(i,k)*B(i,3);
end
N=bspline_oinarri_funtzio_ire(k,n,2,8);
for i=1:n
	P(3,1)=P(3,1)+N(i,k)*B(i,1);
	P(3,2)=P(3,2)+N(i,k)*B(i,2);
	P(3,3)=P(3,3)+N(i,k)*B(i,3);
end
N=bspline_oinarri_funtzio_ire(k,n,3,8);
for i=1:n
	P(4,1)=P(4,1)+N(i,k)*B(i,1);
	P(4,2)=P(4,2)+N(i,k)*B(i,2);
	P(4,3)=P(4,3)+N(i,k)*B(i,3);
end
N=bspline_oinarri_funtzio_ire(k,n,4,8);
for i=1:n
	P(5,1)=P(5,1)+N(i,k)*B(i,1);
	P(5,2)=P(5,2)+N(i,k)*B(i,2);
	P(5,3)=P(5,3)+N(i,k)*B(i,3);
end
N=bspline_oinarri_funtzio_ire(k,n,5,8);
for i=1:n
	P(6,1)=P(6,1)+N(i,k)*B(i,1);
	P(6,2)=P(6,2)+N(i,k)*B(i,2);
	P(6,3)=P(6,3)+N(i,k)*B(i,3);
end
N=bspline_oinarri_funtzio_ire(k,n,6,8);
for i=1:n
	P(7,1)=P(7,1)+N(i,k)*B(i,1);
	P(7,2)=P(7,2)+N(i,k)*B(i,2);
	P(7,3)=P(7,3)+N(i,k)*B(i,3);
end
N=bspline_oinarri_funtzio_ire(k,n,7,8);
for i=1:n
	P(8,1)=P(8,1)+N(i,k)*B(i,1);
	P(8,2)=P(8,2)+N(i,k)*B(i,2);
	P(8,3)=P(8,3)+N(i,k)*B(i,3);
end
N=bspline_oinarri_funtzio_ire(k,n,8,8);
for i=1:n
	P(9,1)=P(9,1)+N(i,k)*B(i,1);
	P(9,2)=P(9,2)+N(i,k)*B(i,2);
	P(9,3)=P(9,3)+N(i,k)*B(i,3);
end
X1=[P(1,1) P(2,1)];Y1=[P(1,2) P(2,2)];Z1=[P(1,3) P(2,3)];line(X1,Y1,Z1);
X1=[P(2,1) P(3,1)];Y1=[P(2,2) P(3,2)];Z1=[P(2,3) P(3,3)];line(X1,Y1,Z1);
X1=[P(3,1) P(4,1)];Y1=[P(3,2) P(4,2)];Z1=[P(3,3) P(4,3)];line(X1,Y1,Z1);
X1=[P(4,1) P(5,1)];Y1=[P(4,2) P(5,2)];Z1=[P(4,3) P(5,3)];line(X1,Y1,Z1);
X1=[P(5,1) P(6,1)];Y1=[P(5,2) P(6,2)];Z1=[P(5,3) P(6,3)];line(X1,Y1,Z1);
X1=[P(6,1) P(7,1)];Y1=[P(6,2) P(7,2)];Z1=[P(6,3) P(7,3)];line(X1,Y1,Z1);
X1=[P(7,1) P(8,1)];Y1=[P(7,2) P(8,2)];Z1=[P(7,3) P(8,3)];line(X1,Y1,Z1);
X1=[P(8,1) P(9,1)];Y1=[P(8,2) P(9,2)];Z1=[P(8,3) P(9,3)];line(X1,Y1,Z1);

