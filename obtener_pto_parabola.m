function C=obtener_pto_parabola(a,dtheta,x1,y1,z1)
% lortu parabolaren hurrengo puntua
C(1,:)=[x1+y1*dtheta+a*(dtheta^2) y1+2*a*dtheta z1];
end