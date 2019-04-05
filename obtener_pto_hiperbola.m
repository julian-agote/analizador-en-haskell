function C=obtener_pto_hiperbola(a,b,cd,sd,x1,y1,z1)
% lortu hiperbolaren hurrengo puntua
C(1,:)=[x1*cd+(a/b)*y1*sd (b/a)*x1*sd+y1*cd z1];
end