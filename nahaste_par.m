function C=nahaste_par(alfa,beta,t,inter,G)
% Nahaste paraboliko orokortuaren Kurba. t balioentzako tarteko puntuak ebatzi
h=1/inter;
t=t*h;
A=[-(1-alfa)^2/alfa ((1-alfa)+alfa*beta)/alfa (-(1-alfa)-alfa*beta)/(1-beta) beta^2/(1-beta);2*(1-alfa)^2/alfa (-2*(1-alfa)-alfa*beta)/alfa (2*(1-alfa)-beta*(1-2*alfa))/(1-beta) -beta^2/(1-beta);-(1-alfa)^2/alfa (1-2*alfa)/alfa alfa 0;0 1 0 0];
C(1,:)=[t^3 t^2 t 1]*A*G;
end