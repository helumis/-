*2;
proc format ;
VALUE sexfmt 1='�k' 0='�k';
VALUE roofmt 1='�_��'2='����'3='�n��'4='�F��';
VALUE lea1fmt 1='��p'2='�ꤤ'3='����';
run;
PROC IMPORT OUT=net
            DATAFILE="C:\Users\410650229\Documents\net.xlsx"
            DBMS=XLSX
            REPLACE;
			format K1 sexfmt.;
			format K3 roofmt.;
			format K4 lea1fmt.;
			label K1='�ʧO' K3='�~��a��'K4='�Ǿ�' K2='�~��';

    GETNAMES=YES; 
RUN;


proc print data=net;
run;
%MACRO f(a);
PROC TABULATE noseps missing format=5.0;
CLASS &a;
KEYLABEL all='�X�p' n='�H'pctn='%';
TABLE (all &a)*(n pctn*f=5.2)/rts=20;
label K1='�ʧO' K3='�~��a��'K4='�Ǿ�'
%MEND;
%f(K1);
%f(K3);
%f(K4);

*3(1);
PROC TABULATE noseps missing format=5.0;
CLASS K1 K3 K4;
KEYLABEL all='�X�p' n='�H'pctn='%';
TABLE (all K1 K3 K4)*(n pctn*f=5.2)/rts=20;
label K1='�ʧO' K3='�~��a��'K4='�Ǿ�';
run;
*3(2)error;
proc sort data=net;
by K2;
run;
PROC TABULATE noseps missing format=5.0;
CLASS K1 K4 K2 ;
KEYLABEL n='�H' mean='������' std='�зǮt' ;
TABLE K1*K4*K2*(n mean std);
label K1='�ʧO' K3='�~��a��'K4='�Ǿ�';
run;
*3(3);
PROC TABULATE noseps missing format=5.0;
CLASS K1  K4;
KEYLABEL all='�X�p' n='�H'pctn='%';
TABLE (all K1) ,all*(n pctn) (K4)*(n pctn*f=5.2);
label K1='�ʧO' K4='�Ǿ�';
run;
*3(4);
PROC TABULATE noseps missing format=5.0;
CLASS K1  K4;
KEYLABEL all='�X�p' n='�H'colpctn='%';
TABLE (all K1) ,all*(n colpctn) (K4)*(n colpctn*f=5.2);
label K1='�ʧO' K4='�Ǿ�';
run;
*3(5);
PROC TABULATE noseps missing format=5.0;
CLASS K1  K4;
KEYLABEL all='�X�p' n='�H'rowpctn='%';
TABLE (all K1) ,all*(n rowpctn) (K4)*(n pctn*f=5.2);
label K1='�ʧO' K4='�Ǿ�';
run;