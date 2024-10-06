*2;
proc format ;
VALUE sexfmt 1='男' 0='女';
VALUE roofmt 1='北部'2='中部'3='南部'4='東部';
VALUE lea1fmt 1='國小'2='國中'3='高中';
run;
PROC IMPORT OUT=net
            DATAFILE="C:\Users\410650229\Documents\net.xlsx"
            DBMS=XLSX
            REPLACE;
			format K1 sexfmt.;
			format K3 roofmt.;
			format K4 lea1fmt.;
			label K1='性別' K3='居住地區'K4='學歷' K2='年齡';

    GETNAMES=YES; 
RUN;


proc print data=net;
run;
%MACRO f(a);
PROC TABULATE noseps missing format=5.0;
CLASS &a;
KEYLABEL all='合計' n='人'pctn='%';
TABLE (all &a)*(n pctn*f=5.2)/rts=20;
label K1='性別' K3='居住地區'K4='學歷'
%MEND;
%f(K1);
%f(K3);
%f(K4);

*3(1);
PROC TABULATE noseps missing format=5.0;
CLASS K1 K3 K4;
KEYLABEL all='合計' n='人'pctn='%';
TABLE (all K1 K3 K4)*(n pctn*f=5.2)/rts=20;
label K1='性別' K3='居住地區'K4='學歷';
run;
*3(2)error;
proc sort data=net;
by K2;
run;
PROC TABULATE noseps missing format=5.0;
CLASS K1 K4 K2 ;
KEYLABEL n='人' mean='平均數' std='標準差' ;
TABLE K1*K4*K2*(n mean std);
label K1='性別' K3='居住地區'K4='學歷';
run;
*3(3);
PROC TABULATE noseps missing format=5.0;
CLASS K1  K4;
KEYLABEL all='合計' n='人'pctn='%';
TABLE (all K1) ,all*(n pctn) (K4)*(n pctn*f=5.2);
label K1='性別' K4='學歷';
run;
*3(4);
PROC TABULATE noseps missing format=5.0;
CLASS K1  K4;
KEYLABEL all='合計' n='人'colpctn='%';
TABLE (all K1) ,all*(n colpctn) (K4)*(n colpctn*f=5.2);
label K1='性別' K4='學歷';
run;
*3(5);
PROC TABULATE noseps missing format=5.0;
CLASS K1  K4;
KEYLABEL all='合計' n='人'rowpctn='%';
TABLE (all K1) ,all*(n rowpctn) (K4)*(n pctn*f=5.2);
label K1='性別' K4='學歷';
run;