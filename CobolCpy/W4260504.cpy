000100 01  W4260504.                                                            
000200*                                 KVALITET F÷RDELNING FELGRUPPER          
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 IDKVAOMR             PIC X.                                       
001100*                                 KVALITET KONTROLLOMR≈DE                 
001200*                                 QUALITY CONTROL AREA                    
001300     03 BEKVAOMR             OCCURS 2 TIMES                               
001400                             INDEXED IX1                                  
001500                             PIC X(10).                                   
001600*                                 KVALITET KONTROLLOMR≈DESNAMN            
001700*                                 QUALITY NAME OF CONTROL AREA            
001800     03 KDKVAFG              PIC S9              COMP-3.                  
001900*                                 FELGRUPP F÷R FELKOD                     
002000*                                 ERROR GROUP FOR ERROR CODE              
002100     03 RAD                  OCCURS 20 TIMES                              
002200                             INDEXED IX1.                                 
002300*                                  TABELL-RADER                           
002400*                                                                         
002500        05 TIAARP            PIC S9(5)           COMP-3.                  
002600*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
002700*                                 12 PER ≈R                               
002800*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
002900*                                 12 PER YEAR                             
003000        05 REKVAREL          PIC S9(5)           COMP-3.                  
003100*                                 KVALITET F÷RDELNINGSTAL                 
003200*                                 QUALITY RELATION NUMBER                 
003300*** END OF VILMAII-COPY LENGTH= 147 BYTES                                 
