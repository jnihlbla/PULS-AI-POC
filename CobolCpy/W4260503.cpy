000100 01  W4260503.                                                            
000200*                                 KVALITET KVALITETSINDEX                 
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
001800     03 RAD                  OCCURS 13 TIMES                              
001900                             INDEXED IX1.                                 
002000*                                  TABELL-RADER                           
002100*                                                                         
002200        05 TIAARP            PIC S9(5)           COMP-3.                  
002300*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
002400*                                 12 PER ≈R                               
002500*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
002600*                                 12 PER YEAR                             
002700        05 REKVAIND          PIC S9(5)           COMP-3.                  
002800*                                 KVALITET KVALITETSINDEX                 
002900*                                 QUALITY QUALITYINDEX                    
003000*** END OF VILMAII-COPY LENGTH= 104 BYTES                                 
