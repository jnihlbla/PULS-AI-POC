000100 01  W47843.                                                              
000200*                                 DISTRIBUTION FOLLOW UP PER              
000300*                                 DC, PRC, CLASS, RFS.                    
000400*                                                                         
000500     03 KDMFUP               PIC X(2).                                    
000600*                                 RAPPORTGRUPP  MA/CN/PF/NA               
000700*                                 REPORT GROUP  MA/CN/PF/NA               
000800     03 IDDC                 PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 ADCITY               PIC X(20).                                   
001200     03 IDPRC.                                                            
001300*                                 PRODUKTIONSKANAL                        
001400*                                 PRODUCTION CHANNEL                      
001500        05 IDPRCBAS          PIC X(3).                                    
001600*                                 PRC-BAS                                 
001700*                                 PRC-BASIC                               
001800        05 IDPRCVAR          PIC X.                                       
001900*                                 PRC-VARIANT                             
002000*                                 PRC-VARIANT                             
002100     03 KDORDKL              PIC S9              COMP-3.                  
002200*                                 ORDERKLASS                              
002300*                                 ORDER CLASS                             
002400     03 TIDATUM              PIC S9(7)           COMP-3.                  
002500*                                 DATUM ENLIGT KDDATFORM                  
002600*                                 DATE AS SPECIFIED BY KDDATFORM          
002700     03 TIAAVV               PIC 9(4).                                    
002800*                                 ≈R - VECKA  (≈≈VV)                      
002900*                                 YEAR - WEEK  (YYWW)                     
003000     03 TIAARP               PIC 9(4).                                    
003100*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
003200*                                 12 PER ≈R (OCKS≈ LOGISTIKPER)           
003300*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
003400*                                 12 PER YEAR, ALSO LOGISTICS PER         
003500     03 TIRFSTID             PIC 9(4).                                    
003600*                                 KLART F÷R TRANSPORT (TTMM)              
003700*                                 READY FOR SHIPMENT  (HHMM)              
003800     03 KVORDER              PIC S9(7)           COMP-3.                  
003900*                                 ANTAL ORDER                             
004000*                                 QUANTITY OF ORDERS                      
004100     03 KVORDER-UTSKR        PIC S9(7)           COMP-3.                  
004200*                                 ANTAL ORDER                             
004300*                                 QUANTITY OF ORDERS                      
004400     03 KVORDER-PACK         PIC S9(7)           COMP-3.                  
004500*                                 ANTAL PACKADE ORDER                     
004600*                                 QUANTITY OF PACKED ORDERS               
004700     03 KVORDRAD             PIC S9(7)           COMP-3.                  
004800     03 KVORDRAD-UTSKR       PIC S9(7)           COMP-3.                  
004900     03 KVORDRAD-PACK        PIC S9(7)           COMP-3.                  
005000     03 KVKOLLI              PIC S9(7)           COMP-3.                  
005100     03 KVKOLLI-LAST         PIC S9(7)           COMP-3.                  
005200     03 KVKOLLI-PROC         PIC 9(3)V9(1).                               
005300*** END OF VILMAII-COPY LENGTH= 81 BYTES                                  
