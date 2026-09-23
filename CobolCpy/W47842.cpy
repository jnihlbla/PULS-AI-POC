000100 01  W47842.                                                              
000200*                                 DISTRIBUTION FOLLOW UP PER              
000300*                                 DC, PRC, CLASS, RFS.                    
000400*                                                                         
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 IDPRC.                                                            
000900*                                 PRODUKTIONSKANAL                        
001000*                                 PRODUCTION CHANNEL                      
001100        05 IDPRCBAS          PIC X(3).                                    
001200*                                 PRC-BAS                                 
001300*                                 PRC-BASIC                               
001400        05 IDPRCVAR          PIC X.                                       
001500*                                 PRC-VARIANT                             
001600*                                 PRC-VARIANT                             
001700     03 KDORDKL              PIC S9              COMP-3.                  
001800*                                 ORDERKLASS                              
001900*                                 ORDER CLASS                             
002000     03 IDDISTR              PIC S9(5)           COMP-3.                  
002100*                                 DISTRIKTNUMMER                          
002200*                                 DISTRICT NUMBER                         
002300     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
002400*                                 KUNDNUMMER                              
002500*                                 CUSTOMER NO                             
002600     03 FLDIRLEV             PIC X.                                       
002700*                                 DIREKTLEVERANS ?                        
002800*                                 DIRECT DELIVERY ?                       
002900     03 TIDATUM              PIC S9(7)           COMP-3.                  
003000*                                 DATUM ENLIGT KDDATFORM                  
003100*                                 DATE AS SPECIFIED BY KDDATFORM          
003200     03 TIAAVV               PIC 9(4).                                    
003300*                                 ≈R - VECKA  (≈≈VV)                      
003400*                                 YEAR - WEEK  (YYWW)                     
003500     03 TIAARP               PIC 9(4).                                    
003600*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
003700*                                 12 PER ≈R (OCKS≈ LOGISTIKPER)           
003800*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
003900*                                 12 PER YEAR, ALSO LOGISTICS PER         
004000     03 TIRFSTID             PIC 9(4).                                    
004100*                                 KLART F÷R TRANSPORT (TTMM)              
004200*                                 READY FOR SHIPMENT  (HHMM)              
004300     03 KVORDER              PIC S9(7)           COMP-3.                  
004400*                                 ANTAL ORDER                             
004500*                                 QUANTITY OF ORDERS                      
004600     03 KVORDER-UTSKR        PIC S9(7)           COMP-3.                  
004700*                                 ANTAL ORDER                             
004800*                                 QUANTITY OF ORDERS                      
004900     03 KVORDER-PACK         PIC S9(7)           COMP-3.                  
005000*                                 ANTAL PACKADE ORDER                     
005100*                                 QUANTITY OF PACKED ORDERS               
005200     03 KVORDRAD             PIC S9(7)           COMP-3.                  
005300     03 KVORDRAD-UTSKR       PIC S9(7)           COMP-3.                  
005400     03 KVORDRAD-PACK        PIC S9(7)           COMP-3.                  
005500     03 KVKOLLI              PIC S9(7)           COMP-3.                  
005600     03 KVKOLLI-LAST         PIC S9(7)           COMP-3.                  
005700     03 IDORDNR7             PIC 9(7).                                    
005800*                                 ORDERNUMMER                             
005900*                                 ORDER NUMBER                            
006000*** END OF VILMAII-COPY LENGTH= 70 BYTES                                  
