000100 01  W47844.                                                              
000200*                                 DISTRIBUTION FOLLOW UP PER              
000300*                                 DC, PRC, CLASS, RFS.                    
000400*                                                                         
000500     03 IDAFPRCD             PIC X(10).                                   
000600*                                 AFP-BLANKETT POSTTYP                    
000700*                                 AFP FORMS RECORD TYPE                   
000800     03 IDFTG                PIC 9(2).                                    
000900*                                 F÷RETAGSID EKONOM REDOVISNING           
001000*                                 COMPANY IDENTITY ACCOUNTING             
001100     03 IDDC                 PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 ADCITY               PIC X(20).                                   
001500     03 IDPRC.                                                            
001600*                                 PRODUKTIONSKANAL                        
001700*                                 PRODUCTION CHANNEL                      
001800        05 IDPRCBAS          PIC X(3).                                    
001900*                                 PRC-BAS                                 
002000*                                 PRC-BASIC                               
002100        05 IDPRCVAR          PIC X.                                       
002200*                                 PRC-VARIANT                             
002300*                                 PRC-VARIANT                             
002400     03 KDORDKL              PIC 9.                                       
002500*                                 ORDERKLASS                              
002600*                                 ORDER CLASS                             
002700     03 TIDATUM              PIC 9(6).                                    
002800*                                 DATUM ENLIGT KDDATFORM                  
002900*                                 DATE AS SPECIFIED BY KDDATFORM          
003000     03 TIAAVV               PIC 9(4).                                    
003100*                                 ≈R - VECKA  (≈≈VV)                      
003200*                                 YEAR - WEEK  (YYWW)                     
003300     03 TIAARP REDEFINES TIAAVV                                           
003400                             PIC 9(4).                                    
003500*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
003600*                                 12 PER ≈R (OCKS≈ LOGISTIKPER)           
003700*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
003800*                                 12 PER YEAR, ALSO LOGISTICS PER         
003900     03 TIRFSTID             PIC X(5).                                    
004000     03 KVORDER              PIC Z(6)9.                                   
004100*                                 ANTAL ORDER                             
004200*                                 QUANTITY OF ORDERS                      
004300     03 KVORDER-UTSKR        PIC Z(6)9.                                   
004400*                                 ANTAL ORDER                             
004500*                                 QUANTITY OF ORDERS                      
004600     03 KVORDER-UTSKR-PROC   PIC Z(2)9.9.                                 
004700     03 KVORDER-PACK         PIC Z(6)9.                                   
004800*                                 ANTAL PACKADE ORDER                     
004900*                                 QUANTITY OF PACKED ORDERS               
005000     03 KVORDER-PACK-PROC    PIC Z(2)9.9.                                 
005100     03 KVORDRAD             PIC Z(6)9.                                   
005200     03 KVORDRAD-UTSKR       PIC Z(6)9.                                   
005300     03 KVORDRAD-UTSKR-PROC  PIC Z(2)9.9.                                 
005400     03 KVORDRAD-PACK        PIC Z(6)9.                                   
005500     03 KVORDRAD-PACK-PROC   PIC Z(2)9.9.                                 
005600     03 KVKOLLI              PIC Z(6)9.                                   
005700     03 KVKOLLI-LAST         PIC Z(6)9.                                   
005800     03 KVKOLLI-PROC         PIC Z(2)9.9.                                 
005900*** END OF VILMAII-COPY LENGTH= 135 BYTES                                 
