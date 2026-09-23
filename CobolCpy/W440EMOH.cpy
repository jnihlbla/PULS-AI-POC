000100 01  EMOH-W440EMOH.                                                       
000200*                                 LINK AREA FOR W440EMOH                  
000300*                                 CREATES DOSKALLER FOR BPACK COD         
000400*                                 E 3                                     
000500     03 EMOH-IDDISTR         PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700*                                 DISTRICT NUMBER                         
000800     03 EMOH-IDKUNDNR        PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000*                                 CUSTOMER NO                             
001100     03 EMOH-IDDC            PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 EMOH-IDORDNR7        PIC 9(7).                                    
001500*                                 ORDERNUMMER                             
001600*                                 ORDER NUMBER                            
001700     03 EMOH-KDORDKL         PIC S9              COMP-3.                  
001800      88 EMOH-KDORDKL-VOR    VALUE +0.                                    
001900      88 EMOH-KDORDKL-DAG    VALUE +1.                                    
002000      88 EMOH-KDORDKL-2      VALUE +2.                                    
002100      88 EMOH-KDORDKL-SNABB  VALUE +2.                                    
002200      88 EMOH-KDORDKL-SPECIAL                                             
002300                             VALUE +3.                                    
002400      88 EMOH-KDORDKL-KVANT  VALUE +4.                                    
002500      88 EMOH-KDORDKL-SATS   VALUE +5.                                    
002600*                                 ORDERKLASS                              
002700*                                 ORDER CLASS                             
002800     03 EMOH-BEGMT.                                                       
002900*                                 GODSMOTTAGARNAMN                        
003000*                                 GOODS RECEIVER NAME                     
003100        05 EMOH-BEGMT-RAD1   PIC X(35).                                   
003200*                                 GODSMOTTAGARNAMN RAD 1                  
003300*                                 GOODS RECEIVER NAME LINE 1              
003400        05 EMOH-BEGMT-RAD2   PIC X(35).                                   
003500*                                 GODSMOTTAGARNAMN RAD 2                  
003600*                                 GOODS RECEIVER NAME LINE 2              
003700     03 EMOH-ADGMT.                                                       
003800*                                 GODSMOTTAGARADRESS                      
003900*                                 GOODS RECEIVER ADDRESS                  
004000        05 EMOH-ADGMT-GATA   PIC X(35).                                   
004100*                                 GODSMOTTAGARADRESS GATA                 
004200*                                 GOODS RECEIVER ADDRESS STREET           
004300        05 EMOH-ADGMT-PADR   PIC X(35).                                   
004400*                                 GODSMOTTAGARADRESS POSTADRESS           
004500*                                 GOODS RECEIVER ADDRESS TOWN             
004600        05 EMOH-ADPOST-PNRORT REDEFINES EMOH-ADGMT-PADR.                  
004700*                                 POSTNUMMER + ORT                        
004800*                                 POSTAL CODE + CITY                      
004900           07 EMOH-ADPOSTNR  PIC X(10).                                   
005000*                                 POSTNUMMER I ADRESS                     
005100*                                 POSTAL CODE IN ADDRESS                  
005200           07 EMOH-ADCITY    PIC X(25).                                   
005300*                                 BENÄMNING PÅ STAD                       
005400*                                 CITY                                    
005500        05 EMOH-ADPOST-ORTPNR REDEFINES EMOH-ADGMT-PADR.                  
005600*                                 ORT + POSTNUMMER                        
005700*                                 CITY + POSTAL CODE                      
005800           07 EMOH-ADCITY    PIC X(25).                                   
005900*                                 BENÄMNING PÅ STAD                       
006000*                                 CITY                                    
006100           07 EMOH-ADPOSTNR  PIC X(10).                                   
006200*                                 POSTNUMMER I ADRESS                     
006300*                                 POSTAL CODE IN ADDRESS                  
006400        05 EMOH-ADGMT-LAND   PIC X(35).                                   
006500*                                 GODSMOTTAGARADRESS LAND                 
006600*                                 GOODS RECEIVER ADDRESS COUNTRY          
006700     03 EMOH-BELAGINS-GRP.                                                
006800*                                 LAGERINSTRUKTIONER                      
006900*                                 WAREHOUSE INSTRUCTIONS                  
007000        05 EMOH-BELAGINS-DEL1                                             
007100                             PIC X(60).                                   
007200*                                 DEL AV LAGERINSTRUKTION                 
007300*                                 PART OF WAREHOUSE INSTRUCTIONS          
007400        05 EMOH-BELAGINS-DEL2                                             
007500                             PIC X(60).                                   
007600*                                 DEL AV LAGERINSTRUKTION                 
007700*                                 PART OF WAREHOUSE INSTRUCTIONS          
007800     03 EMOH-BEBET.                                                       
007900*                                 BETALNINGSANSVARIG NAMN                 
008000*                                 NAME OF PAYER                           
008100        05 EMOH-BEBETRAD-1   PIC X(35).                                   
008200*                                 DEL AV BETALNINGSANSVARIGS NAMN         
008300*                                 PART OF FINANCIAL CUSTOMER NAME         
008400        05 EMOH-BEBETRAD-2   PIC X(35).                                   
008500*                                 DEL AV BETALNINGSANSVARIGS NAMN         
008600*                                 PART OF FINANCIAL CUSTOMER NAME         
008700        05 EMOH-BETELNR-FILLER REDEFINES EMOH-BEBETRAD-2.                 
008800           07 EMOH-BETELNR   PIC X(10).                                   
008900*                                 TELEFONNUMMER                           
009000*                                 TELEPHONE NUMBER                        
009100           07 FILLER         PIC X(25).                                   
009200     03 EMOH-ADBET.                                                       
009300*                                 BETALNINGSANSVARIG ADRESS               
009400*                                 ADDRESS OF PAYER                        
009500        05 EMOH-ADBETRAD-1   PIC X(35).                                   
009600*                                 ADRESSRAD BETALNINGSANSVARIG            
009700*                                 PART OF FINANCIAL CUSTOMER ADDR         
009800*                                 ESS                                     
009900        05 EMOH-ADBETRAD-2   PIC X(35).                                   
010000*                                 ADRESSRAD BETALNINGSANSVARIG            
010100*                                 PART OF FINANCIAL CUSTOMER ADDR         
010200*                                 ESS                                     
010300     03 EMOH-IXHALV          PIC S9(4)           COMP.                    
010400*                                 INDEX HALVORD                           
010500*                                 INDEX HALFWORD                          
010600     03 EMOH-KDSVAR          PIC X.                                       
010700      88 EMOH-KDSVAR-CREATE  VALUE '1'.                                   
010800      88 EMOH-KDSVAR-EXISTS  VALUE '2'.                                   
010900*                                                       KDSVAR-88         
011000*                                 SVARSKOD FRÅN SUBPROGRAM                
011100*                                                       KDSVAR-88         
011200*                                 RETURN CODE FROM SUBPROGRAM             
011300*** END OF VILMAII-COPY LENGTH= 455 BYTES                                 
