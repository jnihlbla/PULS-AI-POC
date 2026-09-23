000100 01  REQU-W40411I1.                                                       
000200*                                                                         
000300     03 REQU-IDDISTR-KEY     PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 REQU-IDKUNDNR-KEY    PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 REQU-INPUT.                                                       
000800*                                                                         
000900        05 REQU-IDKUNDNR-COPY                                             
001000                             PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200        05 REQU-BEGMT-RAD1   PIC X(35).                                   
001300*                                 GODSMOTTAGARNAMN RAD 1                  
001400        05 REQU-IDPARTNER    PIC X(9).                                    
001500*                                 PARTNER ID                              
001600        05 REQU-BEGMT-RAD2   PIC X(35).                                   
001700*                                 GODSMOTTAGARNAMN RAD 2                  
001800        05 REQU-IDDEALER-VIPS                                             
001900                             PIC X(6).                                    
002000*                                 VIPS ÅTERFÖRSÄLJARE                     
002100        05 REQU-ADGMT-GATA   PIC X(35).                                   
002200*                                 GODSMOTTAGARADRESS GATA                 
002300        05 REQU-IDLANDX2     PIC X(2).                                    
002400*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002500        05 REQU-ADPOSTNR     PIC X(10).                                   
002600*                                 POSTNUMMER I ADRESS                     
002700        05 REQU-KDPOSTNR     PIC X.                                       
002800*                                 OM/HUR POSTNUMMER JUSTERATS             
002900        05 REQU-KDKUNDKAT    PIC X(2).                                    
003000*                                 TYP OF KUND I KUND TABELL - WDB         
003100*                                 201                                     
003200*                                    01 = DEALER                          
003300*                                                                         
003400*                                    02 = POLESTAR                        
003500*                                                                         
003600*                                    03 = LYNK                            
003700*                                                                         
003800*                                    04 = IMPORTER                        
003900*                                                                         
004000*                                    05 = INTERNAL CUSTOMER               
004100*                                                                         
004200*                                    06 = SALES COMPANY                   
004300*                                                                         
004400*                                    07 = SUPPLIER                        
004500*                                                                         
004600*                                    08 = REFILL                          
004700*                                                                         
004800*                                    09 = TRANSFER                        
004900*                                                                         
005000*                                    10 = EXTENDED REFILL                 
005100*                                                                         
005200*                                    11 = RETURNS                         
005300*                                                                         
005400*                                    12 = QUALITY RETURNS                 
005500*                                                                         
005600*                                    13 = SCRAPL                          
005700*                                                                         
005800*                                    14 = QUALITY SCRAP                   
005900*                                                                         
006000*                                    15 = MIXED STOCK                     
006100*                                                                         
006200*                                    16 = INTERNAL EXCHANGE ORDER         
006300*                                                                         
006400*                                    17 = EMBALLAGE                       
006500*                                                                         
006600        05 REQU-ADCITY       PIC X(25).                                   
006700*                                 BENÄMNING PÅ STAD                       
006800        05 REQU-IDLONGITUDE  PIC X(10).                                   
006900*                                 LONGITUDE                               
007000        05 REQU-ADGMT-LAND   PIC X(35).                                   
007100*                                 GODSMOTTAGARADRESS LAND                 
007200        05 REQU-IDLATITUDE   PIC X(10).                                   
007300*                                 LATITUDE                                
007400        05 REQU-IDTFN        PIC X(20).                                   
007500*                                 TELEFONNUMMER EXTERNT                   
007600        05 REQU-BETEXT       PIC X(35).                                   
007700        05 REQU-FLRESTN      PIC X.                                       
007800*                                 RESTNOTERING ?                          
007900        05 REQU-FLPRELRO     PIC X.                                       
008000*                                 PRELIMINÄR RESTORDERFLAGGA              
008100        05 REQU-RESLATT-UPD  PIC X(2).                                    
008200*                                 SLATTGRÄNS                              
008300        05 REQU-IDKUNDNR-H-UPD                                            
008400                             PIC X(6).                                    
008500*                                 KUNDNR. TILL GMT:S HUVUDKONTOR          
008600        05 REQU-KDBEKALT-UPD PIC X.                                       
008700*                                 ORDERBEKRÄFTELSEALTERNATIV              
008800        05 REQU-FLOBKR-TACD-UPD                                           
008900                             PIC X.                                       
009000*                                 ORDERBEKRÄFTELSE TILL TACDIS            
009100        05 REQU-FLDNDAP-UPD  PIC X.                                       
009200*                                 FLAGGA DNOT VIA D&P-KUND                
009300        05 REQU-FLORDTIL-KL1 PIC X.                                       
009400*                                 TVINGANDE TILÄGG ORDER KLASS 1          
009500        05 REQU-FLORDTIL-KL2 PIC X.                                       
009600*                                 TVINGANDE TILÄGG ORDER KLASS 2          
009700        05 REQU-FLORDTIL-KL3 PIC X.                                       
009800*                                 TVINGANDE TILÄGG ORDER KLASS 3          
009900        05 REQU-FLORDTIL-KL4 PIC X.                                       
010000*                                 TVINGANDE TILÄGG ORDER KLASS 4          
010100*** END OF VILMAII-COPY LENGTH= 303 BYTES                                 
