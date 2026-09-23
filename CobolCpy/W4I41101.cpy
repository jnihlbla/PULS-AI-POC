000100 01  MID-W4I41101.                                                        
000200*                                                                         
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-INPUT.                                                        
001200*                                                                         
001300        05 MID-IDKUNDNR-COPY PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500        05 MID-BEGMT-RAD1    PIC X(35).                                   
001600*                                 GODSMOTTAGARNAMN RAD 1                  
001700        05 MID-IDPARTNER     PIC X(9).                                    
001800*                                 PARTNER ID                              
001900        05 MID-BEGMT-RAD2    PIC X(35).                                   
002000*                                 GODSMOTTAGARNAMN RAD 2                  
002100        05 MID-ADGMT-GATA    PIC X(35).                                   
002200*                                 GODSMOTTAGARADRESS GATA                 
002300        05 MID-IDLANDX2-IN   PIC X(2).                                    
002400*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002500        05 MID-ADPOSTNR      PIC X(10).                                   
002600*                                 POSTNUMMER I ADRESS                     
002700        05 MID-KDPOSTNR      PIC X.                                       
002800*                                 OM/HUR POSTNUMMER JUSTERATS             
002900        05 MID-KDKUNDKAT-IN  PIC 9(2).                                    
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
006600        05 MID-ADCITY        PIC X(25).                                   
006700*                                 BENÄMNING PÅ STAD                       
006800        05 MID-IDLONGITUDE-IN                                             
006900                             PIC X(10).                                   
007000*                                 LONGITUDE                               
007100        05 MID-ADGMT-LAND    PIC X(35).                                   
007200*                                 GODSMOTTAGARADRESS LAND                 
007300        05 MID-IDLATITUDE-IN PIC X(10).                                   
007400*                                 LATITUDE                                
007500        05 MID-IDTFN         PIC X(20).                                   
007600*                                 TELEFONNUMMER EXTERNT                   
007700        05 MID-BETEXT-IN     PIC X(35).                                   
007800        05 MID-FLRESTN       PIC X.                                       
007900*                                 RESTNOTERING ?                          
008000        05 MID-FLPRELRO      PIC X.                                       
008100*                                 PRELIMINÄR RESTORDERFLAGGA              
008200        05 MID-RESLATT-IN    PIC X(2).                                    
008300*                                 SLATTGRÄNS                              
008400        05 MID-IDKUNDNR-H-IN PIC X(6).                                    
008500*                                 KUNDNR. TILL GMT:S HUVUDKONTOR          
008600        05 MID-KDBEKALT-IN   PIC X.                                       
008700*                                 ORDERBEKRÄFTELSEALTERNATIV              
008800        05 MID-FLOBKR-TACD-IN                                             
008900                             PIC X.                                       
009000*                                 ORDERBEKRÄFTELSE TILL TACDIS            
009100        05 MID-FLDNDAP-IN    PIC X.                                       
009200*                                 FLAGGA DNOT VIA D&P-KUND                
009300        05 MID-FLORDTIL-KL1  PIC X.                                       
009400*                                 TVINGANDE TILÄGG ORDER KLASS 1          
009500        05 MID-FLORDTIL-KL2  PIC X.                                       
009600*                                 TVINGANDE TILÄGG ORDER KLASS 2          
009700        05 MID-FLORDTIL-KL3  PIC X.                                       
009800*                                 TVINGANDE TILÄGG ORDER KLASS 3          
009900        05 MID-FLORDTIL-KL4  PIC X.                                       
010000*                                 TVINGANDE TILÄGG ORDER KLASS 4          
010100*** END OF VILMAII-COPY LENGTH= 307 BYTES                                 
