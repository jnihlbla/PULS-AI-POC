000100 01  RESP-W40411O1.                                                       
000200*                                                                         
000300     03 RESP-IDKUNDNR-COPY-ATTR                                           
000400                             PIC X(2).                                    
000500*                                 MFS ATTRIBUTFÄLT                        
000600     03 RESP-IDKUNDNR-COPY   PIC X(6).                                    
000700*                                 KUNDNUMMER                              
000800     03 RESP-BEGMT-RAD1-ATTR PIC X(2).                                    
000900*                                 MFS ATTRIBUTFÄLT                        
001000     03 RESP-BEGMT-RAD1      PIC X(35).                                   
001100*                                 GODSMOTTAGARNAMN RAD 1                  
001200     03 RESP-IDPARTNER-ATTR  PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400     03 RESP-IDPARTNER       PIC X(9).                                    
001500*                                 PARTNER ID                              
001600     03 RESP-BEGMT-RAD2-ATTR PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 RESP-BEGMT-RAD2      PIC X(35).                                   
001900*                                 GODSMOTTAGARNAMN RAD 2                  
002000     03 RESP-IDDEALER-VIPS-ATTR                                           
002100                             PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300     03 RESP-IDDEALER-VIPS   PIC X(6).                                    
002400*                                 VIPS ÅTERFÖRSÄLJARE                     
002500     03 RESP-ADGMT-GATA-ATTR PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700     03 RESP-ADGMT-GATA      PIC X(35).                                   
002800*                                 GODSMOTTAGARADRESS GATA                 
002900     03 RESP-IDLANDX2-ATTR   PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 RESP-IDLANDX2        PIC X(2).                                    
003200*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
003300     03 RESP-ADPOSTNR-ATTR   PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 RESP-ADPOSTNR        PIC X(10).                                   
003600*                                 POSTNUMMER I ADRESS                     
003700     03 RESP-KDPOSTNR-ATTR   PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 RESP-KDPOSTNR        PIC X.                                       
004000*                                 OM/HUR POSTNUMMER JUSTERATS             
004100     03 RESP-KDKUNDKAT-ATTR  PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 RESP-KDKUNDKAT       PIC X(2).                                    
004400*                                 TYP OF KUND I KUND TABELL - WDB         
004500*                                 201                                     
004600*                                    01 = DEALER                          
004700*                                                                         
004800*                                    02 = POLESTAR                        
004900*                                                                         
005000*                                    03 = LYNK                            
005100*                                                                         
005200*                                    04 = IMPORTER                        
005300*                                                                         
005400*                                    05 = INTERNAL CUSTOMER               
005500*                                                                         
005600*                                    06 = SALES COMPANY                   
005700*                                                                         
005800*                                    07 = SUPPLIER                        
005900*                                                                         
006000*                                    08 = REFILL                          
006100*                                                                         
006200*                                    09 = TRANSFER                        
006300*                                                                         
006400*                                    10 = EXTENDED REFILL                 
006500*                                                                         
006600*                                    11 = RETURNS                         
006700*                                                                         
006800*                                    12 = QUALITY RETURNS                 
006900*                                                                         
007000*                                    13 = SCRAPL                          
007100*                                                                         
007200*                                    14 = QUALITY SCRAP                   
007300*                                                                         
007400*                                    15 = MIXED STOCK                     
007500*                                                                         
007600*                                    16 = INTERNAL EXCHANGE ORDER         
007700*                                                                         
007800*                                    17 = EMBALLAGE                       
007900*                                                                         
008000     03 RESP-ADCITY-ATTR     PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200     03 RESP-ADCITY          PIC X(25).                                   
008300*                                 BENÄMNING PÅ STAD                       
008400     03 RESP-IDLONGITUDE-ATTR                                             
008500                             PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700     03 RESP-IDLONGITUDE     PIC X(10).                                   
008800*                                 LONGITUDE                               
008900     03 RESP-ADGMT-LAND-ATTR PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100     03 RESP-ADGMT-LAND      PIC X(35).                                   
009200*                                 GODSMOTTAGARADRESS LAND                 
009300     03 RESP-IDLATITUDE-ATTR PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500     03 RESP-IDLATITUDE      PIC X(10).                                   
009600*                                 LATITUDE                                
009700     03 RESP-IDTFN-ATTR      PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900     03 RESP-IDTFN           PIC X(20).                                   
010000*                                 TELEFONNUMMER EXTERNT                   
010100     03 RESP-BETEXT-ATTR     PIC X(2).                                    
010200*                                 MFS ATTRIBUTFÄLT                        
010300     03 RESP-BETEXT          PIC X(35).                                   
010400     03 RESP-FLRESTN-ATTR    PIC X(2).                                    
010500*                                 MFS ATTRIBUTFÄLT                        
010600     03 RESP-FLRESTN         PIC X.                                       
010700*                                 RESTNOTERING ?                          
010800     03 RESP-IDZON           PIC X(2).                                    
010900*                                 TRANSPORTVÄG (RUTT,ZON)                 
011000     03 RESP-FLPRELRO-ATTR   PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200     03 RESP-FLPRELRO        PIC X.                                       
011300*                                 PRELIMINÄR RESTORDERFLAGGA              
011400     03 RESP-IDDEPOT         PIC X(2).                                    
011500*                                 TRANSPORT DEPOT                         
011600     03 RESP-RESLATT-UT      PIC Z9.                                      
011700*                                 SLATTGRÄNS                              
011800     03 RESP-RESLATT-UPD-ATTR                                             
011900                             PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100     03 RESP-RESLATT-UPD     PIC Z9.                                      
012200*                                 SLATTGRÄNS                              
012300     03 RESP-IDROUTE         PIC X.                                       
012400*                                 TRANSPORT ROUTE                         
012500     03 RESP-IDKUNDNR-H-UT   PIC Z(5)9.                                   
012600*                                 KUNDNR. TILL GMT:S HUVUDKONTOR          
012700     03 RESP-IDKUNDNR-H-UPD-ATTR                                          
012800                             PIC X(2).                                    
012900*                                 MFS ATTRIBUTFÄLT                        
013000     03 RESP-IDKUNDNR-H-UPD  PIC X(6).                                    
013100*                                 KUNDNR. TILL GMT:S HUVUDKONTOR          
013200     03 RESP-KDBEKALT-UT     PIC 9.                                       
013300*                                 ORDERBEKRÄFTELSEALTERNATIV              
013400     03 RESP-KDBEKALT-UPD-ATTR                                            
013500                             PIC X(2).                                    
013600*                                 MFS ATTRIBUTFÄLT                        
013700     03 RESP-KDBEKALT-UPD    PIC X.                                       
013800*                                 ORDERBEKRÄFTELSEALTERNATIV              
013900     03 RESP-FLOBKR-TACD-UT  PIC X.                                       
014000*                                 ORDERBEKRÄFTELSE TILL TACDIS            
014100     03 RESP-FLOBKR-TACD-UPD-ATTR                                         
014200                             PIC X(2).                                    
014300*                                 MFS ATTRIBUTFÄLT                        
014400     03 RESP-FLOBKR-TACD-UPD PIC X.                                       
014500*                                 ORDERBEKRÄFTELSE TILL TACDIS            
014600     03 RESP-FLDNDAP-UT      PIC X.                                       
014700*                                 FLAGGA DNOT VIA D&P-KUND                
014800     03 RESP-FLDNDAP-UPD-ATTR                                             
014900                             PIC X(2).                                    
015000*                                 MFS ATTRIBUTFÄLT                        
015100     03 RESP-FLDNDAP-UPD     PIC X.                                       
015200*                                 FLAGGA DNOT VIA D&P-KUND                
015300     03 RESP-FLORDTIL-KL1-ATTR                                            
015400                             PIC X(2).                                    
015500*                                 MFS ATTRIBUTFÄLT                        
015600     03 RESP-FLORDTIL-KL1    PIC X.                                       
015700*                                 TVINGANDE TILÄGG ORDER KLASS 1          
015800     03 RESP-FLORDTIL-KL2-ATTR                                            
015900                             PIC X(2).                                    
016000*                                 MFS ATTRIBUTFÄLT                        
016100     03 RESP-FLORDTIL-KL2    PIC X.                                       
016200*                                 TVINGANDE TILÄGG ORDER KLASS 2          
016300     03 RESP-FLORDTIL-KL3-ATTR                                            
016400                             PIC X(2).                                    
016500*                                 MFS ATTRIBUTFÄLT                        
016600     03 RESP-FLORDTIL-KL3    PIC X.                                       
016700*                                 TVINGANDE TILÄGG ORDER KLASS 3          
016800     03 RESP-FLORDTIL-KL4-ATTR                                            
016900                             PIC X(2).                                    
017000*                                 MFS ATTRIBUTFÄLT                        
017100     03 RESP-FLORDTIL-KL4    PIC X.                                       
017200*                                 TVINGANDE TILÄGG ORDER KLASS 4          
017300*** END OF VILMAII-COPY LENGTH= 363 BYTES                                 
