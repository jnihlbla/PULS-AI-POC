000100 01  MOD-W4O41101.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDKUNDNR-COPY-ATTR                                            
001600                             PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 MOD-IDKUNDNR-COPY    PIC X(6).                                    
001900*                                 KUNDNUMMER                              
002000     03 MOD-BEGMT-RAD1-ATTR  PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-BEGMT-RAD1       PIC X(35).                                   
002300*                                 GODSMOTTAGARNAMN RAD 1                  
002400     03 MOD-IDPARTNER-ATTR   PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 MOD-IDPARTNER        PIC X(9).                                    
002700*                                 PARTNER ID                              
002800     03 MOD-BEGMT-RAD2-ATTR  PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 MOD-BEGMT-RAD2       PIC X(35).                                   
003100*                                 GODSMOTTAGARNAMN RAD 2                  
003200     03 MOD-IDDEALER-VIPS    PIC X(6).                                    
003300*                                 VIPS ÅTERFÖRSÄLJARE                     
003400     03 MOD-ADGMT-GATA-ATTR  PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-ADGMT-GATA       PIC X(35).                                   
003700*                                 GODSMOTTAGARADRESS GATA                 
003800     03 MOD-IDLANDX2-ATTR    PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-IDLANDX2         PIC X(2).                                    
004100*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
004200     03 MOD-ADPOSTNR-ATTR    PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-ADPOSTNR         PIC X(10).                                   
004500*                                 POSTNUMMER I ADRESS                     
004600     03 MOD-KDPOSTNR-ATTR    PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-KDPOSTNR         PIC X.                                       
004900*                                 OM/HUR POSTNUMMER JUSTERATS             
005000     03 MOD-KDKUNDKAT-ATTR   PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-KDKUNDKAT        PIC 9(2).                                    
005300*                                 TYP OF KUND I KUND TABELL - WDB         
005400*                                 201                                     
005500*                                    01 = DEALER                          
005600*                                                                         
005700*                                    02 = POLESTAR                        
005800*                                                                         
005900*                                    03 = LYNK                            
006000*                                                                         
006100*                                    04 = IMPORTER                        
006200*                                                                         
006300*                                    05 = INTERNAL CUSTOMER               
006400*                                                                         
006500*                                    06 = SALES COMPANY                   
006600*                                                                         
006700*                                    07 = SUPPLIER                        
006800*                                                                         
006900*                                    08 = REFILL                          
007000*                                                                         
007100*                                    09 = TRANSFER                        
007200*                                                                         
007300*                                    10 = EXTENDED REFILL                 
007400*                                                                         
007500*                                    11 = RETURNS                         
007600*                                                                         
007700*                                    12 = QUALITY RETURNS                 
007800*                                                                         
007900*                                    13 = SCRAPL                          
008000*                                                                         
008100*                                    14 = QUALITY SCRAP                   
008200*                                                                         
008300*                                    15 = MIXED STOCK                     
008400*                                                                         
008500*                                    16 = INTERNAL EXCHANGE ORDER         
008600*                                                                         
008700*                                    17 = EMBALLAGE                       
008800*                                                                         
008900     03 MOD-BEKUNDKAT        PIC X(10).                                   
009000*                                 TYP OF KUND I KUND TABELL               
009100*                                    XX = REFILL                          
009200*                                    XX = SCRAP                           
009300*                                    XX = DEALER                          
009400     03 MOD-ADCITY-ATTR      PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600     03 MOD-ADCITY           PIC X(25).                                   
009700*                                 BENÄMNING PÅ STAD                       
009800     03 MOD-IDLONGITUDE-ATTR PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000     03 MOD-IDLONGITUDE      PIC X(10).                                   
010100*                                 LONGITUDE                               
010200     03 MOD-ADGMT-LAND-ATTR  PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400     03 MOD-ADGMT-LAND       PIC X(35).                                   
010500*                                 GODSMOTTAGARADRESS LAND                 
010600     03 MOD-IDLATITUDE-ATTR  PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800     03 MOD-IDLATITUDE       PIC X(10).                                   
010900*                                 LATITUDE                                
011000     03 MOD-IDTFN-ATTR       PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200     03 MOD-IDTFN            PIC X(20).                                   
011300*                                 TELEFONNUMMER EXTERNT                   
011400     03 MOD-BETEXT-ATTR      PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600     03 MOD-BETEXT           PIC X(35).                                   
011700     03 MOD-FLRESTN-ATTR     PIC X(2).                                    
011800*                                 MFS ATTRIBUTFÄLT                        
011900     03 MOD-FLRESTN          PIC X.                                       
012000*                                 RESTNOTERING ?                          
012100     03 MOD-IDZON            PIC X(2).                                    
012200*                                 TRANSPORTVÄG (RUTT,ZON)                 
012300     03 MOD-FLPRELRO-ATTR    PIC X(2).                                    
012400*                                 MFS ATTRIBUTFÄLT                        
012500     03 MOD-FLPRELRO         PIC X.                                       
012600*                                 PRELIMINÄR RESTORDERFLAGGA              
012700     03 MOD-IDDEPOT          PIC X(2).                                    
012800*                                 TRANSPORT DEPOT                         
012900     03 MOD-RESLATT-UT       PIC Z9.                                      
013000*                                 SLATTGRÄNS                              
013100     03 MOD-RESLATT-IN-ATTR  PIC X(2).                                    
013200*                                 MFS ATTRIBUTFÄLT                        
013300     03 MOD-RESLATT-IN       PIC Z9.                                      
013400*                                 SLATTGRÄNS                              
013500     03 MOD-IDROUTE          PIC X.                                       
013600*                                 TRANSPORT ROUTE                         
013700     03 MOD-IDKUNDNR-H-UT    PIC Z(5)9.                                   
013800*                                 KUNDNR. TILL GMT:S HUVUDKONTOR          
013900     03 MOD-IDKUNDNR-H-IN-ATTR                                            
014000                             PIC X(2).                                    
014100*                                 MFS ATTRIBUTFÄLT                        
014200     03 MOD-IDKUNDNR-H-IN    PIC X(6).                                    
014300*                                 KUNDNR. TILL GMT:S HUVUDKONTOR          
014400     03 MOD-KDBEKALT-UT      PIC 9.                                       
014500*                                 ORDERBEKRÄFTELSEALTERNATIV              
014600     03 MOD-KDBEKALT-IN-ATTR PIC X(2).                                    
014700*                                 MFS ATTRIBUTFÄLT                        
014800     03 MOD-KDBEKALT-IN      PIC X.                                       
014900*                                 ORDERBEKRÄFTELSEALTERNATIV              
015000     03 MOD-FLOBKR-TACD-UT   PIC X.                                       
015100*                                 ORDERBEKRÄFTELSE TILL TACDIS            
015200     03 MOD-FLOBKR-TACD-IN-ATTR                                           
015300                             PIC X(2).                                    
015400*                                 MFS ATTRIBUTFÄLT                        
015500     03 MOD-FLOBKR-TACD-IN   PIC X.                                       
015600*                                 ORDERBEKRÄFTELSE TILL TACDIS            
015700     03 MOD-FLDNDAP-UT       PIC X.                                       
015800*                                 FLAGGA DNOT VIA D&P-KUND                
015900     03 MOD-FLDNDAP-IN-ATTR  PIC X(2).                                    
016000*                                 MFS ATTRIBUTFÄLT                        
016100     03 MOD-FLDNDAP-IN       PIC X.                                       
016200*                                 FLAGGA DNOT VIA D&P-KUND                
016300     03 MOD-FLORDTIL-KL1-ATTR                                             
016400                             PIC X(2).                                    
016500*                                 MFS ATTRIBUTFÄLT                        
016600     03 MOD-FLORDTIL-KL1     PIC X.                                       
016700*                                 TVINGANDE TILÄGG ORDER KLASS 1          
016800     03 MOD-FLORDTIL-KL2-ATTR                                             
016900                             PIC X(2).                                    
017000*                                 MFS ATTRIBUTFÄLT                        
017100     03 MOD-FLORDTIL-KL2     PIC X.                                       
017200*                                 TVINGANDE TILÄGG ORDER KLASS 2          
017300     03 MOD-FLORDTIL-KL3-ATTR                                             
017400                             PIC X(2).                                    
017500*                                 MFS ATTRIBUTFÄLT                        
017600     03 MOD-FLORDTIL-KL3     PIC X.                                       
017700*                                 TVINGANDE TILÄGG ORDER KLASS 3          
017800     03 MOD-FLORDTIL-KL4-ATTR                                             
017900                             PIC X(2).                                    
018000*                                 MFS ATTRIBUTFÄLT                        
018100     03 MOD-FLORDTIL-KL4     PIC X.                                       
018200*                                 TVINGANDE TILÄGG ORDER KLASS 4          
018300     03 MOD-TEMFSINF         PIC X(55).                                   
018400*                                 INFORMATIONSMEDDELANDE                  
018500*** END OF VILMAII-COPY LENGTH= 490 BYTES                                 
