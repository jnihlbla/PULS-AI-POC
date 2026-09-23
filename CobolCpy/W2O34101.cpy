000100 01  MOD-W2O34101.                                                        
000200*                                 MOD-COPYTEXT FÖR W2034100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDDC-IN          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDARTNR-UT       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDDISTR          PIC Z(3)9.                                   
001600*                                 DISTRIKTNUMMER                          
001700     03 MOD-BEART            PIC X(25).                                   
001800*                                 ARTIKELBENÄMNING                        
001900     03 MOD-IDANSK           PIC Z(2)9.                                   
002000*                                 ANSKAFFARNUMMER                         
002100     03 MOD-PRICE-TEXT       PIC X(4).                                    
002200     03 MOD-PRARTSTD         PIC Z(6)9.9(2).                              
002300*                                 ARTIKELSTANDARDPRIS                     
002400     03 MOD-REPPFAKT         PIC 9.9(2).                                  
002500*                                 PREPLANNED FACTOR                       
002600     03 MOD-FLCDART          PIC X.                                       
002700*                                 CROSS-DOCKING PART                      
002800     03 MOD-ADLAGOMR         PIC Z9.                                      
002900*                                 LAGEROMRÅDE                             
003000     03 MOD-ADGANG           PIC Z9.                                      
003100*                                 GÅNG                                    
003200     03 MOD-ADPLATS          PIC Z(4)9.                                   
003300*                                 LAGERPLATSNUMMER                        
003400     03 MOD-KDERS            PIC Z9.                                      
003500*                                 ERSÄTTNINGSKOD                          
003600     03 MOD-KLASS            PIC X(3).                                    
003700     03 MOD-IDREFTAB-IN-ATTR PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-IDREFTAB-IN      PIC X(2).                                    
004000*                                 MFS BEHANDLING AV INPUTFÄLT             
004100     03 MOD-IDREFTAB         PIC X.                                       
004200*                                 IDENTITET REFILLTABELL                  
004300     03 MOD-FLWILSON-IN-ATTR PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-FLWILSON-IN      PIC X(2).                                    
004600*                                 MFS BEHANDLING AV INPUTFÄLT             
004700     03 MOD-FLWILSON         PIC X.                                       
004800*                                 WILSONFORMEL                            
004900     03 MOD-KVDISP           PIC -(7)9.                                   
005000*                                 DISPONIBELT LAGER                       
005100     03 MOD-TIORDREG         PIC 9(6).                                    
005200*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
005300     03 MOD-KVAKS-PAV        PIC -(7)9.                                   
005400*                                 DEL AV AK PÅ VÄG                        
005500     03 MOD-TIREFEFT         PIC 9(6).                                    
005600*                                 DATUM SENAST EFTERFRÅGAD                
005700     03 MOD-KVAKS-SDC        PIC -(7)9.                                   
005800*                                 DEL AV AK SOM LIGGER I SDC              
005900     03 MOD-KVREFPKT-IN-ATTR PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 MOD-KVREFPKT-IN      PIC Z(6)9.                                   
006200*                                 BERÄKNAD PÅFYLLNADSPUNKT                
006300     03 MOD-KVREFPKT-ATTR    PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500     03 MOD-KVREFPKT         PIC Z(6)9.                                   
006600*                                 BERÄKNAD PÅFYLLNADSPUNKT                
006700     03 MOD-KVBEART          PIC -(6)9.                                   
006800*                                 BESTÄLLT ANTAL STYCKEN                  
006900     03 MOD-TIREFPKT-IN-ATTR PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100     03 MOD-TIREFPKT-IN      PIC 9(6).                                    
007200*                                 DATUM MANUELL REFILLPUNKT               
007300     03 MOD-TIREFPKT         PIC 9(6).                                    
007400*                                 DATUM MANUELL REFILLPUNKT               
007500     03 MOD-FLREFILL-IN-ATTR PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700     03 MOD-FLREFILL-IN      PIC X.                                       
007800*                                 REFILLARTIKEL                           
007900     03 MOD-FLREFILL         PIC X.                                       
008000*                                 REFILLARTIKEL                           
008100     03 MOD-KVREFBER-IN-ATTR PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300     03 MOD-KVREFBER-IN      PIC Z(6)9.                                   
008400*                                 BERÄKNAD REFILLINGKVANTITET             
008500     03 MOD-KVREFBER         PIC Z(6)9.                                   
008600*                                 BERÄKNAD REFILLINGKVANTITET             
008700     03 MOD-FLREFBEO-IN-ATTR PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900     03 MOD-FLREFBEO-IN      PIC X.                                       
009000*                                 AUTOMATISK REFILL BEORDRING?            
009100     03 MOD-FLREFBEO         PIC X.                                       
009200*                                 AUTOMATISK REFILL BEORDRING?            
009300     03 MOD-TIREFPAF-IN-ATTR PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500     03 MOD-TIREFPAF-IN      PIC 9(6).                                    
009600*                                 DATUM MANUELL PÅFYLLNADSKVANT           
009700     03 MOD-TIREFPAF         PIC 9(6).                                    
009800*                                 DATUM MANUELL PÅFYLLNADSKVANT           
009900     03 MOD-KVREFOVL         PIC Z(6)9.                                   
010000*                                 BERÄKNAD ÖVERLAGERPUNKT                 
010100     03 MOD-TIREFSTO-IN-ATTR PIC X(2).                                    
010200*                                 MFS ATTRIBUTFÄLT                        
010300     03 MOD-TIREFSTO-IN      PIC X(6).                                    
010400*                                 BEORDRINGSSTOPPAD T.OM.                 
010500     03 MOD-TIREFSTO         PIC 9(6).                                    
010600*                                 BEORDRINGSSTOPPAD T.OM.                 
010700     03 MOD-LEDTID           PIC Z(2)9.                                   
010800*                                 ANTAL DAGAR                             
010900     03 MOD-PB-JUST1.                                                     
011000*                                                                         
011100        05 MOD-IN-KVPB-JUST-1-ATTR                                        
011200                             PIC X(2).                                    
011300*                                 MFS ATTRIBUTFÄLT                        
011400        05 MOD-IN-KVPB-JUST-1                                             
011500                             PIC X(8).                                    
011600*                                 PERIODBEHOVSJUSTERING                   
011700        05 MOD-IN-TIPBJUST-1-ATTR                                         
011800                             PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000        05 MOD-IN-TIPBJUST-1 PIC 9(4).                                    
012100*                                 DATUM FÖR PB-JUSTERING (ÅÅVV)           
012200        05 MOD-KVPB-JUST-1   PIC Z(5)9.9.                                 
012300*                                 PERIODBEHOVSJUSTERING                   
012400        05 MOD-TIPBJUST-1    PIC 9(4).                                    
012500*                                 DATUM FÖR PB-JUSTERING (ÅÅVV)           
012600     03 MOD-KDREFSTA         PIC X.                                       
012700*                                 STATUS REFILLARTIKEL                    
012800     03 MOD-TIREFSTA         PIC 9(6).                                    
012900*                                 DATUM AKT/PASS REFILLARTIKEL            
013000     03 MOD-PB-JUST2.                                                     
013100*                                                                         
013200        05 MOD-IN-KVPB-JUST-2-ATTR                                        
013300                             PIC X(2).                                    
013400*                                 MFS ATTRIBUTFÄLT                        
013500        05 MOD-IN-KVPB-JUST-2                                             
013600                             PIC X(8).                                    
013700*                                 PERIODBEHOVSJUSTERING                   
013800        05 MOD-IN-TIPBJUST-2-ATTR                                         
013900                             PIC X(2).                                    
014000*                                 MFS ATTRIBUTFÄLT                        
014100        05 MOD-IN-TIPBJUST-2 PIC 9(4).                                    
014200*                                 DATUM FÖR PB-JUSTERING (ÅÅVV)           
014300        05 MOD-KVPB-JUST-2   PIC Z(5)9.9.                                 
014400*                                 PERIODBEHOVSJUSTERING                   
014500        05 MOD-TIPBJUST-2    PIC 9(4).                                    
014600*                                 DATUM FÖR PB-JUSTERING (ÅÅVV)           
014700     03 MOD-IDPERSON-BUY-IN-ATTR                                          
014800                             PIC X(2).                                    
014900*                                 MFS ATTRIBUTFÄLT                        
015000     03 MOD-IDPERSON-BUY-IN  PIC X(3).                                    
015100*                                 PERSONKOD REFILLANSVARIG                
015200     03 MOD-IDPERSON-BUY     PIC Z(2)9.                                   
015300*                                 PERSONKOD REFILLANSVARIG                
015400     03 MOD-FLBUYUPD-IN-ATTR PIC X(2).                                    
015500*                                 MFS ATTRIBUTFÄLT                        
015600     03 MOD-FLBUYUPD-IN      PIC X.                                       
015700*                                 OM IDPERSONKOD ÄR LÅST                  
015800     03 MOD-FLBUYUPD         PIC X.                                       
015900*                                 OM IDPERSONKOD ÄR LÅST                  
016000     03 MOD-FLTABUPD-IN-ATTR PIC X(2).                                    
016100*                                 MFS ATTRIBUTFÄLT                        
016200     03 MOD-FLTABUPD-IN      PIC X.                                       
016300*                                 OM REFILLTABELL ÄR LÅST                 
016400     03 MOD-FLTABUPD         PIC X.                                       
016500*                                 OM REFILLTABELL ÄR LÅST                 
016600     03 MOD-PB-TEXT          PIC X(7).                                    
016700     03 MOD-KVPB-SUM         PIC Z(5)9.9.                                 
016800     03 MOD-KVPB-REF         PIC Z(5)9.9.                                 
016900*                                 PERIODBEHOV REFILLING                   
017000     03 MOD-KVPBREOI         PIC Z(5)9.9.                                 
017100*                                 PERIODBEHOV FÖR REFILL OI               
017200     03 MOD-FLPB-FLYTT-IN-ATTR                                            
017300                             PIC X(2).                                    
017400*                                 MFS ATTRIBUTFÄLT                        
017500     03 MOD-FLPB-FLYTT-IN    PIC X.                                       
017600*                                 FLAGGA VID ERSÄTTNING FÖR HÅLLA         
017700*                                  REDA PÅ KOPIERING AV PROGNOS           
017800     03 MOD-FLPB-FLYTT       PIC X.                                       
017900*                                 FLAGGA VID ERSÄTTNING FÖR HÅLLA         
018000*                                  REDA PÅ KOPIERING AV PROGNOS           
018100     03 MOD-FLREFNYO-IN-ATTR PIC X(2).                                    
018200*                                 MFS ATTRIBUTFÄLT                        
018300     03 MOD-FLREFNYO-IN      PIC X.                                       
018400*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
018500     03 MOD-FLREFNYO         PIC X.                                       
018600*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
018700     03 MOD-TIREFMPB         PIC 9(6).                                    
018800*                                 DATUM MANUELL PROGNOS REFILLING         
018900     03 MOD-TIPBREOI         PIC 9(6).                                    
019000*                                 DATUM MAN.PB REFILL OI (ÅÅMMDD)         
019100     03 MOD-TEREFMED-ATTR    PIC X(2).                                    
019200*                                 MFS ATTRIBUTFÄLT                        
019300     03 MOD-TEREFMED         PIC X(72).                                   
019400     03 MOD-TEMFSINF         PIC X(55).                                   
019500*                                 INFORMATIONSMEDDELANDE                  
019600*** END OF VILMAII-COPY LENGTH= 537 BYTES                                 
