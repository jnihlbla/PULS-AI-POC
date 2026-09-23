000100 01  MOD-W2O34201.                                                        
000200*                                 MOD-COPYTEXT FÖR W2034200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDARTNR-IN       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDARTNR-UT       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MOD-IDDISTR          PIC Z(3)9.                                   
001600*                                 DISTRIKTNUMMER                          
001700     03 MOD-GEMINFO.                                                      
001800        05 MOD-BEART-SVE     PIC X(25).                                   
001900*                                 SVENSK ARTIKELBENÄMNING                 
002000        05 MOD-IDANSK        PIC Z(2)9.                                   
002100*                                 ANSKAFFARNUMMER                         
002200        05 MOD-PRICE-TEXT    PIC X(4).                                    
002300        05 MOD-PRARTSTD      PIC Z(6)9.9(2).                              
002400*                                 ARTIKELSTANDARDPRIS                     
002500        05 MOD-KVQPACK-1     PIC Z(4)9.                                   
002600*                                 ANTAL I Q1 FÖRPACKNING                  
002700        05 MOD-KDERS         PIC Z9.                                      
002800*                                 ERSÄTTNINGSKOD                          
002900        05 MOD-FLERS         PIC X.                                       
003000*                                 TILLKOMMANDE ARTIKEL ?                  
003100        05 MOD-TIFINLV       PIC Z(4)9.                                   
003200*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
003300        05 MOD-IDDC-REF      PIC X(2).                                    
003400*                                 SÄNDANDE LAGER FÖR REFILL               
003500     03 MOD-C1-INFO.                                                      
003600        05 MOD-KVDISP        PIC -(7)9.                                   
003700*                                 DISPONIBELT LAGER                       
003800        05 MOD-KVAKS         PIC -(7)9.                                   
003900*                                 ANKOMSTSALDO                            
004000        05 MOD-KVART-FORAVIS PIC -(7)9.                                   
004100*                                 ANKOMSTSALDO                            
004200        05 MOD-KVPB-TOT      PIC Z(9)9.9.                                 
004300*                                 TOTALT PERIODBEHOV                      
004400        05 MOD-KVQPACK-3     PIC Z(4)9.                                   
004500*                                 ANTAL I Q3 FÖRPACKNING                  
004600     03 MOD-SL-INFO.                                                      
004700        05 MOD-KVLS          PIC -(7)9.                                   
004800*                                 LAGERSALDO                              
004900        05 MOD-KVAKS-S       PIC -(7)9.                                   
005000*                                 ANKOMSTSALDO                            
005100        05 MOD-KVBEART       PIC -(5)9.                                   
005200*                                 BESTÄLLT ANTAL STYCKEN                  
005300        05 MOD-KVPB-REF      PIC Z(5)9.9.                                 
005400*                                 PERIODBEHOV REFILLING                   
005500        05 MOD-FC-TEXT       PIC X(5).                                    
005600        05 MOD-KVPBREOI      PIC Z(5)9.9.                                 
005700*                                 PERIODBEHOV FÖR REFILL OI               
005800        05 MOD-KVREFBER      PIC Z(6)9.                                   
005900*                                 BERÄKNAD REFILLINGKVANTITET             
006000     03 MOD-OI-FORE          OCCURS 2 TIMES.                              
006100        05 MOD-TIAA-FORE     PIC X(4).                                    
006200        05 MOD-KVOT-FORE     PIC -(5)9.                                   
006300*                                 ANTAL ORDERTRÄFF                        
006400        05 MOD-KVOT-CDC-FORE PIC Z(6)9.                                   
006500*                                 ORDERTRÄFFAR LEV FRÅN CDC               
006600        05 MOD-KVOI-FORE     PIC -(5)9.                                   
006700*                                 ORDERINGÅNG I STYCK PER TIDSENH         
006800        05 MOD-KVOI-REFILL-FORE                                           
006900                             PIC -(5)9.                                   
007000*                                 ORDERINGÅNG LEV FRÅN REFILL             
007100     03 MOD-OI-RULL-12.                                                   
007200        05 MOD-KVOT-RULL-12  PIC -(5)9.                                   
007300*                                 ANTAL ORDERTRÄFF                        
007400        05 MOD-KVOT-CDC-RULL-12                                           
007500                             PIC Z(6)9.                                   
007600*                                 ORDERTRÄFFAR LEV FRÅN CDC               
007700        05 MOD-KVOI-RULL-12  PIC -(5)9.                                   
007800*                                 ORDERINGÅNG I STYCK PER TIDSENH         
007900        05 MOD-KVOI-REF-RULL-12                                           
008000                             PIC -(5)9.                                   
008100*                                 ORDERINGÅNG LEV FRÅN REFILL             
008200     03 MOD-OI-RULL-6.                                                    
008300        05 MOD-KVOT-RULL-6   PIC -(5)9.                                   
008400*                                 ANTAL ORDERTRÄFF                        
008500        05 MOD-KVOT-CDC-RULL-6                                            
008600                             PIC Z(6)9.                                   
008700*                                 ORDERTRÄFFAR LEV FRÅN CDC               
008800        05 MOD-KVOI-RULL-6   PIC -(5)9.                                   
008900*                                 ORDERINGÅNG I STYCK PER TIDSENH         
009000        05 MOD-KVOI-REF-RULL-6                                            
009100                             PIC -(5)9.                                   
009200*                                 ORDERINGÅNG LEV FRÅN REFILL             
009300     03 MOD-OI-PER           OCCURS 6 TIMES.                              
009400        05 MOD-TIPP          PIC X(2).                                    
009500*                                 PLANERINGSPERIOD (PP)                   
009600*                                 12 PER ÅR                               
009700        05 MOD-TIVV-FOM-TOM  PIC X(5).                                    
009800        05 MOD-KVOT          PIC -(5)9.                                   
009900*                                 ANTAL ORDERTRÄFF                        
010000        05 MOD-KVOT-CDC      PIC Z(6)9.                                   
010100*                                 ORDERTRÄFFAR LEV FRÅN CDC               
010200        05 MOD-KVOI          PIC -(5)9.                                   
010300*                                 ORDERINGÅNG I STYCK PER TIDSENH         
010400        05 MOD-KVOI-REFILL   PIC -(5)9.                                   
010500*                                 ORDERINGÅNG LEV FRÅN REFILL             
010600     03 MOD-DAREFESC         PIC 9(6).                                    
010700*                                 DATUM ESCLÅSTPROGNOS REFILLING          
010800     03 MOD-DAREFESC-REOI    PIC 9(6).                                    
010900*                                 DATUM ESCLÅSTPROGNOS KVPBREOI           
011000     03 MOD-OI-INN.                                                       
011100        05 MOD-KVOT-INNEV    PIC -(5)9.                                   
011200*                                 ANTAL ORDERTRÄFF                        
011300        05 MOD-KVOT-CDC-INNEV                                             
011400                             PIC Z(6)9.                                   
011500*                                 ORDERTRÄFFAR LEV FRÅN CDC               
011600        05 MOD-KVOI-INNEV    PIC -(5)9.                                   
011700*                                 ORDERINGÅNG I STYCK PER TIDSENH         
011800        05 MOD-KVOI-REF-INNEV                                             
011900                             PIC -(5)9.                                   
012000*                                 ORDERINGÅNG LEV FRÅN REF-INNEV          
012100     03 MOD-TIREFMPB         PIC 9(6).                                    
012200*                                 DATUM MANUELL PROGNOS REFILLING         
012300     03 MOD-TIREFMPB-IN-ATTR PIC X(2).                                    
012400*                                 MFS ATTRIBUTFÄLT                        
012500     03 MOD-TIREFMPB-IN      PIC 9(6).                                    
012600*                                 DATUM MANUELL PROGNOS REFILLING         
012700     03 MOD-TIPBREOI         PIC 9(6).                                    
012800*                                 DATUM MAN.PB REFILL OI (ÅÅMMDD)         
012900     03 MOD-TIPBREOI-IN-ATTR PIC X(2).                                    
013000*                                 MFS ATTRIBUTFÄLT                        
013100     03 MOD-TIPBREOI-IN      PIC 9(6).                                    
013200*                                 DATUM MAN.PB REFILL OI (ÅÅMMDD)         
013300     03 MOD-TEMFSINF         PIC X(55).                                   
013400*                                 INFORMATIONSMEDDELANDE                  
013500*** END OF VILMAII-COPY LENGTH= 638 BYTES                                 
