000100 01  MOD-W1O11801-CTX.                                                    
000200*                                 MOD COPYTEXT FÖR W1011800               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDPSN-DOLT       PIC 9(3).                                    
001200*                                 PROPER SHIPPING NAME                    
001300     03 MOD-BEART-S          PIC X(25).                                   
001400*                                 ARTIKELBENÄMNING                        
001500     03 MOD-KDARTHNT-H       PIC Z(2)9.                                   
001600*                                 HANTERINGSKOD HÖGRA DELEN               
001700     03 MOD-KDFARLIG         PIC 9.                                       
001800*                                 KOD FÖR FARLIGT GODS                    
001900     03 MOD-BEART-GB         PIC X(25).                                   
002000*                                 ARTIKELBENÄMNING                        
002100     03 MOD-FLIART           PIC X.                                       
002200*                                 ARTIKELN INGÅR I SATS                   
002300     03 MOD-KDERS            PIC Z9.                                      
002400*                                 ERSÄTTNINGSKOD                          
002500     03 MOD-IDRITN           PIC X(10).                                   
002600*                                 RITNINGSNUMMER                          
002700     03 MOD-FL-KH            PIC X.                                       
002800*                                 ALLMÄN FLAGGA                           
002900     03 MOD-FL-KR            PIC X.                                       
003000*                                 ALLMÄN FLAGGA                           
003100     03 MOD-KDFARG-UT-ATTR   PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-KDFARG-UT        PIC Z(3).                                    
003400*                                 FÄRG KOD FARLIGT GODS                   
003500     03 MOD-KDLACK-UT-ATTR   PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-KDLACK-UT        PIC X(2).                                    
003800*                                 LACKTYP                                 
003900     03 MOD-IDARTNR-RECEPT-UT-ATTR                                        
004000                             PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-IDARTNR-RECEPT-UT                                             
004300                             PIC Z(8)9.                                   
004400*                                 ARTIKELNUMMER FÖR KEMI                  
004500     03 MOD-VKFORSFG-UT-ATTR PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-VKFORSFG-UT      PIC Z(3)9.9(3).                              
004800*                                 FÖRSÄLJNINGSVIKT FARLIGT GODS           
004900     03 MOD-KDFARG-ATTR      PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-KDFARG-IN        PIC X(2).                                    
005200*                                 MFS BEHANDLING AV INPUTFÄLT             
005300     03 MOD-KDLACK-ATTR      PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-KDLACK-IN        PIC X(2).                                    
005600*                                 MFS BEHANDLING AV INPUTFÄLT             
005700     03 MOD-IDARTNR-RECEPT-ATTR                                           
005800                             PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-IDARTNR-RECEPT-IN                                             
006100                             PIC X(2).                                    
006200*                                 MFS BEHANDLING AV INPUTFÄLT             
006300     03 MOD-VKFORSFG-ATTR    PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500     03 MOD-VKFORSFG-IN      PIC X(2).                                    
006600*                                 MFS BEHANDLING AV INPUTFÄLT             
006700     03 MOD-FLVARINF-UT-ATTR PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 MOD-FLVARINF-UT      PIC X.                                       
007000*                                 VARU-INFO KEMISKA PRODUKTER             
007100     03 MOD-IDVARINF-UT-ATTR PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300     03 MOD-IDVARINF-UT      PIC X(4).                                    
007400*                                 ID VARUINFO KEMISKA PRODUKTER           
007500     03 MOD-KVNTOFG-UT-ATTR  PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700     03 MOD-KVNTOFG-UT       PIC Z9.9(3).                                 
007800*                                 NETTOINNEHÅLL FARLIGT GODS              
007900     03 MOD-KDSORT-KVNTOFG-UT-ATTR                                        
008000                             PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200     03 MOD-KDSORT-KVNTOFG-UT                                             
008300                             PIC X(2).                                    
008400*                                 SORT-KOD                                
008500     03 MOD-KVVOC-UT-ATTR    PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700     03 MOD-KVVOC-UT         PIC Z9.9(3).                                 
008800*                                 HALT AV LÖSNINGSMEDEL PER KILO          
008900     03 MOD-SUEQFG-UT-ATTR   PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100     03 MOD-SUEQFG-UT        PIC Z(2)9.9(4).                              
009200*                                 EQ-VÄRDE FARLIGT GODS                   
009300     03 MOD-FLVARINF-ATTR    PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500     03 MOD-FLVARINF-IN      PIC X(2).                                    
009600*                                 MFS BEHANDLING AV INPUTFÄLT             
009700     03 MOD-IDVARINF-ATTR    PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900     03 MOD-IDVARINF-IN      PIC X(2).                                    
010000*                                 MFS BEHANDLING AV INPUTFÄLT             
010100     03 MOD-KVNTOFG-ATTR     PIC X(2).                                    
010200*                                 MFS ATTRIBUTFÄLT                        
010300     03 MOD-KVNTOFG-IN       PIC X(2).                                    
010400*                                 MFS BEHANDLING AV INPUTFÄLT             
010500     03 MOD-KDSORT-KVNTOFG-ATTR                                           
010600                             PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800     03 MOD-KDSORT-KVNTOFG-IN                                             
010900                             PIC X(2).                                    
011000*                                 MFS BEHANDLING AV INPUTFÄLT             
011100     03 MOD-KVVOC-ATTR       PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300     03 MOD-KVVOC-IN         PIC X(2).                                    
011400*                                 MFS BEHANDLING AV INPUTFÄLT             
011500     03 MOD-SUEQFG-ATTR      PIC X(2).                                    
011600*                                 MFS ATTRIBUTFÄLT                        
011700     03 MOD-SUEQFG-IN        PIC X(2).                                    
011800*                                 MFS BEHANDLING AV INPUTFÄLT             
011900     03 MOD-FLVARINF-SDS-UT-ATTR                                          
012000                             PIC X(2).                                    
012100*                                 MFS ATTRIBUTFÄLT                        
012200     03 MOD-FLVARINF-SDS-UT  PIC X.                                       
012300*                                 SAFETY DATA SHEET                       
012400     03 MOD-IDVARINF-SDS-UT-ATTR                                          
012500                             PIC X(2).                                    
012600*                                 MFS ATTRIBUTFÄLT                        
012700     03 MOD-IDVARINF-SDS-UT  PIC X(4).                                    
012800*                                 ID SAFETY DATA SHEET                    
012900     03 MOD-VLFG-UT-ATTR     PIC X(2).                                    
013000*                                 MFS ATTRIBUTFÄLT                        
013100     03 MOD-VLFG-UT          PIC Z(3)9.9(3).                              
013200*                                 VOLYM FARLIGT GODS                      
013300     03 MOD-KDSORT-VLFG-UT-ATTR                                           
013400                             PIC X(2).                                    
013500*                                 MFS ATTRIBUTFÄLT                        
013600     03 MOD-KDSORT-VLFG-UT   PIC X(4).                                    
013700*                                 SORT-KOD VOLYM FARLIGT GODS             
013800     03 MOD-KVFLAMP-UT-ATTR  PIC X(2).                                    
013900*                                 MFS ATTRIBUTFÄLT                        
014000     03 MOD-KVFLAMP-UT       PIC -Z(3).                                   
014100*                                 FLAMPUNKT FÖR FARLIGT GODS              
014200     03 MOD-FLFROST-UT-ATTR  PIC X(2).                                    
014300*                                 MFS ATTRIBUTFÄLT                        
014400     03 MOD-FLFROST-UT       PIC X.                                       
014500*                                 FROSTKÄNSLIG                            
014600     03 MOD-FLVARINF-SDS-ATTR                                             
014700                             PIC X(2).                                    
014800*                                 MFS ATTRIBUTFÄLT                        
014900     03 MOD-FLVARINF-SDS-IN  PIC X(2).                                    
015000*                                 MFS BEHANDLING AV INPUTFÄLT             
015100     03 MOD-IDVARINF-SDS-ATTR                                             
015200                             PIC X(2).                                    
015300*                                 MFS ATTRIBUTFÄLT                        
015400     03 MOD-IDVARINF-SDS-IN  PIC X(2).                                    
015500*                                 MFS BEHANDLING AV INPUTFÄLT             
015600     03 MOD-VLFG-ATTR        PIC X(2).                                    
015700*                                 MFS ATTRIBUTFÄLT                        
015800     03 MOD-VLFG-IN          PIC X(2).                                    
015900*                                 MFS BEHANDLING AV INPUTFÄLT             
016000     03 MOD-KDSORT-VLFG-ATTR PIC X(2).                                    
016100*                                 MFS ATTRIBUTFÄLT                        
016200     03 MOD-KDSORT-VLFG-IN   PIC X(4).                                    
016300*                                 SORT-KOD VOLYM FARLIGT GODS             
016400     03 MOD-FLNEG-ATTR       PIC X(2).                                    
016500*                                 MFS ATTRIBUTFÄLT                        
016600     03 MOD-FLNEG-IN         PIC X(2).                                    
016700*                                 MFS BEHANDLING AV INPUTFÄLT             
016800     03 MOD-KVFLAMP-ATTR     PIC X(2).                                    
016900*                                 MFS ATTRIBUTFÄLT                        
017000     03 MOD-KVFLAMP-IN       PIC X(2).                                    
017100*                                 MFS BEHANDLING AV INPUTFÄLT             
017200     03 MOD-FLFROST-ATTR     PIC X(2).                                    
017300*                                 MFS ATTRIBUTFÄLT                        
017400     03 MOD-FLFROST-IN       PIC X(2).                                    
017500*                                 MFS BEHANDLING AV INPUTFÄLT             
017600     03 MOD-IDPSN-UT-ATTR    PIC X(2).                                    
017700*                                 MFS ATTRIBUTFÄLT                        
017800     03 MOD-IDPSN-UT         PIC Z(2)9.                                   
017900*                                 PROPER SHIPPING NAME                    
018000     03 MOD-IDAO-FG-UT-ATTR  PIC X(2).                                    
018100*                                 MFS ATTRIBUTFÄLT                        
018200     03 MOD-IDAO-FG-UT       PIC X(10).                                   
018300*                                 ÄNDRINGSORDER NR FARLIGT GODS           
018400     03 MOD-KDFGPRIO-UT-ATTR PIC X(2).                                    
018500*                                 MFS ATTRIBUTFÄLT                        
018600     03 MOD-KDFGPRIO-UT      PIC Z(2)9.                                   
018700*                                 HANTERINGSPRIO FARLIGT GODS             
018800     03 MOD-FLTACTIL-UT-ATTR PIC X(2).                                    
018900*                                 MFS ATTRIBUTFÄLT                        
019000     03 MOD-FLTACTIL-UT      PIC X.                                       
019100*                                 VARNINGSMÄRKE FÖR SYNSKADADE            
019200     03 MOD-IDPSN-ATTR       PIC X(2).                                    
019300*                                 MFS ATTRIBUTFÄLT                        
019400     03 MOD-IDPSN-IN         PIC X(2).                                    
019500*                                 MFS BEHANDLING AV INPUTFÄLT             
019600     03 MOD-IDAO-FG-ATTR     PIC X(2).                                    
019700*                                 MFS ATTRIBUTFÄLT                        
019800     03 MOD-IDAO-FG-IN       PIC X(2).                                    
019900*                                 MFS BEHANDLING AV INPUTFÄLT             
020000     03 MOD-KDFGPRIO-ATTR    PIC X(2).                                    
020100*                                 MFS ATTRIBUTFÄLT                        
020200     03 MOD-KDFGPRIO-IN      PIC X(2).                                    
020300*                                 MFS BEHANDLING AV INPUTFÄLT             
020400     03 MOD-FLTACTIL-ATTR    PIC X(2).                                    
020500*                                 MFS ATTRIBUTFÄLT                        
020600     03 MOD-FLTACTIL-IN      PIC X(2).                                    
020700*                                 MFS BEHANDLING AV INPUTFÄLT             
020800     03 MOD-BEEMBMAT-UT-ATTR PIC X(2).                                    
020900*                                 MFS ATTRIBUTFÄLT                        
021000     03 MOD-BEEMBMAT-UT      PIC X(15).                                   
021100*                                 BENÄMNING EMBALLAGE MATERIAL            
021200     03 MOD-IDANMNR-UT-ATTR  PIC X(2).                                    
021300*                                 MFS ATTRIBUTFÄLT                        
021400     03 MOD-IDANMNR-UT       PIC Z(6).                                    
021500*                                 A-NUMMER                                
021600     03 MOD-REKSIFFR-UT-ATTR PIC X(2).                                    
021700*                                 MFS ATTRIBUTFÄLT                        
021800     03 MOD-REKSIFFR-UT      PIC 9.                                       
021900*                                 KONTR.SIFFRA A-NUMMER                   
022000     03 MOD-VKART-FG-UT-ATTR PIC X(2).                                    
022100*                                 MFS ATTRIBUTFÄLT                        
022200     03 MOD-VKART-FG-UT      PIC Z(6)9.                                   
022300*                                 NETTOVIKT EXPLOSIVA ÄMNEN               
022400     03 MOD-BEEMBMAT-ATTR    PIC X(2).                                    
022500*                                 MFS ATTRIBUTFÄLT                        
022600     03 MOD-BEEMBMAT-IN      PIC X(2).                                    
022700*                                 MFS BEHANDLING AV INPUTFÄLT             
022800     03 MOD-IDANMNR-ATTR     PIC X(2).                                    
022900*                                 MFS ATTRIBUTFÄLT                        
023000     03 MOD-IDANMNR-IN       PIC Z(6).                                    
023100*                                 A-NUMMER                                
023200     03 MOD-REKSIFFR-ATTR    PIC X(2).                                    
023300*                                 MFS ATTRIBUTFÄLT                        
023400     03 MOD-REKSIFFR-IN      PIC 9.                                       
023500*                                 KONTR.SIFFRA A-NUMMER                   
023600     03 MOD-VKART-FG-ATTR    PIC X(2).                                    
023700*                                 MFS ATTRIBUTFÄLT                        
023800     03 MOD-VKART-FG-IN      PIC X(2).                                    
023900*                                 MFS BEHANDLING AV INPUTFÄLT             
024000     03 MOD-W1O11801-001-GRP OCCURS 2 TIMES.                              
024100        05 MOD-TENOTE-ATTR   PIC X(2).                                    
024200*                                 MFS ATTRIBUTFÄLT                        
024300        05 MOD-TENOTE        PIC X(40).                                   
024400*                                 NOTERINGSFÄLT                           
024500     03 MOD-FLBORT-ATTR      PIC X(2).                                    
024600*                                 MFS ATTRIBUTFÄLT                        
024700     03 MOD-FLBORT           PIC X.                                       
024800*                                 ALLMÄN SVARSFLAGGA                      
024900     03 MOD-TEMFSINF         PIC X(55).                                   
025000*                                 INFORMATIONSMEDDELANDE                  
025100*** END OF VILMAII-COPY LENGTH= 539 BYTES                                 
