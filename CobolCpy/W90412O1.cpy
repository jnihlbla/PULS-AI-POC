000100 01  MOD-W90412O1-CTX.                                                    
000200*                                 MOD COPYTEXT FÖR W9041200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-FILLERX2         PIC X(2).                                    
000800     03 MOD-FILLERX9         PIC X(9).                                    
000900     03 MOD-FILLERN3         PIC 9(3).                                    
001000     03 MOD-FILLERX25        PIC X(25).                                   
001100     03 MOD-KDARTHNT-H       PIC 9(3).                                    
001200*                                 HANTERINGSKOD HÖGRA DELEN               
001300     03 MOD-FILLERN1         PIC 9.                                       
001400     03 MOD-FILLERX25        PIC X(25).                                   
001500     03 MOD-FILLERX1         PIC X.                                       
001600     03 MOD-FILLERN2         PIC 9(2).                                    
001700     03 MOD-FILLERX10        PIC X(10).                                   
001800     03 MOD-FILLERX1         PIC X.                                       
001900     03 MOD-FILLERX1         PIC X.                                       
002000     03 MOD-KDFARG-UT-ATTR   PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-KDFARG-UT        PIC Z(3).                                    
002300*                                 FÄRG KOD FARLIGT GODS                   
002400     03 MOD-KDLACK-UT-ATTR   PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 MOD-KDLACK-UT        PIC X(2).                                    
002700*                                 LACKTYP                                 
002800     03 MOD-IDARTNR-RECEPT-UT-ATTR                                        
002900                             PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 MOD-IDARTNR-RECEPT-UT                                             
003200                             PIC Z(8)9.                                   
003300*                                 ARTIKELNUMMER FÖR KEMI                  
003400     03 MOD-VKFORSFG-UT-ATTR PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-VKFORSFG-UT      PIC Z(3)9.9(3).                              
003700*                                 FÖRSÄLJNINGSVIKT FARLIGT GODS           
003800     03 MOD-KDFARG-ATTR      PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-FILLERX2         PIC X(2).                                    
004100     03 MOD-KDLACK-ATTR      PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-FILLERX2         PIC X(2).                                    
004400     03 MOD-IDARTNR-RECEPT-ATTR                                           
004500                             PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-FILLERX2         PIC X(2).                                    
004800     03 MOD-VKFORSFG-ATTR    PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-FILLERX2         PIC X(2).                                    
005100     03 MOD-FLVARINF-UT-ATTR PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-FLVARINF-UT      PIC X.                                       
005400*                                 VARU-INFO KEMISKA PRODUKTER             
005500     03 MOD-FILLERX2         PIC X(2).                                    
005600     03 MOD-IDVARINF-UT      PIC X(4).                                    
005700*                                 ID VARUINFO KEMISKA PRODUKTER           
005800     03 MOD-KVNTOFG-UT-ATTR  PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-KVNTOFG-UT       PIC Z9.9(3).                                 
006100*                                 NETTOINNEHÅLL FARLIGT GODS              
006200     03 MOD-KDSORT-KVNTOFG-UT-ATTR                                        
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500     03 MOD-KDSORT-KVNTOFG-UT                                             
006600                             PIC X(2).                                    
006700*                                 SORT-KOD                                
006800     03 MOD-KVVOC-UT-ATTR    PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000     03 MOD-KVVOC-UT         PIC Z9.9(3).                                 
007100*                                 HALT AV LÖSNINGSMEDEL PER KILO          
007200     03 MOD-SUEQFG-UT-ATTR   PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400     03 MOD-SUEQFG-UT        PIC Z(2)9.9(4).                              
007500*                                 EQ-VÄRDE FARLIGT GODS                   
007600     03 MOD-FLVARINF-ATTR    PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800     03 MOD-FILLERX2         PIC X(2).                                    
007900     03 MOD-IDVARINF-ATTR    PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100     03 MOD-FILLERX2         PIC X(2).                                    
008200     03 MOD-KVNTOFG-ATTR     PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400     03 MOD-FILLERX2         PIC X(2).                                    
008500     03 MOD-KDSORT-KVNTOFG-ATTR                                           
008600                             PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800     03 MOD-KDSORT-KVNTOFG-IN                                             
008900                             PIC X(2).                                    
009000*                                 MFS BEHANDLING AV INPUTFÄLT             
009100     03 MOD-KVVOC-ATTR       PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300     03 MOD-FILLERX2         PIC X(2).                                    
009400     03 MOD-SUEQFG-ATTR      PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600     03 MOD-FILLERX2         PIC X(2).                                    
009700     03 MOD-FLVARINF-SDS-UT-ATTR                                          
009800                             PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000     03 MOD-FLVARINF-SDS-UT  PIC X.                                       
010100*                                 SAFETY DATA SHEET                       
010200     03 MOD-IDVARINF-SDS-UT-ATTR                                          
010300                             PIC X(2).                                    
010400*                                 MFS ATTRIBUTFÄLT                        
010500     03 MOD-IDVARINF-SDS-UT  PIC X(4).                                    
010600*                                 ID SAFETY DATA SHEET                    
010700     03 MOD-VLFG-UT-ATTR     PIC X(2).                                    
010800*                                 MFS ATTRIBUTFÄLT                        
010900     03 MOD-VLFG-UT          PIC Z(3)9.9(3).                              
011000*                                 VOLYM FARLIGT GODS                      
011100     03 MOD-FILLERX2         PIC X(2).                                    
011200     03 MOD-KDSORT-VLFG-UT   PIC X(4).                                    
011300*                                 SORT-KOD VOLYM FARLIGT GODS             
011400     03 MOD-KVFLAMP-UT-ATTR  PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600     03 MOD-KVFLAMP-UT       PIC -Z(3).                                   
011700*                                 FLAMPUNKT FÖR FARLIGT GODS              
011800     03 MOD-FLFROST-UT-ATTR  PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000     03 MOD-FLFROST-UT       PIC X.                                       
012100*                                 FROSTKÄNSLIG                            
012200     03 MOD-FLVARINF-SDS-ATTR                                             
012300                             PIC X(2).                                    
012400*                                 MFS ATTRIBUTFÄLT                        
012500     03 MOD-FILLERX2         PIC X(2).                                    
012600     03 MOD-IDVARINF-SDS-ATTR                                             
012700                             PIC X(2).                                    
012800*                                 MFS ATTRIBUTFÄLT                        
012900     03 MOD-FILLERX2         PIC X(2).                                    
013000     03 MOD-VLFG-ATTR        PIC X(2).                                    
013100*                                 MFS ATTRIBUTFÄLT                        
013200     03 MOD-FILLERX2         PIC X(2).                                    
013300     03 MOD-KDSORT-VLFG-ATTR PIC X(2).                                    
013400*                                 MFS ATTRIBUTFÄLT                        
013500     03 MOD-FILLERX4         PIC X(4).                                    
013600     03 MOD-FLNEG-ATTR       PIC X(2).                                    
013700*                                 MFS ATTRIBUTFÄLT                        
013800     03 MOD-FILLERX2         PIC X(2).                                    
013900     03 MOD-KVFLAMP-ATTR     PIC X(2).                                    
014000*                                 MFS ATTRIBUTFÄLT                        
014100     03 MOD-FILLERX2         PIC X(2).                                    
014200     03 MOD-FLFROST-ATTR     PIC X(2).                                    
014300*                                 MFS ATTRIBUTFÄLT                        
014400     03 MOD-FILLERX2         PIC X(2).                                    
014500     03 MOD-IDPSN-UT-ATTR    PIC X(2).                                    
014600*                                 MFS ATTRIBUTFÄLT                        
014700     03 MOD-IDPSN-UT         PIC 9(3).                                    
014800*                                 PROPER SHIPPING NAME                    
014900     03 MOD-FILLERX2         PIC X(2).                                    
015000     03 MOD-IDPSN-IN-ATTR    PIC X(2).                                    
015100*                                 MFS ATTRIBUTFÄLT                        
015200     03 MOD-FILLERX2         PIC X(2).                                    
015300     03 MOD-IDAO-FG-UT       PIC X(10).                                   
015400*                                 ÄNDRINGSORDER NR FARLIGT GODS           
015500     03 MOD-IDAO-FG-UT-ATTR  PIC X(2).                                    
015600*                                 MFS ATTRIBUTFÄLT                        
015700     03 MOD-KDFGPRIO-UT      PIC Z(2)9.                                   
015800*                                 HANTERINGSPRIO FARLIGT GODS             
015900     03 MOD-KDFGPRIO-UT-ATTR PIC X(2).                                    
016000*                                 MFS ATTRIBUTFÄLT                        
016100     03 MOD-FLTACTIL-UT      PIC X.                                       
016200*                                 VARNINGSMÄRKE FÖR SYNSKADADE            
016300     03 MOD-FLTACTIL-UT-ATTR PIC X(2).                                    
016400*                                 MFS ATTRIBUTFÄLT                        
016500     03 MOD-FILLERX2         PIC X(2).                                    
016600     03 MOD-IDAO-FG-ATTR     PIC X(2).                                    
016700*                                 MFS ATTRIBUTFÄLT                        
016800     03 MOD-FILLERX2         PIC X(2).                                    
016900     03 MOD-KDFGPRIO-ATTR    PIC X(2).                                    
017000*                                 MFS ATTRIBUTFÄLT                        
017100     03 MOD-FILLERX2         PIC X(2).                                    
017200     03 MOD-FLTACTIL-ATTR    PIC X(2).                                    
017300*                                 MFS ATTRIBUTFÄLT                        
017400     03 MOD-FILLERX2         PIC X(2).                                    
017500     03 MOD-BEEMBMAT-UT-ATTR PIC X(2).                                    
017600*                                 MFS ATTRIBUTFÄLT                        
017700     03 MOD-BEEMBMAT-UT      PIC X(15).                                   
017800*                                 BENÄMNING EMBALLAGE MATERIAL            
017900     03 MOD-IDANMNR-UT-ATTR  PIC X(2).                                    
018000*                                 MFS ATTRIBUTFÄLT                        
018100     03 MOD-IDANMNR-UT       PIC Z(6).                                    
018200*                                 A-NUMMER                                
018300     03 MOD-FILLERX2         PIC X(2).                                    
018400     03 MOD-REKSIFFR-UT      PIC 9.                                       
018500*                                 KONTR.SIFFRA A-NUMMER                   
018600     03 MOD-VKART-FG-UT-ATTR PIC X(2).                                    
018700*                                 MFS ATTRIBUTFÄLT                        
018800     03 MOD-VKART-FG-UT      PIC Z(6)9.                                   
018900*                                 NETTOVIKT EXPLOSIVA ÄMNEN               
019000     03 MOD-BEEMBMAT-ATTR    PIC X(2).                                    
019100*                                 MFS ATTRIBUTFÄLT                        
019200     03 MOD-FILLERX2         PIC X(2).                                    
019300     03 MOD-IDANMNR-ATTR     PIC X(2).                                    
019400*                                 MFS ATTRIBUTFÄLT                        
019500     03 MOD-FILLERN6         PIC 9(6).                                    
019600     03 MOD-REKSIFFR-ATTR    PIC X(2).                                    
019700*                                 MFS ATTRIBUTFÄLT                        
019800     03 MOD-FILLERN1         PIC 9.                                       
019900     03 MOD-VKART-FG-ATTR    PIC X(2).                                    
020000*                                 MFS ATTRIBUTFÄLT                        
020100     03 MOD-FILLERX2         PIC X(2).                                    
020200     03 MOD-W90412O1-001-GRP OCCURS 2 TIMES.                              
020300        05 MOD-TENOTE-ATTR   PIC X(2).                                    
020400*                                 MFS ATTRIBUTFÄLT                        
020500        05 MOD-TENOTE        PIC X(40).                                   
020600*                                 NOTERINGSFÄLT                           
020700     03 MOD-FLBORT-ATTR      PIC X(2).                                    
020800*                                 MFS ATTRIBUTFÄLT                        
020900     03 MOD-FLBORT           PIC X.                                       
021000*                                 ALLMÄN SVARSFLAGGA                      
021100     03 MOD-TEMFSINF         PIC X(55).                                   
021200*                                 INFORMATIONSMEDDELANDE                  
021300*** END OF VILMAII-COPY LENGTH= 543 BYTES                                 
