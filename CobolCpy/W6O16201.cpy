000100 01  MOD-W6O16201.                                                        
000200*                                 MOD TILL FRÅGA/UPPDATERING AV           
000300*                                 BUFFERSALDOBASEN (WDD8)                 
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-KVBUFF-IN        PIC X(7).                                    
001100*                                 FÖRÄDLAT BUFFERSALDO                    
001200     03 MOD-ADBUFFOMR-IN     PIC X(2).                                    
001300*                                 BUFFERTOMRÅDE                           
001400     03 MOD-ADBUFFGANG-IN    PIC X(2).                                    
001500*                                 BUFFERT GÅNG                            
001600     03 MOD-ADBUFFPL-IN      PIC X(5).                                    
001700*                                 BUFFERPLATSNUMMER                       
001800     03 MOD-IDARTNR-UT       PIC X(9).                                    
001900*                                 ARTIKELNUMMER                           
002000     03 MOD-KVBUFF-UT        PIC X(7).                                    
002100*                                 FÖRÄDLAT BUFFERSALDO                    
002200     03 MOD-ADBUFFOMR-UT     PIC X(2).                                    
002300*                                 BUFFERTOMRÅDE                           
002400     03 MOD-ADBUFFGANG-UT    PIC X(2).                                    
002500*                                 BUFFERT GÅNG                            
002600     03 MOD-ADBUFFPL-UT      PIC X(5).                                    
002700*                                 BUFFERPLATSNUMMER                       
002800     03 MOD-ADLAGOMR         PIC Z9.                                      
002900*                                 LAGEROMRÅDE                             
003000     03 MOD-ADGANG           PIC Z9.                                      
003100*                                 GÅNG                                    
003200     03 MOD-ADPLATS          PIC Z(4)9.                                   
003300*                                 LAGERPLATSNUMMER                        
003400     03 MOD-FLERPL           PIC X.                                       
003500     03 MOD-BEART            PIC X(25).                                   
003600*                                 ARTIKELBENÄMNING                        
003700     03 MOD-KVLS             PIC -(7)9.                                   
003800*                                 LAGERSALDO                              
003900     03 MOD-KVQPACK-3        PIC Z(4)9.                                   
004000*                                 ANTAL I Q3 FÖRPACKNING                  
004100     03 MOD-KDBRIST-IN-ATTR  PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-KDBRIST-IN       PIC X.                                       
004400*                                 BRIST KOD                               
004500     03 MOD-KDBRIST-UT       PIC X.                                       
004600*                                 BRIST KOD                               
004700     03 MOD-KDPAF-IN-ATTR    PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-KDPAF-IN         PIC X.                                       
005000*                                 PÅFYLLNADSKOD                           
005100     03 MOD-KDPAF-UT         PIC X.                                       
005200*                                 PÅFYLLNADSKOD                           
005300     03 MOD-MOD-LEDTEXT      PIC X(28).                                   
005400     03 MOD-IDARTNR          PIC Z(8)9.                                   
005500*                                 ARTIKELNUMMER                           
005600     03 MOD-RAD              OCCURS 7 TIMES.                              
005700        05 MOD-KDCMDVAL-ATTR PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 MOD-KDCMDVAL      PIC X(3).                                    
006000*                                 GENERELL KOMMANDOKOD                    
006100        05 MOD-ADBUFFOMR     PIC Z9.                                      
006200*                                 BUFFERTOMRÅDE                           
006300        05 MOD-ADBUFFGANG    PIC Z9.                                      
006400*                                 BUFFERT GÅNG                            
006500        05 MOD-ADBUFFPL      PIC Z(4)9.                                   
006600*                                 BUFFERPLATSNUMMER                       
006700        05 MOD-DABUFPAF-ATTR PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900        05 MOD-DABUFPAF      PIC 9(8).                                    
007000*                                 BUFFERT PÅFYLLNINGS DATUM               
007100        05 MOD-KVBUFF-F      PIC -(6)9.                                   
007200*                                 FÖRÄDLAT BUFFERSALDO                    
007300        05 MOD-KVBUFF-F-IN-ATTR                                           
007400                             PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600        05 MOD-KVBUFF-F-IN   PIC X(7).                                    
007700*                                 FÖRÄDLAT BUFFERSALDO                    
007800        05 MOD-KVBUFF-OF     PIC -(7)9.                                   
007900*                                 BUFFERSALDO OFÖRÄDLAT GODS              
008000        05 MOD-KVBUFF-OF-IN-ATTR                                          
008100                             PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300        05 MOD-KVBUFF-OF-IN  PIC X(7).                                    
008400*                                 BUFFERSALDO OFÖRÄDLAT GODS              
008500        05 MOD-KVKOLLI-F     PIC Z(3)9.                                   
008600*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
008700        05 MOD-KVKOLLI-F-IN-ATTR                                          
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000        05 MOD-KVKOLLI-F-IN  PIC X(4).                                    
009100*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
009200        05 MOD-KVKOLLI-OF    PIC Z(3)9.                                   
009300*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
009400        05 MOD-KVKOLLI-OF-IN-ATTR                                         
009500                             PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700        05 MOD-KVKOLLI-OF-IN PIC X(4).                                    
009800*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
009900     03 MOD-SUMMA-RAD.                                                    
010000        05 MOD-SUBUFF-F      PIC -(7)9.                                   
010100*                                 FÖRÄDLAT BUFFERSALDO                    
010200        05 MOD-SUBUFF-OF     PIC -(7)9.                                   
010300*                                 BUFFERSALDO OFÖRÄDLAT GODS              
010400        05 MOD-SUKOLLI-F     PIC Z(3)9.                                   
010500*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
010600        05 MOD-SUKOLLI-OF    PIC Z(3)9.                                   
010700*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
010800     03 MOD-INPUT-RAD.                                                    
010900        05 MOD-ADBUFFOMR-UPD-ATTR                                         
011000                             PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200        05 MOD-ADBUFFOMR-UPD PIC X(2).                                    
011300*                                 BUFFERTOMRÅDE                           
011400        05 MOD-ADBUFFGANG-UPD-ATTR                                        
011500                             PIC X(2).                                    
011600*                                 MFS ATTRIBUTFÄLT                        
011700        05 MOD-ADBUFFGANG-UPD                                             
011800                             PIC X(2).                                    
011900*                                 BUFFERT GÅNG                            
012000        05 MOD-ADBUFFPL-UPD-ATTR                                          
012100                             PIC X(2).                                    
012200*                                 MFS ATTRIBUTFÄLT                        
012300        05 MOD-ADBUFFPL-UPD  PIC X(5).                                    
012400*                                 BUFFERPLATSNUMMER                       
012500        05 MOD-KVBUFF-F-UPD-ATTR                                          
012600                             PIC X(2).                                    
012700*                                 MFS ATTRIBUTFÄLT                        
012800        05 MOD-KVBUFF-F-UPD  PIC X(7).                                    
012900*                                 FÖRÄDLAT BUFFERSALDO                    
013000        05 MOD-KVBUFF-OF-UPD-ATTR                                         
013100                             PIC X(2).                                    
013200*                                 MFS ATTRIBUTFÄLT                        
013300        05 MOD-KVBUFF-OF-UPD PIC X(7).                                    
013400*                                 BUFFERSALDO OFÖRÄDLAT GODS              
013500        05 MOD-KVKOLLI-F-UPD-ATTR                                         
013600                             PIC X(2).                                    
013700*                                 MFS ATTRIBUTFÄLT                        
013800        05 MOD-KVKOLLI-F-UPD PIC X(4).                                    
013900*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
014000        05 MOD-KVKOLLI-OF-UPD-ATTR                                        
014100                             PIC X(2).                                    
014200*                                 MFS ATTRIBUTFÄLT                        
014300        05 MOD-KVKOLLI-OF-UPD                                             
014400                             PIC X(4).                                    
014500*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
014600        05 MOD-KVANTAL-UPD-ATTR                                           
014700                             PIC X(2).                                    
014800*                                 MFS ATTRIBUTFÄLT                        
014900        05 MOD-KVANTAL-UPD   PIC Z(5)9.                                   
015000*                                 ANTAL                                   
015100        05 MOD-TREATED-UPD-ATTR                                           
015200                             PIC X(2).                                    
015300*                                 MFS ATTRIBUTFÄLT                        
015400        05 MOD-TREATED-UPD   PIC X.                                       
015500        05 MOD-LOCATION-UPD-ATTR                                          
015600                             PIC X(2).                                    
015700*                                 MFS ATTRIBUTFÄLT                        
015800        05 MOD-LOCATION-UPD  PIC X(10).                                   
015900     03 MOD-TEMFSINF         PIC X(55).                                   
016000*                                 INFORMATIONSMEDDELANDE                  
016100*** END OF VILMAII-COPY LENGTH= 873 BYTES                                 
