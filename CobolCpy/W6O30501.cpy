000100 01  MOD-W6O30501.                                                        
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
002800     03 MOD-IDDC             PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000     03 MOD-ADLAGOMR         PIC Z9.                                      
003100*                                 LAGEROMRÅDE                             
003200     03 MOD-ADGANG           PIC Z9.                                      
003300*                                 GÅNG                                    
003400     03 MOD-ADPLATS          PIC Z(4)9.                                   
003500*                                 LAGERPLATSNUMMER                        
003600     03 MOD-BEART            PIC X(25).                                   
003700*                                 ARTIKELBENÄMNING                        
003800     03 MOD-KVLS             PIC -(7)9.                                   
003900*                                 LAGERSALDO                              
004000     03 MOD-MOD-LEDTEXT      PIC X(28).                                   
004100     03 MOD-IDARTNR          PIC Z(8)9.                                   
004200*                                 ARTIKELNUMMER                           
004300     03 MOD-RAD              OCCURS 7 TIMES.                              
004400        05 MOD-KDCMDVAL-ATTR PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 MOD-KDCMDVAL      PIC X(3).                                    
004700*                                 GENERELL KOMMANDOKOD                    
004800        05 MOD-ADBUFFOMR     PIC Z9.                                      
004900*                                 BUFFERTOMRÅDE                           
005000        05 MOD-ADBUFFGANG    PIC Z9.                                      
005100*                                 BUFFERT GÅNG                            
005200        05 MOD-ADBUFFPL      PIC Z(4)9.                                   
005300*                                 BUFFERPLATSNUMMER                       
005600        05 MOD-DABUFPAF      PIC 9(8).                                    
005700*                                 BUFFERT PÅFYLLNINGS DATUM               
005800        05 MOD-KVBUFF-F      PIC -(7)9.                                   
005900*                                 FÖRÄDLAT BUFFERSALDO                    
006000        05 MOD-KVBUFF-F-IN-ATTR                                           
006100                             PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-KVBUFF-F-IN   PIC X(7).                                    
006400*                                 FÖRÄDLAT BUFFERSALDO                    
006500        05 MOD-KVKOLLI-F     PIC Z(3)9.                                   
006600*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
006700        05 MOD-KVKOLLI-F-IN-ATTR                                          
006800                             PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-KVKOLLI-F-IN  PIC X(4).                                    
007100*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
007200     03 MOD-SUMMA-RAD.                                                    
007300        05 MOD-SUBUFF-F      PIC -(7)9.                                   
007400*                                 FÖRÄDLAT BUFFERSALDO                    
007500        05 MOD-SUKOLLI-F     PIC Z(3)9.                                   
007600*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
007700     03 MOD-INPUT-RAD.                                                    
007800        05 MOD-ADBUFFOMR-UPD-ATTR                                         
007900                             PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100        05 MOD-ADBUFFOMR-UPD PIC X(2).                                    
008200*                                 BUFFERTOMRÅDE                           
008300        05 MOD-ADBUFFGANG-UPD-ATTR                                        
008400                             PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-ADBUFFGANG-UPD                                             
008700                             PIC X(2).                                    
008800*                                 BUFFERT GÅNG                            
008900        05 MOD-ADBUFFPL-UPD-ATTR                                          
009000                             PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200        05 MOD-ADBUFFPL-UPD  PIC X(5).                                    
009300*                                 BUFFERPLATSNUMMER                       
009400        05 MOD-KVBUFF-F-UPD-ATTR                                          
009500                             PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700        05 MOD-KVBUFF-F-UPD  PIC X(7).                                    
009800*                                 FÖRÄDLAT BUFFERSALDO                    
009900        05 MOD-KVKOLLI-F-UPD-ATTR                                         
010000                             PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200        05 MOD-KVKOLLI-F-UPD PIC X(4).                                    
010300*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
010400     03 MOD-TEMFSINF         PIC X(55).                                   
010500*                                 INFORMATIONSMEDDELANDE                  
010400*** END OF VILMAII-COPY LENGTH= 615 BYTES                                 
