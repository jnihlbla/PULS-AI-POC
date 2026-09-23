000100 01  MOD-W6O26201.                                                        
000200*                                 MOD TILL FRÅGA/UPPDATERING AV           
000300*                                 BUFFERSALDOBASEN (WDD8)                 
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-STRECK           PIC X.                                       
001300     03 MOD-REKSIFFR         PIC 9.                                       
001400*                                 KONTROLLSIFFRA                          
001500     03 MOD-KDBRIST-ATTR     PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-KDBRIST          PIC 9.                                       
001800*                                 BRIST KOD                               
001900     03 MOD-KDBRIST1-ATTR    PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-KDBRIST1         PIC 9.                                       
002200*                                 BRIST KOD                               
002300     03 MOD-KDPAF-ATTR       PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-KDPAF            PIC 9.                                       
002600*                                 PÅFYLLNADSKOD                           
002700     03 MOD-KDPAF1-ATTR      PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-KDPAF1           PIC 9.                                       
003000*                                 PÅFYLLNADSKOD                           
003100     03 MOD-BEART            PIC X(25).                                   
003200*                                 ARTIKELBENÄMNING                        
003300     03 MOD-ADRESSER         OCCURS 3 TIMES                               
003400                             INDEXED MOD-IX1.                             
003500        05 MOD-ADBUFFOMR-ATTR                                             
003600                             PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800        05 MOD-ADBUFFOMR     PIC Z9.                                      
003900*                                 BUFFERTOMRÅDE                           
004000        05 MOD-ADBUFFGANG-ATTR                                            
004100                             PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 MOD-ADBUFFGANG    PIC Z9.                                      
004400*                                 BUFFERT GÅNG                            
004500        05 MOD-ADBUFFPL-ATTR PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-ADBUFFPL      PIC Z(4)9.                                   
004800*                                 BUFFERPLATSNUMMER                       
004900     03 MOD-ADRESS           OCCURS 3 TIMES                               
005000                             INDEXED MOD-IX14.                            
005100        05 MOD-ADBUFFOMR1-ATTR                                            
005200                             PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-ADBUFFOMR1    PIC Z9.                                      
005500*                                 BUFFERTOMRÅDE                           
005600        05 MOD-ADBUFFGANG1-ATTR                                           
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 MOD-ADBUFFGANG1   PIC Z9.                                      
006000*                                 BUFFERT GÅNG                            
006100        05 MOD-ADBUFFPL1-ATTR                                             
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400        05 MOD-ADBUFFPL1     PIC Z(4)9.                                   
006500*                                 BUFFERPLATSNUMMER                       
006600     03 MOD-F-KVBUFF         OCCURS 3 TIMES                               
006700                             INDEXED MOD-IX2.                             
006800        05 MOD-KVBUFF-F-ATTR PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-KVBUFF-F      PIC Z(6)9.                                   
007100*                                 FÖRÄDLAT BUFFERSALDO                    
007200     03 MOD-KVBUFF-F-TOT     PIC -(7)9.                                   
007300*                                 FÖRÄDLAT BUFFERSALDO                    
007400     03 MOD-F-KVBUFF         OCCURS 3 TIMES                               
007500                             INDEXED MOD-IX3.                             
007600        05 MOD-KVBUFF-F-PLUS-ATTR                                         
007700                             PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900        05 MOD-KVBUFF-F-PLUS PIC Z(6)9.                                   
008000*                                 FÖRÄDLAT BUFFERSALDO                    
008100     03 MOD-F-KVBUFF-MINUS   OCCURS 3 TIMES                               
008200                             INDEXED MOD-IX4.                             
008300        05 MOD-KVBUFF-F-MINUS-ATTR                                        
008400                             PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-KVBUFF-F-MINUS                                             
008700                             PIC Z(6)9.                                   
008800*                                 FÖRÄDLAT BUFFERSALDO                    
008900     03 MOD-OF-KVBUFF        OCCURS 3 TIMES                               
009000                             INDEXED MOD-IX5.                             
009100        05 MOD-KVBUFF-OF-ATTR                                             
009200                             PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400        05 MOD-KVBUFF-OF     PIC Z(6)9.                                   
009500*                                 BUFFERSALDO OFÖRÄDLAT GODS              
009600     03 MOD-KVBUFF-OF-TOT    PIC -(7)9.                                   
009700*                                 BUFFERSALDO OFÖRÄDLAT GODS              
009800     03 MOD-OF-KVBUFF-PLUS   OCCURS 3 TIMES                               
009900                             INDEXED MOD-IX6.                             
010000        05 MOD-KVBUFF-OF-PLUS-ATTR                                        
010100                             PIC X(2).                                    
010200*                                 MFS ATTRIBUTFÄLT                        
010300        05 MOD-KVBUFF-OF-PLUS                                             
010400                             PIC Z(6)9.                                   
010500*                                 BUFFERSALDO OFÖRÄDLAT GODS              
010600     03 MOD-OF-KVBUFF-MINUS  OCCURS 3 TIMES                               
010700                             INDEXED MOD-IX7.                             
010800        05 MOD-KVBUFF-OF-MINUS-ATTR                                       
010900                             PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100        05 MOD-KVBUFF-OF-MINUS                                            
011200                             PIC Z(6)9.                                   
011300*                                 BUFFERSALDO OFÖRÄDLAT GODS              
011400     03 MOD-F-KVKOLLI        OCCURS 3 TIMES                               
011500                             INDEXED MOD-IX8.                             
011600        05 MOD-KVKOLLI-F-ATTR                                             
011700                             PIC X(2).                                    
011800*                                 MFS ATTRIBUTFÄLT                        
011900        05 MOD-KVKOLLI-F     PIC Z(3)9.                                   
012000*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
012100     03 MOD-KVKOLLI-F-TOT    PIC Z(3)9.                                   
012200*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
012300     03 MOD-F-KVKOLLI-PLUS   OCCURS 3 TIMES                               
012400                             INDEXED MOD-IX9.                             
012500        05 MOD-KVKOLLI-F-PLUS-ATTR                                        
012600                             PIC X(2).                                    
012700*                                 MFS ATTRIBUTFÄLT                        
012800        05 MOD-KVKOLLI-F-PLUS                                             
012900                             PIC Z(3)9.                                   
013000*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
013100     03 MOD-F-KVKOLLI-MINUS  OCCURS 3 TIMES                               
013200                             INDEXED MOD-IX10.                            
013300        05 MOD-KVKOLLI-F-MINUS-ATTR                                       
013400                             PIC X(2).                                    
013500*                                 MFS ATTRIBUTFÄLT                        
013600        05 MOD-KVKOLLI-F-MINUS                                            
013700                             PIC Z(3)9.                                   
013800*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
013900     03 MOD-OF-KVKOLLI       OCCURS 3 TIMES                               
014000                             INDEXED MOD-IX11.                            
014100        05 MOD-KVKOLLI-OF-ATTR                                            
014200                             PIC X(2).                                    
014300*                                 MFS ATTRIBUTFÄLT                        
014400        05 MOD-KVKOLLI-OF    PIC Z(3)9.                                   
014500*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
014600     03 MOD-KVKOLLI-OF-TOT   PIC Z(3)9.                                   
014700*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
014800     03 MOD-OF-KVKOLLI-PLUS  OCCURS 3 TIMES                               
014900                             INDEXED MOD-IX12.                            
015000        05 MOD-KVKOLLI-OF-PLUS-ATTR                                       
015100                             PIC X(2).                                    
015200*                                 MFS ATTRIBUTFÄLT                        
015300        05 MOD-KVKOLLI-OF-PLUS                                            
015400                             PIC Z(3)9.                                   
015500*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
015600     03 MOD-OF-KVKOLLI-MINUS OCCURS 3 TIMES                               
015700                             INDEXED MOD-IX13.                            
015800        05 MOD-KVKOLLI-OF-MINUS-ATTR                                      
015900                             PIC X(2).                                    
016000*                                 MFS ATTRIBUTFÄLT                        
016100        05 MOD-KVKOLLI-OF-MINUS                                           
016200                             PIC Z(3)9.                                   
016300*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
016400     03 MOD-TEMFSINF         PIC X(55).                                   
016500*                                 INFORMATIONSMEDDELANDE                  
