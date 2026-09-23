000100 01  MID-W3I18301.                                                        
000200*                                 MID-COPYTEXT FÖR W3018300               
000300     03 MID-IDBYTKOL-IN      PIC X(3).                                    
000400*                                 BYTES KOLLINUMMER                       
000500     03 MID-IDBYTKOL-UT      PIC X(3).                                    
000600*                                 BYTES KOLLINUMMER                       
000700     03 MID-IDARTNR-OBJ-IN   PIC X(9).                                    
000800*                                 OBJEKTNUMMER                            
000900     03 MID-IDARTNR-OBJ-UT   PIC X(9).                                    
001000*                                 OBJEKTNUMMER                            
001100     03 MID-IDDISTR-IN       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MID-IDDISTR-UT       PIC X(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500     03 MID-NYRAD.                                                        
001600*                                 RADINFORMATION                          
001700        05 MID-IDARTNR-OBJ-UPPD                                           
001800                             PIC X(9).                                    
001900*                                 OBJEKTNUMMER                            
002000        05 MID-KVRETUR-UPPD  PIC X(7).                                    
002100*                                 ANTAL I RETUR                           
002200        05 MID-FLBYTKNR-UPPD PIC X.                                       
002300*                                 KOLLI PROFORMA FLAGGA                   
002400*                                 Y = NYTT KOLLINR                        
002500*                                 N = SAMMA KOLLINR                       
002600     03 MID-BORT.                                                         
002700*                                 RADINFORMATION                          
002800        05 MID-IDARTNR-OBJ-BORT                                           
002900                             PIC X(9).                                    
003000*                                 OBJEKTNUMMER                            
003100        05 MID-KVRETUR-BORT  PIC X(7).                                    
003200*                                 ANTAL I RETUR                           
003300        05 MID-IDBYTKOL-BORT PIC X(3).                                    
003400*                                 BYTES KOLLINUMMER                       
003500     03 MID-GODKRAD.                                                      
003600*                                 RADINFORMATION                          
003700        05 MID-VKORDBTO-FAKT-IN                                           
003800                             PIC X(8).                                    
003900*                                 ORDERVIKT BRUTTO PER FAKTURA            
004000        05 MID-VLORDBTO-FAKT-IN                                           
004100                             PIC X(8).                                    
004200*                                 ORDERVOLYM BRUTTO PER FAKTURA           
004300        05 MID-GODK-FAKT     PIC X.                                       
004400*** END OF VILMAII-COPY LENGTH= 85 BYTES                                  
