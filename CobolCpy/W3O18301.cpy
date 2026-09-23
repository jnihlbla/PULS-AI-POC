000100 01  MOD-W3O18301.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W3O18301                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDBYTKOL-IN      PIC X(3).                                    
000900*                                 BYTES KOLLINUMMER                       
001000     03 MOD-IDBYTKOL-UT      PIC X(3).                                    
001100*                                 BYTES KOLLINUMMER                       
001200     03 MOD-IDARTNR-OBJ-IN   PIC X(9).                                    
001300*                                 OBJEKTNUMMER                            
001400     03 MOD-IDARTNR-OBJ-UT   PIC X(9).                                    
001500*                                 OBJEKTNUMMER                            
001600     03 MOD-IDDISTR-IN       PIC X(4).                                    
001700*                                 DISTRIKTNUMMER                          
001800     03 MOD-IDDISTR-UT       PIC X(4).                                    
001900*                                 DISTRIKTNUMMER                          
002000     03 MOD-IDARTNR-OBJ-UPPD-ATTR                                         
002100                             PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300     03 MOD-IDARTNR-OBJ-UPPD PIC X(9).                                    
002400*                                 OBJEKTNUMMER                            
002500     03 MOD-KVRETUR-UPPD-ATTR                                             
002600                             PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 MOD-KVRETUR-UPPD     PIC X(7).                                    
002900*                                 ANTAL I RETUR                           
003000     03 MOD-FLBYTKNR-UPPD-ATTR                                            
003100                             PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-FLBYTKNR-UPPD    PIC X.                                       
003400*                                 KOLLI PROFORMA FLAGGA                   
003500*                                 Y = NYTT KOLLINR                        
003600*                                 N = SAMMA KOLLINR                       
003700     03 MOD-KVLS             PIC Z(6)9.                                   
003800*                                 LAGERSALDO                              
003900     03 MOD-IDBYTFAK         PIC Z(3)9.                                   
004000*                                 BYTES FAKTURANUMMER                     
004100     03 MOD-PROFORMA-RAD     OCCURS 10 TIMES.                             
004200*                                 ARTIKELNUMMER I PROFORMA-FAKTUR         
004300*                                 A                                       
004400        05 MOD-IDARTNR-OBJ   PIC Z(9).                                    
004500*                                 ARTIKELNUMMER                           
004600        05 MOD-KVRETUR       PIC Z(7).                                    
004700*                                 ANTAL I RETUR                           
004800        05 MOD-IDBYTKOL      PIC Z(2)9.                                   
004900*                                 BYTES KOLLINUMMER                       
005000        05 MOD-BEART         PIC X(25).                                   
005100*                                 ARTIKELBENÄMNING                        
005200     03 MOD-BORT.                                                         
005300*                                 ARTIKELNUMMER I PROFORMA-FAKTUR         
005400*                                 A                                       
005500        05 MOD-IDARTNR-OBJ-BORT-ATTR                                      
005600                             PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-IDARTNR-OBJ-BORT                                           
005900                             PIC X(9).                                    
006000*                                 OBJEKTNUMMER                            
006100        05 MOD-KVRETUR-BORT-ATTR                                          
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400        05 MOD-KVRETUR-BORT  PIC X(7).                                    
006500*                                 ANTAL I RETUR                           
006600        05 MOD-IDBYTKOL-BORT-ATTR                                         
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900        05 MOD-IDBYTKOL-BORT PIC X(3).                                    
007000*                                 BYTES KOLLINUMMER                       
007100        05 MOD-VKORDBTO-FAKT-IN-ATTR                                      
007200                             PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400        05 MOD-VKORDBTO-FAKT-IN                                           
007500                             PIC X(8).                                    
007600*                                 ORDERVIKT BRUTTO PER FAKTURA            
007700        05 MOD-VKORDBTO-FAKT-UT                                           
007800                             PIC Z(5)9.9.                                 
007900*                                 ORDERVIKT BRUTTO PER FAKTURA            
008000        05 MOD-VLORDBTO-FAKT-IN-ATTR                                      
008100                             PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300        05 MOD-VLORDBTO-FAKT-IN                                           
008400                             PIC X(8).                                    
008500*                                 ORDERVOLYM BRUTTO PER FAKTURA           
008600        05 MOD-VLORDBTO-FAKT-UT                                           
008700                             PIC Z(5)9.9.                                 
008800        05 MOD-GODK-FAKT-ATTR                                             
008900                             PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100        05 MOD-GODK-FAKT     PIC X.                                       
009200     03 MOD-TEMFSINF         PIC X(55).                                   
009300*                                 INFORMATIONSMEDDELANDE                  
009400*** END OF VILMAII-COPY LENGTH= 669 BYTES                                 
