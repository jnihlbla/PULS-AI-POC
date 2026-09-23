000100 01  MOD-W3O15301.                                                        
000200*                                 MOD-COPYTEXT FÖR W3015300               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-BEART-SVE        PIC X(25).                                   
001200*                                 SVENSK ARTIKELBENÄMNING                 
001300     03 MOD-ADLAGOMR-UT      PIC Z9.                                      
001400*                                 LAGEROMRÅDE                             
001500     03 MOD-ADGANG-UT        PIC Z9.                                      
001600*                                 GÅNG                                    
001700     03 MOD-ADPLATS-UT       PIC Z(4)9.                                   
001800*                                 LAGERPLATSNUMMER                        
001900     03 MOD-IDDISTR-RENOV-IN-ATTR                                         
002000                             PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-IDDISTR-RENOV-IN PIC X(5).                                    
002300*                                 DISTRIKTNUMMER                          
002400     03 MOD-IDDISTR-RENOV-UT PIC Z(4)9.                                   
002500*                                 DISTRIKTNUMMER                          
002600     03 MOD-IDDISTR-RENOV-NDC-IN-ATTR                                     
002700                             PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-IDDISTR-RENOV-NDC-IN                                          
003000                             PIC X(5).                                    
003100*                                 DISTRIKTNUMMER                          
003200     03 MOD-IDDISTR-RENOV-NDC-UT                                          
003300                             PIC Z(4)9.                                   
003400*                                 DISTRIKTNUMMER                          
003500     03 MOD-IDDISTR-RENOV-CAN-IN-ATTR                                     
003600                             PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-IDDISTR-RENOV-CAN-IN                                          
003900                             PIC X(5).                                    
004000*                                 DISTRIKTNUMMER                          
004100     03 MOD-IDDISTR-RENOV-CAN-UT                                          
004200                             PIC Z(4)9.                                   
004300*                                 DISTRIKTNUMMER                          
004400     03 MOD-IDDISTR-RENOV-PAC-IN-ATTR                                     
004500                             PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-IDDISTR-RENOV-PAC-IN                                          
004800                             PIC X(5).                                    
004900*                                 DISTRIKTNUMMER                          
005000     03 MOD-IDDISTR-RENOV-PAC-UT                                          
005100                             PIC Z(4)9.                                   
005200*                                 DISTRIKTNUMMER                          
005300     03 MOD-IDDISTR-RENOV-AUS-IN-ATTR                                     
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-IDDISTR-RENOV-AUS-IN                                          
005700                             PIC X(5).                                    
005800*                                 DISTRIKTNUMMER                          
005900     03 MOD-IDDISTR-RENOV-AUS-UT                                          
006000                             PIC Z(4)9.                                   
006100*                                 DISTRIKTNUMMER                          
006200     03 MOD-IDDISTR-RENOV-CHN-IN-ATTR                                     
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500     03 MOD-IDDISTR-RENOV-CHN-IN                                          
006600                             PIC X(5).                                    
006700*                                 DISTRIKTNUMMER                          
006800     03 MOD-IDDISTR-RENOV-CHN-UT                                          
006900                             PIC Z(4)9.                                   
007000*                                 DISTRIKTNUMMER                          
007100     03 MOD-IDDISTR-RENOV-KOR-IN-ATTR                                     
007200                             PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400     03 MOD-IDDISTR-RENOV-KOR-IN                                          
007500                             PIC X(5).                                    
007600*                                 DISTRIKTNUMMER                          
007700     03 MOD-IDDISTR-RENOV-KOR-UT                                          
007800                             PIC Z(4)9.                                   
007900*                                 DISTRIKTNUMMER                          
008000     03 MOD-IDDISTR-RENOV-MY-IN-ATTR                                      
008100                             PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300     03 MOD-IDDISTR-RENOV-MY-IN                                           
008400                             PIC X(5).                                    
008500*                                 DISTRIKTNUMMER                          
008600     03 MOD-IDDISTR-RENOV-MY-UT                                           
008700                             PIC Z(4)9.                                   
008800*                                 DISTRIKTNUMMER                          
008900     03 MOD-IDDISTR-RENOV-TW-IN-ATTR                                      
009000                             PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200     03 MOD-IDDISTR-RENOV-TW-IN                                           
009300                             PIC X(5).                                    
009400*                                 DISTRIKTNUMMER                          
009500     03 MOD-IDDISTR-RENOV-TW-UT                                           
009600                             PIC Z(4)9.                                   
009700*                                 DISTRIKTNUMMER                          
009800     03 MOD-IDDISTR-RENOV-TH-IN-ATTR                                      
009900                             PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100     03 MOD-IDDISTR-RENOV-TH-IN                                           
010200                             PIC X(5).                                    
010300*                                 DISTRIKTNUMMER                          
010400     03 MOD-IDDISTR-RENOV-TH-UT                                           
010500                             PIC Z(4)9.                                   
010600*                                 DISTRIKTNUMMER                          
010700     03 MOD-KVBYTPKO-IN-ATTR PIC X(2).                                    
010800*                                 MFS ATTRIBUTFÄLT                        
010900     03 MOD-KVBYTPKO-IN      PIC 9(3).                                    
011000*                                 ANTAL BYTESOBJEKT PER PALL              
011100     03 MOD-KVBYTPKO-UT      PIC 9(3).                                    
011200*                                 ANTAL BYTESOBJEKT PER PALL              
011300     03 MOD-TEBYTKVA1-ATTR   PIC X(2).                                    
011400*                                 MFS ATTRIBUTFÄLT                        
011500     03 MOD-TEBYTKVA1        PIC X(75).                                   
011600*                                 KVALITETSNOTERING BYTESOBJEKT           
011700     03 MOD-TEBYTKVA2-ATTR   PIC X(2).                                    
011800*                                 MFS ATTRIBUTFÄLT                        
011900     03 MOD-TEBYTKVA2        PIC X(75).                                   
012000*                                 KVALITETSNOTERING BYTESOBJEKT           
012100     03 MOD-TEBYTKVA3-ATTR   PIC X(2).                                    
012200*                                 MFS ATTRIBUTFÄLT                        
012300     03 MOD-TEBYTKVA3        PIC X(75).                                   
012400*                                 KVALITETSNOTERING BYTESOBJEKT           
012500     03 MOD-TEBYTKVA4-ATTR   PIC X(2).                                    
012600*                                 MFS ATTRIBUTFÄLT                        
012700     03 MOD-TEBYTKVA4        PIC X(75).                                   
012800*                                 KVALITETSNOTERING BYTESOBJEKT           
012900     03 MOD-TEMFSINF         PIC X(55).                                   
013000*                                 INFORMATIONSMEDDELANDE                  
013100*** END OF VILMAII-COPY LENGTH= 587 BYTES                                 
