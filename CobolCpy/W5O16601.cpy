000100 01  MOD-W5O16601.                                                        
000200*                                 MOD-COPYTEXT FÖR W5016600               
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
001500     03 MOD-BEART            PIC X(25).                                   
001600*                                 ARTIKELBENÄMNING                        
001700     03 MOD-PRARTSTD         PIC Z(6)9.9(2).                              
001800*                                 ARTIKELSTANDARDPRIS                     
001900     03 MOD-KVLS             PIC -(7)9.                                   
002000*                                 LAGERSALDO                              
002100     03 MOD-AVAILABLE        PIC -(7)9.                                   
002200     03 MOD-KVAKS            PIC -(7)9.                                   
002300*                                 ANKOMSTSALDO                            
002400     03 MOD-KVEFRS           PIC -(7)9.                                   
002500*                                 EJ FAKTURERAT ANTAL STYCK               
002600     03 MOD-KVAKS-PAV        PIC -(7)9.                                   
002700*                                 DEL AV AK PÅ VÄG                        
002800     03 MOD-OQB              PIC -(7)9.                                   
002900     03 MOD-KVBEART          PIC -(7)9.                                   
003000*                                 BESTÄLLT ANTAL STYCKEN                  
003100     03 MOD-KVRESS           PIC -(7)9.                                   
003200*                                 RESERVERAT ANTAL ARTIKLAR               
003300     03 MOD-BO               PIC -(7)9.                                   
003400     03 MOD-SALDON.                                                       
003500*                                 GRUPP MED SALDON                        
003600        05 MOD-JUST1-ATTR    PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800        05 MOD-JUST1         PIC X.                                       
003900        05 MOD-KVAKS-IN-ATTR PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100        05 MOD-KVAKS-IN      PIC Z(6)9.                                   
004200*                                 ANKOMSTSALDO                            
004300        05 MOD-JUST2-ATTR    PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 MOD-JUST2         PIC X.                                       
004600        05 MOD-KVEFRS-IN-ATTR                                             
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-KVEFRS-IN     PIC Z(6)9.                                   
005000*                                 EJ FAKTURERAT ANTAL STYCK               
005100        05 MOD-JUST3-ATTR    PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300        05 MOD-JUST3         PIC X.                                       
005400        05 MOD-KVAKS-PAV-IN-ATTR                                          
005500                             PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 MOD-KVAKS-PAV-IN  PIC Z(6)9.                                   
005800*                                 DEL AV AK PÅ VÄG                        
005900        05 MOD-JUST4-ATTR    PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 MOD-JUST4         PIC X.                                       
006200        05 MOD-OQB-IN-ATTR   PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400        05 MOD-OQB-IN        PIC Z(6)9.                                   
006500        05 MOD-VDB4-ATTR     PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700        05 MOD-VDB4          PIC X.                                       
006800        05 MOD-JUST5-ATTR    PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-JUST5         PIC X.                                       
007100        05 MOD-KVBEART-IN-ATTR                                            
007200                             PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400        05 MOD-KVBEART-IN    PIC Z(6)9.                                   
007500*                                 BESTÄLLT ANTAL STYCKEN                  
007600        05 MOD-JUST6-ATTR    PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-JUST6         PIC X.                                       
007900        05 MOD-KVRESS-IN-ATTR                                             
008000                             PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200        05 MOD-KVRESS-IN     PIC Z(6)9.                                   
008300*                                 RESERVERAT ANTAL ARTIKLAR               
008400        05 MOD-JUST7-ATTR    PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-JUST7         PIC X.                                       
008700        05 MOD-BO-IN-ATTR    PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900        05 MOD-BO-IN         PIC Z(6)9.                                   
009000        05 MOD-VDB7-ATTR     PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200        05 MOD-VDB7          PIC X.                                       
009300     03 MOD-LOGG             OCCURS 14 TIMES.                             
009400*                                 GRUPP MED LOGGNING                      
009500        05 MOD-DATE          PIC 9(6).                                    
009600        05 MOD-QTY           PIC -(7)9.                                   
009700        05 MOD-TYPE          PIC X(3).                                    
009800        05 MOD-IDUSER        PIC X(8).                                    
009900*                                 ANVÄNDARENS SÄKERHETS ID                
010000     03 MOD-COMM-ATTR        PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200     03 MOD-COMM             PIC X(17).                                   
010300     03 MOD-TEMFSINF         PIC X(55).                                   
010400*                                 INFORMATIONSMEDDELANDE                  
010500*** END OF VILMAII-COPY LENGTH= 687 BYTES                                 
