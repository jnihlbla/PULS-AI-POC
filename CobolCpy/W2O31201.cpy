000100 01  MOD-W2O31201-CTX.                                                    
000200*                                 MOD-COPYTEXT TILL W2031200              
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDKAMPRF-IN      PIC X(7).                                    
000800*                                 KAMPANJREFERENS                         
000900     03 MOD-IDKAMPRF-UT      PIC X(7).                                    
001000*                                 KAMPANJREFERENS                         
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDARTNR-IN       PIC X(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MOD-IDARTNR-UT       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MOD-KVKVARFD-ATTR    PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-KVKVARFD         PIC Z(6)9.                                   
002200*                                 KVAR ATT FÖRDELA TILL KUND              
002300     03 MOD-W2O31201-001-GRP OCCURS 12 TIMES.                             
002400        05 MOD-CMD-UPD-ATTR  PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600        05 MOD-CMD-UPD       PIC X.                                       
002700        05 MOD-IDDISTR-FOM-ATTR                                           
002800                             PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000        05 MOD-IDDISTR-FOM   PIC Z(3)9.                                   
003100*                                 DISTRIKTNUMMER                          
003200        05 MOD-IDDISTR-TOM-ATTR                                           
003300                             PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-IDDISTR-TOM   PIC Z(3)9.                                   
003600*                                 DISTRIKTNUMMER                          
003700        05 MOD-IDKUNDNR-FOM-ATTR                                          
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 MOD-IDKUNDNR-FOM  PIC Z(5)9.                                   
004100*                                 KUNDNUMMER                              
004200        05 MOD-IDKUNDNR-TOM-ATTR                                          
004300                             PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 MOD-IDKUNDNR-TOM  PIC Z(5)9.                                   
004600*                                 KUNDNUMMER                              
004700        05 MOD-KVBEART-KAMP-ATTR                                          
004800                             PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000        05 MOD-KVBEART-KAMP  PIC Z(5)9.                                   
005100*                                 BESTÄLLT ANTAL FÖR KAMPANJEN            
005200        05 MOD-KVBEART-KUND-ATTR                                          
005300                             PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500        05 MOD-KVBEART-KUND  PIC Z(5)9.                                   
005600*                                 AV KUND BESTÄLLT KVANTITET              
005700        05 MOD-KVBEART-REM-ATTR                                           
005800                             PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000        05 MOD-KVBEART-REM   PIC Z(5)9.                                   
006100*                                 AV KUND BESTÄLLT KVANTITET              
006200     03 MOD-IDDISTR-FOM-UPD-ATTR                                          
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500     03 MOD-IDDISTR-FOM-UPD  PIC Z(3)9.                                   
006600*                                 DISTRIKTNUMMER                          
006700     03 MOD-IDDISTR-TOM-UPD-ATTR                                          
006800                             PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000     03 MOD-IDDISTR-TOM-UPD  PIC Z(3)9.                                   
007100*                                 DISTRIKTNUMMER                          
007200     03 MOD-IDKUNDNR-FOM-UPD-ATTR                                         
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500     03 MOD-IDKUNDNR-FOM-UPD PIC Z(5)9.                                   
007600*                                 KUNDNUMMER                              
007700     03 MOD-IDKUNDNR-TOM-UPD-ATTR                                         
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000     03 MOD-IDKUNDNR-TOM-UPD PIC Z(5)9.                                   
008100*                                 KUNDNUMMER                              
008200     03 MOD-KVBEART-KAMP-UPD-ATTR                                         
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500     03 MOD-KVBEART-KAMP-UPD PIC Z(5)9.                                   
008600*                                 BESTÄLLT ANTAL FÖR KAMPANJEN            
008700     03 MOD-IDKAMPRF-COPY-ATTR                                            
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000     03 MOD-IDKAMPRF-COPY    PIC X(7).                                    
009100*                                 KAMPANJREFERENS                         
009200     03 MOD-IDDC-COPY-ATTR   PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400     03 MOD-IDDC-COPY        PIC X(2).                                    
009500*                                 IDENTIFIERARE LAGER                     
009600     03 MOD-IDARTNR-COPY-ATTR                                             
009700                             PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900     03 MOD-IDARTNR-COPY     PIC X(9).                                    
010000*                                 ARTIKELNUMMER                           
010100     03 MOD-TEMFSINF         PIC X(55).                                   
010200*                                 INFORMATIONSMEDDELANDE                  
010300*** END OF VILMAII-COPY LENGTH= 864 BYTES                                 
