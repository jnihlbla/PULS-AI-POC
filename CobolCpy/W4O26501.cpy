000100 01  MOD-W4O26501.                                                        
000200*                                 MOD-COPYTEXT FÖR W40265                 
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MOD-IDKUNDRF-IN      PIC X(7).                                    
001200*                                 ORDERNUMMER                             
001300     03 MOD-IDARTNR-IN       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MOD-IDDISTR-UT       PIC X(4).                                    
001600*                                 DISTRIKTNUMMER                          
001700     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001800*                                 KUNDNUMMER                              
001900     03 MOD-IDKUNDRF-UT      PIC X(7).                                    
002000*                                 ORDERNUMMER                             
002100     03 MOD-IDARTNR-UT       PIC X(9).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MOD-KDORDKL          PIC 9.                                       
002400*                                 ORDERKLASS                              
002500     03 MOD-KDFRAKT          PIC Z9.                                      
002600*                                 FRAKTSÄTT DC TILL KUND                  
002700     03 MOD-KDPROTYP         PIC X.                                       
002800*                                 TYP AV PROFORMA                         
002900     03 MOD-IDARTNR-ENTER    PIC 9(9).                                    
003000*                                 ARTIKELNUMMER                           
003100     03 MOD-IDLOPNR-ENTER    PIC 9(3).                                    
003200*                                 LÖPNUMMER                               
003300     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
003400*                                 ARTIKELNUMMER                           
003500     03 MOD-IDLOPNR-NEXT     PIC 9(3).                                    
003600*                                 LÖPNUMMER                               
003700     03 MOD-TEDDI            PIC X(11).                                   
003800*                                 TEXTFÄLT DDI                            
003900     03 MOD-KDVALISO         PIC X(3).                                    
004000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004100     03 MOD-UPDATE           OCCURS 7 TIMES.                              
004200*                                 TABELL-UPDATE                           
004300        05 MOD-CMD-UPDATE-ATTR                                            
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 MOD-CMD-UPDATE    PIC X.                                       
004700*                                 BEHANDLINGSKOD-X                        
004800     03 MOD-IDARTNR          OCCURS 7 TIMES                               
004900                             PIC Z(8)9.                                   
005000*                                 ARTIKELNUMMER                           
005100     03 MOD-KVART            OCCURS 7 TIMES                               
005200                             PIC Z(6)9.                                   
005300*                                 ANTAL ARTNR PER BRYTBEGREPP             
005400     03 MOD-PRARTNTO         OCCURS 7 TIMES                               
005500                             PIC Z(6)9.9(2).                              
005600*                                 ARTIKELPRIS NETTO                       
005700     03 MOD-TEASTRIX         OCCURS 7 TIMES                               
005800                             PIC X.                                       
005900*                                 ASTERISK                                
006000     03 MOD-BEART            OCCURS 7 TIMES                               
006100                             PIC X(17).                                   
006200     03 MOD-BERADREF         OCCURS 7 TIMES                               
006300                             PIC X(10).                                   
006400*                                 KUNDENS RADREFERENS                     
006500     03 MOD-KDKVBRYT         OCCURS 7 TIMES                               
006600                             PIC 9.                                       
006700*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
006800     03 MOD-KVVECKOR-TPO5    OCCURS 7 TIMES                               
006900                             PIC Z9.                                      
007000*                                 ANTAL VECKOR FÖR TPO5                   
007100     03 MOD-KDTPOTYP         OCCURS 7 TIMES                               
007200                             PIC 9.                                       
007300*                                 TYP AV TIDPLANERAD ORDER                
007400     03 MOD-IDLOPNR          OCCURS 7 TIMES                               
007500                             PIC 9(3).                                    
007600*                                 LÖPNUMMER                               
007700     03 MOD-UPDATE           OCCURS 7 TIMES.                              
007800*                                 TABELL-UPDATE                           
007900        05 MOD-KVBEART-Q-U-ATTR                                           
008000                             PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200        05 MOD-KVBEART-Q-U   PIC X(2).                                    
008300*                                 MFS BEHANDLING AV INPUTFÄLT             
008400     03 MOD-UPDATE           OCCURS 7 TIMES.                              
008500*                                 TABELL-UPDATE                           
008600        05 MOD-PRARTNTO-U-ATTR                                            
008700                             PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900        05 MOD-PRARTNTO-U    PIC X(2).                                    
009000*                                 MFS BEHANDLING AV INPUTFÄLT             
009100     03 MOD-UPDATE           OCCURS 7 TIMES.                              
009200*                                 TABELL-UPDATE                           
009300        05 MOD-BEART-U-ATTR  PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500        05 MOD-BEART-U       PIC X(2).                                    
009600*                                 MFS BEHANDLING AV INPUTFÄLT             
009700     03 MOD-UPDATE           OCCURS 7 TIMES.                              
009800*                                 TABELL-UPDATE                           
009900        05 MOD-BERADREF-U-ATTR                                            
010000                             PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200        05 MOD-BERADREF-U    PIC X(2).                                    
010300*                                 MFS BEHANDLING AV INPUTFÄLT             
010400     03 MOD-TEMFSINF         PIC X(55).                                   
010500*                                 INFORMATIONSMEDDELANDE                  
010600*** END OF VILMAII-COPY LENGTH= 753 BYTES                                 
