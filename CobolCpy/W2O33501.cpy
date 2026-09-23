000100 01  MOD-W2O33501.                                                        
000200*                                 MOD-COPYTEXT FÖR W20335                 
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDLEVNR-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200     03 MOD-IDDIRGRP-IN      PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-IDDIRGRP-UT      PIC X(10).                                   
001500*                                 DIREKTLEVERANSGRUPP                     
001600     03 MOD-IDDISTR-IN       PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-IDDISTR-UT       PIC X(4).                                    
001900*                                 DISTRIKTNUMMER                          
002000     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
002300*                                 KUNDNUMMER                              
002400     03 MOD-CMD-E-ATTR       PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 MOD-CMD-E            PIC X.                                       
002700     03 MOD-IDDISTR-FOM-E-ATTR                                            
002800                             PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 MOD-IDDISTR-FOM-E    PIC Z(3)9.                                   
003100*                                 DISTRIKTNUMMER                          
003200     03 MOD-IDDISTR-TOM-E-ATTR                                            
003300                             PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 MOD-IDDISTR-TOM-E    PIC Z(3)9.                                   
003600*                                 DISTRIKTNUMMER                          
003700     03 MOD-IDKUNDNR-FOM-E-ATTR                                           
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-IDKUNDNR-FOM-E   PIC Z(5)9.                                   
004100*                                 KUNDNUMMER                              
004200     03 MOD-IDKUNDNR-TOM-E-ATTR                                           
004300                             PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-IDKUNDNR-TOM-E   PIC Z(5)9.                                   
004600*                                 KUNDNUMMER                              
004700     03 MOD-INPUT            OCCURS 5 TIMES.                              
004800*                                 GRUPP MED TABELL RADER                  
004900        05 MOD-KVBEART-MIN-E-ATTR                                         
005000                             PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 MOD-KVBEART-MIN-E PIC Z(5)9.                                   
005300*                                 BESTÄLLT ANTAL MIN-KVANTITET            
005400        05 MOD-KDDDGS-E-ATTR PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 MOD-KDDDGS-E      PIC X.                                       
005700*                                 REGEL HUR DDGS ART LEVERERAS            
005800        05 MOD-FLDDGS-E-ATTR PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000        05 MOD-FLDDGS-E      PIC X.                                       
006100*                                 VISAR OM DDGS REGEL ÖVERLAPPAS          
006200        05 MOD-IDDC-E-ATTR   PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400        05 MOD-IDDC-E        PIC X(2).                                    
006500*                                 IDENTIFIERARE LAGER                     
006600     03 MOD-TABELLRAD        OCCURS 6 TIMES.                              
006700*                                 GRUPP MED TABELL RADER                  
006800        05 MOD-CMD-ATTR      PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-CMD           PIC X.                                       
007100        05 MOD-IDDISTR-FOM   PIC Z(3)9.                                   
007200*                                 DISTRIKTNUMMER                          
007300        05 MOD-IDDISTR-TOM   PIC Z(3)9.                                   
007400*                                 DISTRIKTNUMMER                          
007500        05 MOD-IDKUNDNR-FOM  PIC Z(5)9.                                   
007600*                                 KUNDNUMMER                              
007700        05 MOD-IDKUNDNR-TOM  PIC Z(5)9.                                   
007800*                                 KUNDNUMMER                              
007900        05 MOD-KVBEART-MIN   OCCURS 5 TIMES                               
008000                             PIC Z(5)9.                                   
008100*                                 BESTÄLLT ANTAL MIN-KVANTITET            
008200        05 MOD-KDDDGS        OCCURS 5 TIMES                               
008300                             PIC X.                                       
008400*                                 REGEL HUR DDGS ART LEVERERAS            
008500        05 MOD-FLDDGS        OCCURS 5 TIMES                               
008600                             PIC X.                                       
008700*                                 VISAR OM DDGS REGEL ÖVERLAPPAS          
008800        05 MOD-IDDC          OCCURS 5 TIMES                               
008900                             PIC X(2).                                    
009000*                                 IDENTIFIERARE LAGER                     
009100     03 MOD-TEMFSINF         PIC X(55).                                   
009200*                                 INFORMATIONSMEDDELANDE                  
009300*** END OF VILMAII-COPY LENGTH= 691 BYTES                                 
