000100 01  MOD-W4O66101.                                                        
000200*                                 MOD-COPYTEXT FÖR W40661                 
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDTRPTNR-IN      PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDTRPTNR-UT      PIC X(3).                                    
001000*                                 TRANSPORTIDENTITET                      
001100     03 MOD-ADFLGEO-IN       PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-ADFLGEO-UT       PIC X(3).                                    
001400*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
001500     03 MOD-ADFLOMR-IN       PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-ADFLOMR-UT       PIC X(3).                                    
001800*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
001900     03 MOD-IDDC-IN          PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-IDDISTR-B1       PIC 9(4).                                    
002400*                                 DISTRIKTNUMMER                          
002500     03 MOD-IDKUNDNR-B1      PIC 9(6).                                    
002600*                                 KUNDNUMMER                              
002700     03 MOD-KDFRAKT-B1       PIC 9(2).                                    
002800*                                 FRAKTSÄTT DC TILL KUND                  
002900     03 MOD-KDORDKLX-B1      PIC X.                                       
003000*                                 ORDERKLASS + BLANK                      
003100     03 MOD-IDDISTR-BN       PIC 9(4).                                    
003200*                                 DISTRIKTNUMMER                          
003300     03 MOD-IDKUNDNR-BN      PIC 9(6).                                    
003400*                                 KUNDNUMMER                              
003500     03 MOD-KDFRAKT-BN       PIC 9(2).                                    
003600*                                 FRAKTSÄTT DC TILL KUND                  
003700     03 MOD-KDORDKLX-BN      PIC X.                                       
003800*                                 ORDERKLASS + BLANK                      
003900     03 MOD-FLUTLAST-ATTR    PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-FLUTLAST         PIC X(2).                                    
004200*                                 MFS BEHANDLING AV INPUTFÄLT             
004300     03 MOD-ADFLGEO          PIC X(3).                                    
004400*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
004500     03 MOD-ADFLOMR          PIC Z(2)9.                                   
004600*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
004700     03 MOD-ADRUTNIV         PIC Z(2)9.                                   
004800*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
004900     03 MOD-FLTOTMS-ATTR     PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-FLTOTMS          PIC X.                                       
005200*                                 SEND TO TMS FLAGGA                      
005300     03 MOD-RAD              OCCURS 12 TIMES.                             
005400*                                                                         
005500        05 MOD-KDCMD-ATTR    PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 MOD-KDCMD         PIC X(2).                                    
005800*                                 MFS BEHANDLING AV INPUTFÄLT             
005900        05 MOD-IDDISTR-FOM   PIC Z(3)9.                                   
006000*                                 DISTRIKTNUMMER                          
006100        05 MOD-IDDISTR-TOM   PIC Z(3)9.                                   
006200*                                 DISTRIKTNUMMER                          
006300        05 MOD-IDKUNDNR-FOM  PIC Z(5)9.                                   
006400*                                 KUNDNUMMER                              
006500        05 MOD-IDKUNDNR-TOM  PIC Z(5)9.                                   
006600*                                 KUNDNUMMER                              
006700        05 MOD-KDFRAKT       PIC Z9.                                      
006800*                                 FRAKTSÄTT DC TILL KUND                  
006900        05 MOD-KDORDKLX      PIC X.                                       
007000*                                 ORDERKLASS + BLANK                      
007100        05 MOD-IDDC-CROSS    PIC X(2).                                    
007200*                                 DC FÖR CROSS DOCKING                    
007300        05 MOD-TEFLNOTE-ATTR PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500        05 MOD-TEFLNOTE      PIC X(20).                                   
007600*                                 NOTERING FÄRDIGLAGRET                   
007700     03 MOD-IDDISTR-FOM-NY-ATTR                                           
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000     03 MOD-IDDISTR-FOM-NY   PIC X(2).                                    
008100*                                 MFS BEHANDLING AV INPUTFÄLT             
008200     03 MOD-IDDISTR-TOM-NY-ATTR                                           
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500     03 MOD-IDDISTR-TOM-NY   PIC X(2).                                    
008600*                                 MFS BEHANDLING AV INPUTFÄLT             
008700     03 MOD-IDKUNDNR-FOM-NY-ATTR                                          
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000     03 MOD-IDKUNDNR-FOM-NY  PIC X(2).                                    
009100*                                 MFS BEHANDLING AV INPUTFÄLT             
009200     03 MOD-IDKUNDNR-TOM-NY-ATTR                                          
009300                             PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500     03 MOD-IDKUNDNR-TOM-NY  PIC X(2).                                    
009600*                                 MFS BEHANDLING AV INPUTFÄLT             
009700     03 MOD-KDFRAKT-NY-ATTR  PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900     03 MOD-KDFRAKT-NY       PIC X(2).                                    
010000*                                 MFS BEHANDLING AV INPUTFÄLT             
010100     03 MOD-KDORDKLX-NY-ATTR PIC X(2).                                    
010200*                                 MFS ATTRIBUTFÄLT                        
010300     03 MOD-KDORDKLX-NY      PIC X(2).                                    
010400*                                 MFS BEHANDLING AV INPUTFÄLT             
010500     03 MOD-IDDC-CROSS-NY-ATTR                                            
010600                             PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800     03 MOD-IDDC-CROSS-NY    PIC X(2).                                    
010900*                                 MFS BEHANDLING AV INPUTFÄLT             
011000     03 MOD-TEFLNOTE-NY-ATTR PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200     03 MOD-TEFLNOTE-NY      PIC X(2).                                    
011300*                                 MFS BEHANDLING AV INPUTFÄLT             
011400     03 MOD-TEMFSINF         PIC X(55).                                   
011500*                                 INFORMATIONSMEDDELANDE                  
011600*** END OF VILMAII-COPY LENGTH= 804 BYTES                                 
