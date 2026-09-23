000100 01  MOD-W3O21101.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W3O211                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-SAMMA    PIC 9(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDDISTR-SAMMA    PIC 9(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-TIFSGVV-SAMMA    PIC 9(4).                                    
001300*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
001400     03 MOD-IDARTNR-NAESTA   PIC 9(9).                                    
001500*                                 ARTIKELNUMMER                           
001600     03 MOD-IDDISTR-NAESTA   PIC 9(4).                                    
001700*                                 DISTRIKTNUMMER                          
001800     03 MOD-TIFSGVV-NAESTA   PIC 9(4).                                    
001900*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
002000     03 MOD-UTRAD            OCCURS 12 TIMES.                             
002100        05 MOD-UTRAD-IDARTNR PIC Z(8)9.                                   
002200*                                 ARTIKELNUMMER                           
002300        05 MOD-UTRAD-IDDISTR PIC 9(4).                                    
002400*                                 DISTRIKTNUMMER                          
002500        05 MOD-UTRAD-TIFSGVV PIC 9(4).                                    
002600*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
002700        05 MOD-UTRAD-PRARTNTO                                             
002800                             PIC Z(6)9.9(2)-.                             
002900*                                 ARTIKELPRIS NETTO                       
003000        05 MOD-UTRAD-KVLEVART                                             
003100                             PIC Z(6)9-.                                  
003200*                                 LEVERERAT ANTAL ARTIKLAR                
003300        05 MOD-UTRAD-PRARTSJK                                             
003400                             PIC Z(6)9.9(2).                              
003500*                                 ARTIKELNS SJÄLVKOSTNAD                  
003600     03 MOD-KDSVAR-ATTR      PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-KDSVAR           PIC X(2).                                    
003900*                                 MFS BEHANDLING AV INPUTFÄLT             
004000     03 MOD-IDARTNR-ATTR     PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-IDARTNR          PIC X(2).                                    
004300*                                 MFS BEHANDLING AV INPUTFÄLT             
004400     03 MOD-IDDISTR-ATTR     PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-IDDISTR          PIC X(2).                                    
004700*                                 MFS BEHANDLING AV INPUTFÄLT             
004800     03 MOD-TIFSGVV-ATTR     PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-TIFSGVV          PIC X(2).                                    
005100*                                 MFS BEHANDLING AV INPUTFÄLT             
005200     03 MOD-PRARTNTO-ATTR    PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-PRARTNTO         PIC X(2).                                    
005500*                                 MFS BEHANDLING AV INPUTFÄLT             
005600     03 MOD-KVLEVART-ATTR    PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-KVLEVART         PIC X(2).                                    
005900*                                 MFS BEHANDLING AV INPUTFÄLT             
006000     03 MOD-PRARTSJK-ATTR    PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200     03 MOD-PRARTSJK         PIC X(2).                                    
006300*                                 MFS BEHANDLING AV INPUTFÄLT             
006400     03 MOD-TEMFSINF         PIC X(61).                                   
006500*                                 INFORMATIONSMEDDELANDE                  
006600*** END COPY W3O21101C0  LENGTH=719                                       
