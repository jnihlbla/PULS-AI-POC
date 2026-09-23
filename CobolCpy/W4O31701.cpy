000100 01  MOD-W4O31701.                                                        
000200*                                 MOD-COPYTEXT PGM W40317                 
000300*                                 AVVIKELSE URSPRUNG                      
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDANSTNR-IN      PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDANSTNR-UT      PIC X(5).                                    
001100*                                 ANSTÄLLNINGSNUMMER                      
001200     03 MOD-IDDISTR-IN       PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-IDDISTR-UT       PIC X(4).                                    
001500*                                 DISTRIKTNUMMER                          
001600     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001900*                                 KUNDNUMMER                              
002000     03 MOD-IDORDNR-IN       PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200     03 MOD-IDORDNR-UT       PIC X(5).                                    
002300*                                 ORDERNUMMER UTGÅR PD90                  
002400     03 MOD-IDKOLLI-IN       PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-IDKOLLI-UT       PIC X(5).                                    
002700*                                 KOLLINUMMER                             
002800     03 MOD-IDPRODNR-IN      PIC X(2).                                    
002900*                                 MFS BEHANDLING AV INPUTFÄLT             
003000     03 MOD-IDPRODNR-UT      PIC X(7).                                    
003100*                                 PRODUKTIONSNUMMER                       
003200     03 MOD-IDDC-IN          PIC X(2).                                    
003300*                                 IDENTIFIERARE LAGER                     
003400     03 MOD-IDDC-UT          PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600     03 MOD-IDTRANS-START    PIC X(4).                                    
003700*                                 BILDNUMMER                              
003800     03 MOD-FLSISTAK         PIC X(2).                                    
003900*                                 MFS BEHANDLING AV INPUTFÄLT             
004000     03 MOD-IDRADNR-S        PIC Z(3)9.                                   
004100*                                 RADNUMMER                               
004200     03 MOD-KDARTURS-S       PIC X(2).                                    
004300*                                 ARTIKELURSPRUNGSKOD                     
004400     03 MOD-RAD              OCCURS 13 TIMES.                             
004500        05 MOD-IDRADNR-ATTR  PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-IDRADNR       PIC X(2).                                    
004800*                                 MFS BEHANDLING AV INPUTFÄLT             
004900        05 MOD-KDARTURS-ATTR PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-KDARTURS      PIC X(2).                                    
005200*                                 MFS BEHANDLING AV INPUTFÄLT             
005300     03 MOD-TEMFSINF         PIC X(55).                                   
005400*                                 INFORMATIONSMEDDELANDE                  
005500*** END OF VILMAII-COPY LENGTH= 263 BYTES                                 
