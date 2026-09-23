000100 01  MOD-W3O16101.                                                        
000200*                                 MOD-COPYTEXT FÖR W3016100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-BELEV-IN         PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-BELEV-UT         PIC X(30).                                   
001000*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001100     03 MOD-IDPRODNR-LO      PIC 9(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDPRODNR-HI      PIC 9(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MOD-IDARTNR-BYT      PIC Z(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MOD-BEART-SVE        PIC X(25).                                   
001800*                                 SVENSK ARTIKELBENÄMNING                 
001900     03 MOD-TEBYTNOT         OCCURS 4 TIMES                               
002000                             PIC X(30).                                   
002100*                                 BYTES ARTIKEL NOTERING                  
002200     03 MOD-BETFLEV          PIC X(30).                                   
002300*                                 TILLFÄLLIG LEVERANTÖR                   
002400     03 MOD-IDARTNR          OCCURS 48 TIMES                              
002500                             PIC Z(7)9.                                   
002600*                                 ARTIKELNUMMER                           
002700     03 MOD-TEMFSINF         PIC X(55).                                   
002800*                                 INFORMATIONSMEDDELANDE                  
002900*** END OF VILMAII-COPY LENGTH= 717 BYTES                                 
