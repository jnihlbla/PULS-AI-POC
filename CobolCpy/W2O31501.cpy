000100 01  MOD-W2O31501-CTX.                                                    
000200*                                 MOD-COPYTEXT TILL W2031500              
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC Z(8)9.                                   
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-W2O31501-001-GRP OCCURS 13 TIMES.                             
001200        05 MOD-BILD-ATTR     PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400        05 MOD-BILD          PIC X(4).                                    
001500*                                 BILDNUMMER                              
001600        05 MOD-IDKAMP        PIC X(7).                                    
001700*                                 SERVICEKAMPANJ                          
001800        05 MOD-IDKAMP-GRP    PIC X(7).                                    
001900*                                 ID FÖR KAMPANJGRUPPER                   
002000        05 MOD-TISTADAT-KAMP PIC 9(6).                                    
002100*                                 STARTDATUM FÖR KAMPANJ                  
002200        05 MOD-TISTODAT-KAMP PIC 9(6).                                    
002300*                                 STOPPDATUM FÖR KAMPANJ                  
002400        05 MOD-KDKAMP        PIC X.                                       
002500*                                                                         
002600     03 MOD-TEMFSINF         PIC X(55).                                   
002700*                                 INFORMATIONSMEDDELANDE                  
002800*** END OF VILMAII-COPY LENGTH= 546 BYTES                                 
