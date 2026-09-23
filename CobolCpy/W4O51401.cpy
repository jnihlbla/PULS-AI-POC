000100 01  MOD-W4O51401.                                                        
000200*                                 MOD-COPYTEXT FÖR W4O51401               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDDC-IN          PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-KDORDKL-IN       PIC 9.                                       
002000*                                 ORDERKLASS                              
002100     03 MOD-KDORDKL-UT       PIC X.                                       
002200*                                 ORDERKLASS                              
002300     03 MOD-KDFRAKT-IN       PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500     03 MOD-KDFRAKT-UT       PIC X(2).                                    
002600*                                 FRAKTSÄTT DC TILL KUND                  
002700     03 MOD-KDSTATUS-IN      PIC X.                                       
002800*                                 VOLVOORDERSTATUS                        
002900     03 MOD-KDSTATUS-UT      PIC X.                                       
003000*                                 VOLVOORDERSTATUS                        
003100     03 MOD-RADER            OCCURS 13 TIMES.                             
003200*                                 TRANSPORTINFO PER ORDERDEL              
003300        05 MOD-IDTRANS-RAD   PIC X(4).                                    
003400*                                 BILDNUMMER                              
003500        05 MOD-IDKUNDNR-RAD  PIC Z(5)9.                                   
003600*                                 KUNDNUMMER                              
003700        05 FILLER            PIC X.                                       
003800        05 MOD-IDORDNR7-RAD  PIC Z(6)9.                                   
003900*                                 ORDERNUMMER                             
004000        05 FILLER            PIC X(3).                                    
004100        05 MOD-IDDC-RAD      PIC X(2).                                    
004200*                                 IDENTIFIERARE LAGER                     
004300        05 FILLER            PIC X.                                       
004400        05 MOD-KDFRAKT-RAD   PIC Z9.                                      
004500*                                 FRAKTSÄTT DC TILL KUND                  
004600        05 MOD-FILLERX2      PIC X(2).                                    
004700        05 MOD-KDORDKL-RAD   PIC 9.                                       
004800*                                 ORDERKLASS                              
004900        05 FILLER            PIC X.                                       
005000        05 MOD-IDTRP-RAD.                                                 
005100*                                 TRANSPORTIDENTITET                      
005200           07 MOD-IDTRPLOS   PIC X(3).                                    
005300*                                 TRANSPORTLÖSNING                        
005400           07 MOD-IDTRPVAR   PIC X(2).                                    
005500*                                 TRANSPORTLÖSNINGSGRUPP                  
005600        05 FILLER            PIC X.                                       
005700        05 MOD-BETRPDST-RAD  PIC X(15).                                   
005800*                                 TRANSPORTDESTINATION                    
005900        05 FILLER            PIC X.                                       
006000        05 MOD-BETRPFIR-RAD  PIC X(15).                                   
006100*                                 TRANSPORTFIRMANS NAMN                   
006200        05 FILLER            PIC X.                                       
006300        05 MOD-TITRPAVG-RAD  PIC Z(2)BZBZ9B9(2).                          
006400*                                 TRANSPORTAVGÅNGSTID, VVDTTMM            
006500     03 MOD-TEMFSINF         PIC X(55).                                   
006600*                                 INFORMATIONSMEDDELANDE                  
006700*** END OF VILMAII-COPY LENGTH= 1139 BYTES                                
