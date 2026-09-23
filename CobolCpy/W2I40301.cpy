000100 01  W2I40301.                                                            
000200*                                 COPYTEXT FÖR MID W2I40301               
000300     03 IDARTNR-IN           PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 IDDC-IN              PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 KDAVROP-IN           PIC X.                                       
000800*                                 AVROPSKOD                               
000900     03 IDLEVNR-IN           PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 INPUT.                                                            
001200        05 KDKOM             PIC X.                                       
001300*                                 KOMMENTARKOD                            
001400        05 KDOMSPEC          PIC X.                                       
001500*                                 OMSPEC 1=VANLIG,2=OPTIMAL               
001600        05 AVROP-INPUT       OCCURS 4 TIMES.                              
001700           07 TIAVROP-AVS    PIC X(4).                                    
001800*                                 AVSÄNDNINGSVECKA (PLANERAD)             
001900*                                 (ÅÅVV)                                  
002000           07 KVAVROP        PIC 9(7).                                    
002100*                                 AVROPSKVANTITET                         
002200        05 KDLEVPLF          PIC X.                                       
002300*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
002400        05 FLJIT             PIC X.                                       
002500*                                 JUST-IN-TIME FLAGGA                     
002600        05 TILPSP            PIC X(4).                                    
002700*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
002800        05 TEREFMED-1        PIC X(36).                                   
002900        05 TEREFMED-2        PIC X(36).                                   
003000*** END OF VILMAII-COPY LENGTH= 141 BYTES                                 
