000100 01  W2I10301.                                                            
000200*                                 COPYTEXT FÖR MID W2I10301               
000300     03 IDARTNR-IN           PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 IDARTNR-UT           PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 KDBEHX-PLAN-IN       PIC X.                                       
000800*                                 BEHANDLINGSKOD-X                        
000900     03 KDBEHX-PLAN-UT       PIC X.                                       
001000*                                 BEHANDLINGSKOD-X                        
001100     03 IDLEVNR-IN           PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 IDLEVNR-UT           PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 KOMKOD               PIC X.                                       
001600*                                 KOMMENTARKOD                            
001700     03 AVROP-INPUT          OCCURS 4 TIMES.                              
001800*                                                                         
001900        05 TIAVROP-AVS       PIC X(4).                                    
002000*                                 AVSÄNDNINGSVECKA (PLANERAD)             
002100*                                 (ÅÅVV)                                  
002200        05 KVAVROP           PIC X(7).                                    
002300*                                 AVROPSKVANTITET                         
002400     03 KVBEST-PL            PIC X(6).                                    
002500*                                 BESTÄLLNINGSKVANTITET PÅ PLAN           
002600     03 KDOMSPEC             PIC X.                                       
002700*                                 OMSPEC 1=VANLIG,2=OPTIMAL               
002800     03 TILPSP               PIC X(4).                                    
002900*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
003000     03 KDLEVPLF             PIC X.                                       
003100*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
003200     03 FLJIT                PIC X.                                       
003300*                                 JUST-IN-TIME FLAGGA                     
003400     03 TEARTNOT1            PIC X(40).                                   
003500*                                 ARTIKEL NOTERING                        
003600     03 TEARTNOT2            PIC X(40).                                   
003700*                                 ARTIKEL NOTERING                        
003800*** END OF VILMAII-COPY LENGTH= 168 BYTES                                 
