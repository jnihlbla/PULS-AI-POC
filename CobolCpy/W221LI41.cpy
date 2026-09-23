000100 01  W221LI41.                                                            
000200*                                 POST FÖR FRAMSTÄLLNING AV               
000300*                                 BESTÄLLNINGSRAPPORT                     
000400*                                                                         
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 IDANSK               PIC S9(3)           COMP-3.                  
000800*                                 ANSKAFFARNUMMER                         
000900     03 IDLEVNR              PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 TIBESRPT-FOREG       PIC S9(5)           COMP-3.                  
001200*                                 SENASTE BESTÄLLNINGSRAPPORT DAT         
001300*                                 UM                                      
001400     03 KDBEH-ORSAK          PIC S9              COMP-3.                  
001500*                                 KOD FÖR ORSAK TILL BEST.RAPPORT         
001600     03 FLAGGA-PAAM          PIC X.                                       
001700*                                 ANGER OM "PÅMINNELSE" AVSES             
001800     03 KVBEST-BER           PIC S9(7)           COMP-3.                  
001900*                                 MASKINELLT FÖRSLAG TILL                 
002000*                                 BESTÄLLNINGSKVANTITET                   
002100     03 KVBR-TOT             PIC S9(7)           COMP-3.                  
002200*                                 TOT BEST REST                           
002300*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
