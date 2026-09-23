000100 01  MID-W4I34701.                                                        
000200*                                 MID-COPYTEXT FÖR PROGRAM 4347           
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDORDNR-IN       PIC X(5).                                    
000800*                                 ORDERNUMMER UTGÅR PD90                  
000900     03 MID-IDPRODNR-IN      PIC X(7).                                    
001000*                                 PRODUKTIONSNUMMER                       
001100     03 MID-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MID-FLTRPTCHG        PIC X.                                       
001400*                                 SKA TRANSPORTNUMMER ÄNDRAS?             
001500*                                                                         
001600     03 MID-IDTRPTNR         PIC X(3).                                    
001700*                                 TRANSPORTIDENTITET                      
001800     03 MID-KDFRAKT          PIC X(2).                                    
001900*                                 FRAKTSÄTT DC TILL KUND                  
002000     03 MID-RAD              OCCURS 12 TIMES.                             
002100*                                 RADER MED ALLA TRANSPORTER I EN         
002200*                                 ORDER.                                  
002300        05 MID-VALFLAGGA     PIC X.                                       
002400*                                 ALLMÄN FLAGGA                           
002500        05 MID-IDKOLLI-IN    PIC X(5).                                    
002600*                                 KOLLINUMMER                             
002700        05 MID-IDTRPTNR-IN   PIC X(3).                                    
002800*                                 TRANSPORTIDENTITET                      
002900        05 MID-KDFRAKT-IN    PIC X(2).                                    
003000*                                 FRAKTSÄTT DC TILL KUND                  
003100*** END OF VILMAII-COPY LENGTH= 162 BYTES                                 
