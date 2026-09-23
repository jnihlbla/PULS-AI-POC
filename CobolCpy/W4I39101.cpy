000100 01  MID-W4I39101.                                                        
000200*                                 MID-COPYTEXT FÖR W40391                 
000300     03 MID-IDPRODNR-IN      PIC X(7).                                    
000400*                                 PRODUKTIONSNUMMER                       
000500     03 MID-IDPRODNR-UT      PIC X(7).                                    
000600*                                 PRODUKTIONSNUMMER                       
000700     03 MID-IDDISTR-UT       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-KDFRAKT-UT       PIC X(2).                                    
001200*                                 FRAKTSÄTT DC TILL KUND                  
001300     03 MID-IDORDNR-UT       PIC X(5).                                    
001400*                                 ORDERNUMMER                             
001500     03 MID-KDORDKL-UT       PIC X.                                       
001600*                                 ORDERKLASS                              
001700     03 MID-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-PRTVAL-ADRESSFL  PIC X(2).                                    
002000*                                 PRINTER-VAL KOD                         
002100     03 MID-IDRADNR-SENAST   PIC X(4).                                    
002200*                                 RADNUMMER                               
002300     03 MID-RAPP-DEL.                                                     
002400*                                 MID-COPYTEXT FÖR W40391                 
002500        05 MID-RAD           OCCURS 13 TIMES.                             
002600*                                 MID-COPYTEXT FÖR W40391                 
002700           07 MID-IDRADNR    PIC 9(4).                                    
002800*                                 RADNUMMER                               
002900           07 MID-KVLEVART   PIC 9(6).                                    
003000*                                 LEVERERAT ANTAL STYCK                   
003100*** END OF VILMAII-COPY LENGTH= 170 BYTES                                 
