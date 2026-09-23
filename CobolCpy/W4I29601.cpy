000100 01  MID-W4I29601.                                                        
000200*                                 MID-COPYTEXT FÖR W40296                 
000300     03 MID-IDDISTR          PIC S9(5).                                   
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR         PIC S9(7).                                   
000600*                                 KUNDNUMMER                              
000700     03 MID-IDKUNDRF         PIC X(10).                                   
000800*                                 KUNDENS REFERENS (ORDERID)              
000900     03 MID-REOMRTAL         PIC 9(2)V9(3).                               
001000*                                 OMRÄKNINGSTAL                           
001100     03 MID-SUORDV           PIC 9(9)V9(2).                               
001200*                                 SUMMA ORDERVÄRDE                        
001300     03 MID-SUORDV-LOC       PIC 9(9)V9(2).                               
001400*                                 ORDERVÄRDE SLUTKUNDPRIS                 
001500*                                 I LOKAL VALUTA                          
001600     03 MID-SUORDV-LOCPREL   PIC 9(9)V9(2).                               
001700*                                 ORDERVÄRDE PREL SLUT-                   
001800*                                 KUNDPRIS, LOKAL VALUTA                  
001900     03 MID-NYCKEL-GRP.                                                   
002000        05 MID-IDORDER       PIC S9(7)           COMP-3.                  
002100*                                 VOLVO PARTS ORDERNUMMER                 
002200        05 MID-IDARTNR       PIC S9(9)           COMP-3.                  
002300*                                 ARTIKELNUMMER                           
002400        05 MID-IDLOPNR       PIC S9(3)           COMP-3.                  
002500*                                 LÖPNUMMER                               
002600*** END OF VILMAII-COPY LENGTH= 71 BYTES                                  
