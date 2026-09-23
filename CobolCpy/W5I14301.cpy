000100 01  MID-W5I14301.                                                        
000200*                                 COPYTEXT FÖR MID W5I14301               
000300*                                                                         
000400     03 MID-IDARTNR-EMB-IN   PIC X(9).                                    
000500*                                 EMBALLAGE-ARTIKELNUMMER                 
000600     03 MID-IDARTNR-EMB-UT   PIC X(9).                                    
000700*                                 EMBALLAGE-ARTIKELNUMMER                 
000800     03 MID-IDARTNR-EMB-SPAR PIC X(9).                                    
000900*                                 EMBALLAGE-ARTIKELNUMMER                 
001000     03 MID-IDARTNR-EMB-RAD  PIC X(9).                                    
001100*                                 EMBALLAGE-ARTIKELNUMMER                 
001200     03 MID-IDARTNR-EMB      PIC X(9).                                    
001300*                                 EMBALLAGE-ARTIKELNUMMER                 
001400     03 MID-PRDMTRL          PIC X(10).                                   
001500*                                 DIREKT MATERIAL                         
001600     03 MID-KDCMD            PIC X.                                       
001700      88 MID-KDCMD-INGENTING VALUE ' '.                                   
001800      88 MID-KDCMD-DELETE    VALUE 'D'                                    
001900                             'B'.                                         
002000      88 MID-KDCMD-REPLACE   VALUE 'R'                                    
002100                             'Ä'.                                         
002200      88 MID-KDCMD-INSERT    VALUE 'I'                                    
002300                             'N'.                                         
002400*                                 RAD-UPPDATERINGSKOMMANDO                
002500*** END OF VILMAII-COPY LENGTH= 56 BYTES                                  
