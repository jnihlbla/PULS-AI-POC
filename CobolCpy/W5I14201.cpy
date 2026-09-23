000100 01  MID-W5I14201.                                                        
000200*                                 COPYTEXT FÖR MID W5I142                 
000300*                                                                         
000400     03 MID-IDARTNR-IN       PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-IDARTNR-UT       PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 MID-IDARTNR-SPAR     PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MID-IDARTNR-SPAR-RAD1                                             
001100                             PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MID-IDARTNR-UPP      PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MID-PRDIRLON         PIC X(8).                                    
001600*                                 DIREKT LÖN                              
001700     03 MID-PRDMTRL          PIC X(10).                                   
001800*                                 DIREKT MATERIAL                         
001900     03 MID-PROVRPAL         PIC X(8).                                    
002000*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
002100     03 MID-FLFPTILL         PIC X.                                       
002200*                                 FÖRPACKN. PRISTILLÄGGS FLAGGA           
002300     03 MID-KDCMD            PIC X.                                       
002400      88 MID-KDCMD-INGENTING VALUE ' '.                                   
002500      88 MID-KDCMD-DELETE    VALUE 'D'                                    
002600                             'B'.                                         
002700      88 MID-KDCMD-REPLACE   VALUE 'R'                                    
002800                             'Ä'.                                         
002900      88 MID-KDCMD-INSERT    VALUE 'I'                                    
003000                             'N'.                                         
003100*                                 RAD-UPPDATERINGSKOMMANDO                
003200*** END OF VILMAII-COPY LENGTH= 73 BYTES                                  
