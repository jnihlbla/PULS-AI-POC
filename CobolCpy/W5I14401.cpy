000100 01  MID-W5I14401.                                                        
000200*                                 COPYTEXT FÖR MID W5I144                 
000300*                                                                         
000400     03 MID-KDFORPGP-IN      PIC X(2).                                    
000500*                                 FÖRPACKNINGSGRUPP                       
000600     03 MID-KDFORPGP-UT      PIC X(2).                                    
000700*                                 FÖRPACKNINGSGRUPP                       
000800     03 MID-KDFORPGP-SPAR    PIC X(2).                                    
000900*                                 FÖRPACKNINGSGRUPP                       
001000     03 MID-KDFORPGP-SPAR-RAD1                                            
001100                             PIC X(2).                                    
001200*                                 FÖRPACKNINGSGRUPP                       
001300     03 MID-KDFORPGP-UPP     PIC X(2).                                    
001400*                                 FÖRPACKNINGSGRUPP                       
001500     03 MID-PRDIRLON         PIC X(8).                                    
001600*                                 DIREKT LÖN                              
001700     03 MID-PRDMTRL          PIC X(10).                                   
001800*                                 DIREKT MATERIAL                         
001900     03 MID-PROVRPAL         PIC X(8).                                    
002000*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
002100     03 MID-KDCMD            PIC X.                                       
002200      88 MID-KDCMD-INGENTING VALUE ' '.                                   
002300      88 MID-KDCMD-DELETE    VALUE 'D'                                    
002400                             'B'.                                         
002500      88 MID-KDCMD-REPLACE   VALUE 'R'                                    
002600                             'Ä'.                                         
002700      88 MID-KDCMD-INSERT    VALUE 'I'                                    
002800                             'N'.                                         
002900*                                 RAD-UPPDATERINGSKOMMANDO                
003000*** END OF VILMAII-COPY LENGTH= 37 BYTES                                  
