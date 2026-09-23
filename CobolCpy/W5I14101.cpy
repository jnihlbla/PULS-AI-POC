000100 01  MID-W5I14101.                                                        
000200*                                 COPYTEXT FÖR MID W5I141                 
000300*                                                                         
000400     03 MID-BEFT-IN          PIC X(2).                                    
000500*                                 FÖRPACKNINGSTYP                         
000600     03 MID-BEFT-UT          PIC X(2).                                    
000700*                                 FÖRPACKNINGSTYP                         
000800     03 MID-BEFT-SPAR        PIC X(2).                                    
000900*                                 FÖRPACKNINGSTYP                         
001000     03 MID-BEFT-SPAR-RAD1   PIC X(2).                                    
001100*                                 FÖRPACKNINGSTYP                         
001200     03 MID-BEFT-UPP         PIC X(2).                                    
001300*                                 FÖRPACKNINGSTYP                         
001400     03 MID-PRDIRLON         PIC X(8).                                    
001500*                                 DIREKT LÖN                              
001600     03 MID-PRDMTRL          PIC X(10).                                   
001700*                                 DIREKT MATERIAL                         
001800     03 MID-PROVRPAL         PIC X(8).                                    
001900*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
002000     03 MID-KDCMD            PIC X.                                       
002100      88 MID-KDCMD-INGENTING VALUE ' '.                                   
002200      88 MID-KDCMD-DELETE    VALUE 'D'                                    
002300                             'B'.                                         
002400      88 MID-KDCMD-REPLACE   VALUE 'R'                                    
002500                             'Ä'.                                         
002600      88 MID-KDCMD-INSERT    VALUE 'I'                                    
002700                             'N'.                                         
002800*                                 RAD-UPPDATERINGSKOMMANDO                
002900*** END OF VILMAII-COPY LENGTH= 37 BYTES                                  
