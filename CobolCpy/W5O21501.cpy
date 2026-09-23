000100 01  MOD-W5O21501.                                                        
000200*                                 MOD-COPYTEXT FÖR W50215                 
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-KDEKHHT-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-KDEKHHT-UT       PIC X(3).                                    
001100*                                 EKONOMISK HUVUDHÄNDELSE                 
001200     03 MOD-IDFTG-UT         PIC 9(2).                                    
001300*                                 FÖRETAGSID EKONOM REDOVISNING           
001400     03 MOD-KDEKHHT          PIC X(3).                                    
001500*                                 EKONOMISK HUVUDHÄNDELSE                 
001600     03 MOD-BEEKHHT          PIC X(25).                                   
001700*                                 BESKR. EKONOMISK HUVUDHÄNDELSE          
001800     03 MOD-TABELLRAD        OCCURS 13 TIMES.                             
001900*                                 GRUPP MED TABELL RADER                  
002000        05 MOD-KDEKSHT       PIC X(3).                                    
002100*                                 EKONOMISK SUBHÄNDELSE                   
002200        05 MOD-BEEKSHT       PIC X(25).                                   
002300*                                 BESKR. EKONOMISK SUBHÄNDELSE            
002400     03 MOD-KDEKHHT-NY       PIC X(3).                                    
002500*                                 EKONOMISK HUVUDHÄNDELSE                 
002600     03 MOD-BEEKHHT-NY       PIC X(25).                                   
002700*                                 BESKR. EKONOMISK HUVUDHÄNDELSE          
002800     03 MOD-KDEKSHT-NY-ATTR  PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 MOD-KDEKSHT-NY       PIC X(3).                                    
003100*                                 EKONOMISK SUBHÄNDELSE                   
003200     03 MOD-BEEKSHT-NY-ATTR  PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-BEEKSHT-NY       PIC X(25).                                   
003500*                                 BESKR. EKONOMISK SUBHÄNDELSE            
003600     03 MOD-TEMFSINF         PIC X(55).                                   
003700*                                 INFORMATIONSMEDDELANDE                  
003800*** END OF VILMAII-COPY LENGTH= 558 BYTES                                 
