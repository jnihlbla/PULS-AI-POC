000100 01  MOD-W5O21601.                                                        
000200*                                 MOD-COPYTEXT FÖR W50216                 
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-KDEKHHT-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-KDEKHHT-UT       PIC X(3).                                    
001100*                                 EKONOMISK HUVUDHÄNDELSE                 
001200     03 MOD-KDEKSHT-IN       PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-KDEKSHT-UT       PIC X(3).                                    
001500*                                 EKONOMISK SUBHÄNDELSE                   
001600     03 MOD-IDFTG-UT         PIC 9(2).                                    
001700*                                 FÖRETAGSID EKONOM REDOVISNING           
001800     03 MOD-TABELLRAD        OCCURS 13 TIMES.                             
001900*                                 GRUPP MED TABELL RADER                  
002000        05 MOD-KDEKHHT       PIC X(3).                                    
002100*                                 EKONOMISK HUVUDHÄNDELSE                 
002200        05 MOD-BEEKHHT       PIC X(25).                                   
002300*                                 BESKR. EKONOMISK HUVUDHÄNDELSE          
002400        05 MOD-KDEKSHT       PIC X(3).                                    
002500*                                 EKONOMISK SUBHÄNDELSE                   
002600        05 MOD-BEEKSHT       PIC X(25).                                   
002700*                                 BESKR. EKONOMISK SUBHÄNDELSE            
002800        05 MOD-KDEKNIVA      PIC X(5).                                    
002900*                                 EKONOMISK HÄNDELSENIVÅ                  
003000     03 MOD-KDEKHHT-NY       PIC X(3).                                    
003100*                                 EKONOMISK HUVUDHÄNDELSE                 
003200     03 MOD-BEEKHHT-NY       PIC X(25).                                   
003300*                                 BESKR. EKONOMISK HUVUDHÄNDELSE          
003400     03 MOD-KDEKSHT-NY       PIC X(3).                                    
003500*                                 EKONOMISK SUBHÄNDELSE                   
003600     03 MOD-BEEKSHT-NY       PIC X(25).                                   
003700*                                 BESKR. EKONOMISK SUBHÄNDELSE            
003800     03 MOD-KDEKNIVA-NY-ATTR PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-KDEKNIVA-NY      PIC X(5).                                    
004100*                                 EKONOMISK HÄNDELSENIVÅ                  
004200     03 MOD-TEMFSINF         PIC X(55).                                   
004300*                                 INFORMATIONSMEDDELANDE                  
004400*** END OF VILMAII-COPY LENGTH= 967 BYTES                                 
