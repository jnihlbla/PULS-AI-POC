000100 01  MOD-W5O21701.                                                        
000200*                                 MOD-COPYTEXT FÖR W50217                 
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
001600     03 MOD-KDEKNIVA-IN      PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-KDEKNIVA-UT      PIC X(5).                                    
001900*                                 EKONOMISK HÄNDELSENIVÅ                  
002000     03 MOD-IDFTG-UT         PIC 9(2).                                    
002100*                                 FÖRETAGSID EKONOM REDOVISNING           
002200     03 MOD-KDEKHHT          PIC X(3).                                    
002300*                                 EKONOMISK HUVUDHÄNDELSE                 
002400     03 MOD-BEEKHHT          PIC X(20).                                   
002500     03 MOD-KDEKSHT          PIC X(3).                                    
002600*                                 EKONOMISK SUBHÄNDELSE                   
002700     03 MOD-BEEKSHT          PIC X(20).                                   
002800     03 MOD-KDEKNIVA         PIC X(5).                                    
002900*                                 EKONOMISK HÄNDELSENIVÅ                  
003000     03 MOD-TABELLRAD        OCCURS 13 TIMES.                             
003100*                                 GRUPP MED TABELL RADER                  
003200        05 MOD-IDSYSMOT      PIC X(6).                                    
003300*                                 PULS MOTTAGANDE SYSTEMNAMN              
003400        05 MOD-IDPTYP        PIC X(3).                                    
003500*                                 POSTTYP                                 
003600     03 MOD-KDEKHHT-NY       PIC X(3).                                    
003700*                                 EKONOMISK HUVUDHÄNDELSE                 
003800     03 MOD-BEEKHHT-NY       PIC X(20).                                   
003900     03 MOD-KDEKSHT-NY       PIC X(3).                                    
004000*                                 EKONOMISK SUBHÄNDELSE                   
004100     03 MOD-BEEKSHT-NY       PIC X(20).                                   
004200     03 MOD-KDEKNIVA-NY      PIC X(5).                                    
004300*                                 EKONOMISK HÄNDELSENIVÅ                  
004400     03 MOD-IDSYSMOT-NY-ATTR PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-IDSYSMOT-NY      PIC X(6).                                    
004700*                                 PULS MOTTAGANDE SYSTEMNAMN              
004800     03 MOD-IDPTYP-NY-ATTR   PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-IDPTYP-NY        PIC X(3).                                    
005100*                                 POSTTYP                                 
005200     03 MOD-TEMFSINF         PIC X(55).                                   
005300*                                 INFORMATIONSMEDDELANDE                  
005400*** END OF VILMAII-COPY LENGTH= 350 BYTES                                 
