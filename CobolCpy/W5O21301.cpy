000100 01  MOD-W5O21301.                                                        
000200*                                 MOD-COPYTEXT FÖR W50213                 
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
002000     03 MOD-IDSYSMOT-IN      PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200     03 MOD-IDSYSMOT-UT      PIC X(6).                                    
002300*                                 PULS MOTTAGANDE SYSTEMNAMN              
002400     03 MOD-IDPTYP-IN        PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-IDPTYP-UT        PIC X(3).                                    
002700*                                 POSTTYP                                 
002800     03 MOD-IDFTG-UT         PIC 9(2).                                    
002900*                                 FÖRETAGSID EKONOM REDOVISNING           
003000     03 MOD-TABELLRAD        OCCURS 15 TIMES.                             
003100*                                 GRUPP MED TABELL RADER                  
003200        05 MOD-CMD-ATTR      PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400        05 MOD-CMD           PIC X.                                       
003500        05 MOD-KDEKHHT       PIC X(3).                                    
003600*                                 EKONOMISK HUVUDHÄNDELSE                 
003700        05 MOD-BEEKHHT       PIC X(20).                                   
003800        05 MOD-KDEKSHT       PIC X(3).                                    
003900*                                 EKONOMISK SUBHÄNDELSE                   
004000        05 MOD-BEEKSHT       PIC X(20).                                   
004100        05 MOD-KDEKNIVA      PIC X(5).                                    
004200*                                 EKONOMISK HÄNDELSENIVÅ                  
004300        05 MOD-IDSYSMOT      PIC X(6).                                    
004400*                                 PULS MOTTAGANDE SYSTEMNAMN              
004500        05 MOD-IDPTYP        PIC X(3).                                    
004600*                                 POSTTYP                                 
004700     03 MOD-TEMFSINF         PIC X(55).                                   
004800*                                 INFORMATIONSMEDDELANDE                  
004900*** END OF VILMAII-COPY LENGTH= 1076 BYTES                                
