000100 01  MOD-W5O21101.                                                        
000200*                                 MOD-COPYTEXT FÖR W50211                 
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-FROM-DAREGDAT-IN PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-FROM-DAREGDAT-UT PIC 9(8).                                    
001100*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001200     03 MOD-TOM-DAREGDAT-IN  PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-TOM-DAREGDAT-UT  PIC 9(8).                                    
001500*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001600     03 MOD-IDPGM-IN         PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-IDPGM-UT         PIC X(8).                                    
001900*                                 PROGRAM IDENTITET                       
002000     03 MOD-ERROR-NR         PIC Z(6)9.                                   
002100     03 MOD-TABELLRAD        OCCURS 13 TIMES.                             
002200*                                 GRUPP MED TABELL RADER                  
002300        05 MOD-CMD-ATTR      PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500        05 MOD-CMD           PIC X.                                       
002600        05 MOD-DAREGDAT      PIC 9(8).                                    
002700*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002800        05 MOD-KDEKHHT       PIC X(3).                                    
002900*                                 EKONOMISK HUVUDHÄNDELSE                 
003000        05 MOD-KDEKSHT       PIC X(3).                                    
003100*                                 EKONOMISK SUBHÄNDELSE                   
003200        05 MOD-KDEKNIVA      PIC X(5).                                    
003300*                                 EKONOMISK HÄNDELSENIVÅ                  
003400        05 MOD-IDPGM         PIC X(8).                                    
003500*                                 PROGRAM IDENTITET                       
003600        05 MOD-IDTRANS-RAD   PIC X(4).                                    
003700*                                 BILDNUMMER                              
003800        05 MOD-BEFEL         PIC X(38).                                   
003900     03 MOD-TEMFSINF         PIC X(55).                                   
004000*                                 INFORMATIONSMEDDELANDE                  
004100*** END OF VILMAII-COPY LENGTH= 1072 BYTES                                
