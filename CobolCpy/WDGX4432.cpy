000100 01  4432-WDGX4432.                                                       
000200*                                 BESKRIVNING AV TRANSPORT                
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                 (IDTRP)                                 
000500     03 4432-IDTRP.                                                       
000600*                                 TRANSPORTIDENTITET                      
000700*                                 TRANSPORTIDENTITY                       
000800        05 4432-IDTRPLOS     PIC X(3).                                    
000900*                                 TRANSPORTLÖSNING                        
001000*                                 TRANSPORTSOLUTION                       
001100        05 4432-IDTRPVAR     PIC X(2).                                    
001200*                                 TRANSPORTLÖSNINGSGRUPP                  
001300*                                 TRANSPORTSOLUTIONGROUP                  
001400     03 4432-BETRPDST        PIC X(15).                                   
001500*                                 TRANSPORTDESTINATION                    
001600*                                 TRANSPORTDESTINATION                    
001700     03 4432-KVLASTTI        PIC S9(3)V9(2)      COMP-3.                  
001800*                                 TID DET TAR ATT LASTA                   
001900*                                 TIME REQUIRED TO LOAD                   
002000     03 4432-KVADMFL         PIC S9(3)V9(2)      COMP-3.                  
002100*                                 ADM-TID FÖRE LASTNING                   
002200*                                 ADM-TIME BEFORE LOADING                 
002300     03 4432-KVADMEL         PIC S9(3)V9(2)      COMP-3.                  
002400*                                 ADM-TID EFTER LASTNING                  
002500*                                 ADM-TIME AFTER LOADING                  
002600     03 4432-FILLER          PIC X.                                       
002700*** END COPY WDGX4432C0  LENGTH=30                                        
