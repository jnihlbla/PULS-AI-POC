000100 01  CURR-W510CURR.                                                       
000200*                                 PARAMETERS TO W510CURR SUBPGM           
000300*                                 INPUT - KDVALISO-HUV                    
000400*                                         KDVALTYP                        
000500*                                         KDVALISO-ROW                    
000600*                                         TIAAMM                          
000700*                                 OUTPUT- REVALUTA-FROM                   
000800*                                         REVALUTA-TO                     
000900*                                         PRKURS-NEW                      
001000*                                         TISTADAT                        
001100*                                 KDSVAR- BLANK - SUCCESS                 
001200*                                         2     - ERROR                   
001300*                                                                         
001400*                                                                         
001500     03 CURR-KDVALISO-HUV    PIC X(3).                                    
001600*                                 HUVUDVALUTA                             
001700     03 CURR-KDVALTYP        PIC X.                                       
001800*                                 KURSENS PER A=ÅR/M=MÅNAD/D=DAG          
001900     03 CURR-TIAAMM          PIC S9(5)           COMP-3.                  
002000*                                 ÅR - MÅNAD (ÅÅMM)                       
002100     03 CURR-KDVALISO-ROW    PIC X(3).                                    
002200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002300     03 CURR-REVALUTA-TO     PIC S9(5)           COMP-3.                  
002400*                                 OMRÄKNINGSFAKTOR TILL HUVUDVALU         
002500*                                 TA FROM ANDRA VALUTOR                   
002600     03 CURR-REVALUTA-FROM   PIC S9(5)           COMP-3.                  
002700*                                 OMRÄKNINGSFAKTOR FRÅN HUVUDVALU         
002800*                                 TA TILL ANDRA VALUTOR                   
002900     03 CURR-PRKURS-NEW      PIC S9(6)V9(6)      COMP-3.                  
003000*                                 VALUTAKURS                              
003100     03 CURR-TISTADAT        PIC S9(7)           COMP-3.                  
003200*                                 GENERELLT STARTDATUM                    
003300     03 CURR-TIREGDAT        PIC S9(7)           COMP-3.                  
003400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003500     03 CURR-KDSVAR          PIC X.                                       
003600*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
003700*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  
