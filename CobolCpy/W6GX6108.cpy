000100 01  6108-W6GX6108.                                                       
000200*                                 BESKRIVNING AV                          
000300*                                 LASTBÄRARE - IN                         
000400*                                 FYSISK NYCKEL                           
000500*                                 W6GXKEY =                               
000600*                                 (IDLBBET + LOW-VALUE)                   
000700     03 6108-IDLBBET         PIC X(12).                                   
000800*                                 LASTBÄRARBETECKNING                     
000900*                                 TRAILER NUMBER                          
001000     03 6108-LOW-VALUE       PIC X(3).                                    
001100     03 6108-ADINLOMR-LPL    PIC X(4).                                    
001200*                                 LOSSNINGSPLATS                          
001300*                                 UNLOADING AREA                          
001400     03 6108-TIREGDAT        PIC S9(7)           COMP-3.                  
001500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001600*                                 REGISTRATION DATE (YYMMDD)              
001700     03 6108-VLLBNTO         PIC S9(2)V9(3)      COMP-3.                  
001800*                                 VOLYM PER LASTBÄRARE NETTO              
001900*                                 VOLUME OF A CARRIER NET                 
002000     03 6108-FILLER          PIC X(4).                                    
002100*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
