000100 01  WDD905.                                                              
000200*                                 LEVERANSPLANEREGISTER                   
000300*                                 AVROPSINFORMATION                       
000400*                                 FYSISK NYCKEL: WDD905KY                 
000500*                                 (DAAVROP(-AVS), TILEVDAG)               
000600*                                 SÖKARGUMENT    KDAVROP                  
000700     03 KDAVROP              PIC S9              COMP-3.                  
000800*                                 AVROPSKOD                               
000900     03 DAAVROP-AVS          PIC 9(6).                                    
001000*                                 AVSÄNDNINGSVECKA (PLANERAD)             
001100*                                 (ÅÅÅÅVV)                                
001200     03 TILEVDAG             PIC S9              COMP-3.                  
001300*                                 AVSÄNDNINGSDAG INOM VECKA               
001400     03 TIAVRDAT-INL         PIC S9(7)           COMP-3.                  
001500*                                 PLANERAT INLEVERANSDATUM                
001600     03 TIAVRDAT-DISP        PIC S9(7)           COMP-3.                  
001700*                                 PLANERAT DISPONIBLEDATUM                
001800     03 KVAVROP              PIC S9(7)           COMP-3.                  
001900*                                 AVROPSKVANTITET                         
002000*** END OF VILMAII-COPY LENGTH= 20 BYTES                                  
