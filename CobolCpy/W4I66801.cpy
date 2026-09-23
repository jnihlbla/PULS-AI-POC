000100 01  MID-W4I66801.                                                        
000200*                                 MID-COPYTEXT FÖR W40668                 
000300     03 MID-TIDATUM-IN       PIC X(6).                                    
000400*                                 DATUM ENLIGT KDDATFORM                  
000500     03 MID-TIDATUM-UT       PIC X(6).                                    
000600*                                 DATUM ENLIGT KDDATFORM                  
000700     03 MID-INPUT            OCCURS 42 TIMES.                             
000800*                                                                         
000900        05 MID-KDSVAR        PIC X.                                       
001000*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
001100        05 MID-IDTRPTNR      PIC 9(3).                                    
001200*                                 TRANSPORTIDENTITET                      
001300        05 MID-IDLBBET       PIC X(12).                                   
001400*                                 LASTBÄRARBETECKNING                     
001500*** END OF VILMAII-COPY LENGTH= 684 BYTES                                 
