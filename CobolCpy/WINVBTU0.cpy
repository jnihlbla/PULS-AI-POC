000100*** EDIT ALLOWED                                                          
000200 01  WDIRBTU0.                                                            
000300*                               RECORD TRAILER-UT DIRECT BUSINESS.        
000400*                                                                         
000500     03 T0-PROGRAM        PIC X(4).                                       
000600*                                                                         
000700     03 T0-FILEDEFNO      PIC X(2).                                       
000800*                                                                         
000900     03 T0-SENDLOC        PIC X(3).                                       
001000*                                                                         
001100     03 T0-RECLOC         PIC X(3).                                       
001200*                                                                         
001300     03 T0-SERIALNO       PIC X(7).                                       
001400*                                                                         
001500     03 T0-RECORDTYPE     PIC X(1).                                       
001600*                                                                         
001700*                                                                         
001800     03 T0-RECORD-COUNTS  OCCURS 24                                       
001810                          PIC 9(5).                                       
002700*                                                                         
002800     03 FILLER            PIC X(84).                                      
002900*                                                                         
003000*** END OF VILMAII-COPY LENGTH=224                                        
