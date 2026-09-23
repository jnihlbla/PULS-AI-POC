000100 01  4462-WDGX4462.                                                       
000200*                                 LÖPNUMMER PLOCKSATSER                   
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                 (KDPRCGRP)                              
000500     03 4462-KDPRCGRP        PIC X(5).                                    
000600*                                 PRODUKTIONSKANALSGRUPP                  
000700*                                 GROUP OF PRODUCTION CHANNELS            
000800     03 4462-IDLOPNR-PL      PIC S9(3)           COMP-3.                  
000900*                                 PLOCKSATSENS LÖPNUMMER                  
001000*                                 INOM PRC-GRUPP                          
001100*                                 SEQUENCE-NUMBER FOR THE                 
001200*                                 PICKING UNIT WITHIN PRC-GROUP           
001300     03 4462-TIDATUM         PIC S9(7)           COMP-3.                  
001400*                                 DATUM ENLIGT KDDATFORM                  
001500*                                 DATE AS SPECIFIED BY KDDATFORM          
001600     03 4462-FILLER          PIC X(9).                                    
001700*** END COPY WDGX4462C0  LENGTH=20                                        
