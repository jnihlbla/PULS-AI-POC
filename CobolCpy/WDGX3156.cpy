000100 01  3156-WDGX3156.                                                       
000200*                                 BYTES STYR-PARAMETRAR                   
000300*                                 FYSISK NYCKEL: KDSEGKEY                 
000400*                                 (SKALL VARA "1")                        
000500     03 3156-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 3156-FLEXCREP        PIC X.                                       
000900*                                 EXCHANGE REPORT FLAG                    
001000     03 3156-FLEXCBLK        PIC X.                                       
001100*                                 EXCHANGE BLOCK CODE                     
001200     03 3156-FILLER          PIC X(16).                                   
001300     03 3156-TIVECKNR-BYTDEB OCCURS 4 TIMES                               
001400                             PIC S9(3)           COMP-3.                  
001500*                                 VECKONUMMER (VV)                        
001600     03 3156-REPOINT         PIC S9(5)V9(4)      COMP-3.                  
001700*                                 CONVERSION FACTOR                       
001800     03 3156-IDMAIL          PIC X(60).                                   
001900*                                 MAIL ADRESS                             
002000*                                 MAIL ADDRESS                            
002100*** END OF VILMAII-COPY LENGTH= 92 BYTES                                  
