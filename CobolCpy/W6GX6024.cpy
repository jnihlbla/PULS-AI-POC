000010 01  6024-W6GX6024.                                                       
000020*                                 POSTRÄKNARE VID                         
000030*                                 CHECKPOINT                              
000040*                                 FYSISK NYCKEL: KDSEGKEY 1               
000050     03 6024-KDSEGKEY        PIC X.                                       
000060*                                 TEKNISK SEGMENT-NYCKEL                  
000070*                                 TECHNICAL SEGMENT KEY                   
000080     03 6024-KVPOST          PIC S9(7)           COMP-3.                  
000090*                                 RÄKNARE, ANTAL POSTER                   
000100*                                 RECORD COUNTER                          
000110     03 6024-TIUPPDAT        PIC S9(7)           COMP-3.                  
000120*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
000130*                                 UPDATING DATE     (YYMMDD)              
000140     03 6024-TIUPPTID        PIC S9(9)           COMP-3.                  
000150*                                 UPPDATERINGSTID  (TTMMSSTH)             
000160*                                 UPDATING TIME    (HHMMSSTH)             
000170     03 FILLER               PIC X(6).                                    
      *** END COPY W6GX6024    LENGTH=20                                        
