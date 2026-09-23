000010 01  3144-WDGX3144.                                                       
000020*                                 BYTES                                   
000030*                                 ÅTERSTARTSINFORMATION                   
000040*                                 LADDNING AV BYTESREG                    
000050*                                 INFO FRÅN SUPPORTLAGER                  
000060*                                 FYSISK NYCKEL: KDSEGKEY                 
000070*                                 (SKALL VARA "1")                        
000080     03 3144-KDSEGKEY        PIC X.                                       
000090*                                 TEKNISK SEGMENT-NYCKEL                  
000100*                                 TECHNICAL SEGMENT KEY                   
000110     03 3144-KVPOST          PIC S9(7)           COMP-3.                  
000120*                                 RÄKNARE, ANTAL POSTER                   
000130*                                 RECORD COUNTER                          
000140     03 FILLER               PIC X(25).                                   
      *** END COPY WDGX3144    LENGTH=30                                        
