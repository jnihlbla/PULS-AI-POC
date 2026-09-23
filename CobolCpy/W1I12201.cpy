000100 01  MID-W1I12201.                                                        
000200     03 MID-BEART-IN         PIC X(25).                                   
000300*                                 ARTIKELBENÄMNING                        
000400     03 MID-BEART-UT         PIC X(25).                                   
000500*                                 ARTIKELBENÄMNING                        
000600     03 MID-KDHOMONYM-IN     PIC X.                                       
000700*                                 HOMONYMKOD                              
000800     03 MID-KDHOMONYM-UT     PIC X.                                       
000900*                                 HOMONYMKOD                              
001000     03 MID-RAD              OCCURS 11 TIMES.                             
001100        05 MID-BEART         PIC X(25).                                   
001200*                                 ARTIKELBENÄMNING                        
001300     03 MID-SKAPA-BEN        PIC X.                                       
001400     03 MID-RAD2             OCCURS 2 TIMES.                              
001500        05 MID-TEHOMONYM     PIC X(60).                                   
001600*                                 HOMONYMTEXT                             
001700*** END OF VILMAII-COPY LENGTH= 448 BYTES                                 
