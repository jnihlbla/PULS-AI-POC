000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    HUVUDINFORMATION                                                     
000030*    ANVÄNDS FÖR TRANSM. TILL TULLEN, GENERELL COPYTEXT                   
000050*                                                                         
000060*    FUNKTION,                                                            
000070*    -INNEHÅLLER HUVUDINFORMATION                                         
000071*     POSTTYP - H26                                                       
000080*                                                                         
000100 01  WEDIH26.                                                             
000110     03 H26-SLINE                              PIC X(01).                 
000111*                                                                         
000112     03 H26-IDPTYP                             PIC X(03).                 
000113*                                              H26                        
000230     03 H26-FILLER                             PIC X(01).                 
000231     03 H26-PREMBHNT                           PIC X(10).                 
000232     03 H26-FILLER                             PIC X(01).                 
000233     03 H26-PRFRAKT                            PIC X(10).                 
000234     03 H26-FILLER                             PIC X(01).                 
000235     03 H26-PRFOERS                            PIC X(10).                 
000236     03 H26-FILLER                             PIC X(01).                 
000237     03 H26-PRLEGKST                           PIC X(10).                 
000238     03 H26-FILLER                             PIC X(04).                 
000240*                                                                         
000250     03 H26-BOOKING-REF-1                      PIC X(35).                 
000260*                                                                         
000270*                                                                         
000280*** END OF VILMAII-COPY LENGTH=87                                         
