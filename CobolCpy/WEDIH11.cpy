000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    HUVUDINFORMATION                                                     
000030*    ANVÄNDS FÖR TRANSM. TILL TULLEN, GENERELL COPYTEXT                   
000050*                                                                         
000060*    FUNKTION,                                                            
000070*    -INNEHÅLLER HUVUDINFORMATION                                         
000071*     POSTTYP - H11                                                       
000080*                                                                         
000100 01  WEDIH11.                                                             
000110     03 H11-SLINE                              PIC X(01).                 
000111*                                                                         
000112     03 H11-IDPTYP                             PIC X(03).                 
000113*                                              H11                        
000230     03 H11-VEHICLE-NO                         PIC X(35).                 
000240*                                                                         
000501     03 H11-FULL-CONTAINER                     PIC X(17).                 
000502*                                                                         
000503     03 H11-MODE-OF-TRANSPORT                  PIC X(17).                 
000504*                                                                         
000505     03 H11-DESTINATION                        PIC X(25).                 
000560*                                                                         
000561*                                                                         
000570*** END OF VILMAII-COPY LENGTH=98                                         
