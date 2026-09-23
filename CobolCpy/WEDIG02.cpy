000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    SAMSÄNDNINGSFÄLT                                                     
000030*    ANVÄNDS FÖR TRANSM. TILL TULLEN, GENERELL COPYTEXT                   
000050*                                                                         
000060*    FUNKTION,                                                            
000070*    -INNEHÅLLER SAMPACKNINGSINFO                                         
000070*     POSTTYP - G02                                                       
000080*                                                                         
000100 01  W476G02.                                                             
000230     03 G02-SLINE                              PIC X(01).                 
000504*                                                                         
000230     03 G02-IDPTYP                             PIC X(03).                 
000240*                                              G02                        
000501     03 G02-INPUT-TYPE                         PIC X(01).                 
000504*                                                                         
000230     03 G02-CARRIAGE-CODE                      PIC X(17).                 
000504*                                                                         
000542*                                                                         
000550*** END OF VILMAII-COPY LENGTH=22                                         
