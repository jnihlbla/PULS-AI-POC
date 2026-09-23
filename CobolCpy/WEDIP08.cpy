000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    FÖRPACKNINGSRADER                                                    
000030*    ANVÄNDS FÖR TRANSM. TILL TULLEN, GENERELL COPYTEXT                   
000050*                                                                         
000060*    FUNKTION,                                                            
000070*    -INNEHÅLLER FÖRPACKNINGSINFO                                         
000070*     POSTTYP - P08                                                       
000080*                                                                         
000100 01  WEDIP08.                                                             
000230     03 P08-SLINE                              PIC X(01).                 
000240*                                                                         
000230     03 P08-IDPTYP                             PIC X(03).                 
000240*                                              P08                        
000230     03 P08-FILLER                             PIC X(09).                 
000240*                                                                         
000230     03 P08-CONTAINER-NO                       PIC X(12).                 
000504*                                                                         
000542*                                                                         
000550*** END OF VILMAII-COPY LENGTH=25                                         
