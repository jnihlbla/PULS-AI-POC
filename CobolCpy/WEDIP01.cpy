000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    FÖRPACKNINGSRADER                                                    
000030*    ANVÄNDS FÖR TRANSM. TILL TULLEN, GENERELL COPYTEXT                   
000050*                                                                         
000060*    FUNKTION,                                                            
000070*    -INNEHÅLLER FÖRPACKNINGSINFO                                         
000070*     POSTTYP - P01                                                       
000080*                                                                         
000100 01  WEDIP01.                                                             
000230     03 P01-SLINE                              PIC X(01).                 
000240*                                                                         
000230     03 P01-IDPTYP                             PIC X(03).                 
000240*                                              P01                        
000230     03 P01-NUMBER                             PIC X(35).                 
000240*                                                                         
000230     03 P01-NUMBER-OF-PACKAGES                 PIC 9(08).                 
000240*                                                                         
000230     03 P01-FILLER                             PIC X(06).                 
000240*                                                                         
000230     03 P01-TYPE-OF-PACKAGES                   PIC X(07).                 
000504*                                                                         
000542*                                                                         
000550*** END OF VILMAII-COPY LENGTH=60                                         
