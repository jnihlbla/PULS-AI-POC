000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    FÖRPACKNINGSRADER                                                    
000030*    ANVÄNDS FÖR TRANSM. TILL TULLEN, GENERELL COPYTEXT                   
000050*                                                                         
000060*    FUNKTION,                                                            
000070*    -INNEHÅLLER FÖRPACKNINGSINFO                                         
000071*     POSTTYP - P05                                                       
000080*                                                                         
000100 01  WEDIP05.                                                             
000110     03 P05-SLINE                              PIC X(01).                 
000111*                                                                         
000112     03 P05-IDPTYP                             PIC X(03).                 
000113*                                              P05                        
000230     03 P05-FILLER1                            PIC X(18).                 
000240*                                                                         
000250     03 P05-VKORDNTO-FAKT                      PIC X(15).                 
000260*                                                                         
000270     03 P05-VKORDBTO-FAKT                      PIC X(50).                 
000280*                                                                         
000290     03 P05-KIND-OF-GOODS                      PIC X(35).                 
000560*                                                                         
000561*                                                                         
000570*** END OF VILMAII-COPY LENGTH=122                                        
