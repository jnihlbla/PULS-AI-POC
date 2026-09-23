000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    HUVUDINFORMATION                                                     
000030*    ANVÄNDS FÖR TRANSM. TILL TULLEN, GENERELL COPYTEXT                   
000050*                                                                         
000060*    FUNKTION,                                                            
000070*    -INNEHÅLLER HUVUDINFORMATION                                         
000071*     POSTTYP - H13                                                       
000080*                                                                         
000100 01  WEDIH13.                                                             
000110     03 H13-SLINE                              PIC X(01).                 
000111*                                                                         
000112     03 H13-IDPTYP                             PIC X(03).                 
000113*                                              H13                        
000230     03 H13-IDPARTNR                           PIC X(18).                 
000240*                                                                         
000250     03 H13-IDSIGILL                           PIC X(51).                 
000260*                                                                         
000501     03 H13-MARKS-1                            PIC X(35).                 
000502*                                                                         
000503*                                                                         
000504*** END OF VILMAII-COPY LENGTH=108                                        
