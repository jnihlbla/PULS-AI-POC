000010*** EDIT ALLOWED                                                          
000012                                                                          
000020*    ARTIKELRADER                                                         
000030*    ANVÄNDS FÖR TRANSM. TILL TULLEN, GENERELL COPYTEXT                   
000050*                                                                         
000060*    FUNKTION,                                                            
000070*    -INNEHÅLLER ARTIKELRADINFORMATION                                    
000071*     POSTTYP - A04                                                       
000080*                                                                         
000100 01  WEDIA04.                                                             
000110     03 A04-SLINE                              PIC X(01).                 
000111*                                                                         
000112     03 A04-IDPTYP                             PIC X(03).                 
000113*                                              A04                        
000230     03 A04-IDKOLLI-MIC                        PIC X(35).                 
000240*                                                                         
000241     03 A04-BEEMBTYP                           PIC X(05).                 
000242*                                                                         
000243     03 A04-VKORDBTO-KOLLI                     PIC X(10).                 
000244*                                                                         
000245     03 A04-VLORDBTO-KOLLI                     PIC X(10).                 
000246*                                                                         
000247     03 A04-FILLER                             PIC X(40).                 
000248*                                                                         
000250     03 A04-OTHER-QUANTITY                     PIC X(15).                 
000260*                                                                         
000270*                                                                         
000280*** END OF VILMAII-COPY LENGTH=119                                        
