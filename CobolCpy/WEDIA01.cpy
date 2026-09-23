000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    ARTIKELRADER                                                         
000030*    ANVÄNDS FÖR TRANSM. TILL TULLEN, GENERELL COPYTEXT                   
000050*                                                                         
000060*    FUNKTION,                                                            
000070*    -INNEHÅLLER ARTIKELRADINFORMATION                                    
000071*     POSTTYP - A01                                                       
000080*                                                                         
000100 01  WEDIA01.                                                             
000110     03 A01-SLINE                              PIC X(01).                 
000111*                                                                         
000112     03 A01-IDPTYP                             PIC X(03).                 
000113*                                              A01                        
000230     03 A01-IDFAKT                             PIC X(35).                 
000240*                                                                         
000250     03 A01-LINE                               PIC 9(06).                 
000260*                                                                         
000270     03 A01-ARTICLE-NUMBER                     PIC X(25).                 
000560*                                                                         
000561     03 A01-BEPSN-UN                           PIC X(10).                 
000562*                                                                         
000563     03 A01-KDSORT                             PIC X(35).                 
000564*                                                                         
000565*                                                                         
000570*** END OF VILMAII-COPY LENGTH=115                                        
