000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    HUVUDINFORMATION                                                     
000030*    ANVÄNDS FÖR TRANSM. TILL TULLEN, GENERELL COPYTEXT                   
000050*                                                                         
000060*    FUNKTION,                                                            
000070*    -INNEHÅLLER HUVUDINFORMATION                                         
000071*     POSTTYP - H01                                                       
000080*                                                                         
000100 01  WEDIH01.                                                             
000110     03 H01-SLINE                              PIC X(01).                 
000111*                                                                         
000112     03 H01-IDPTYP                             PIC X(03).                 
000113*                                              H01                        
000230     03 H01-INPUT-TYPE                         PIC X(01).                 
000240*                                                                         
000501     03 H01-IDFAKT                             PIC X(07).                 
000504*                                                                         
000505     03 H01-DATE-YYYY                          PIC X(28).                 
000506*                                                                         
000507     03 H01-SHORT-NAME                         PIC X(17).                 
000518*                                                                         
000519     03 H01-CARRIAGE-CODE                      PIC X(17).                 
000520*                                                                         
000530     03 H01-INVOICE-DATE-DELIVERY              PIC X(12).                 
000560*                                                                         
000561*                                                                         
000570*** END OF VILMAII-COPY LENGTH=86                                         
