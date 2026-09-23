000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    HUVUDINFORMATION                                                     
000030*    ANVÄNDS FÖR TRANSM. TILL TULLEN, GENERELL COPYTEXT                   
000050*                                                                         
000060*    FUNKTION,                                                            
000070*    -INNEHÅLLER HUVUDINFORMATION                                         
000071*     POSTTYP - H03                                                       
000080*                                                                         
000100 01  WEDIH03.                                                             
000110     03 H03-SLINE                              PIC X(01).                 
000111*                                                                         
000112     03 H03-IDPTYP                             PIC X(03).                 
000113*                                              H03                        
000230     03 H03-CONSIGNEE-1                        PIC X(35).                 
000240*                                                                         
000250     03 H03-CONSIGNEE-2                        PIC X(35).                 
000260*                                                                         
000501     03 H03-CONSIGNEE-3                        PIC X(35).                 
000560*                                                                         
000561*                                                                         
000570*** END OF VILMAII-COPY LENGTH=109                                        
