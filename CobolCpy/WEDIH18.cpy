000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    HUVUDINFORMATION                                                     
000030*    ANVÄNDS FÖR TRANSM. TILL TULLEN, GENERELL COPYTEXT                   
000050*                                                                         
000060*    FUNKTION,                                                            
000070*    -INNEHÅLLER HUVUDINFORMATION                                         
000071*     POSTTYP - H18                                                       
000080*                                                                         
000100 01  WEDIH18.                                                             
000110     03 H18-SLINE                              PIC X(01).                 
000111*                                                                         
000112     03 H18-IDPTYP                             PIC X(03).                 
000113*                                              H18                        
000230     03 H18-INVOICE-NO                         PIC X(35).                 
000240*                                                                         
000561     03 H18-INVOICE-DATE-YYYYMMDD              PIC X(10).                 
000562*                                                                         
000565     03 H18-RFS-DATE-YYYYMMDDHHMM              PIC X(59).                 
000566*                                                                         
000567     03 H18-TRANS-CARR-ISO-AVG                 PIC X(2).                  
000568*                                                                         
000569     03 H18-TRANS-CARR-ISO-GRP                 PIC X(2).                  
000570*                                                                         
000571*                                                                         
000580*** END OF VILMAII-COPY LENGTH=112                                        
