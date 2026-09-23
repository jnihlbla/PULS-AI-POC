000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    ARTIKELRADER                                                         
000030*    ANVÄNDS FÖR TRANSM. TILL TULLEN, GENERELL COPYTEXT                   
000050*                                                                         
000060*    FUNKTION,                                                            
000070*    -INNEHÅLLER ARTIKELRADINFORMATION                                    
000070*     POSTTYP - A02                                                       
000080*                                                                         
000100 01  WEDIA02.                                                             
000230     03 A02-SLINE                              PIC X(01).                 
000240*                                                                         
000230     03 A02-IDPTYP                             PIC X(03).                 
000240*                                              A02                        
000230     03 A02-QUANTITY                           PIC X(18).                 
000240*                                                                         
000230     03 A02-TRANS-TYPE                         PIC 9(03).                 
000240*                                                                         
000230     03 A02-PRICE                              PIC X(15).                 
000240*                                                                         
000230     03 A02-AMOUNT                             PIC X(15).                 
000240*                                                                         
000230     03 A02-AMOUNT-SEK                         PIC X(15).                 
000240*                                                                         
000230     03 A02-WEIGHT-KG                          PIC X(15).                 
000240*                                                                         
000230     03 A02-ORIGIN                             PIC X(02).                 
000240*                                                                         
000230     03 A02-PRICE-SEK                          PIC X(15).                 
000240*                                                                         
000230     03 A02-TYPE-ART                           PIC X(02).                 
000504*                                                                         
000542*                                                                         
000550*** END OF VILMAII-COPY LENGTH=104                                        
