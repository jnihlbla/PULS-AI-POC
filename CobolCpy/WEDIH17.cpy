000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    HUVUDINFORMATION                                                     
000030*    ANVÄNDS FÖR TRANSM. TILL TULLEN, GENERELL COPYTEXT                   
000050*                                                                         
000060*    FUNKTION,                                                            
000070*    -INNEHÅLLER HUVUDINFORMATION                                         
000070*     POSTTYP - H17                                                       
000080*                                                                         
000100 01  WEDIH17.                                                             
000230     03 H17-SLINE                              PIC X(01).                 
000240*                                                                         
000230     03 H17-IDPTYP                             PIC X(03).                 
000240*                                              H17                        
000230     03 H17-CURRENCY                           PIC X(03).                 
000240*                                              H17                        
000230     03 H17-DESTINATION-ISO-CO                 PIC X(02).                 
000240*                                                                         
000501     03 H17-BUYER-ISO-COUNTRY                  PIC X(02).                 
000240*                                                                         
000501     03 H17-USER-ID                            PIC X(08).                 
000240*                                                                         
000501     03 H17-COMPANY-CODE                       PIC X(17).                 
000240*                                                                         
000501     03 H17-ORIGIN-COUNTRY                     PIC X(35).                 
000240*                                                                         
000501     03 H17-INVOICE-TOTAL                      PIC X(15).                 
000504*                                                                         
000542*                                                                         
000550*** END OF VILMAII-COPY LENGTH=86                                         
