000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    HUVUDINFORMATION                                                     
000030*    ANVÄNDS FÖR TRANSM. TILL TULLEN, GENERELL COPYTEXT                   
000050*                                                                         
000060*    FUNKTION,                                                            
000070*    -INNEHÅLLER HUVUDINFORMATION                                         
000070*     POSTTYP - H04                                                       
000080*                                                                         
000100 01  WEDIH04.                                                             
000230     03 H04-SLINE                              PIC X(01).                 
000240*                                                                         
000230     03 H04-IDPTYP                             PIC X(03).                 
000240*                                              H04                        
000230     03 H04-CONSIGNEE-4                        PIC X(35).                 
000240*                                                                         
000501     03 H04-CONSIGNEE-5                        PIC X(35).                 
000240*                                                                         
000501     03 H04-CONSIGNEE-6                        PIC X(35).                 
000504*                                                                         
000542*                                                                         
000550*** END OF VILMAII-COPY LENGTH=109                                        
