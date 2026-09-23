000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    SAMSÄNDNINGSFÄLT                                                     
000030*    ANVÄNDS FÖR TRANSM. TILL TULLEN, GENERELL COPYTEXT                   
000050*                                                                         
000060*    FUNKTION,                                                            
000070*    -INNEHÅLLER SAMPACKNINGSINFO                                         
000070*     POSTTYP - G01                                                       
000080*                                                                         
000100 01  W476G01.                                                             
000230     03 G01-SLINE                              PIC X(01).                 
000240*                                                                         
000230     03 G01-IDPTYP                             PIC X(03).                 
000240*                                              G01                        
000230     03 G01-NUMBER                             PIC X(35).                 
000240*                                                                         
000501     03 G01-USER-ID                            PIC X(08).                 
000504*                                                                         
000515     03 G01-COMPANY-CODE                       PIC X(17).                 
000516*                                                                         
000501     03 G01-SHORT-NAME                         PIC X(17).                 
000504*                                                                         
000230     03 G01-CONSIGNEE-1                        PIC X(35).                 
000240*                                                                         
000501     03 G01-LAST-INVOICE                       PIC X(01).                 
000504*                                                                         
000542*                                                                         
000550*** END OF VILMAII-COPY LENGTH=117                                        
