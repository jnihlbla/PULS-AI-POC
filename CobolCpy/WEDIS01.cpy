000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    STYRRADEN                                                            
000030*    ANVÄNDS FÖR TRANSM. TILL TULLEN, GENERELL COPYTEXT                   
000050*                                                                         
000060*    FUNKTION,                                                            
000070*    -VISAR VILKA POSTER I INDATA SOM INITIERAR OCH LAGRAR                
000071*     POSTTYP - S01                                                       
000080*                                                                         
000100 01  WEDIS01.                                                             
000230     03 S01-SLINE                              PIC X(01).                 
000231*                                                                         
000232     03 S01-IDPTYP                             PIC X(03).                 
000240*                                              S01                        
000250     03 S01-G-TYPE                             PIC X(01).                 
000260*                                                                         
000501     03 S01-G-POSTER                           PIC X(02).                 
000504*                                                                         
000515     03 S01-H-TYPE                             PIC X(01).                 
000516*                                                                         
000517     03 S01-H-POSTER                           PIC X(02).                 
000518*                                                                         
000519     03 S01-A-TYPE                             PIC X(01).                 
000520*                                                                         
000530     03 S01-A-POSTER                           PIC X(02).                 
000540*                                                                         
000541     03 S01-P-TYPE                             PIC X(01).                 
000542*                                                                         
000543     03 S01-P-POSTER                           PIC X(02).                 
000544*                                                                         
000545     03 S01-D-TYPE                             PIC X(01).                 
000546*                                                                         
000547     03 S01-D-POSTER                           PIC X(02).                 
000548*                                                                         
000549     03 S01-N-TYPE                             PIC X(01).                 
000550*                                                                         
000551     03 S01-N-POSTER                           PIC X(02).                 
000552*                                                                         
000553     03 S01-E-TYPE                             PIC X(01).                 
000554*                                                                         
000555     03 S01-E-POSTER                           PIC X(02).                 
000560*                                                                         
000561*                                                                         
000570*** END OF VILMAII-COPY LENGTH=25                                         
