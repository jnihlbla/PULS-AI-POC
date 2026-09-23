000010*** EDIT ALLOWED                                                          
000011                                                                          
000012*    EDI RFF REFERENCE                                                    
000013*    ANVÄNDS FÖR IFCSUM MEDDELANDE, GENERELL COPYTEXT                     
000014*                                                                         
000015*    FUNCTION,                                                            
000016*    -A GROUP OF SEG. TO IDENTIFY REF TO A GOODS ITEM                     
000018*                                                                         
000040*                                                                         
000100 01  WEDIRFF.                                                             
000230     03 RFF-IDPTYP                             PIC X(03).                 
000240*                                              RFF                        
000501     03 RFF-LENGTH                             PIC 9(03).                 
000502*                                              LENGTH = 079               
000503*                                                                         
000504*                                                                         
000515     03 RFF-C506-REFERENCE.                                               
000516*                                                                         
000517        05 RFF-1153-REFERENCE-QUAL             PIC X(03).                 
000518*                                                                         
000519        05 RFF-1154-REFERENCE-NO               PIC X(35).                 
000518*                                                                         
000519        05 RFF-1156-LINE-NUMBER                PIC X(06).                 
000518*                                                                         
000519        05 RFF-4000-REF-VERS-NO                PIC X(35).                 
000528*                                                                         
000530*** END OF VILMAII-COPY LENGTH=85                                         
