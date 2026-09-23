000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI LOC, LOCATION                                                    
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE, GENERELL COPYTEXT                     
000031*                                                                         
000032*    FUNCTION,                                                            
000033*    -TO IDENTIFY A LOCATION APPLYING TO THE ENTIRE CONSIGNMENT           
000040*                                                                         
000100 01  WEDILOC.                                                             
000230     03 LOC-IDPTYP                             PIC X(03).                 
000240*                                              LOC                        
000501     03 LOC-LENGTH                             PIC 9(03).                 
000502*                                              LENGTH = 256               
000503*                                                                         
000515     03 LOC-3227-PLACE-LOC-QUAL                PIC X(03).                 
000516*                                                                         
000518     03 LOC-C517-LOC-IDENTIFICATION.                                      
000519*                                                                         
000520        05 LOC-3225-LOC-ID                     PIC X(25).                 
000521*                                                                         
000520        05 LOC-3055-CODE-LIST-AGENCY           PIC X(03).                 
000521*                                                                         
000520        05 LOC-FILLER                          PIC X(225).                
000521*                                                                         
000570*                                                                         
000580*** END OF VILMAII-COPY LENGTH=259                                        
