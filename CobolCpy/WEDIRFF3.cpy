000010*** EDIT ALLOWED                                                          
000011                                                                          
000012*    EDI RFF REFERENCE                                                    
000013*    ANVÄNDS FÖR IFCSUM MEDDELANDE                                        
000014*                                                                         
000015*    FUNCTION,                                                            
000016*    -TO INCLUDE MORE RFF-IDENTIFIERS E. G. CONSIGNEES REFERENCE          
000018*                                                                         
000040*                                                                         
000100 01  WEDIRFF3.                                                            
000230     03 RFF3-IDPTYP                            PIC X(03).                 
000240*                                              RFF                        
000501     03 RFF3-LENGTH                            PIC 9(03).                 
000502*                                              LENGTH = 038               
000503*                                                                         
000504*                                                                         
000515     03 RFF3-C506-REFERENCE.                                              
000516*                                                                         
000517        05 RFF3-1153-REFERENCE-QUAL            PIC X(03).                 
000518*                                                                         
000519        05 RFF3-1154-REFERENCE-NUMBER          PIC X(35).                 
000528*                                                                         
000530*** END OF VILMAII-COPY LENGTH=44                                         
