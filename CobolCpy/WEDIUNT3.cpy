000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI UNT MESSAGE TRAILOR                                              
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE                                        
000040*                                                                         
000050*    FUNCTION,                                                            
000060*    -TO END AND CHECK THE COMPLETENESS OF A MESSAGE.                     
000070*                                                                         
000200 01  WEDIUNT3.                                                            
000300     03 UNT3-IDPTYP                            PIC X(03).                 
000400*                                              UNT                        
000500     03 UNT3-LENGTH                            PIC 9(03).                 
000510*                                              LENGTH = 028               
000511*                                                                         
000515     03 UNT3-0074-NUMBER-OF-SEGM               PIC 9(06).                 
000516*                                                                         
000517     03 UNT3-0062-MESSAGE-REF-NO               PIC X(14).                 
000518*                                                                         
000538*                                                                         
000550*** END OF VILMAII-COPY LENGTH=34                                         
