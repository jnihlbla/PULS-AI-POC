000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI CNI CONSIGNMENT INFORMATION                                      
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE                                        
000040*                                                                         
000050*    FUNCTION,                                                            
000060*    -TO IDENTIFY EACH PACKAGE FOR THE MANIFEST.                          
000070*                                                                         
000100 01  WEDICNI3.                                                            
000230     03 CNI3-IDPTYP                            PIC X(03).                 
000240*                                              CNI                        
000501     03 CNI3-LENGTH                            PIC 9(03).                 
000502*                                              LENGTH = 039               
000503*                                                                         
000515     03 CNI3-1490-CONSOLID-ITEM-NUMBER         PIC 9(04).                 
000516*                                                                         
000517     03 CNI3-C503-DOCUMENT-MESSAGE.                                       
000518*                                                                         
000519        05 CNI3-1004-DOCUMENT-NUMBER           PIC X(35).                 
000560*                                                                         
000570*** END OF VILMAII-COPY LENGTH=45                                         
