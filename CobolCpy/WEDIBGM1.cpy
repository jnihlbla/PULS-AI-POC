000010*** EDIT ALLOWED                                                          
000100 01  WEDIBGM1.                                                            
000200*                                 EDI BGM BEGINNING OF MESSAGE            
000220*                                                                         
000230     03 BGM1-IDPTYP          PIC X(3).                                    
000240*                                 BGM                                     
000501     03 BGM1-LENGTH          PIC 9(3).                                    
000502*                                 LENGTH = 044                            
000510     03 BGM1-DOC-NAME-CODE   PIC X(3).                                    
000511*                                                                         
000512     03 BGM1-DOCNO           PIC X(35).                                   
000513*                                                                         
000514     03 BGM1-MESSAGE-FUNCT   PIC X(3).                                    
000515*                                                                         
000516     03 BGM1-RESPONSE-TYPE   PIC X(3).                                    
000517*                                                                         
000530*** END OF VILMAII-COPY LENGTH=50                                         
