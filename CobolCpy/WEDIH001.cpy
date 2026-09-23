000010*** EDIT ALLOWED                                                          
000100 01  WEDIH001.                                                            
000200*                                 EDI FILE HEADER RECORD                  
000220*                                                                         
000230     03 H001-IDPTYP          PIC X(3).                                    
000240*                                 001                                     
000501     03 H001-LENGTH          PIC 9(3).                                    
000502*                                 LENGTH = 73                             
000510     03 H001-SENDER-NODE     PIC X(4).                                    
000511*                                                                         
000512     03 H001-RECEIVER-NODE   PIC X(4).                                    
000513*                                                                         
000516     03 H001-FILE-NAME       PIC X(8).                                    
000517*                                                                         
000526     03 H001-DATE            PIC 9(6).                                    
000527*                                                                         
000528     03 H001-TIME            PIC 9(6).                                    
000531*                                                                         
000532     03 FILLER               PIC X(45).                                   
000533*                                                                         
000540*** END OF VILMAII-COPY LENGTH=79                                         
