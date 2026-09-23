000010*** EDIT ALLOWED                                                          
000100 01  WEDIUNB.                                                             
000200*                                 EDI UNB INTERCHANGE HEADER              
000220*                                                                         
000230     03 UNB-IDPTYP           PIC X(3).                                    
000240*                                 UNH                                     
000501     03 UNB-LENGTH           PIC 9(3).                                    
000502*                                 LENGTH = 125                            
000510     03 UNB-SYNTAX-ID        PIC X(4).                                    
000511*                                                                         
000512     03 UNB-SYNTAX-VERS-NO   PIC 9(1).                                    
000513*                                                                         
000514     03 UNB-SENDER-ID        PIC X(14).                                   
000515*                                                                         
000520     03 UNB-RECIPIENT-ID     PIC X(14).                                   
000525*                                                                         
000526     03 UNB-DATE             PIC 9(6).                                    
000527*                                                                         
000528     03 UNB-TIME             PIC 9(4).                                    
000529*                                                                         
000530     03 UNB-INTERCHANGE-REF  PIC X(14).                                   
000531*                                                                         
000532     03 FILLER               PIC X(68).                                   
000533*                                                                         
000540*** END OF VILMAII-COPY LENGTH=131                                        
