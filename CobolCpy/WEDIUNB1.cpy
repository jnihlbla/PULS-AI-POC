000010*** EDIT ALLOWED                                                          
000100 01  WEDIUNB1.                                                            
000200*                                 EDI UNB INTERCHANGE HEADER              
000220*                                                                         
000230     03 UNB1-IDPTYP           PIC X(3).                                   
000240*                                 UNH                                     
000501     03 UNB1-LENGTH           PIC 9(3).                                   
000502*                                 LENGTH = 123                            
000510     03 UNB1-SYNTAX-ID        PIC X(4).                                   
000511*                                                                         
000512     03 UNB1-SYNTAX-VERS-NO   PIC 9(1).                                   
000513*                                                                         
000514     03 UNB1-SENDER-ID        PIC X(24).                                  
000515*                                                                         
000516     03 UNB1-CODE-QUALIFIER-1 PIC X(2).                                   
000517*                                                                         
000520     03 UNB1-ROUTING-ADRESS-1 PIC X(14).                                  
000521*                                                                         
000522     03 UNB1-RECIPIENT-ID     PIC X(24).                                  
000525*                                                                         
000526     03 UNB1-CODE-QUALIFIER-2 PIC X(2).                                   
000527*                                                                         
000528     03 UNB1-ROUTING-ADRESS-2 PIC X(14).                                  
000529*                                                                         
000530     03 UNB1-DATE             PIC 9(6).                                   
000531*                                                                         
000532     03 UNB1-TIME             PIC 9(4).                                   
000533*                                                                         
000534     03 UNB1-INTERCHANGE-REF  PIC X(14).                                  
000535*                                                                         
000536     03 FILLER                PIC X(14).                                  
000537*                                                                         
000540*** END OF VILMAII-COPY LENGTH=129                                        
