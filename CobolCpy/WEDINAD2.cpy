000010*** EDIT ALLOWED                                                          
000100 01  WEDINAD2.                                                            
000200*                                 EDI NAD NAME AND ADRESS                 
000220*                                                                         
000230     03 NAD2-IDPTYP          PIC X(3).                                    
000240*                                 NAD                                     
000501     03 NAD2-LENGTH          PIC 9(3).                                    
000502*                                 LENGTH = 225                            
000510     03 NAD2-QUAL            PIC X(3).                                    
000511*                                                                         
000512     03 NAD2-PARTY-ID        PIC X(35).                                   
000513*                                                                         
000514     03 NAD2-RESPONSIBLE     PIC X(3).                                    
000516*                                                                         
000527     03 NAD2-PARTY-NAME      PIC X(35).                                   
000528*                                                                         
000529     03 NAD2-STREET-1        PIC X(35).                                   
000530*                                                                         
000531     03 NAD2-STREET-2        PIC X(35).                                   
000532*                                                                         
000533     03 NAD2-STREET-3        PIC X(35).                                   
000534*                                                                         
000535     03 NAD2-CITY            PIC X(30).                                   
000536*                                                                         
000537     03 NAD2-COUNTRY-SUB-ID  PIC X(2).                                    
000538*                                                                         
000539     03 NAD2-POST-CODE       PIC X(9).                                    
000540*                                                                         
000541     03 NAD2-COUNTRY-CODE    PIC X(3).                                    
000542*                                                                         
000550*** END OF VILMAII-COPY LENGTH=231                                        
