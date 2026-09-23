000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI UNB INTERCHANGE HEADER                                           
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE                                        
000050*                                                                         
000060*    FUNCTION,                                                            
000070*    -INTERCHANGE HEADER                                                  
000080*                                                                         
000100 01  WEDIUNB3.                                                            
000230     03 UNB3-IDPTYP                            PIC X(03).                 
000240*                                              UNB                        
000501     03 UNB3-LENGTH                            PIC 9(03).                 
000504*                                              LENGTH = 125               
000505*                                                                         
000522     03 UNB3-E0001-SYNTAX-IDENTIFIER           PIC X(04).                 
000523*                                                                         
000524     03 UNB3-E0002-SYNTAX-VERSION-NO           PIC X(01).                 
000525*                                                                         
000526     03 UNB3-E0004-SENDER-ID                   PIC X(14).                 
000527*                                                                         
000528     03 UNB3-E0010-RECIPIENT-ID                PIC X(14).                 
000529*                                                                         
000530     03 UNB3-E0017-DATE                        PIC X(06).                 
000531*                                                                         
000532     03 UNB3-E0019-TIME                        PIC X(06).                 
000533*                                                                         
000534     03 UNB3-FILLER                            PIC X(77).                 
000535*                                                                         
000550*** END OF VILMAII-COPY LENGTH=131                                        
