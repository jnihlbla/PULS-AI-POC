000010*** EDIT ALLOWED                                                          
000011                                                                          
000020*    EDI UNH DETAILS OF TRANSPORT                                         
000030*    ANVÄNDS FÖR IFCSUM MEDDELANDE                                        
000040*                                                                         
000050*    FUNCTION,                                                            
000060*    -TO HEAD,IDENTIFY AND SPECIFY THE MESSAGE.                           
000070*                                                                         
000100 01  WEDIUNH3.                                                            
000230     03 UNH3-IDPTYP                            PIC X(03).                 
000240*                                              UNH                        
000501     03 UNH3-LENGTH                            PIC 9(03).                 
000502*                                              LENGTH = 072               
000503*                                                                         
000515     03 UNH3-0062-MESSAGE-REFERENCE            PIC X(14).                 
000516*                                                                         
000517     03 UNH3-S009-MESSAGE-IDENTIFIER.                                     
000518*                                                                         
000519        05 UNH3-0065-MESSAGE-TYPE-ID           PIC X(06).                 
000520*                                                                         
000521        05 UNH3-0052-MESSAGE-VERSION           PIC X(03).                 
000522*                                                                         
000523        05 UNH3-0054-MESSAGE-RELEASE           PIC X(03).                 
000524*                                                                         
000525        05 UNH3-0051-CONTROLING-AGENCY         PIC X(02).                 
000526*                                                                         
000527        05 UNH3-0057-ASSOCIATION-CODE          PIC X(06).                 
000528*                                                                         
000529*                                                                         
000530     03 UNH3-0068-ACCESS-REFERENCE             PIC X(35).                 
000531*                                                                         
000532     03 UNH3-S010-TRANSFER-STATUS.                                        
000533*                                                                         
000535        05 UNH3-0070-SEQ-MESS-TRANSFER-NO      PIC 9(02).                 
000536*                                                                         
000537        05 UNH3-0073-SEQ-MESS-TRANSF-ID        PIC X(01).                 
000538*                                                                         
000550*** END OF VILMAII-COPY LENGTH=78                                         
