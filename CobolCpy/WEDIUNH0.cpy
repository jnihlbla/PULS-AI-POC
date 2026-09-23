000010*** EDIT ALLOWED                                                          
000100 01  WEDIUNH.                                                             
000200*                                 EDI UNH MESSAGE HEADER                  
000220*                                                                         
000230     03 UNH-IDPTYP           PIC X(3).                                    
000240*                                 UNH                                     
000501     03 UNH-LENGTH           PIC 9(3).                                    
000502*                                 LENGTH = 072                            
000510     03 UNH-REFNO            PIC X(14).                                   
000511*                                                                         
000512     03 UNH-TYPE             PIC X(6).                                    
000513*                                                                         
000514     03 UNH-VERSION          PIC X(3).                                    
000515*                                                                         
000516     03 UNH-RELEASE          PIC X(3).                                    
000517*                                                                         
000518     03 UNH-CONTR-AGENCY     PIC X(2).                                    
000519*                                                                         
000520     03 UNH-ASS-ASSIGN-CODE  PIC X(6).                                    
000521*                                                                         
000522     03 UNH-COM-ACCESS-REF   PIC X(35).                                   
000523*                                                                         
000524     03 UNH-SEQ-OF-TRAN      PIC 9(2).                                    
000525*                                                                         
000526     03 UNH-FIRST-LAST       PIC X(1).                                    
000527*                                                                         
000530*** END OF VILMAII-COPY LENGTH=78                                         
