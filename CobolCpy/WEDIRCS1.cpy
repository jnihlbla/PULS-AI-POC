000010*** EDIT ALLOWED                                                          
000100 01  WEDIRCS1.                                                            
000200*                                 EDI RCS REQUIREM. AND COND.             
000220*                                                                         
000230     03 RCS1-IDPTYP          PIC X(3).                                    
000240*                                 RCS                                     
000501     03 RCS1-LENGTH          PIC 9(3).                                    
000502*                                 LENGTH = 058                            
000510     03 RCS1-QUAL            PIC X(3).                                    
000511*                                                                         
000512     03 RCS1-REQ-COND-ID     PIC X(17).                                   
000513*                                                                         
000514     03 RCS1-RESPONSIBLE     PIC X(3).                                    
000515*                                                                         
000516     03 RCS1-REQ-COND        PIC X(35).                                   
000517*                                                                         
000540*** END OF VILMAII-COPY LENGTH=064                                        
