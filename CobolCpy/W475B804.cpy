000010*** EDIT ALLOWED                                                          
000100 01  W475B804.                                                            
000200*                                                                         
000300*   LAYOUT OF F4-TRANSACTION FOR FABRY                                    
000400*                                                                         
000500     03  IDPTYP          PIC X(2)   VALUE 'F4'.                           
000600*                TRANSACTION TYPE                                         
000700     03  FILLER          PIC X(1)   VALUE SPACE.                          
000800*                                                                         
000900     03  TIXMIT          PIC 9(6).                                        
001000*                TRANSMISSION-DATE YYMMDD                                 
001100     03  FILLER          PIC X(1)  VALUE SPACES.                          
001200*                                                                         
001300     03  TEMPS           PIC 9(6).                                        
001400*                CREATION TIME HHMMSS                                     
001500     03  FILLER          PIC X(59)  VALUE SPACES.                         
001600*                                                                         
001700     03  NREC            PIC 9(5).                                        
001800*                NUMBER OF F1 F2 F3  TRANSACTIONS                         
001900*** END COPY W475B804    LENGTH=80                                        
