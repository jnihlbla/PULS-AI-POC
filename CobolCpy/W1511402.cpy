000100 01  W1511402.                                                            
000200*                                 ANVÄNDES VID LÄSNING AV                 
000300*                                 RESERVDELSKATALOGENS                    
000400*                                 FÖRKORTNINGAR                           
000500*                                                                         
000600     03 FORDON               OCCURS 6 TIMES                               
000700                             INDEXED IX-FORD.                             
000800*                                                                         
000900        05 COPYMARK          PIC X(6).                                    
001000*                                 COPYMARK                                
001100        05 KDFORDON          PIC X(2).                                    
001200*                                 FORDONSSLAG                             
001300     03 FILLER               PIC X(32).                                   
001400*** END COPY W1511402C0  LENGTH=80                                        
