000100 01  MID-W6I35101.                                                        
000200*                                                                         
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-IDDC-UT          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-INPUT.                                                        
000800*                                                                         
000900        05 MID-CMD           PIC X.                                       
001000        05 MID-NY-IDDC       PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200        05 MID-LEVEL         PIC X.                                       
001300        05 MID-UNDER-IDDC    PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500        05 MID-IDUSER        OCCURS 4 TIMES                               
001600                             PIC X(7).                                    
001700*                                 ANVÄNDARENS SÄKERHETS ID                
001800        05 MID-KDDC          PIC X.                                       
001900*** END OF VILMAII-COPY LENGTH= 39 BYTES                                  
