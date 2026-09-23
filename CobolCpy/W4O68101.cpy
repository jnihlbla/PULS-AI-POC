000010*** EDIT ALLOWED                                                          
000100 01  MOD-W4O68101.                                                        
000200*                                 MOD TILL                                
000300*                                 IMS TILL MEMO SÄNDNING                  
000400     03 MOD-DG-REC-ATTR      PIC X(2).                                    
000500     03 MOD-DG-REC           PIC X(8).                                    
000600*                                 DISTRIBUTION GROUP RECEIVER             
000700     03 MOD-MEMOID-REC-ATTR  PIC X(2).                                    
000800     03 MOD-MEMOID-REC       PIC X(8).                                    
000900*                                 MEMOID RECEIVER                         
001000     03 MOD-TITLE-ATTR       PIC X(2).                                    
001100     03 MOD-TITLE            PIC X(14).                                   
001200*                                 MEMO TITLE                              
001300     03 MOD-DG-SEND-ATTR     PIC X(2).                                    
001400     03 MOD-DG-SEND          PIC X(8).                                    
001500*                                 DISTRIBUTION GROUP SENDER               
001600     03 MOD-MEMOID-SEND-ATTR PIC X(2).                                    
001700     03 MOD-MEMOID-SEND      PIC X(8).                                    
001800*                                 MEMOID SENDER                           
001900     03 MOD-MEMOPW-ATTR      PIC X(2).                                    
002000     03 MOD-MEMOPW           PIC X(8).                                    
002100*                                 PASSWORD                                
002200     03 MOD-LINE             PIC X(66) OCCURS 16.                         
002300*                                 MEMO LINE                               
002400     03 MOD-TEMFSINF-ATTR    PIC X(2).                                    
002500     03 MOD-TEMFSINF         PIC X(79).                                   
002600*                                 INFORMATIONSMEDDELANDE                  
002700*** END COPY W4O68101C0  LENGTH=1203  OLD LENGTH=                         
