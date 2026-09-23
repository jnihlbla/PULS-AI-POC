000010*** EDIT ALLOWED                                                          
000100 01  MID-W4O68101.                                                        
000200*                                 MID TILL                                
000300*                                 IMS TILL MEMO SÄNDNING                  
000400     03 MID-IDTRANS          PIC X(8).                                    
000500*                                 TRANSACTION IDENTITY                    
000600     03 MID-DG-REC           PIC X(8).                                    
000700*                                 DISTRIBUTION GROUP RECEIVER             
000800     03 MID-MEMOID-REC       PIC X(8).                                    
000900*                                 MEMOID RECEIVER                         
001000     03 MID-TITLE            PIC X(14).                                   
001100*                                 MEMO TITLE                              
001200     03 MID-DG-SEND          PIC X(8).                                    
001300*                                 DISTRIBUTION GROUP SENDER               
001400     03 MID-MEMOID-SEND      PIC X(8).                                    
001500*                                 MEMOID SENDER                           
001600     03 MID-MEMOPW           PIC X(8).                                    
001700*                                 PASSWORD                                
001800     03 MID-LINE             PIC X(66) OCCURS 16.                         
001900*                                 MEMO LINE                               
002000     03 MID-TEMFSINF         PIC X(79).                                   
002100*                                 INFORMATIONSMEDDELANDE                  
002200*** END COPY W4I68101C0  LENGTH=1197  OLD LENGTH=                         
