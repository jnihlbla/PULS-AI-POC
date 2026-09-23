000010 01  MOD-W0O54101.                                                        
000020*                                 MOD TILL PROGRAM W00541                 
000030*                                 FÖR ATT SÄNDA MEMO.                     
000040     03 MOD-IDTRANS          PIC X(4).                                    
000050*                                 BILDNUMMER                              
000060*                                 SCREEN NUMBER                           
000070     03 MOD-TEMFSFEL         PIC X(40).                                   
000080*                                 MFS FELMEDDELANDE                       
000090*                                 MFS ERROR MESSAGE                       
000100     03 MOD-IDMEMODG         PIC X(8).                                    
000110*                                 MEMO DISTRIBUTIONSGRUPP                 
000120*                                 MEMO DISTRIBUTION GROUP                 
000130     03 MOD-IDMEMO           PIC X(8).                                    
000140*                                 MEMO ANVÄNDARIDENTITET                  
000150*                                 MEMO USER IDENTITY                      
000160     03 MOD-IDMTITEL         PIC X(14).                                   
000170*                                 MEMO TITEL                              
000180*                                 MEMO TITLE                              
000190     03 MOD-IDMEMODG-SEND    PIC X(8).                                    
000200*                                 MEMO DISTRIBUTIONSGRUPP                 
000210*                                 MEMO DISTRIBUTION GROUP                 
000220     03 MOD-IDMEMO-SEND      PIC X(8).                                    
000230*                                 MEMO ANVÄNDARIDENTITET                  
000240*                                 MEMO USER IDENTITY                      
000250     03 MOD-IDPW-SEND        PIC X(8).                                    
000260*                                 PASSWORD   (LÖSENORD)                   
000270*                                 PASSWORD                                
000280     03 MOD-TEMEMO           OCCURS 15 TIMES                              
000290                             PIC X(66).                                   
000300*                                 TEXTRAD MEMO                            
000310*                                 MEMO TEXTLINE                           
000320     03 MOD-TEMFSINF         PIC X(61).                                   
000330*                                 INFORMATIONSMEDDELANDE                  
000340*                                 INFORMATION MESSAGE                     
000350*** END COPY W0O54101  LENGTH=1149                                        
