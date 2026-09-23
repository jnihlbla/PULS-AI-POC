000010 01  MID-W0I54101.                                                        
000020*                                 MID TILL PROGRAM W00541                 
000030*                                 FÖR ATT SÄNDA MEMO.                     
000040     03 MID-IDMEMODG         PIC X(8).                                    
000050*                                 MEMO DISTRIBUTIONSGRUPP                 
000060*                                 MEMO DISTRIBUTION GROUP                 
000070     03 MID-IDMEMO           PIC X(8).                                    
000080*                                 MEMO ANVÄNDARIDENTITET                  
000090*                                 MEMO USER IDENTITY                      
000100     03 MID-IDMTITEL         PIC X(14).                                   
000110*                                 MEMO TITEL                              
000120*                                 MEMO TITLE                              
000130     03 MID-IDMEMODG-SEND    PIC X(8).                                    
000140*                                 MEMO DISTRIBUTIONSGRUPP                 
000150*                                 MEMO DISTRIBUTION GROUP                 
000160     03 MID-IDMEMO-SEND      PIC X(8).                                    
000170*                                 MEMO ANVÄNDARIDENTITET                  
000180*                                 MEMO USER IDENTITY                      
000190     03 MID-IDPW-SEND        PIC X(8).                                    
000200*                                 PASSWORD   (LÖSENORD)                   
000210*                                 PASSWORD                                
000220     03 MID-TEMEMO           OCCURS 15 TIMES                              
000230                             PIC X(66).                                   
000240*                                 TEXTRAD MEMO                            
000250*                                 MEMO TEXTLINE                           
000260*** END COPY W0I54101  LENGTH=1044                                        
