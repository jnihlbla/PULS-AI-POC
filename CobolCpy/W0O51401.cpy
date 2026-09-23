000100 01  MOD-W0O51401.                                                        
000200*                                 MOD TILL SECURITY UPDATE                
000300*                                 I USER-INIT-REG                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900*                                 MFS ERROR MESSAGE                       
001000     03 MOD-IDUSER-IN        PIC X(8).                                    
001100*                                 ANVÄNDARENS SÄKERHETS ID                
001200*                                 USER SECURITY-IDENTITY                  
001300     03 MOD-IDUSER-UT        PIC X(8).                                    
001400*                                 ANVÄNDARENS SÄKERHETS ID                
001500*                                 USER SECURITY-IDENTITY                  
001600     03 MOD-KDARBTYP-SEC-ATTR                                             
001700                             PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 MOD-KDARBTYP-SEC     PIC X(8).                                    
002000*                                 TYP AV ARBETE FÖR SECURITY              
002100*                                 TYPE OF WORK FOR SECURITY               
002200     03 MOD-KDARBTYP-SEC-IDLEV-ATTR                                       
002300                             PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-KDARBTYP-SEC-IDLEV                                            
002600                             PIC X(8).                                    
002700*                                 ARBETSTYP FÖR SÄKERHET PÅ LEV           
002800*                                 WORK CATEG. FOR SUPPL. SECURITY         
002900     03 MOD-KDARBTYP-SEC-4352-ATTR                                        
003000                             PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 MOD-KDARBTYP-SEC-4352                                             
003300                             PIC X(8).                                    
003400*                                 TYP AV ARBETE FÖR SECURITY-4352         
003500*                                 TYPE OF WORK FOR SECURITY-4352          
003600     03 MOD-TEMFSINF         PIC X(55).                                   
003700*                                 INFORMATIONSMEDDELANDE                  
003800*                                 INFORMATION MESSAGE                     
003900*** END OF VILMAII-COPY LENGTH= 145 BYTES                                 
