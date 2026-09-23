000100 01  MOD-W30307O1.                                                        
000200*                                 MOD-COPYTEXT FOR W30307                 
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN-ATTR  PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900     03 MOD-IDDISTR-IN       PIC Z(3)9.                                   
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDDISTR-UT       PIC Z(3)9.                                   
001200*                                 DISTRIKTNUMMER                          
001300     03 MOD-IDMAIL-ATTR      PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500     03 MOD-IDMAIL           PIC X(60).                                   
001600*                                 MAIL ADRESS                             
001700     03 MOD-ADDISPABS-ASYNC-ATTR                                          
001800                             PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 MOD-ADDISPABS-ASYNC  PIC X(50).                                   
002100*                                 ABSTRACT ADRESS(ASYNKRONT)              
002200     03 MOD-ADDISPABS-SYNC-ATTR                                           
002300                             PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-ADDISPABS-SYNC   PIC X(50).                                   
002600*                                 ABSTRACT ADRESS(SYNKRONT)               
002700     03 MOD-CD-ATTR          PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-CD               PIC X.                                       
003000     03 MOD-TEMFSINF         PIC X(55).                                   
003100*                                 INFORMATIONSMEDDELANDE                  
003200*** END OF VILMAII-COPY LENGTH= 278 BYTES                                 
