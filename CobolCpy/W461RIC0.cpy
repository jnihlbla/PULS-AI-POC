000100 01  RIC-W461RIC0.                                                        
000200*                                 SEPARATOR RECORD WITHIN                 
000300*                                 ORDERCONFIRMATION                       
000400*                                 RECORD TYPE RIC                         
000500     03 RIC-IDPTYP           PIC X(3).                                    
000600*                                 RECORD TYPE                             
000700     03 RIC-IDDISTR          PIC 9(4).                                    
000800*                                 DISTRICT NUMBER                         
000900     03 RIC-IDKUNDNR         PIC 9(6).                                    
001000*                                 CUSTOMER NO                             
001100     03 RIC-IDORDNR          PIC 9(7).                                    
001200*                                 ORDER NUMBER        IDORDNR-002         
001300     03 RIC-KDLIDEL          PIC 9.                                       
001400*                                 PART OF LIST                            
001500     03 FILLER               PIC X(59).                                   
001600*** END COPY W461RIC0C0  LENGTH=80                                        
