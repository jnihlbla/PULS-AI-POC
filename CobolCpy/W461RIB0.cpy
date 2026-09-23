000100 01  RIB-W461RIB0.                                                        
000200*                                 ORDERCONF. HEAD  TO                     
000300*                                 IMPORTER RECORD TYPE RIB                
000400     03 RIB-IDPTYP           PIC X(3).                                    
000500*                                 RECORD TYPE                             
000600     03 RIB-IDDISTR          PIC 9(4).                                    
000700*                                 DISTRICT NUMBER                         
000800     03 RIB-IDKUNDNR         PIC 9(6).                                    
000900*                                 CUSTOMER NO                             
001000     03 RIB-IDORDNR          PIC 9(7).                                    
001100*                                 ORDER NUMBER        IDORDNR-002         
001200     03 RIB-KDFRAKT          PIC 9(2).                                    
001300*                                 FREIGHT CODE                            
001400     03 RIB-BEVOLREF         PIC X(10).                                   
001500*                                 VOLVO REFERENCE                         
001600     03 RIB-BEVARREF         PIC X(10).                                   
001700*                                 OUR REFERENCE                           
001800     03 RIB-KDORDKL          PIC 9.                                       
001900*                                 ORDER CLASS                             
002000     03 RIB-TIORDREG         PIC 9(6).                                    
002100*                                 ORDER REGISTRATION DATE  YYMMDD         
002200     03 FILLER               PIC X(31).                                   
002300*** END COPY W461RIB0C0  LENGTH=80                                        
