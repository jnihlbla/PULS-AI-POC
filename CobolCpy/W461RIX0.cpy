000100 01  RIX-W461RIX0.                                                        
000200*                                 TRANSACTION FOR ORDER FROM VR           
000300*                                 TO VOLVO PARTS SYSTEM                   
000400*                                 RECORD TYPE RIX (HEADER)                
000500     03 RIX-IDPTYP           PIC X(3).                                    
000600*                                 RECORD TYPE                             
000700     03 RIX-IDDISTR          PIC 9(4).                                    
000800*                                 DISTRICT NUMBER                         
000900     03 RIX-IDKUNDNR         PIC 9(6).                                    
001000*                                 CUSTOMER NO                             
001100     03 RIX-IDORDNR          PIC 9(7).                                    
001200*                                 ORDER NUMBER        IDORDNR-002         
001300     03 RIX-KDORDKL          PIC 9.                                       
001400*                                 ORDER CLASS                             
001500     03 RIX-BEVARREF         PIC X(10).                                   
001600*                                 OUR REFERENCE                           
001700     03 RIX-BEVOLREF         PIC X(10).                                   
001800*                                 VOLVO REFERENCE                         
001900     03 RIX-TIORDREG         PIC 9(6).                                    
002000*                                 ORDER REGISTRATION DATE  YYMMDD         
002100     03 FILLER               PIC X(33).                                   
002200*** END COPY W461RIX0C0  LENGTH=80                                        
