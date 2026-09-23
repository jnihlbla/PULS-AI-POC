000100 01  W4770102.                                                            
000200*                                 EDI PC INFORMATION                      
000300*                                 RECORD TYPE 1000                        
000400     03 IDPTYP               PIC X(4).                                    
000500*                                 RECORD TYPE          IDPTYP-004         
000600     03 IDSKEPPN             PIC 9(7).                                    
000700*                                 SHIPMENT NO                             
000800     03 IDFAKT               PIC 9(7).                                    
000900*                                 INVOICE NO.                             
001000     03 IDDISTR              PIC 9(4).                                    
001100*                                 DISTRICT NUMBER                         
001200     03 IDKUNDNR             PIC 9(6).                                    
001300*                                 CUSTOMER NO                             
001400     03 BEGODSM.                                                          
001500*                                 GOODS RECEIVER NAME                     
001600        05 BEGODSM-RAD1      PIC X(27).                                   
001700*                                 GOODS RECEIVER NAME LINE 1              
001800        05 BEGODSM-RAD2      PIC X(27).                                   
001900*                                 GOODS RECEIVER NAME LINE 2              
002000     03 ADGODSM.                                                          
002100*                                 GOODS RECEIVER ADDRESS                  
002200        05 ADGODSM-RAD1      PIC X(27).                                   
002300*                                 GOODS RECEIVER ADDRESS LINE 1           
002400        05 ADGODSM-RAD2      PIC X(27).                                   
002500*                                 GOODS RECEIVER ADDRESS LINE 2           
002600     03 KDFRAKT              PIC 9(2).                                    
002700*                                 FREIGHT CODE                            
002800*** END COPY W4770102    LENGTH=138                                       
