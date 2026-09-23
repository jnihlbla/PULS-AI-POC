000100 01  MOD-W4O62301.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 SCREEN NUMBER                           
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS ERROR MESSAGE                       
000600     03 MOD-IDDISTR-IN       PIC X(4).                                    
000700*                                 DISTRICT NUMBER                         
000800     03 MOD-IDDISTR-UT       PIC X(4).                                    
000900*                                 DISTRICT NUMBER                         
001000     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001100*                                 CUSTOMER NO                             
001200     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001300*                                 CUSTOMER NO                             
001400     03 MOD-IDDC-IN          PIC X(2).                                    
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 MOD-IDDC-UT          PIC X(2).                                    
001700*                                 WAREHOUSE IDENTIFIER                    
001800     03 MOD-SHIPMENT         OCCURS 14 TIMES.                             
001900        05 MOD-KDCMD-ATTR    PIC X(2).                                    
002000        05 MOD-KDCMD         PIC X.                                       
002100*                                 LINE UPDATE COMMAND                     
002200        05 MOD-IDSHIPM       PIC 9(7).                                    
002300*                                 SHIPMENT NO                             
002400        05 MOD-IDTRPTNR      PIC Z(2)9.                                   
002500*                                 TRANSPORT IDENTITY                      
002600        05 MOD-IDLBBET       PIC X(12).                                   
002700*                                 TRAILER NUMBER                          
002800        05 MOD-TISKPTID      PIC 9(6).                                    
002900        05 MOD-TISKEPPN      PIC 9(6).                                    
003000*                                 SHIPPING DATE    (YYMMDD)               
003100     03 MOD-TEMFSINF         PIC X(55).                                   
003200*                                 INFORMATION MESSAGE                     
003300*** END OF VILMAII-COPY LENGTH= 641 BYTES                                 
