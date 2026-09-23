000100 01  MOD-W4O62201.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 SCREEN NUMBER                           
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS ERROR MESSAGE                       
000600     03 MOD-IDTRPTNR-IN      PIC X(3).                                    
000700*                                 TRANSPORT IDENTITY                      
000800     03 MOD-IDTRPTNR-UT      PIC X(3).                                    
000900*                                 TRANSPORT IDENTITY                      
001000     03 MOD-IDLBBET-IN       PIC X(12).                                   
001100*                                 TRAILER NUMBER                          
001200     03 MOD-IDLBBET-UT       PIC X(12).                                   
001300*                                 TRAILER NUMBER                          
001400     03 MOD-TISKEPPN-IN      PIC X(6).                                    
001500*                                 SHIPPING DATE    (YYMMDD)               
001600     03 MOD-TISKEPPN-UT      PIC X(6).                                    
001700*                                 SHIPPING DATE    (YYMMDD)               
001800     03 MOD-IDDC-IN          PIC X(2).                                    
001900*                                 WAREHOUSE IDENTIFIER                    
002000     03 MOD-IDDC-UT          PIC X(2).                                    
002100*                                 WAREHOUSE IDENTIFIER                    
002200     03 MOD-OUTPUT           OCCURS 12 TIMES.                             
002300        05 MOD-KDCMD-ATTR    PIC X(2).                                    
002400        05 MOD-KDCMD         PIC X.                                       
002500*                                 LINE UPDATE COMMAND                     
002600        05 MOD-IDLBBET       PIC X(12).                                   
002700*                                 TRAILER NUMBER                          
002800        05 MOD-TISKEPPN      PIC 9(6).                                    
002900*                                 SHIPPING DATE    (YYMMDD)               
003000        05 MOD-TISKPTID      PIC 9(6).                                    
003100        05 MOD-IDSHIPM       PIC Z(6)9.                                   
003200*                                 SHIPMENT NO                             
003300        05 MOD-KVANTEX       PIC 9.                                       
003400     03 MOD-IDLTERM-ATTR     PIC X(2).                                    
003500     03 MOD-IDLTERM          PIC X(8).                                    
003600*                                 IDENTITY OF LOGICAL TERMINAL            
003700     03 MOD-IDDC-REC-ATTR    PIC X(2).                                    
003800     03 MOD-IDDC-REC         PIC X(2).                                    
003900*                                 RECEIVING WAREHOUSE                     
004000     03 MOD-TEMFSINF         PIC X(55).                                   
004100*                                 INFORMATION MESSAGE                     
004200*** END OF VILMAII-COPY LENGTH= 579 BYTES                                 
