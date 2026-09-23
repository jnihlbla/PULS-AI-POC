000100 01  MOD-W4O62501.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 SCREEN NUMBER                           
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS ERROR MESSAGE                       
000600     03 MOD-IDSHIPM-IN       PIC 9(7).                                    
000700*                                 SHIPMENT NO                             
000800     03 MOD-IDSHIPM-UT       PIC Z(6)9.                                   
000900*                                 SHIPMENT NO                             
001000     03 MOD-IDFAKT-IN        PIC 9(7).                                    
001100*                                 INVOICE NO.                             
001200     03 MOD-IDFAKT-UT        PIC Z(6)9.                                   
001300*                                 INVOICE NO.                             
001400     03 MOD-SHIPMENT         OCCURS 14 TIMES.                             
001500        05 MOD-IDSHIPM       PIC Z(6)9.                                   
001600*                                 SHIPMENT NO                             
001700        05 MOD-TISKEPPN      PIC 9(6).                                    
001800*                                 SHIPPING DATE    (YYMMDD)               
001900        05 MOD-IDFAKT        PIC Z(6)9.                                   
002000*                                 INVOICE NO.                             
002100        05 MOD-TIFAKT        PIC 9(6).                                    
002200*                                 INVOICING DATE   (YYMMDD)               
002300        05 MOD-IDDISTR       PIC Z(3)9.                                   
002400*                                 DISTRICT NUMBER                         
002500     03 MOD-TEMFSINF         PIC X(55).                                   
002600*                                 INFORMATION MESSAGE                     
002700*** END OF VILMAII-COPY LENGTH= 547 BYTES                                 
