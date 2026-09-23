000100 01  MOD-W4O57801.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 SCREEN NUMBER                           
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS ERROR MESSAGE                       
000600     03 MOD-IDDISTR-IN       PIC Z(3)9.                                   
000700*                                 DISTRICT NUMBER                         
000800     03 MOD-IDDISTR-UT       PIC Z(4).                                    
000900*                                 DISTRICT NUMBER                         
001000     03 MOD-IDKUNDNR-IN      PIC Z(5)9.                                   
001100*                                 CUSTOMER NO                             
001200     03 MOD-IDKUNDNR-UT      PIC Z(6).                                    
001300*                                 CUSTOMER NO                             
001400     03 MOD-IDARTNR-IN       PIC Z(8)9.                                   
001500*                                 PART NUMBER                             
001600     03 MOD-IDARTNR-UT       PIC Z(9).                                    
001700*                                 PART NUMBER                             
001800     03 MOD-KDFARLIG-IN      PIC X.                                       
001900*                                 DANGEROUS GOODS CODE                    
002000     03 MOD-KDFARLIG-UT      PIC X.                                       
002100*                                 DANGEROUS GOODS CODE                    
002200     03 MOD-TIRFSDAT-CDC-IN  PIC X(6).                                    
002300*                                 READY FOR SHIPMENT CDC YYMMDD           
002400     03 MOD-TIRFSDAT-CDC-UT  PIC X(6).                                    
002500*                                 READY FOR SHIPMENT CDC YYMMDD           
002600     03 MOD-OUTPUT           OCCURS 6 TIMES.                              
002700        05 MOD-KDCMD-ATTR    PIC X(2).                                    
002800        05 MOD-KDCMD         PIC X.                                       
002900*                                 LINE UPDATE COMMAND                     
003000        05 MOD-IDDISTR-ATTR  PIC X(2).                                    
003100        05 MOD-IDDISTR       PIC Z(3)9.                                   
003200*                                 DISTRICT NUMBER                         
003300        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
003400*                                 CUSTOMER NO                             
003500        05 MOD-IDARTNR       PIC Z(8)9.                                   
003600*                                 PART NUMBER                             
003700        05 MOD-KVART         PIC Z(3)9.                                   
003800*                                 NO OF PARTNOS PER TYPE                  
003900        05 MOD-IDORDNR5      PIC Z(4)9.                                   
004000*                                 ORDER NUMBER                            
004100        05 MOD-KDORDKL-ATTR  PIC X(2).                                    
004200        05 MOD-KDORDKL       PIC 9.                                       
004300*                                 ORDER CLASS                             
004400        05 MOD-KDFARLIG-ATTR PIC X(2).                                    
004500        05 MOD-KDFARLIG      PIC X.                                       
004600*                                 DANGEROUS GOODS CODE                    
004700        05 MOD-TIRFSDAT-CDC  PIC X(6).                                    
004800*                                 READY FOR SHIPMENT  YYMMDD              
004900        05 MOD-TIRFSDAT-LDC  PIC X(6).                                    
005000*                                 READY FOR SHIPMENT  YYMMDD              
005100        05 MOD-TIREPDAT      PIC 9(6).                                    
005200*                                 REPAIR DATE                             
005300        05 MOD-IDKUNDRF-WIP  PIC X(10).                                   
005400*                                 WORK ORDER NUMBER, LDC DEALER           
005500        05 MOD-IDDC-PRIM-ATTR                                             
005600                             PIC X(2).                                    
005700        05 MOD-IDDC-PRIM     PIC X(2).                                    
005800*                                 PRIMARY DELIVERING DC                   
005900        05 MOD-BELAGINS-ATTR PIC X(2).                                    
006000        05 MOD-BELAGINS      PIC X(60).                                   
006100*                                 PART OF WAREHOUSE INSTRUCTIONS          
006200        05 MOD-BETEXT-ATTR   PIC X(2).                                    
006300        05 MOD-BETEXT        PIC X(10).                                   
006400     03 MOD-RUBVAR           PIC X(6).                                    
006500     03 MOD-TEMFSINF         PIC X(55).                                   
006600*                                 INFORMATION MESSAGE                     
006700*** END OF VILMAII-COPY LENGTH= 1027 BYTES                                
