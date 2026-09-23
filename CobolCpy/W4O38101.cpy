000100 01  MOD-W4O38101.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 SCREEN NUMBER                           
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS ERROR MESSAGE                       
000600     03 MOD-KDPRCGRP-IN      PIC X(5).                                    
000700*                                 GROUP OF PRODUCTION CHANNELS            
000800     03 MOD-KDPRODKL-IN      PIC X.                                       
000900*                                 PRODUCTION CLASS                        
001000     03 MOD-IDPRC-IN.                                                     
001100*                                 PRODUCTION CHANNEL                      
001200        05 MOD-IDPRCBAS      PIC X(3).                                    
001300*                                 PRC-BASIC                               
001400        05 MOD-IDPRCVAR      PIC X.                                       
001500*                                 PRC-VARIANT                             
001600     03 MOD-IDDC-IN          PIC X(2).                                    
001700*                                 WAREHOUSE IDENTIFIER                    
001800     03 MOD-KDPRCGRP-OUT     PIC X(5).                                    
001900*                                 GROUP OF PRODUCTION CHANNELS            
002000     03 MOD-KDPRODKL-OUT     PIC X.                                       
002100*                                 PRODUCTION CLASS                        
002200     03 MOD-IDPRC-OUT.                                                    
002300*                                 PRODUCTION CHANNEL                      
002400        05 MOD-IDPRCBAS      PIC X(3).                                    
002500*                                 PRC-BASIC                               
002600        05 MOD-IDPRCVAR      PIC X.                                       
002700*                                 PRC-VARIANT                             
002800     03 MOD-IDDC-OUT         PIC X(2).                                    
002900*                                 WAREHOUSE IDENTIFIER                    
003000     03 MOD-IDORDER-ENTER    PIC X(7).                                    
003100*                                 VOLVO PARTS ORDER NUMBER                
003200     03 MOD-TIRFS-ENTER      PIC X(10).                                   
003300*                                 READY FOR SHIPMENT  YYMMDDHHMM          
003400     03 MOD-TILST-O-ENTER    PIC X(10).                                   
003500*                                 LATEST START-TIME ORDER                 
003600     03 MOD-IDPRCVAR-ENTER   OCCURS 10 TIMES                              
003700                             PIC X.                                       
003800*                                 PRC-VARIANT                             
003900     03 MOD-IDORDER-NEXT     PIC X(7).                                    
004000*                                 VOLVO PARTS ORDER NUMBER                
004100     03 MOD-TIRFS-NEXT       PIC X(10).                                   
004200*                                 READY FOR SHIPMENT  YYMMDDHHMM          
004300     03 MOD-TILST-O-NEXT     PIC X(10).                                   
004400*                                 LATEST START-TIME ORDER                 
004500     03 MOD-IDPRCVAR-NEXT    OCCURS 10 TIMES                              
004600                             PIC X.                                       
004700*                                 PRC-VARIANT                             
004800     03 MOD-LINE1            OCCURS 10 TIMES.                             
004900        05 MOD-IDPRC-ATTR    PIC X(2).                                    
005000        05 MOD-IDPRC-LINE.                                                
005100*                                 PRODUCTION CHANNEL                      
005200           07 MOD-IDPRCBAS   PIC X(3).                                    
005300*                                 PRC-BASIC                               
005400           07 MOD-IDPRCVAR   PIC X.                                       
005500*                                 PRC-VARIANT                             
005600     03 MOD-LINE2            OCCURS 14 TIMES.                             
005700        05 MOD-IDDISTR-ATTR  PIC X(2).                                    
005800        05 MOD-IDDISTR       PIC Z(3)9.                                   
005900*                                 DISTRICT NUMBER                         
006000        05 MOD-IDKUNDNR-ATTR PIC X(2).                                    
006100        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
006200*                                 CUSTOMER NO                             
006300        05 MOD-IDORDNR7-ATTR PIC X(2).                                    
006400        05 MOD-IDORDNR7      PIC Z(6)9.                                   
006500*                                 ORDER NUMBER                            
006600        05 MOD-IDPRODNR-ATTR PIC X(2).                                    
006700        05 MOD-IDPRODNR      PIC Z(6)9.                                   
006800*                                 PRODUCTION-NUMBER                       
006900        05 MOD-LINE3         OCCURS 10 TIMES.                             
007000           07 MOD-KDODELSTA-ATTR                                          
007100                             PIC X(2).                                    
007200           07 MOD-KDODELSTA  PIC X.                                       
007300*                                 ORDER PART STATUS                       
007400           07 MOD-STARSPAC   PIC X.                                       
007500*                                 LINE UPDATE COMMAND                     
007600     03 MOD-TEMFSINF         PIC X(55).                                   
007700*                                 INFORMATION MESSAGE                     
