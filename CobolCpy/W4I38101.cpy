000100 01  MID-W4I38101.                                                        
000200     03 MID-KDPRCGRP-IN      PIC X(5).                                    
000300*                                 GROUP OF PRODUCTION CHANNELS            
000400     03 MID-KDPRCGRP-OUT     PIC X(5).                                    
000500*                                 GROUP OF PRODUCTION CHANNELS            
000600     03 MID-KDPRODKL-IN      PIC X.                                       
000700*                                 PRODUCTION CLASS                        
000800     03 MID-KDPRODKL-OUT     PIC X.                                       
000900*                                 PRODUCTION CLASS                        
001000     03 MID-IDPRC-IN.                                                     
001100*                                 PRODUCTION CHANNEL                      
001200        05 MID-IDPRCBAS      PIC X(3).                                    
001300*                                 PRC-BASIC                               
001400        05 MID-IDPRCVAR      PIC X.                                       
001500*                                 PRC-VARIANT                             
001600     03 MID-IDPRC-OUT.                                                    
001700*                                 PRODUCTION CHANNEL                      
001800        05 MID-IDPRCBAS      PIC X(3).                                    
001900*                                 PRC-BASIC                               
002000        05 MID-IDPRCVAR      PIC X.                                       
002100*                                 PRC-VARIANT                             
002200     03 MID-IDDC-IN          PIC X(2).                                    
002300*                                 WAREHOUSE IDENTIFIER                    
002400     03 MID-IDDC-OUT         PIC X(2).                                    
002500*                                 WAREHOUSE IDENTIFIER                    
002600     03 MID-IDORDER-ENTER    PIC X(7).                                    
002700*                                 VOLVO PARTS ORDER NUMBER                
002800     03 MID-TIRFS-ENTER      PIC X(10).                                   
002900*                                 READY FOR SHIPMENT  YYMMDDHHMM          
003000     03 MID-TILST-O-ENTER    PIC X(10).                                   
003100*                                 LATEST START-TIME ORDER                 
003200     03 MID-IDPRCVAR-ENTER   OCCURS 10 TIMES                              
003300                             PIC X.                                       
003400*                                 PRC-VARIANT                             
003500     03 MID-IDORDER-NEXT     PIC X(7).                                    
003600*                                 VOLVO PARTS ORDER NUMBER                
003700     03 MID-TIRFS-NEXT       PIC X(10).                                   
003800*                                 READY FOR SHIPMENT  YYMMDDHHMM          
003900     03 MID-TILST-O-NEXT     PIC X(10).                                   
004000*                                 LATEST START-TIME ORDER                 
004100     03 MID-IDPRCVAR-NEXT    OCCURS 10 TIMES                              
004200                             PIC X.                                       
004300*                                 PRC-VARIANT                             
