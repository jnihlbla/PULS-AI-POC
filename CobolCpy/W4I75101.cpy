000100 01  MID-W4I75101.                                                        
000200*                                 COPYTEXT FOR MID W4I75101               
000300     03 MID-INPUT            OCCURS 12 TIMES.                             
000400*                                 OCCURS CLAUSE FOR W4I75101 COPY         
000500*                                 TEXT                                    
000600        05 MID-KDCMDVAL      PIC X.                                       
000700*                                 GENERAL COMMAND-CODE                    
000800        05 MID-BESORTRT      PIC X(20).                                   
000900*                                 SORT ALLOWED FOR RETURNS                
001000        05 MID-IDARTNR       PIC 9(9).                                    
001100*                                 PART NUMBER                             
001200        05 MID-KDFARLIG      PIC X.                                       
001300*                                 DANGEROUS GOODS CODE                    
001400        05 MID-KDSORT        PIC X(2).                                    
001500*                                 UNIT OF MEASURE                         
001600        05 MID-IDLEVNR       PIC X(5).                                    
001700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001800        05 MID-IDFKNGRP      PIC 9(4).                                    
001900*                                 FUNCTION GROUP                          
002000        05 MID-KDPRODSL      PIC 9(2).                                    
002100*                                 PRODUCT GROUP                           
002200     03 MID-KDCMDVAL-UP      PIC X.                                       
002300*                                 GENERAL COMMAND-CODE                    
002400     03 MID-BESORTRT-UP      PIC X(20).                                   
002500*                                 SORT ALLOWED FOR RETURNS                
002600     03 MID-IDARTNR-UP       PIC 9(9).                                    
002700*                                 PART NUMBER                             
002800     03 MID-KDFARLIG-UP      PIC X.                                       
002900*                                 DANGEROUS GOODS CODE                    
003000     03 MID-KDSORT-UP        PIC X(2).                                    
003100*                                 UNIT OF MEASURE                         
003200     03 MID-IDLEVNR-UP       PIC X(5).                                    
003300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003400     03 MID-IDFKNGRP-UP      PIC 9(4).                                    
003500*                                 FUNCTION GROUP                          
003600     03 MID-KDPRODSL-UP      PIC 9(2).                                    
003700*                                 PRODUCT GROUP                           
003800*** END OF VILMAII-COPY LENGTH= 572 BYTES                                 
