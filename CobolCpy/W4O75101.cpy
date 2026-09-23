000100 01  MOD-W4O75101.                                                        
000200*                                 COPYTEXT FOR MOD W4O75101               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 SCREEN NUMBER                           
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS ERROR MESSAGE                       
000700     03 MOD-INPUT            OCCURS 12 TIMES.                             
000800*                                 OCCURS CLAUSE FOR W4O75101 COPY         
000900*                                 TEXT                                    
001000        05 MOD-KDCMDVAL-ATTR PIC X(2).                                    
001100        05 MOD-KDCMDVAL      PIC X.                                       
001200*                                 GENERAL COMMAND-CODE                    
001300        05 MOD-BESORTRT-ATTR PIC X(2).                                    
001400        05 MOD-BESORTRT      PIC X(20).                                   
001500*                                 SORT ALLOWED FOR RETURNS                
001600        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
001700        05 MOD-IDARTNR       PIC Z(9).                                    
001800*                                 PART NUMBER                             
001900        05 MOD-KDFARLIG-ATTR PIC X(2).                                    
002000        05 MOD-KDFARLIG      PIC X.                                       
002100*                                 DANGEROUS GOODS CODE                    
002200        05 MOD-KDSORT-ATTR   PIC X(2).                                    
002300        05 MOD-KDSORT        PIC X(2).                                    
002400*                                 UNIT OF MEASURE                         
002500        05 MOD-IDLEVNR-ATTR  PIC X(2).                                    
002600        05 MOD-IDLEVNR       PIC X(5).                                    
002700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002800        05 MOD-IDFKNGRP-ATTR PIC X(2).                                    
002900        05 MOD-IDFKNGRP      PIC Z(3)9.                                   
003000*                                 FUNCTION GROUP                          
003100        05 MOD-KDPRODSL-ATTR PIC X(2).                                    
003200        05 MOD-KDPRODSL      PIC 9(2).                                    
003300*                                 PRODUCT GROUP                           
003400     03 MOD-KDCMDVAL-UP-ATTR PIC X(2).                                    
003500     03 MOD-KDCMDVAL-UP      PIC X.                                       
003600*                                 GENERAL COMMAND-CODE                    
003700     03 MOD-BESORTRT-UP-ATTR PIC X(2).                                    
003800     03 MOD-BESORTRT-UP      PIC X(20).                                   
003900*                                 SORT ALLOWED FOR RETURNS                
004000     03 MOD-IDARTNR-UP-ATTR  PIC X(2).                                    
004100     03 MOD-IDARTNR-UP       PIC Z(9).                                    
004200*                                 PART NUMBER                             
004300     03 MOD-KDFARLIG-UP-ATTR PIC X(2).                                    
004400     03 MOD-KDFARLIG-UP      PIC X.                                       
004500*                                 DANGEROUS GOODS CODE                    
004600     03 MOD-KDSORT-UP-ATTR   PIC X(2).                                    
004700     03 MOD-KDSORT-UP        PIC X(2).                                    
004800*                                 UNIT OF MEASURE                         
004900     03 MOD-IDLEVNR-UP-ATTR  PIC X(2).                                    
005000     03 MOD-IDLEVNR-UP       PIC X(5).                                    
005100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005200     03 MOD-IDFKNGRP-UP-ATTR PIC X(2).                                    
005300     03 MOD-IDFKNGRP-UP      PIC Z(4).                                    
005400*                                 FUNCTION GROUP                          
005500     03 MOD-KDPRODSL-UP-ATTR PIC X(2).                                    
005600     03 MOD-KDPRODSL-UP      PIC X(2).                                    
005700*                                 PRODUCT GROUP                           
005800     03 MOD-TEMFSINF         PIC X(55).                                   
005900*                                 INFORMATION MESSAGE                     
006000*** END OF VILMAII-COPY LENGTH= 879 BYTES                                 
