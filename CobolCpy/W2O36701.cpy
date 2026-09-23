000100 01  MOD-W2O36701.                                                        
000200*                                 COPYTEXT FOR MOD W2O36701               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 SCREEN NUMBER                           
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS ERROR MESSAGE                       
000700     03 MOD-IDDC-IN-ATTR     PIC X(2).                                    
000800     03 MOD-IDDC-IN          PIC X(2).                                    
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 MOD-IDDC-UT          PIC X(2).                                    
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 MOD-IDUSER           PIC X(8).                                    
001300*                                 USER SECURITY-IDENTITY                  
001400     03 MOD-TIUPPDAT         PIC Z(6).                                    
001500*                                 UPDATING DATE     (YYMMDD)              
001600     03 MOD-KVAKT-UP-ATTR    PIC X(2).                                    
001700     03 MOD-KVAKT-UP         PIC X(2).                                    
001800*                                 LEVEL FOR ACTIVATING A PART             
001900     03 MOD-KVVECKOR-PUBV-UP-ATTR                                         
002000                             PIC X(2).                                    
002100     03 MOD-KVVECKOR-PUBV-UP PIC X(3).                                    
002200*                                 NO. OF WEEKS SINCE PUBL.WEEK            
002300     03 MOD-PRARTSTD-UP-ATTR PIC X(2).                                    
002400     03 MOD-PRARTSTD-UP      PIC X(7).                                    
002500*                                 STANDARD PRICE                          
002600     03 MOD-VLARTNTO-UP-ATTR PIC X(2).                                    
002700     03 MOD-VLARTNTO-UP      PIC X(8).                                    
002800*                                 PART NET VOLUME    (CM3)                
002900     03 MOD-KVPB-SEP-REF-UP-ATTR                                          
003000                             PIC X(2).                                    
003100     03 MOD-KVPB-SEP-REF-UP  PIC X(6).                                    
003200*                                 SUM OF PB-SEP + REFIL                   
003300     03 MOD-PRODGRP-UP       OCCURS 11 TIMES.                             
003400*                                 OCCURS CLAUSE FOR W2036701 COPY         
003500*                                 TEXT                                    
003600        05 MOD-KDPRODSL-UP-ATTR                                           
003700                             PIC X(2).                                    
003800        05 MOD-KDPRODSL-UP   PIC X(2).                                    
003900*                                 PRODUCT GROUP                           
004000     03 MOD-DATA-LINES       OCCURS 13 TIMES.                             
004100*                                 OCCURS CLAUSE FOR W2036701 COPY         
004200*                                 TEXT                                    
004300        05 MOD-KDCMDVAL-ATTR PIC X(2).                                    
004400        05 MOD-KDCMDVAL      PIC X.                                       
004500*                                 GENERAL COMMAND-CODE                    
004600        05 MOD-KVAKT         PIC Z9.                                      
004700*                                 LEVEL FOR ACTIVATING A PART             
004800        05 MOD-KVVECKOR-PUBV PIC Z(2)9.                                   
004900*                                 NO. OF WEEKS SINCE PUBL.WEEK            
005000        05 MOD-PRARTSTD      PIC Z(6)9.                                   
005100*                                 STANDARD PRICE                          
005200        05 MOD-VLARTNTO      PIC Z(7)9.                                   
005300*                                 PART NET VOLUME    (CM3)                
005400        05 MOD-KVPB-SEP-REF  PIC Z(5)9.                                   
005500*                                 SUM OF PB-SEP + REFIL                   
005600        05 MOD-KDPRODSL      OCCURS 11 TIMES                              
005700                             PIC Z9.                                      
005800*                                 PRODUCT GROUP                           
005900     03 MOD-TEMFSINF         PIC X(55).                                   
006000*                                 INFORMATION MESSAGE                     
006100*** END OF VILMAII-COPY LENGTH= 862 BYTES                                 
