000100 01  MID-W2I36701.                                                        
000200*                                 COPYTEXT FOR MID W2I36701               
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 WAREHOUSE IDENTIFIER                    
000500     03 MID-KVAKT-UP         PIC 9(2).                                    
000600*                                 LEVEL FOR ACTIVATING A PART             
000700     03 MID-KVVECKOR-PUBV-UP PIC 9(3).                                    
000800*                                 NO. OF WEEKS SINCE PUBL.WEEK            
000900     03 MID-PRARTSTD-UP      PIC 9(7).                                    
001000*                                 STANDARD PRICE                          
001100     03 MID-VLARTNTO-UP      PIC 9(8).                                    
001200*                                 PART NET VOLUME    (CM3)                
001300     03 MID-KVPB-SEP-REF-UP  PIC 9(6).                                    
001400*                                 SUM OF PB-SEP + REFIL                   
001500     03 MID-KDPRODSL-UP      OCCURS 11 TIMES                              
001600                             PIC 9(2).                                    
001700*                                 PRODUCT GROUP                           
001800     03 MID-DATA-LINES       OCCURS 13 TIMES.                             
001900*                                 OCCURS CLAUSE FOR W2I36701 COPY         
002000*                                 TEXT                                    
002100        05 MID-KDCMDVAL      PIC X.                                       
002200*                                 GENERAL COMMAND-CODE                    
002300        05 MID-KVAKT         PIC X(2).                                    
002400*                                 LEVEL FOR ACTIVATING A PART             
002500        05 MID-KVVECKOR-PUBV PIC X(3).                                    
002600*                                 NO. OF WEEKS SINCE PUBL.WEEK            
002700        05 MID-PRARTSTD      PIC X(7).                                    
002800*                                 STANDARD PRICE                          
002900        05 MID-VLARTNTO      PIC X(8).                                    
003000*                                 PART NET VOLUME    (CM3)                
003100        05 MID-KVPB-SEP-REF  PIC X(6).                                    
003200*                                 SUM OF PB-SEP + REFIL                   
003300        05 MID-KDPRODSL      OCCURS 11 TIMES                              
003400                             PIC X(2).                                    
003500*                                 PRODUCT GROUP                           
003600*** END OF VILMAII-COPY LENGTH= 687 BYTES                                 
