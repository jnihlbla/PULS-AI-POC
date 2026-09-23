000100 01  MID-W2I36601.                                                        
000200*                                 COPYTEXT FOR MID W2I36601               
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 WAREHOUSE IDENTIFIER                    
000500     03 MID-KVVECKOR-LSALES-UP                                            
000600                             PIC 9(3).                                    
000700*                                 NN.OF WEEKS SINCE LAST SALES            
000800     03 MID-KVVECKOR-PUBV-UP PIC 9(3).                                    
000900*                                 NO. OF WEEKS SINCE PUBL.WEEK            
001000     03 MID-PRARTSTD-UP      PIC 9(7).                                    
001100*                                 STANDARD PRICE                          
001200     03 MID-VLARTNTO-UP      PIC 9(8).                                    
001300*                                 PART NET VOLUME    (CM3)                
001400     03 MID-KDPRODSL-UP      PIC 9(2).                                    
001500*                                 PRODUCT GROUP                           
001600     03 MID-ADLAGOMR-UP      OCCURS 10 TIMES                              
001700                             PIC 9(2).                                    
001800*                                 AREA                                    
001900     03 MID-DATA-LINES       OCCURS 13 TIMES.                             
002000*                                 OCCURS CLAUSE FOR W2I36601 COPY         
002100*                                 TEXT                                    
002200        05 MID-KDCMDVAL      PIC X.                                       
002300*                                 GENERAL COMMAND-CODE                    
002400        05 MID-KVVECKOR-LSALES                                            
002500                             PIC X(3).                                    
002600*                                 NN.OF WEEKS SINCE LAST SALES            
002700        05 MID-KVVECKOR-PUBV PIC X(3).                                    
002800*                                 NO. OF WEEKS SINCE PUBL.WEEK            
002900        05 MID-PRARTSTD      PIC X(7).                                    
003000*                                 STANDARD PRICE                          
003100        05 MID-VLARTNTO      PIC X(8).                                    
003200*                                 PART NET VOLUME    (CM3)                
003300        05 MID-KDPRODSL      PIC X(2).                                    
003400*                                 PRODUCT GROUP                           
003500        05 MID-ADLAGOMR      OCCURS 10 TIMES                              
003600                             PIC X(2).                                    
003700*                                 AREA                                    
003800*** END OF VILMAII-COPY LENGTH= 617 BYTES                                 
