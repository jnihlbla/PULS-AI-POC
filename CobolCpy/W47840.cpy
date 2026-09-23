000100 01  W47840.                                                              
000200*                                 DISTRIBUTION FOLLOW UP PER              
000300*                                 DC, RFSDATE, PRC.                       
000400*                                                                         
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 TIRFSDAT             PIC 9(6).                                    
000900*                                 KLART F÷R TRANSPORT ≈≈MMDD              
001000*                                 READY FOR SHIPMENT  YYMMDD              
001100     03 IDPRC.                                                            
001200*                                 PRODUKTIONSKANAL                        
001300*                                 PRODUCTION CHANNEL                      
001400        05 IDPRCBAS          PIC X(3).                                    
001500*                                 PRC-BAS                                 
001600*                                 PRC-BASIC                               
001700        05 IDPRCVAR          PIC X.                                       
001800*                                 PRC-VARIANT                             
001900*                                 PRC-VARIANT                             
002000     03 KDORDKL              PIC S9              COMP-3.                  
002100*                                 ORDERKLASS                              
002200*                                 ORDER CLASS                             
002300     03 KVORDER              PIC S9(7)           COMP-3.                  
002400*                                 ANTAL ORDER                             
002500*                                 QUANTITY OF ORDERS                      
002600     03 KVORDER-UTSKR        PIC S9(7)           COMP-3.                  
002700*                                 ANTAL ORDER                             
002800*                                 QUANTITY OF ORDERS                      
002900     03 KVORDER-PACK         PIC S9(7)           COMP-3.                  
003000*                                 ANTAL PACKADE ORDER                     
003100*                                 QUANTITY OF PACKED ORDERS               
003200     03 KVORDRAD             PIC S9(5)           COMP-3.                  
003300*                                 ANTAL ORDERRADER                        
003400*                                 NUMBER OF ORDER LINES                   
003500     03 KVORDRAD-UTSKR       PIC S9(5)           COMP-3.                  
003600*                                 ANTAL UTSKR ORDERRAD                    
003700     03 KVORDRAD-PACK        PIC S9(5)           COMP-3.                  
003800*                                 ANTAL PACKADE ORDERRADER                
003900*** END OF VILMAII-COPY LENGTH= 34 BYTES                                  
