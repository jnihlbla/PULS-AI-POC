000100 01  RIR-W461RIRN-CTX.                                                    
000200*                                 CANCELATION TRANS. FOR                  
000300*                                 COMPLETED ORDERS                        
000400*                                   RECORD TYPE RIR                       
000500     03 RIR-IDPTYP           PIC X(3).                                    
000600*                                 RECORD TYPE                             
000700     03 RIR-IDDC             PIC X(2).                                    
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 RIR-IDDISTR          PIC 9(4).                                    
001000*                                 DISTRICT NUMBER                         
001100     03 RIR-IDKUNDNR         PIC 9(6).                                    
001200*                                 CUSTOMER NO                             
001300     03 RIR-IDORDNR          PIC 9(7).                                    
001400*                                 ORDER NUMBER        IDORDNR-002         
001500     03 RIR-BEVOLREF         PIC X(10).                                   
001600*                                 VOLVO REFERENCE                         
001700     03 RIR-TIORDREG         PIC 9(6).                                    
001800*                                 ORDER REGISTRATION DATE  YYMMDD         
001900     03 RIR-FILLERX42        PIC X(42).                                   
002000*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
