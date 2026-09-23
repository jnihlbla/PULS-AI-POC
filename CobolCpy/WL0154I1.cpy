000100 01  REQU-WL0154I1.                                                       
000200*                                 REQUEST TO PGM WL0154                   
000300     03 REQU-IDDC-REP        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 REQU-IDPGM-REP       PIC X(8).                                    
000700*                                 PROGRAM IDENTITET                       
000800*                                 PROGRAM INTENTITY                       
000900     03 REQU-L154-RT-POST    OCCURS 10 TIMES.                             
001000        05 REQU-IDDISTR-REP  PIC 9(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200*                                 DISTRICT NUMBER                         
001300        05 REQU-IDKUNDNR-REP PIC 9(6).                                    
001400*                                 KUNDNUMMER                              
001500*                                 CUSTOMER NO                             
001600        05 REQU-IDRAPPNR-REP PIC 9(7).                                    
001700*                                 RAPPORT NUMMER                          
001800*                                 DISCREPANCY REPORT NUMBER               
001900*** END OF VILMAII-COPY LENGTH= 180 BYTES                                 
