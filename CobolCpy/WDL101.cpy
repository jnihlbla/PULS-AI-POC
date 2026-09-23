000100 01  ORD-WDL101.                                                          
000200*                                 ORDER PACKUNDERLAG HISTORIK             
000300*                                 ORDER INFORMATION                       
000400*                                 FYSISK NYCKEL WDL101KY                  
000500*                                 (IDDISTR + IDKUNDNR +                   
000600*                                  IDKUNDRF + IDDC              )         
000700     03 ORD-IDDISTR          PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900*                                 DISTRICT NUMBER                         
001000     03 ORD-IDKUNDNR         PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200*                                 CUSTOMER NO                             
001300     03 ORD-IDKUNDRF         PIC X(10).                                   
001400*                                 KUNDENS REFERENS (ORDERID)              
001500*                                 CUSTOMER REFERENCE (ORDER ID)           
001600     03 ORD-IDDC             PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800*                                 WAREHOUSE IDENTIFIER                    
001900*** END COPY WDL101      LENGTH=19                                        
