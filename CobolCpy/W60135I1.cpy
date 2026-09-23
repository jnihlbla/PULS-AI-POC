000100 01  REQU-W60135I1.                                                       
000200*                                 REQU-COPYTEXT FÖR W6013500              
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 REQU-IDSPRAK         PIC X(2).                                    
000700*                                 2-STÄLLIG ISO SPRÅKKOD                  
000800*                                 2-LETTER ISO LANGUAGE CODE              
000900     03 REQU-FLGODK          PIC X.                                       
001000     03 REQU-RAD             OCCURS 14 TIMES.                             
001100        05 REQU-RAD          OCCURS 2 TIMES.                              
001200           07 REQU-IDLEVNR-KOLLI                                          
001300                             PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER KOLLI                  
001500*                                 SUPPLIER NUMBER CASE                    
001600           07 REQU-IDOKOLLI  PIC X(9).                                    
001700*                                 ODETTE KOLLINUMMER                      
001800*                                 ODETTE CASE NUMBER                      
001900*** END OF VILMAII-COPY LENGTH= 397 BYTES                                 
