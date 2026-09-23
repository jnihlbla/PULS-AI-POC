000100 01  REQU-W6019EI1.                                                       
000200*                                 REQU-COPYTEXT FÖR W6019E00              
000300*                                                                         
000400     03 REQU-IDDC            PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 REQU-IDLEVNR         PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001000     03 REQU-IDFS            PIC X(8).                                    
001100*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001200*                                 ADVICE NOTE NUMBER ODETTE               
001300     03 REQU-TIAVIDAT        PIC 9(6).                                    
001400*                                 AVISERINGSDATUM (YYMMDD)                
001500*                                 ADVICE NOTE DATE                        
001600     03 REQU-IDRADNR-INL     PIC 9(5).                                    
001700*                                 RADNUMMER INLEVERANS                    
001800*                                 LINE NUMBER GOODS RECEIVING             
001900*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  
