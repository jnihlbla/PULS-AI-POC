000100 01  REQU-W6019DI1.                                                       
000200*                                 REQU-COPYTEXT FÖR W6019D00              
000300*                                                                         
000400     03 REQU-IDDC            PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 REQU-IDLOPNRM        PIC 9(8).                                    
000800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000900*                                 (0VVDLLLLK)                             
001000*                                 SERIAL NO RECEIVING REPORT              
001100*                                 (0WWDLLLLC)                             
001200     03 REQU-IDLEVNR         PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001500     03 REQU-IDFS            PIC X(8).                                    
001600*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001700*                                 ADVICE NOTE NUMBER ODETTE               
001800     03 REQU-TIAVIDAT        PIC 9(6).                                    
001900*                                 AVISERINGSDATUM (YYMMDD)                
002000*                                 ADVICE NOTE DATE                        
002100*** END OF VILMAII-COPY LENGTH= 29 BYTES                                  
