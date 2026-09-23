000100 01  DOC-W428231.                                                         
000200*                                 PRINT QUERY AT LDC,PGM W42823           
000300     03 DOC-IDAFPRCD         PIC X(10).                                   
000400*                                 AFP-BLANKETT POSTTYP                    
000500     03 DOC-IDDC             PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 DOC-ADCITY           PIC X(20).                                   
000800     03 DOC-RUBRIK           PIC X(30).                                   
000900     03 DOC-TIAAVV           PIC 9(4).                                    
001000*                                 ≈R - VECKA  (≈≈VV)                      
001100     03 DOC-KVRETINL-WEEK    PIC Z(5)9.                                   
001200*                                 INLAGT ANTAL VID RETUR                  
001300     03 DOC-REPROCENT-WEEK   PIC Z(2)9.9(3).                              
001400     03 DOC-KVRETINL-YEAR    PIC Z(5)9.                                   
001500*                                 INLAGT ANTAL VID RETUR                  
001600     03 DOC-REPROCENT-YEAR   PIC Z(2)9.9(3).                              
001700*** END OF VILMAII-COPY LENGTH= 92 BYTES                                  
