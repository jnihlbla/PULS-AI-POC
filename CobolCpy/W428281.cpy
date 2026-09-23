000100 01  DOC-W428281.                                                         
000200*                                 PRINT QUERY AT LDC,PGM W42828           
000300     03 DOC-IDAFPRCD         PIC X(10).                                   
000400*                                 AFP-BLANKETT POSTTYP                    
000500     03 DOC-IDDC             PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 DOC-ADCITY           PIC X(20).                                   
000800     03 DOC-RUBRIK           PIC X(30).                                   
000900     03 DOC-TIAAPP           PIC 9(4).                                    
001000*                                 ≈R - PLANERINGSPERIOD (≈≈PP)            
001100*                                 12 PER ≈R                               
001200     03 DOC-KVRETINL-PERIOD  PIC Z(5)9.                                   
001300*                                 INLAGT ANTAL VID RETUR                  
001400     03 DOC-REPROCENT-PERIOD PIC Z(2)9.9(3).                              
001500     03 DOC-KVRETINL-YEAR    PIC Z(5)9.                                   
001600*                                 INLAGT ANTAL VID RETUR                  
001700     03 DOC-REPROCENT-YEAR   PIC Z(2)9.9(3).                              
001800*** END OF VILMAII-COPY LENGTH= 92 BYTES                                  
