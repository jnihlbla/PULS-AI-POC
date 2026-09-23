000100 01  DOC-W428131.                                                         
000200*                                 PRINT QUERY AT LDC,PGM W42813           
000300     03 DOC-IDAFPRCD         PIC X(10).                                   
000400*                                 AFP-BLANKETT POSTTYP                    
000500     03 DOC-IDDC             PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 DOC-ADCITY           PIC X(20).                                   
000800     03 DOC-TIAAPP           PIC 9(4).                                    
000900*                                 ≈R - PLANERINGSPERIOD (≈≈PP)            
001000*                                 12 PER ≈R                               
001100     03 DOC-KVRETINL         PIC Z(5)9.                                   
001200*                                 INLAGT ANTAL VID RETUR                  
001300     03 DOC-KVRETINL-SKR     PIC Z(5)9.                                   
001400*                                 INRPT ANTAL SOM SKROTATS                
001500     03 DOC-KVAVV-KVANT      PIC Z(6)9.                                   
001600*                                 ANTALSAVVIKELSE KVANTITET               
001700     03 DOC-KVDAGDEC         PIC Z(3)9.9.                                 
001800*                                 ANTAL DAGAR MED DECIMAL                 
001900*** END OF VILMAII-COPY LENGTH= 61 BYTES                                  
