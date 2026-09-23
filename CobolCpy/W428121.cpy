000100 01  DOC-W428121.                                                         
000200*                                 PRINT QUERY AT LDC,PGM W42812           
000300     03 DOC-IDAFPRCD         PIC X(10).                                   
000400*                                 AFP-BLANKETT POSTTYP                    
000500     03 DOC-IDDC             PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 DOC-ADCITY           PIC X(20).                                   
000800     03 DOC-TIAAVV           PIC 9(4).                                    
000900*                                 ≈R - VECKA  (≈≈VV)                      
001000     03 DOC-KVRETINL         PIC Z(5)9.                                   
001100*                                 INLAGT ANTAL VID RETUR                  
001200     03 DOC-KVRETINL-SKR     PIC Z(5)9.                                   
001300*                                 INRPT ANTAL SOM SKROTATS                
001400     03 DOC-KVAVV-KVANT      PIC Z(6)9.                                   
001500*                                 ANTALSAVVIKELSE KVANTITET               
001600     03 DOC-KVDAGDEC         PIC Z(3)9.9.                                 
001700*                                 ANTAL DAGAR MED DECIMAL                 
001800*** END OF VILMAII-COPY LENGTH= 61 BYTES                                  
