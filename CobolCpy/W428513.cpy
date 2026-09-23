000100 01  DOC-W428513.                                                         
000200*                                 PRINT QUERY AT LDC,PGM W42851           
000300*                                 TOTAL REPORT ROW                        
000400     03 DOC-IDAFPRCD         PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 DOC-KVRETINL         PIC Z(5)9.                                   
000700*                                 INLAGT ANTAL VID RETUR                  
000800     03 DOC-KVAVV-KVANT      PIC Z(6)9.                                   
000900*                                 ANTALSAVVIKELSE KVANTITET               
001000     03 DOC-KVRETINL-SKR     PIC Z(5)9.                                   
001100*                                 INRPT ANTAL SOM SKROTATS                
001200     03 DOC-KVAVV-KVAL       PIC Z(6)9.                                   
001300*                                 ANTALSAVVIKELSE KVALITET                
001400     03 DOC-KVDAGDEC         PIC Z(3)9.9.                                 
001500*                                 ANTAL DAGAR MED DECIMAL                 
001600*** END OF VILMAII-COPY LENGTH= 42 BYTES                                  
