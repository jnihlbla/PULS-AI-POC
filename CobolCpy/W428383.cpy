000100 01  DOC-W428383.                                                         
000200*                                 PRINT QUERY AT LDC,PGM W42838           
000300*                                 TOTAL REPORT ROW                        
000400     03 DOC-IDAFPRCD         PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 DOC-SURADER-DAY      PIC Z(8)9.                                   
000700*                                 TOTALT ANTAL RADER                      
000800     03 DOC-KVRETINL         PIC Z(5)9.                                   
000900*                                 INLAGT ANTAL VID RETUR                  
001000     03 DOC-KVAVV-KVANT      PIC Z(6)9.                                   
001100*                                 ANTALSAVVIKELSE KVANTITET               
001200     03 DOC-KVRETINL-SKR     PIC Z(5)9.                                   
001300*                                 INRPT ANTAL SOM SKROTATS                
001400     03 DOC-KVAVV-KVAL       PIC Z(6)9.                                   
001500*                                 ANTALSAVVIKELSE KVALITET                
001600     03 DOC-KVDAGDEC         PIC Z(3)9.9.                                 
001700*                                 ANTAL DAGAR MED DECIMAL                 
001800*** END OF VILMAII-COPY LENGTH= 51 BYTES                                  
