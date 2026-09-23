000100 01  DOC-W428543.                                                         
000200*                                 PRINT QUERY AT LDC,PGM W42854           
000300*                                 TOTAL REPORT ROW                        
000400     03 DOC-IDAFPRCD         PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 DOC-SURADER-RET      PIC Z(8)9.                                   
000700*                                 TOTALT ANTAL RADER                      
000800     03 DOC-SUARTSTD-RET     PIC Z(7)9.9(2).                              
000900*                                 SUMMA STANDARDPRIS RADVÄRDE             
001000     03 DOC-KVRETINL         PIC Z(5)9.                                   
001100*                                 INLAGT ANTAL VID RETUR                  
001200     03 DOC-KVAVV-KVANT      PIC Z(6)9.                                   
001300*                                 ANTALSAVVIKELSE KVANTITET               
001400     03 DOC-KVRETINL-SKR     PIC Z(5)9.                                   
001500*                                 INRPT ANTAL SOM SKROTATS                
001600     03 DOC-KVAVV-KVAL       PIC Z(6)9.                                   
001700*                                 ANTALSAVVIKELSE KVALITET                
001800     03 DOC-KVDAGDEC         PIC Z(3)9.9.                                 
001900*                                 ANTAL DAGAR MED DECIMAL                 
002000     03 DOC-SUARTSTD-INL     PIC Z(7)9.9(2).                              
002100*                                 SUMMA STANDARDPRIS RADVÄRDE             
002200*** END OF VILMAII-COPY LENGTH= 73 BYTES                                  
