000100 01  DOC-W428552.                                                         
000200*                                 PRINT QUERY AT LDC,PGM W42855           
000300*                                 REPORT LINE DATA                        
000400     03 DOC-IDAFPRCD         PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 DOC-IDDC-RET         PIC X(2).                                    
000700*                                 MOTTAGANDE LAGER FÖR RETURER            
000800     03 DOC-ADCITY           PIC X(20).                                   
000900     03 DOC-KDANMORS         PIC X(2).                                    
001000*                                 ORSAK TILL LEVERANSANMÄRKNING           
001100     03 DOC-KVRETINL         PIC Z(5)9.                                   
001200*                                 INLAGT ANTAL VID RETUR                  
001300     03 DOC-KVAVV-KVANT      PIC Z(6)9.                                   
001400*                                 ANTALSAVVIKELSE KVANTITET               
001500     03 DOC-KVRETINL-SKR     PIC Z(5)9.                                   
001600*                                 INRPT ANTAL SOM SKROTATS                
001700     03 DOC-KVAVV-KVAL       PIC Z(6)9.                                   
001800*                                 ANTALSAVVIKELSE KVALITET                
001900     03 DOC-KVDAGDEC         PIC Z(3)9.9.                                 
002000*                                 ANTAL DAGAR MED DECIMAL                 
002100     03 DOC-SUARTSTD-INL     PIC Z(7)9.9(2).                              
002200*                                 SUMMA STANDARDPRIS RADVÄRDE             
002300*** END OF VILMAII-COPY LENGTH= 77 BYTES                                  
