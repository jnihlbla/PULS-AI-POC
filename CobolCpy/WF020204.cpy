000100 01  DAP-LINE-WF0202.                                                     
000200*                                 BILLIT ERROR LINE 4                     
000300     03 DAP-LINE-TEXT-11     PIC X(10).                                   
000400     03 DAP-LINE-KDVAT       PIC X(2).                                    
000500*                                 MOMSKOD                                 
000600*                                 VAT CODE                                
000700     03 DAP-LINE-TEXT-12     PIC X(11).                                   
000800     03 DAP-LINE-KDVALISO    PIC X(3).                                    
000900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001000*                                 CURRENCY CODE BY ISO-STANDARD.          
001100     03 DAP-LINE-TEXT-13     PIC X(12).                                   
001200     03 DAP-LINE-PRARTBTO    PIC Z(6)9.9(2).                              
001300*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
001400*                                 GROSS SALES PRICE (SEK)                 
001500     03 DAP-LINE-TEXT-14     PIC X(12).                                   
001600     03 DAP-LINE-PRARTNTO    PIC Z(6)9.9(2).                              
001700*                                 ARTIKELPRIS NETTO                       
001800*                                 NET PRICE EACH   (FOB NET)              
001900*** END OF VILMAII-COPY LENGTH= 70 BYTES                                  
