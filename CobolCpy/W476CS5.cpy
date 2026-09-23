000100 01  SUBFTR-W476CS5.                                                      
000200*                                 COPYTEXT FOR CARGO SPEC CROSS D         
000300*                                 OCK FOOTER WEB-LDC                      
000400     03 SUBFTR-IDAFPRCD      PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 SUBFTR-RAD7-CD-TOTAL-TEXT                                         
000700                             PIC X(26).                                   
000800     03 SUBFTR-RAD7-CD-KOLLI-TOTAL                                        
000900                             PIC Z(4)9.                                   
001000*                                 KOLLINUMMER                             
001100     03 SUBFTR-RAD7-CD-KOLLI-TEXT                                         
001200                             PIC X(7).                                    
001300     03 SUBFTR-RAD7-CD-VKORDBTO                                           
001400                             PIC Z(5)9.9.                                 
001500*                                 ORDERVIKT BRUTTO (KG)                   
001600     03 SUBFTR-RAD7-CD-VKORDNTO                                           
001700                             PIC Z(5)9.9(3).                              
001800     03 SUBFTR-RAD7-CD-VLORDBTO                                           
001900                             PIC Z(3)9.9(3).                              
002000*                                 ORDERVOLYM BRUTTO (M3)                  
002100     03 SUBFTR-RAD7-CD-SUORDV                                             
002200                             PIC Z(8)9.9(2).                              
002300*                                 SUMMA ORDERVÄRDE                        
002400     03 SUBFTR-RAD7-CD-KDVALISO                                           
002500                             PIC X(3).                                    
002600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002700*** END OF VILMAII-COPY LENGTH= 89 BYTES                                  
