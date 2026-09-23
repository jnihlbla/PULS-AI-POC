000100 01  LINE-W476CS3.                                                        
000200*                                 COPYTEXT FOR CARGO SPEC LINE WE         
000300*                                 B-LDC                                   
000400     03 LINE-IDAFPRCD        PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 LINE-RAD6-IDORDNR7   PIC Z(6)9.                                   
000700*                                 ORDERNUMMER                             
000800     03 LINE-RAD6-IDKOLLI    PIC Z(4)9.                                   
000900*                                 KOLLINUMMER                             
001000     03 LINE-RAD6-KDORDKL    PIC 9.                                       
001100*                                 ORDERKLASS                              
001200     03 LINE-RAD6-BEEMBTYP   PIC X(12).                                   
001300*                                 EMBALLAGETYPSTEXT  BEEMBTYP-002         
001400     03 LINE-RAD6-DIKOLLIL   PIC Z(5).                                    
001500*                                 KOLLI-LÄNGD                             
001600     03 LINE-RAD6-DIKOLLIB   PIC Z(3).                                    
001700*                                 KOLLI-BREDD                             
001800     03 LINE-RAD6-DIKOLLIH   PIC Z(3).                                    
001900*                                 KOLLI-HÖJD                              
002000     03 LINE-RAD6-VKORDBTO   PIC Z(5)9.9.                                 
002100*                                 ORDERVIKT BRUTTO (KG)                   
002200     03 LINE-RAD6-VKORDNTO   PIC Z(5)9.9(3).                              
002300     03 LINE-RAD6-VLORDBTO   PIC Z(3)9.9(3).                              
002400*                                 ORDERVOLYM BRUTTO (M3)                  
002500     03 LINE-RAD6-SUORDV     PIC Z(8)9.9(2).                              
002600*                                 SUMMA ORDERVÄRDE                        
002700     03 LINE-RAD6-KDVALISO   PIC X(3).                                    
002800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002900     03 LINE-RAD6-FARLIG     PIC X(27).                                   
003000     03 LINE-RAD6-FLCROSS    PIC X.                                       
003100*                                 CROSS-DOCK KOLLI FLAGGA                 
003200*** END OF VILMAII-COPY LENGTH= 115 BYTES                                 
