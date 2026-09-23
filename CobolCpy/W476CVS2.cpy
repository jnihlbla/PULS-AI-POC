000100 01  SUBHDR-W476CVS2.                                                     
000200*                                 COPYTEXT FOR CARGO VALUE SPEC S         
000300*                                 UBHEADER WEB-LDC                        
000400     03 SUBHDR-IDAFPRCD      PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 SUBHDR-RAD2-GOODS-VALUE                                           
000700                             PIC -Z(8)9.9(2).                             
000800*                                 SUMMABELOPP                             
000900     03 SUBHDR-RAD2-KDVALISO PIC X(3).                                    
001000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001100     03 SUBHDR-RAD3-COMPL-VALUE                                           
001200                             PIC -Z(8)9.9(2).                             
001300*                                 SUMMABELOPP                             
001400     03 SUBHDR-RAD3-KDVALISO PIC X(3).                                    
001500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001600     03 SUBHDR-RAD4-TOTAL-VALUE                                           
001700                             PIC -Z(8)9.9(2).                             
001800*                                 SUMMABELOPP                             
001900     03 SUBHDR-RAD4-KDVALISO PIC X(3).                                    
002000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002100     03 SUBHDR-RAD5-DEL-TERMS-TERMS                                       
002200                             PIC X(35).                                   
002300*                                 LEVERANSVILLKOR                         
002400     03 SUBHDR-RAD6-EQUAL-TO-KDVALISO                                     
002500                             PIC X(3).                                    
002600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002700     03 SUBHDR-RAD6-RATE-PRKURS                                           
002800                             PIC Z(5)9.9(5).                              
002900*                                 VALUTAKURS                              
003000     03 SUBHDR-RAD6-TOTAL-VALUE-PRKURS                                    
003100                             PIC -Z(8)9.9(2).                             
003200*                                 SUMMABELOPP                             
003300     03 SUBHDR-RAD7-AFFIRMATION-TEXT                                      
003400                             PIC X(60).                                   
003500*** END OF VILMAII-COPY LENGTH= 181 BYTES                                 
