000100 01  DP-FOOT-WF2101.                                                      
000200*                                 DOCUMENT DATA FOOTER                    
000300     03 DP-FOOT-IDAFPRCD     PIC X(10).                                   
000400*                                 AFP-BLANKETT POSTTYP                    
000500*                                 AFP FORMS RECORD TYPE                   
000600     03 DP-FOOT-SUNTO-TOT    PIC Z(10)9.9(2).                             
000700*                                 TOTAL SALES AMOUNT EXCL. VAT            
000800     03 DP-FOOT-SUVAT-BILLIT-TOT                                          
000900                             PIC Z(10)9.9(2).                             
001000*                                 SUMMERAT MOMSVÄRDE                      
001100*                                 TOTAL VAT VALUE                         
001200     03 DP-FOOT-SUBTO-TOT    PIC Z(10)9.9(2).                             
001300*                                 TOTAL SALES AMOUNT INCL. VAT            
001400     03 DP-FOOT-SUNTO-TOT-LOC                                             
001500                             PIC Z(10)9.9(2).                             
001600*                                 TOTAL SALES AMOUNT EXCL. VAT            
001700     03 DP-FOOT-SUVAT-BILLIT-TOT-LOC                                      
001800                             PIC Z(10)9.9(2).                             
001900*                                 SUMMERAT MOMSVÄRDE                      
002000*                                 TOTAL VAT VALUE                         
002100     03 DP-FOOT-SUBTO-TOT-LOC                                             
002200                             PIC Z(10)9.9(2).                             
002300*                                 TOTAL SALES AMOUNT INCL. VAT            
002400     03 DP-FOOT-KDVALISO     PIC X(3).                                    
002500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002600*                                 CURRENCY CODE BY ISO-STANDARD.          
002700     03 DP-FOOT-KDVALISO-LOC PIC X(3).                                    
002800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002900*                                 CURRENCY CODE BY ISO-STANDARD.          
003000     03 DP-FOOT-BETEXT-1     PIC X(50).                                   
003100     03 DP-FOOT-BETEXT-2     PIC X(50).                                   
003200     03 DP-FOOT-BETEXT-3     PIC X(50).                                   
003300     03 DP-FOOT-BETEXT-4     PIC X(50).                                   
003400     03 DP-FOOT-SUNTO-TOT-SND                                             
003500                             PIC Z(10)9.9(2).                             
003600*                                 TOTAL SALES AMOUNT EXCL. VAT            
003700     03 DP-FOOT-SUVAT-BILLIT-TOT-SND                                      
003800                             PIC Z(10)9.9(2).                             
003900*                                 SUMMERAT MOMSVÄRDE                      
004000*                                 TOTAL VAT VALUE                         
004100     03 DP-FOOT-SUBTO-TOT-SND                                             
004200                             PIC Z(10)9.9(2).                             
004300*                                 TOTAL SALES AMOUNT INCL. VAT            
004400     03 DP-FOOT-KDVALISO-SND PIC X(3).                                    
004500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004600*                                 CURRENCY CODE BY ISO-STANDARD.          
004700     03 DP-FOOT-PRKURS-SND   PIC Z(5)9.9(5).                              
004800*                                 VALUTAKURS                              
004900*                                 CURRENCY EXCHANGE RATE                  
005000     03 DP-FOOT-KDVALISO-TXT PIC X(3).                                    
005100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005200*                                 CURRENCY CODE BY ISO-STANDARD.          
005300     03 DP-FOOT-REF-MINUS    PIC X.                                       
005400*** END OF VILMAII-COPY LENGTH= 361 BYTES                                 
