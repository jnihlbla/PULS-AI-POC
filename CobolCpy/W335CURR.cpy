000100 01  CURR-W335CURR.                                                       
000200*                                 LÄNKAREA TILL W335CURR -                
000300*                                 OMRÄKNING AV VALUTA LOCAL CURRE         
000400*                                 NCY                                     
000500     03 CURR-INDATA.                                                      
000600        05 CURR-PRKURS       PIC S9(6)V9(5)      COMP-3.                  
000700*                                 VALUTAKURS                              
000800        05 CURR-SUORDV-IN    PIC S9(9)V9(2)      COMP-3.                  
000900*                                 SUMMA ORDERVÄRDE                        
001000        05 CURR-PRARTSTD-IN  PIC S9(7)V9(2)      COMP-3.                  
001100*                                 ARTIKELSTANDARDPRIS                     
001200        05 CURR-PRARTSJK-IN  PIC S9(7)V9(2)      COMP-3.                  
001300*                                 ARTIKELNS SJÄLVKOSTNAD                  
001400        05 CURR-PRARTVNA-IN  PIC S9(7)V9(2)      COMP-3.                  
001500*                                 SJÄLVKOST ELLER BESTPRIS                
001600        05 CURR-KDVALISO-01  PIC X(3).                                    
001700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001800        05 CURR-KDVALISO-02  PIC X(3).                                    
001900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002000        05 CURR-PRKURS-02    PIC S9(6)V9(5)      COMP-3.                  
002100*                                 VALUTAKURS                              
002200        05 CURR-KDCALL       PIC S9(3)           COMP-3.                  
002300*                                 ANROPSTYP                               
002400     03 CURR-UTDATA.                                                      
002500        05 CURR-SUORDV-UT    PIC S9(9)V9(2)      COMP-3.                  
002600*                                 SUMMA ORDERVÄRDE                        
002700        05 CURR-PRARTSTD-UT  PIC S9(7)V9(2)      COMP-3.                  
002800*                                 ARTIKELSTANDARDPRIS                     
002900        05 CURR-PRARTSJK-UT  PIC S9(7)V9(2)      COMP-3.                  
003000*                                 ARTIKELNS SJÄLVKOSTNAD                  
003100        05 CURR-PRARTVNA-UT  PIC S9(7)V9(2)      COMP-3.                  
003200*                                 SJÄLVKOST ELLER BESTPRIS                
003300        05 CURR-PRKURS-UT    PIC S9(6)V9(5)      COMP-3.                  
003400*                                 VALUTAKURS                              
003500*** END OF VILMAII-COPY LENGTH= 68 BYTES                                  
