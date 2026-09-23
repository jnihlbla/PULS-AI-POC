000100 01  4742-WDGX4742.                                                       
000200*                                 FAKTURANUMMERSERIER                     
000300*                                 NYCKEL:                                 
000400*                                 KDSEGKEY = 1                            
000500     03 4742-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 4742-FAKT-SERIE      OCCURS 94 TIMES.                             
000900*                                 INDEX  FAKTURASERIE                     
001000*                                 01 VCCS  R  OCH G                       
001100*                                 02 ALLA  K  FÖR INTERNA DISTR           
001200*                                 03 VCCS  N                              
001300*                                          K  FÖR ÖVR. DISTRIKT           
001400*                                 04 ALLA  P                              
001500*                                 05 VCNA  P  PROFORMOR TILL CDC          
001600*                                                                         
001700*                                 06 NDC41 R  ALLA DISTRIKT               
001800*                                 07 NDC41 G  ALLA DISTRIKT               
001900*                                 08 NDC41 N  ALLA DISTRIKT               
002000*                                 09 NDC41 K  LEVERANTÖR                  
002100*                                 10 NDC41 K  RETUR                       
002200*                                                                         
002300*                                 11 NDC42 R  ALLA DISTRIKT               
002400*                                 12 NDC42 G  ALLA DISTRIKT               
002500*                                 13 NDC42 N  ALLA DISTRIKT               
002600*                                 14 NDC42 K  LEVERANTÖR                  
002700*                                 15 NDC42 K  RETUR                       
002800*                                                                         
002900*                                 16 NDC43 R  ALLA DISTRIKT               
003000*                                 17 NDC43 G  ALLA DISTRIKT               
003100*                                 18 NDC43 N  ALLA DISTRIKT               
003200*                                 19 NDC43 K  LEVERANTÖR                  
003300*                                 20 NDC43 K  RETUR                       
003400*                                                                         
003500*                                 21 NDC51 R  ALLA DISTRIKT               
003600*                                 22 NDC51 G  ALLA DISTRIKT               
003700*                                 23 NDC51 N  ALLA DISTRIKT               
003800*                                 24 NDC51 K  LEVERANTÖR                  
003900*                                 25 NDC51 K  RETUR                       
004000*                                                                         
004100*                                 26 SDC25 R O G  ITALIEN                 
004200*                                 27 SDC25 K      ITALIEN                 
004300*                                                                         
004400        05 4742-IDFAKT-MIN   PIC S9(7)           COMP-3.                  
004500*                                 MIN GRÄNS FAKTURANUMMER                 
004600        05 4742-IDFAKT-MAX   PIC S9(7)           COMP-3.                  
004700*                                 MAX GRÄNS FAKTURANUMMER                 
004800        05 4742-IDFAKT-AKT   PIC S9(7)           COMP-3.                  
004900*                                 AKTUELLT  FAKTURANUMMER                 
005000     03 4742-FILLER          PIC X(71).                                   
005100*** END OF VILMAII-COPY LENGTH= 1200 BYTES                                
