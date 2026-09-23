000100 01  3440-WL013440.                                                       
000200*                                 REQUEST TO PGM WL013440                 
000300     03 3440-IDDC            PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 3440-IDPRODNR-KEY    PIC 9(7).                                    
000600*                                 PRODUKTIONSNUMMER                       
000700     03 3440-IDPLKLST-KEY    PIC 9(3).                                    
000800*                                 PLOCKLISTNUMMER                         
000900     03 3440-TIAAMMDD        PIC 9(6).                                    
001000*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001100     03 3440-TIHHMMSS        PIC 9(6).                                    
001200*                                 TIM - MIN - SEK   (HHMMSS)              
001300     03 3440-IDTRANS         PIC X(4).                                    
001400*                                 BILDNUMMER                              
001500     03 3440-ORDDEL          OCCURS 99 TIMES.                             
001600*                                 GRUPP MED ORDERDELAR FÖR UTSKRI         
001700*                                 FT                                      
001800        05 3440-ADFLGEO      PIC X(3).                                    
001900*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
002000        05 3440-ADFLOMR      PIC 9(3).                                    
002100*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
002200        05 3440-ADRUTNIV     PIC 9(3).                                    
002300*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
002400        05 3440-IDTRPTNR     PIC 9(3).                                    
002500*                                 TRANSPORTIDENTITET                      
002600        05 3440-VKORDNTO     PIC 9(6)V9(1).                               
002700*                                 ORDERVIKT NETTO (KG)                    
002800        05 3440-DATRPAVT.                                                 
002900*                                 TRANSPORTAVGÅNGSTIDPUNKT                
003000           07 3440-DATRPAVD  PIC 9(8).                                    
003100*                                 TRANSPORTAVGÅNGSDATUM                   
003200           07 3440-TIHHMM    PIC S9(5)           COMP-3.                  
003300*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
003400        05 3440-DARFSDAT     PIC 9(8).                                    
003500*                                 KLART FÖR TRANSPORT ÅÅÅÅMMDD            
003600        05 3440-DARFS        PIC 9(12).                                   
003700*                                 KLART FÖR TRANSPORT                     
003800        05 3440-IDPRODNR     PIC 9(7).                                    
003900*                                 PRODUKTIONSNUMMER                       
004000        05 3440-IDDC-CROSS   PIC X(2).                                    
004100*                                 DC FÖR CROSS DOCKING                    
004200*** END OF VILMAII-COPY LENGTH= 5869 BYTES                                
