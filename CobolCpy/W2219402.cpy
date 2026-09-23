000100 01  W2219402.                                                            
000200*                                 ALERT TO SCREEN 2171/2471               
000300*                                                                         
000400     03 IDANSK               PIC S9(3)           COMP-3.                  
000500*                                 ANSKAFFARNUMMER                         
000600     03 KDLARM               PIC S9(3)           COMP-3.                  
000700*                                 LARMORSAKSKOD                           
000800     03 IDARTNR              PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 IDDC                 PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 IDLEVNR              PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400     03 TILEVBSK-DISP-LAST   PIC X(4).                                    
001500     03 KVDISP               PIC S9(7)           COMP-3.                  
001600*                                 DISPONIBELT LAGER                       
001700     03 KVAVIS               PIC S9(7)           COMP-3.                  
001800*                                 AVISERAT ANTAL                          
001900     03 KVAVROP-OLD          PIC S9(7)           COMP-3.                  
002000*                                 AVROPSKVANTITET                         
002100     03 FLLARM-TOT           PIC X.                                       
002200*                                 LARMRAPPORT UTFÄRDAD                    
002300     03 INDATA               OCCURS 30 TIMES.                             
002400        05 TIAAVV            PIC S9(5)           COMP-3.                  
002500*                                 ÅR - VECKA  (ÅÅVV)                      
002600        05 KVBEHOV           PIC S9(7)V9(2)      COMP-3.                  
002700*                                 BEHOVSSTORLEK                           
002800        05 KVAVROP           PIC S9(7)           COMP-3.                  
002900*                                 AVROPSKVANTITET                         
003000        05 SUDISPV           PIC S9(9)V9(2)      COMP-3.                  
003100*                                 DISPONIBELT LAGERVÄRDE                  
003200        05 FLLARM            PIC X.                                       
003300*                                 LARMRAPPORT UTFÄRDAD                    
003400*** END OF VILMAII-COPY LENGTH= 603 BYTES                                 
