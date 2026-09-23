000100 01  LEVP-W215LEVP.                                                       
000200*                                 LÄNKAREA TILL W215LEVP - ANNUL-         
000300*                                 ERING AV GÄLLANDE AVROP, SATS           
000400     03 LEVP-IDSYSTEM        PIC X(4).                                    
000500*                                 VOLVO VCCS SYSTEMNUMMER                 
000600     03 LEVP-IDARTNR         PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 LEVP-IDLEVNR         PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000     03 LEVP-IDORDNSB        PIC S9(5)           COMP-3.                  
001100*                                 SATSORDERNUMMER-BAS                     
001200     03 LEVP-KDCLAGER        PIC S9              COMP-3.                  
001300      88 LEVP-KDCLAGER-C1    VALUE +1.                                    
001400      88 LEVP-KDCLAGER-C2    VALUE +2.                                    
001500*                                 CENTRALLAGERKOD                         
001600     03 LEVP-KVBEART         PIC S9(7)           COMP-3.                  
001700*                                 BESTÄLLT ANTAL STYCKEN                  
001800     03 LEVP-KVANNANT        PIC S9(7)           COMP-3.                  
001900*                                 ANNULLERAT ANTAL ARTIKLAR               
002000     03 LEVP-TIBEGPAC        PIC S9(7)           COMP-3.                  
002100*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
002200     03 LEVP-TIREGDAT        PIC S9(7)           COMP-3.                  
002300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002400     03 LEVP-KDSVAR          PIC X.                                       
002500*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
002600*** END OF VILMAII-COPY LENGTH= 35 BYTES                                  
