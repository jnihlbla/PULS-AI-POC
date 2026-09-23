000100 01  6010-W6GX6010.                                                       
000200*                                 BESKRIVNING AV                          
000300*                                 PARTI PÅ LASTBÄRARE                     
000400*                                 FYSISK NYCKEL                           
000500*                                 W6GXKEY =                               
000600*                                 (IDLEVNR,  IDFS,                        
000700*                                  TIAVIDAT, IDARTNR )                    
000800*                                  KDSORT1           )                    
000900     03 6010-IDLEVNR         PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001200     03 6010-IDFS            PIC X(8).                                    
001300*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001400*                                 ADVICE NOTE NUMBER ODETTE               
001500     03 6010-TIAVIDAT        PIC S9(7)           COMP-3.                  
001600*                                 AVISERINGSDATUM (YYMMDD)                
001700*                                 ADVICE NOTE DATE                        
001800     03 6010-IDARTNR         PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000*                                 PART NUMBER                             
002100     03 6010-KDSORT1         PIC S9              COMP-3.                  
002200*                                 SORTERINGSKOD                           
002300*                                 CODE FOR SORTING                        
002400     03 6010-ADINLOMR        PIC X(4).                                    
002500*                                 INLEVERANSOMRÅDE                        
002600*                                 RECEIVING AREA                          
002700     03 6010-FLKLAR          PIC X.                                       
002800*                                 AVSLUTNINGSMARKERING                    
002900*                                 FINISHED FLAG                           
003000     03 6010-KVAVIS          PIC S9(7)           COMP-3.                  
003100*                                 AVISERAT ANTAL                          
003200*                                 QUANTITY NOTIFIED                       
003300     03 6010-KVAVIS-PRIO     PIC S9(7)           COMP-3.                  
003400*                                 BERÄKN PRIORITERAD KVANT TOT            
003500*                                 CALC PRIO QUANTITY TOT                  
003600     03 6010-KVAVIS-KIT      PIC S9(7)           COMP-3.                  
003700*                                 AVISERAT ANTAL FÖR SATS                 
003800*                                 QUANTITY NOTIFEID FOR KIT               
003900     03 6010-VLARTNTO        PIC S9(8)V9(1)      COMP-3.                  
004000*                                 ARTIKELVOLYM NETTO (CM3)                
004100*                                 PART NET VOLUME    (CM3)                
004200     03 FILLER               PIC X(7).                                    
004300*** END OF VILMAII-COPY LENGTH= 52 BYTES                                  
