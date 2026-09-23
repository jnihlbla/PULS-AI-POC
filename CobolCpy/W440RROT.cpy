000100 01  RROT-W440RROT.                                                       
000200*                                 LÄNKAREA TILL W440RROT -                
000300*                                 BERÄKNAR RANSONERAD DISPONIBEL          
000400*                                 KVANT TILL RESTORDERTÄCKNING            
000500     03 RROT-AREA-IN.                                                     
000600        05 RROT-IDARTNR      PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800        05 RROT-KDLTK        PIC S9              COMP-3.                  
000900*                                 LAGERTILLHÖRIGHETSKOD                   
001000        05 RROT-AREA-CLAGER-IN                                            
001100                             OCCURS 2 TIMES.                              
001200           07 RROT-KVLS      PIC S9(7)           COMP-3.                  
001300*                                 LAGERSALDO                              
001400           07 RROT-KVPB-SATS PIC S9(6)V9(1)      COMP-3.                  
001500*                                 SATS-PERIODBEHOV                        
001600           07 RROT-KVPB-SEP  PIC S9(6)V9(1)      COMP-3.                  
001700*                                 SEPARAT PERIODBEHOV                     
001800           07 RROT-KVPB-TPO  PIC S9(6)V9(1)      COMP-3.                  
001900*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
002000           07 RROT-KVRESS    PIC S9(7)           COMP-3.                  
002100*                                 RESERVERAT ANTAL ARTIKLAR               
002200           07 RROT-KVSPANT   PIC S9(7)           COMP-3.                  
002300*                                 SPÄRRAT ANTAL                           
002400           07 RROT-KVUTRS    PIC S9(7)           COMP-3.                  
002500*                                 UTREDNINGSSALDO                         
002600           07 RROT-KVROS     PIC S9(7)           COMP-3.                  
002700*                                 RESTORDERSALDO                          
002800           07 RROT-TIDISPIN  PIC S9(7)           COMP-3.                  
002900*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
003000           07 RROT-REDIRLEV  PIC S9V9(2)         COMP-3.                  
003100*                                 DIREKTLEVERANSANDEL                     
003200           07 RROT-KDERS     PIC S9(3)           COMP-3.                  
003300*                                 ERSÄTTNINGSKOD                          
003400     03 RROT-AREA-UT.                                                     
003500        05 RROT-AREA-CLAGER-UT                                            
003600                             OCCURS 2 TIMES.                              
003700           07 RROT-KVDISP    PIC S9(7)           COMP-3.                  
003800*                                 DISPONIBELT LAGER                       
003900*** END OF VILMAII-COPY LENGTH= 94 BYTES                                  
