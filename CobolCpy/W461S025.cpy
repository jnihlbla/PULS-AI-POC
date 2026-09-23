000100 01  RKJ-W461S025-CTX.                                                    
000200*                                 SAMMANSLAGNING AV SORTDEL OCH           
000300*                                 KREDIT   POST (RKJ) TILL NOAC           
000400     03 RKJ-SOR0-IDDISTR     PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 RKJ-SOR0-IDKUNDNR    PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 RKJ-SOR0-IDRONR      PIC S9(7)           COMP-3.                  
000900*                                 RESTORDERNUMMER      IDRONR-002         
001000     03 RKJ-SOR0-TIRODAT     PIC S9(7)           COMP-3.                  
001100*                                 RESTORDERDATUM         (ÅÅMMDD)         
001200     03 RKJ-SOR0-IDPTYP      PIC X(3).                                    
001300*                                 POSTTYP                                 
001400     03 RKJ-SOR0-IDLOPNR     PIC S9(5)           COMP-3.                  
001500*                                 LÖPNUMMER          IDLOPNR-002          
001600     03 RKJ-W461RKJN-CTX.                                                 
001700*                                 KREDITTRANS-RAD                         
001800*                                 PÅMINELSE - GAMMAL RT                   
001900*                                 RECORD TYP  RKJ                         
002000        05 RKJ-IDPTYP        PIC X(3).                                    
002100*                                 POSTTYP                                 
002200        05 RKJ-IDDISTR       PIC 9(4).                                    
002300*                                 DISTRIKTNUMMER                          
002400        05 RKJ-IDKUNDNR      PIC 9(6).                                    
002500*                                 KUNDNUMMER                              
002600        05 RKJ-IDRAPPNR      PIC 9(7).                                    
002700*                                 RAPPORT NUMMER                          
002800        05 RKJ-IDRADNR       PIC 9(4).                                    
002900*                                 RADNUMMER                               
003000        05 RKJ-KDANMORS      PIC 9(2).                                    
003100*                                 ORSAK TILL LEVERAN KDANMORS-002         
003200        05 RKJ-IDARTNR       PIC 9(9).                                    
003300*                                 ARTIKELNUMMER                           
003400        05 RKJ-KVLEVANM      PIC 9(6).                                    
003500*                                 LEVERANSANMÄRKNINGSANTAL                
003600        05 RKJ-DARTPMN       PIC 9(6).                                    
003700*                                 PÅMINNELSE FÖR RETURTILLSTÅNDSD         
003800*                                 ATUM (AAAAMMDD)                         
003900        05 RKJ-KVDAGAR-RTATG PIC 9(3).                                    
004000*                                 ANTAL DAGAR ETT RETURTILLSTÅND          
004100*                                 MÅSTE ÅTGÄRDAS                          
004200*                                 EFTER PÅMINNELSE                        
004300*** END OF VILMAII-COPY LENGTH= 71 BYTES                                  
