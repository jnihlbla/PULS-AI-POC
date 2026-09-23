000100 01  MID-W4I73701.                                                        
000200*                                 MID-COPYTEXT FÖR W40737                 
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-IDRAPPNR-IN      PIC X(7).                                    
001200*                                 RAPPORT NUMMER                          
001300     03 MID-IDRAPPNR-UT      PIC X(7).                                    
001400*                                 RAPPORT NUMMER                          
001500     03 MID-IDARTNR-IN       PIC X(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MID-IDARTNR-UT       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MID-FLTOT-IN         PIC X.                                       
002000     03 MID-FLTOT-UT         PIC X.                                       
002100     03 MID-INPUT.                                                        
002200*                                 INMATNINGSFÄLT                          
002300        05 MID-FLILI         PIC X.                                       
002400*                                 ALLMÄN FLAGGA                           
002500        05 MID-IDANSTNR-UPD  PIC X(5).                                    
002600*                                 ANSTÄLLNINGSNUMMER                      
002700        05 MID-IDPRT         PIC X(3).                                    
002800*                                 LOGISK PRINTERIDENTITET                 
002900        05 MID-INPUTLINE     OCCURS 9 TIMES.                              
003000*                                 INMATNINGSFÄLT PÅ RADEN                 
003100           07 MID-KDCMDVAL   PIC X(3).                                    
003200*                                 GENERELL KOMMANDOKOD                    
003300           07 MID-KVANTAL    PIC X(6).                                    
003400*                                 DATAELEMENT                             
003500        05 MID-FLKLAR        PIC X.                                       
003600*                                 AVSLUTNINGSMARKERING                    
003700        05 MID-FLSKROT       PIC X.                                       
003800*                                 SKROTNINGSMARKERING                     
003900        05 MID-FLANTAVV      PIC X.                                       
004000*                                 ANTALSAVVIKELSEFLAGGAN                  
004100*                                                                         
004200     03 MID-KEYFIELD         OCCURS 9 TIMES.                              
004300*                                 NYCKELFÄLT PÅ RADEN                     
004400        05 MID-IDARTNR       PIC X(8).                                    
004500*                                 ARTIKELNUMMER                           
004600        05 MID-IDRADNR       PIC X(4).                                    
004700*                                 RADNUMMER                               
004800     03 MID-IDILIST          PIC X(5).                                    
004900*                                 INLÄGGNINGSLISTEIDENTITET               
005000     03 MID-MODFAELT-IN      PIC X(200).                                  
005100*** END OF VILMAII-COPY LENGTH= 460 BYTES                                 
