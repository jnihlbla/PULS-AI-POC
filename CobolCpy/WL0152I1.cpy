000100 01  REQU-WL0152I1.                                                       
000200*                                 REQUEST TO PGM WL0152                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDFTG-KEY       PIC 9(2).                                    
000600*                                 FÖRETAGSID EKONOM REDOVISNING           
000700     03 REQU-IDDISTR-KEY     PIC 9(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 REQU-IDKUNDNR-KEY    PIC 9(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 REQU-IDRAPPNR-KEY    PIC 9(7).                                    
001200*                                 RAPPORT NUMMER                          
001300     03 REQU-IDARTNR-KEY     PIC 9(8).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 REQU-FLTOT-KEY       PIC X.                                       
001600     03 REQU-IDRT-KEY        PIC X(3).                                    
001700*                                 RETURTERMINAL                           
001800     03 REQU-INPUT.                                                       
001900*                                 INMATNINGSFÄLT                          
002000        05 REQU-FLILI        PIC X.                                       
002100*                                 ALLMÄN FLAGGA                           
002200        05 REQU-IDANSTNR-UPD PIC 9(5).                                    
002300*                                 ANSTÄLLNINGSNUMMER                      
002400        05 REQU-FLKLAR       PIC X.                                       
002500*                                 AVSLUTNINGSMARKERING                    
002600        05 REQU-FLSKROT      PIC X.                                       
002700*                                 SKROTNINGSMARKERING                     
002800        05 REQU-FLANTAVV     PIC X.                                       
002900*                                 ANTALSAVVIKELSEFLAGGAN                  
003000*                                                                         
003100        05 REQU-FLSKRIV      PIC X.                                       
003200*                                 JA = ÅTERSTART AV BEGÄRD LISTA          
003300     03 REQU-KVRADER-MAX1    PIC 9(5).                                    
003400*                                 MAX INDEX KOPPLAT TILL OCCURS N         
003500*                                 EDAN.                                   
003600     03 REQU-RAD             OCCURS 1 TO 500 TIMES                        
003700                             DEPENDING ON REQU-KVRADER-MAX1.              
003800*                                 RAD                                     
003900        05 REQU-KDCMD        PIC X(4).                                    
004000        05 REQU-IDILIST-BEF  PIC 9(3).                                    
004100*                                 INLÄGGNINGSLISTEIDENTITET               
004200        05 REQU-KVANTAL      PIC 9(6).                                    
004300*                                 ANTAL                                   
004400        05 REQU-IDARTNR      PIC 9(8).                                    
004500*                                 ARTIKELNUMMER                           
004600        05 REQU-IDRADNR      PIC 9(4).                                    
004700*                                 RADNUMMER                               
004800*** END OF VILMAII-COPY LENGTH= 12548 BYTES                               
