000100 01  REQU-W40538I1.                                                       
000200*                                 COPYTEXT FÖR REQU W40538I1              
000300*                                                                         
000400*                                                                         
000500     03 REQU-IDDISTR-KEY     PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 REQU-IDFAKT-KEY      PIC X(7).                                    
000800*                                 FAKTURANUMMER                           
000900     03 REQU-IDDC-KEY        PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 REQU-IDFAKT-START    PIC 9(7).                                    
001200*                                 FAKTURANUMMER                           
001300     03 REQU-IDORDNR5-START  PIC 9(5).                                    
001400*                                 ORDERNUMMER                             
001500     03 REQU-IDKOLLI-START   PIC 9(5).                                    
001600*                                 KOLLINUMMER                             
001700     03 REQU-IDPRODNR-START  PIC 9(7).                                    
001800*                                 PRODUKTIONSNUMMER                       
001900     03 REQU-IDDISTR-START   PIC X(4).                                    
002000*                                 DISTRIKTNUMMER                          
002100     03 REQU-IDDC-START      PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 REQU-KDTRPTYP-UT     PIC 9.                                       
002400*                                 TRANSPORTMEDEL FÖR GODS                 
002500     03 REQU-FLCONTAIN-UT    PIC X.                                       
002600*                                 CONTAINER FULL ELLER INTE J/N           
002700     03 REQU-IDLBBET-UT      PIC X(12).                                   
002800*                                 LASTBÄRARBETECKNING                     
002900     03 REQU-IDBOKN-UT       PIC X(15).                                   
003000*                                 BOKNINGSNUMMER                          
003100     03 REQU-INPUT.                                                       
003200        05 REQU-FLSLUT       PIC X.                                       
003300*                                 AVSLUTNINGSFLAGGA                       
003400        05 REQU-FLBORT       PIC X.                                       
003500*                                 ALLMÄN FLAGGA                           
003600        05 REQU-KDTRPTYP     PIC X.                                       
003700*                                 TRANSPORTMEDEL FÖR GODS                 
003800        05 REQU-FLCONTAIN    PIC X.                                       
003900*                                 CONTAINER FULL ELLER INTE J/N           
004000        05 REQU-IDLBBET      PIC X(12).                                   
004100*                                 LASTBÄRARBETECKNING                     
004200        05 REQU-IDBOKN       PIC X(15).                                   
004300*                                 BOKNINGSNUMMER                          
004400        05 REQU-IDFORDREG    PIC X(30).                                   
004500*                                 FORDON REG. NUMMER                      
004600     03 REQU-KVRADER         PIC 9(5).                                    
004700*                                 ANTAL RADER                             
004800*** END OF VILMAII-COPY LENGTH= 138 BYTES                                 
