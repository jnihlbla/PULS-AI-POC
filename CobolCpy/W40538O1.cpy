000100 01  RESP-W40538O1.                                                       
000200*                                 COPYTEXT FÖR RESP W40538O1              
000300*                                                                         
000400*                                                                         
000500     03 RESP-IDFAKT-START    PIC 9(7).                                    
000600*                                 FAKTURANUMMER                           
000700     03 RESP-IDORDNR5-START  PIC 9(5).                                    
000800*                                 ORDERNUMMER                             
000900     03 RESP-IDKOLLI-START   PIC 9(5).                                    
001000*                                 KOLLINUMMER                             
001100     03 RESP-IDPRODNR-START  PIC 9(7).                                    
001200*                                 PRODUKTIONSNUMMER                       
001300     03 RESP-IDDISTR-START   PIC X(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500     03 RESP-IDDC-START      PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 RESP-IDFAKT-NEXT     PIC 9(7).                                    
001800*                                 FAKTURANUMMER                           
001900     03 RESP-IDORDNR5-NEXT   PIC 9(5).                                    
002000*                                 ORDERNUMMER                             
002100     03 RESP-IDKOLLI-NEXT    PIC 9(5).                                    
002200*                                 KOLLINUMMER                             
002300     03 RESP-IDPRODNR-NEXT   PIC 9(7).                                    
002400*                                 PRODUKTIONSNUMMER                       
002500     03 RESP-IDDISTR-NEXT    PIC X(4).                                    
002600*                                 DISTRIKTNUMMER                          
002700     03 RESP-IDDC-NEXT       PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900     03 RESP-FLSLUT-ATTR     PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 RESP-FLSLUT          PIC X.                                       
003200*                                 AVSLUTNINGSFLAGGA                       
003300     03 RESP-FLBORT-ATTR     PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 RESP-FLBORT          PIC X.                                       
003600*                                 ALLMÄN FLAGGA                           
003700     03 RESP-KDTRPTYP-ATTR   PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 RESP-KDTRPTYP        PIC 9.                                       
004000*                                 TRANSPORTMEDEL FÖR GODS                 
004100     03 RESP-FLCONTAIN-ATTR  PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 RESP-FLCONTAIN       PIC X.                                       
004400*                                 CONTAINER FULL ELLER INTE J/N           
004500     03 RESP-IDLBBET-ATTR    PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 RESP-IDLBBET         PIC X(12).                                   
004800*                                 LASTBÄRARBETECKNING                     
004900     03 RESP-IDBOKN-ATTR     PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 RESP-IDBOKN          PIC X(15).                                   
005200*                                 BOKNINGSNUMMER                          
005300     03 RESP-IDFORDREG-ATTR  PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 RESP-IDFORDREG       PIC X(30).                                   
005600*                                 FORDON REG. NUMMER                      
005700     03 RESP-KDTRPTYP-UT     PIC 9.                                       
005800*                                 TRANSPORTMEDEL FÖR GODS                 
005900     03 RESP-FLCONTAIN-UT    PIC X.                                       
006000*                                 CONTAINER FULL ELLER INTE J/N           
006100     03 RESP-IDLBBET-UT      PIC X(12).                                   
006200*                                 LASTBÄRARBETECKNING                     
006300     03 RESP-IDBOKN-UT       PIC X(15).                                   
006400*                                 BOKNINGSNUMMER                          
006500     03 RESP-KVRADER         PIC 9(5).                                    
006600*                                 ANTAL RADER                             
006700     03 RESP-RADER           OCCURS 500 TIMES.                            
006800*                                                                         
006900        05 RESP-IDFAKT       PIC X(9).                                    
007000*                                 FAKTURAIDENTITET  IDFAKT-KDFAKT         
007100        05 RESP-TIFAKT       PIC 9(6).                                    
007200*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
007300        05 RESP-IDDISTR      PIC Z(3)9.                                   
007400*                                 DISTRIKTNUMMER                          
007500        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
007600*                                 KUNDNUMMER                              
007700        05 RESP-IDORDNR5     PIC Z(4)9.                                   
007800*                                 ORDERNUMMER                             
007900        05 RESP-IDKOLLI      PIC Z(4)9.                                   
008000*                                 KOLLINUMMER                             
008100        05 RESP-KDORDKL      PIC 9.                                       
008200*                                 ORDERKLASS                              
008300        05 RESP-FLORDSPE     PIC X.                                       
008400*                                 SPECIALORDERFLAGGA                      
008500        05 RESP-IDPRODNR     PIC Z(6)9.                                   
008600*                                 PRODUKTIONSNUMMER                       
008700        05 RESP-TIREGDAT     PIC 9(6).                                    
008800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
008900        05 RESP-TIREGTID     PIC X(5).                                    
009000*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
009100        05 RESP-FLKLAR       PIC X.                                       
009200*                                 AVSLUTNINGSMARKERING                    
009300*** END OF VILMAII-COPY LENGTH= 28169 BYTES                               
