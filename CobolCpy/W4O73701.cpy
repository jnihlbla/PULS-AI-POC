000100 01  MOD-W4O73701.                                                        
000200*                                 MODCOPYTEXT TILL W40737.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MOD-IDRAPPNR-IN      PIC X(7).                                    
001200*                                 RAPPORT NUMMER                          
001300     03 MOD-IDARTNR-IN       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MOD-FLTOT-IN         PIC X.                                       
001600     03 MOD-IDDISTR-UT       PIC X(4).                                    
001700*                                 DISTRIKTNUMMER                          
001800     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001900*                                 KUNDNUMMER                              
002000     03 MOD-IDRAPPNR-UT      PIC X(7).                                    
002100*                                 RAPPORT NUMMER                          
002200     03 MOD-IDARTNR-UT       PIC X(9).                                    
002300*                                 ARTIKELNUMMER                           
002400     03 MOD-FLTOT-UT         PIC X.                                       
002500     03 MOD-IDANSV           PIC X(6).                                    
002600     03 MOD-ADINLOMR         PIC X(4).                                    
002700*                                 INLEVERANSOMRÅDE                        
002800     03 MOD-IDANSTNR         PIC Z(4)9.                                   
002900*                                 ANSTÄLLNINGSNUMMER                      
003000     03 MOD-TIRETILL         PIC 9(6).                                    
003100*                                 RETURTILLSTÅNDSDATUM                    
003200     03 MOD-TILOSSN          PIC 9(6).                                    
003300*                                 LOSSNINGSDATUM                          
003400     03 MOD-KVRADER          PIC Z(4)9.                                   
003500*                                 ANTAL RADER                             
003600     03 MOD-KVRADER-OBEH     PIC Z(4)9.                                   
003700*                                 ANTAL RADER                             
003800     03 MOD-KVKOLLI          PIC Z(3)9.                                   
003900*                                 ANTAL KOLLI                             
004000     03 MOD-RADER            OCCURS 9 TIMES.                              
004100*                                                                         
004200        05 MOD-IDARTNR       PIC Z(7)9.                                   
004300*                                 ARTIKELNUMMER                           
004400        05 MOD-BEART         PIC X(15).                                   
004500        05 MOD-KVANTAL-KVAR  PIC Z(5).                                    
004600*                                 DATAELEMENT                             
004700        05 MOD-KDANMORS      PIC X(2).                                    
004800*                                 ORSAK TILL LEVERANSANMÄRKNING           
004900        05 MOD-ADLAGOMR      PIC Z9.                                      
005000*                                 LAGEROMRÅDE                             
005100        05 MOD-ADGANG        PIC X(2).                                    
005200*                                 GÅNG                                    
005300        05 MOD-ADPLATS       PIC Z(4)9.                                   
005400*                                 LAGERPLATSNUMMER                        
005500        05 MOD-IDRADNR       PIC Z(3)9.                                   
005600*                                 RADNUMMER                               
005700        05 MOD-IDILIST       PIC 9(5).                                    
005800*                                 INLÄGGNINGSLISTEIDENTITET               
005900        05 MOD-FLTEXT        PIC X.                                       
006000*                                 ALLMÄN FLAGGA                           
006100        05 MOD-FLKONTROLL    PIC X.                                       
006200*                                 ALLMÄN FLAGGA                           
006300     03 MOD-INPUT.                                                        
006400*                                                                         
006500        05 MOD-FLILI-ATTR    PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700        05 MOD-FLILI         PIC X.                                       
006800*                                 ALLMÄN FLAGGA                           
006900        05 MOD-IDANSTNR-UPD-ATTR                                          
007000                             PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200        05 MOD-IDANSTNR-UPD  PIC X(5).                                    
007300*                                 ANSTÄLLNINGSNUMMER                      
007400        05 MOD-IDPRT-ATTR    PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600        05 MOD-IDPRT         PIC X(3).                                    
007700*                                 LOGISK PRINTERIDENTITET                 
007800        05 MOD-INPUTLINE     OCCURS 9 TIMES.                              
007900*                                                                         
008000           07 MOD-KDCMDVAL-ATTR                                           
008100                             PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300           07 MOD-KDCMDVAL   PIC X(3).                                    
008400*                                 GENERELL KOMMANDOKOD                    
008500           07 MOD-KVANTAL-ATTR                                            
008600                             PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800           07 MOD-KVANTAL    PIC X(6).                                    
008900*                                 DATAELEMENT                             
009000        05 MOD-FLKLAR-ATTR   PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200        05 MOD-FLKLAR        PIC X.                                       
009300*                                 AVSLUTNINGSMARKERING                    
009400        05 MOD-FLSKROT-ATTR  PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600        05 MOD-FLSKROT       PIC X.                                       
009700*                                 SKROTNINGSMARKERING                     
009800        05 MOD-FLANTAVV-ATTR PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000        05 MOD-FLANTAVV      PIC X.                                       
010100*                                 ANTALSAVVIKELSEFLAGGAN                  
010200*                                                                         
010300     03 MOD-IDILIST-NY       PIC 9(5).                                    
010400*                                 INLÄGGNINGSLISTEIDENTITET               
010500     03 MOD-TEMFSINF         PIC X(55).                                   
010600*                                 INFORMATIONSMEDDELANDE                  
010700*** END OF VILMAII-COPY LENGTH= 790 BYTES                                 
