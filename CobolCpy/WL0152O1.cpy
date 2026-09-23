000100 01  RESP-WL0152O1.                                                       
000200*                                 RESPONS FROM PGM WL0152                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDDISTR-KEY     PIC Z(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
000800*                                 KUNDNUMMER                              
000900     03 RESP-IDRAPPNR-KEY    PIC Z(7).                                    
001000*                                 RAPPORT NUMMER                          
001100     03 RESP-IDARTNR-KEY     PIC Z(8).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 RESP-FLTOT-KEY       PIC X.                                       
001400     03 RESP-WL0154I1.                                                    
001500        05 RESP-IDPGM-REP    PIC X(8).                                    
001600*                                 PROGRAM IDENTITET                       
001700        05 RESP-L154-RT-POST OCCURS 10 TIMES.                             
001800           07 RESP-IDDISTR-REP                                            
001900                             PIC Z(3)9.                                   
002000*                                 DISTRIKTNUMMER                          
002100           07 RESP-IDKUNDNR-REP                                           
002200                             PIC Z(5)9.                                   
002300*                                 KUNDNUMMER                              
002400           07 RESP-IDRAPPNR-REP                                           
002500                             PIC Z(6)9.                                   
002600*                                 RAPPORT NUMMER                          
002700     03 RESP-TIRETILL        PIC 9(6).                                    
002800*                                 RETURTILLSTÅNDSDATUM                    
002900     03 RESP-TIINLMOT        PIC 9(6).                                    
003000*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
003100     03 RESP-KVRADER-TOT     PIC Z(4)9.                                   
003200*                                 ANTAL RADER                             
003300     03 RESP-KVRADER-OBEH    PIC Z(4)9.                                   
003400*                                 ANTAL RADER                             
003500     03 RESP-INPUT.                                                       
003600*                                                                         
003700        05 RESP-FLILI        PIC X.                                       
003800*                                 ALLMÄN FLAGGA                           
003900        05 RESP-IDANSTNR-UPD PIC Z(4)9.                                   
004000*                                 ANSTÄLLNINGSNUMMER                      
004100        05 RESP-FLKLAR       PIC X.                                       
004200*                                 AVSLUTNINGSMARKERING                    
004300        05 RESP-FLSKROT      PIC X.                                       
004400*                                 SKROTNINGSMARKERING                     
004500        05 RESP-FLANTAVV     PIC X.                                       
004600*                                 ANTALSAVVIKELSEFLAGGAN                  
004700*                                                                         
004800        05 RESP-FLSKRIV      PIC X.                                       
004900*                                 JA = ÅTERSTART AV BEGÄRD LISTA          
005000        05 RESP-IDILIST-NY   PIC Z(4)9.                                   
005100*                                 INLÄGGNINGSLISTEIDENTITET               
005200     03 RESP-KVRADER-MAX1    PIC 9(5).                                    
005300*                                 MAX INDEX KOPPLAT TILL OCCURS N         
005400*                                 EDAN.                                   
005500     03 RESP-RADER           OCCURS 1 TO 500 TIMES                        
005600                             DEPENDING ON RESP-KVRADER-MAX1.              
005700*                                                                         
005800        05 RESP-KDCMD        PIC X(4).                                    
005900        05 RESP-IDILIST-BEF  PIC Z(2)9.                                   
006000*                                 INLÄGGNINGSLISTEIDENTITET               
006100        05 RESP-KVANTAL      PIC Z(5)9.                                   
006200*                                 ANTAL                                   
006300        05 RESP-IDARTNR      PIC Z(7)9.                                   
006400*                                 ARTIKELNUMMER                           
006500        05 RESP-BEART        PIC X(15).                                   
006600        05 RESP-KVANTAL-KVAR PIC Z(5)9.                                   
006700*                                 ANTAL                                   
006800        05 RESP-KDANMORS     PIC X(2).                                    
006900*                                 ORSAK TILL LEVERANSANMÄRKNING           
007000        05 RESP-ADLAGOMR     PIC 9(2).                                    
007100*                                 LAGEROMRÅDE                             
007200        05 RESP-ADGANG       PIC 9(2).                                    
007300*                                 GÅNG                                    
007400        05 RESP-ADPLATS      PIC 9(5).                                    
007500*                                 LAGERPLATSNUMMER                        
007600        05 RESP-IDRADNR      PIC Z(3)9.                                   
007700*                                 RADNUMMER                               
007800        05 RESP-IDILIST      PIC Z(5).                                    
007900*                                 INLÄGGNINGSLISTEIDENTITET               
008000        05 RESP-FLTEXT       PIC X.                                       
008100*                                 ALLMÄN FLAGGA                           
008200        05 RESP-IDMSG-ERROR-LINE                                          
008300                             PIC X(3).                                    
008400*                                 FELMEDDELANDE ID                        
008500        05 RESP-TEANMNOT-REG OCCURS 3 TIMES                               
008600                             PIC X(70).                                   
008700*                                 FRI TEXT FRÅN REGISTRERINGEN            
008800        05 RESP-TEANMNOT-ADM OCCURS 3 TIMES                               
008900                             PIC X(70).                                   
009000*                                 FRI TEXT FRÅN ADMINISTRATION            
009100        05 RESP-TEKVAINF-EXT OCCURS 7 TIMES                               
009200                             PIC X(79).                                   
009300*                                 KVALITETS INFORMATION EXTERNT           
009400*** END OF VILMAII-COPY LENGTH= 519748 BYTES                              
