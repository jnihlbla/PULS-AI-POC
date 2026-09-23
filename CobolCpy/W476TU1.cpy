000100 01  TU1-W476TU1-CTX.                                                     
000200*                                 TULLSYSTEM                              
000300*                                 TULL-HUVUD-UPPGIFTER PTYP TU1           
000400     03 TU1-IDPTYP           PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 TU1-IDFAKT           PIC S9(7)           COMP-3.                  
000700*                                 FAKTURANUMMER                           
000800     03 TU1-TIFAKT           PIC S9(7)           COMP-3.                  
000900*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
001000     03 TU1-IDTULL.                                                       
001100*                                 IDENTITET TULL SÄNDNING                 
001200        05 TU1-IDTULFTG      PIC X(2).                                    
001300*                                 IDENTIFIERARE TULLANDE FÖRETAG          
001400*                                                                         
001500        05 TU1-IDTULLNR      PIC 9(7).                                    
001600*                                 NUMMERSERIE INGÅENDE I TULLID           
001700*                                                                         
001800        05 TU1-RETULKS       PIC 9.                                       
001900*                                 KONTROLLSIFFRA TULLID                   
002000     03 TU1-IDUSER           PIC X(8).                                    
002100*                                 ANVÄNDARENS SÄKERHETS ID                
002200     03 TU1-KVFAKTUR         PIC S9(3)           COMP-3.                  
002300*                                 ANTAL FAKTUROR                          
002400     03 TU1-IDDISTR          PIC S9(5)           COMP-3.                  
002500*                                 DISTRIKTNUMMER                          
002600     03 TU1-IDKUNDNR         PIC S9(7)           COMP-3.                  
002700*                                 KUNDNUMMER                              
002800     03 TU1-KDFRAKT          PIC S9(3)           COMP-3.                  
002900*                                 FRAKTSÄTT DC TILL KUND                  
003000     03 TU1-KDGRANS          PIC S9(3)           COMP-3.                  
003100*                                 GRÄNSKOD                                
003200     03 TU1-ADKOPARE-GRP.                                                 
003300*                                 KÖPARE ADRESS                           
003400        05 TU1-ADKOPARE-RAD1 PIC X(35).                                   
003500*                                 KÖPARE ADRESS RAD 1                     
003600        05 TU1-ADKOPARE-RAD2 PIC X(35).                                   
003700*                                 KÖPARE ADRESS RAD 2                     
003800     03 TU1-BEKOPARE-GRP.                                                 
003900*                                 KÖPARE NAMN                             
004000        05 TU1-BEKOPARE-RAD1 PIC X(35).                                   
004100*                                 KÖPARE NAMN RAD 1                       
004200        05 TU1-BEKOPARE-RAD2 PIC X(35).                                   
004300*                                 KÖPARE NAMN RAD 2                       
004400     03 TU1-KVKOLLI-FAKT     PIC S9(5)           COMP-3.                  
004500*                                 ANTAL FAKTURERADE KOLLIN                
004600     03 TU1-KDORDKL          PIC S9              COMP-3.                  
004700*                                 ORDERKLASS                              
004800     03 TU1-SUORDV-FAKT      PIC S9(9)V9(2)      COMP-3.                  
004900*                                 FAKTURERAT ORDERVÄRDE                   
005000     03 TU1-KDVALISO         PIC X(3).                                    
005100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005200     03 TU1-VKRAD-NTO-KG-FAKT                                             
005300                             PIC S9(6)V9(3)      COMP-3.                  
005400*                                 ARTIKELVIKT NETTO RADVÄRDE              
005500     03 TU1-IDLBBET          PIC X(12).                                   
005600*                                 LASTBÄRARBETECKNING                     
005700     03 TU1-IDBOKN           PIC X(15).                                   
005800*                                 BOKNINGSNUMMER                          
005900     03 TU1-FLSLUT           PIC X.                                       
006000*                                 AVSLUTNINGSFLAGGA                       
006100     03 TU1-PRKURS           PIC S9(6)V9(5)      COMP-3.                  
006200*                                 VALUTAKURS                              
006300     03 TU1-VKORDBTO-FAKT    PIC S9(6)V9(1)      COMP-3.                  
006400*                                 ORDERVIKT BRUTTO PER FAKTURA            
006500     03 TU1-IDPARTNR         PIC X(9).                                    
006600*                                 FINANCIELL KUND                         
006700     03 TU1-BELEVVIL         PIC X(35).                                   
006800*                                 LEVERANSVILLKOR                         
006900     03 TU1-IDDC             PIC X(2).                                    
007000*                                 IDENTIFIERARE LAGER                     
007100     03 TU1-KDTRPTYP         PIC 9.                                       
007200*                                 TRANSPORTMEDEL FÖR GODS                 
007300     03 TU1-FLCONTAIN        PIC X.                                       
007400*                                 CONTAINER FULL ELLER INTE J/N           
007500     03 TU1-PREMBHNT         PIC S9(7)V9(2)      COMP-3.                  
007600*                                 EMBALLAGE O HANTERINGSKOST              
007700     03 TU1-PRFRAKT          PIC S9(7)V9(2)      COMP-3.                  
007800*                                 FRAKTKOSTNAD                            
007900     03 TU1-PRFOERS          PIC S9(7)V9(2)      COMP-3.                  
008000*                                 FÖRSÄKRINGSPREMIE                       
008100     03 TU1-PRLEGKST         PIC S9(7)V9(2)      COMP-3.                  
008200*                                 LEGALISERINSKOSTNAD                     
008300     03 TU1-IDFORDREG        PIC X(30).                                   
008400*                                 FORDON REG. NUMMER                      
008500     03 TU1-DARFS            PIC 9(12).                                   
008600*                                 KLART FÖR TRANSPORT                     
008700     03 TU1-IDSIGILL         PIC X(15).                                   
008800*                                 SIGILL IDENTITET                        
008900*** END OF VILMAII-COPY LENGTH= 363 BYTES                                 
