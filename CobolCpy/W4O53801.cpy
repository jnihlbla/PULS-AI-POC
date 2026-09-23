000100 01  MOD-W4O53801.                                                        
000200*                                 COPYTEXT FÖR MOD W4O53801               
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDFAKT-IN        PIC X(7).                                    
001100*                                 FAKTURANUMMER                           
001200     03 MOD-IDDISTR-UT       PIC X(4).                                    
001300*                                 DISTRIKTNUMMER                          
001400     03 MOD-IDFAKT-UT        PIC X(7).                                    
001500*                                 FAKTURANUMMER                           
001600     03 MOD-IDKUNDNR-ENTER   PIC 9(6).                                    
001700*                                 KUNDNUMMER                              
001800     03 MOD-KDFRAKT-ENTER    PIC 9(2).                                    
001900*                                 FRAKTSÄTT DC TILL KUND                  
002000     03 MOD-IDSKEPPN-ENTER   PIC 9(7).                                    
002100*                                 SKEPPNINGSNUMMER                        
002200     03 MOD-IDFAKT-ENTER     PIC 9(7).                                    
002300*                                 FAKTURANUMMER                           
002400     03 MOD-IDORDNR5-ENTER   PIC 9(5).                                    
002500*                                 ORDERNUMMER                             
002600     03 MOD-IDKOLLI-ENTER    PIC 9(5).                                    
002700*                                 KOLLINUMMER                             
002800     03 MOD-IDPRODNR-ENTER   PIC 9(7).                                    
002900*                                 PRODUKTIONSNUMMER                       
003000     03 MOD-IDKUNDNR-NEXT    PIC 9(6).                                    
003100*                                 KUNDNUMMER                              
003200     03 MOD-KDFRAKT-NEXT     PIC 9(2).                                    
003300*                                 FRAKTSÄTT DC TILL KUND                  
003400     03 MOD-IDSKEPPN-NEXT    PIC 9(7).                                    
003500*                                 SKEPPNINGSNUMMER                        
003600     03 MOD-IDFAKT-NEXT      PIC 9(7).                                    
003700*                                 FAKTURANUMMER                           
003800     03 MOD-IDORDNR5-NEXT    PIC 9(5).                                    
003900*                                 ORDERNUMMER                             
004000     03 MOD-IDKOLLI-NEXT     PIC 9(5).                                    
004100*                                 KOLLINUMMER                             
004200     03 MOD-IDPRODNR-NEXT    PIC 9(7).                                    
004300*                                 PRODUKTIONSNUMMER                       
004400     03 MOD-FLSLUT-ATTR      PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-FLSLUT           PIC X.                                       
004700*                                 AVSLUTNINGSFLAGGA                       
004800     03 MOD-FLBORT-ATTR      PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-FLBORT           PIC X.                                       
005100*                                 ALLMÄN FLAGGA                           
005200     03 MOD-KDTRPTYP-ATTR    PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400     03 MOD-KDTRPTYP         PIC 9.                                       
005500*                                 TRANSPORTMEDEL FÖR GODS                 
005600     03 MOD-FLCONTAIN-ATTR   PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800     03 MOD-FLCONTAIN        PIC X.                                       
005900*                                 CONTAINER FULL ELLER INTE J/N           
006000     03 MOD-IDLBBET-ATTR     PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200     03 MOD-IDLBBET          PIC X(12).                                   
006300*                                 LASTBÄRARBETECKNING                     
006400     03 MOD-IDBOKN-ATTR      PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600     03 MOD-IDBOKN           PIC X(15).                                   
006700*                                 BOKNINGSNUMMER                          
006800     03 MOD-IDFORDREG-ATTR   PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000     03 MOD-IDFORDREG        PIC X(30).                                   
007100*                                 FORDON REG. NUMMER                      
007200     03 MOD-KDTRPTYP-UT      PIC 9.                                       
007300*                                 TRANSPORTMEDEL FÖR GODS                 
007400     03 MOD-FLCONTAIN-UT     PIC X.                                       
007500*                                 CONTAINER FULL ELLER INTE J/N           
007600     03 MOD-IDLBBET-UT       PIC X(12).                                   
007700*                                 LASTBÄRARBETECKNING                     
007800     03 MOD-IDBOKN-UT        PIC X(15).                                   
007900*                                 BOKNINGSNUMMER                          
008000     03 MOD-RADER            OCCURS 9 TIMES.                              
008100*                                                                         
008200        05 MOD-IDFAKT        PIC X(9).                                    
008300*                                 FAKTURAIDENTITET  IDFAKT-KDFAKT         
008400        05 MOD-TIFAKT        PIC 9(6).                                    
008500*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
008600        05 MOD-IDDISTR       PIC Z(3)9.                                   
008700*                                 DISTRIKTNUMMER                          
008800        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
008900*                                 KUNDNUMMER                              
009000        05 MOD-IDORDNR5      PIC Z(4)9.                                   
009100*                                 ORDERNUMMER                             
009200        05 MOD-IDKOLLI       PIC Z(4)9.                                   
009300*                                 KOLLINUMMER                             
009400        05 MOD-KDORDKL       PIC 9.                                       
009500*                                 ORDERKLASS                              
009600        05 MOD-FLORDSPE      PIC X.                                       
009700*                                 SPECIALORDERFLAGGA                      
009800        05 MOD-IDPRODNR      PIC Z(6)9.                                   
009900*                                 PRODUKTIONSNUMMER                       
010000        05 MOD-TIREGDAT      PIC 9(6).                                    
010100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
010200        05 MOD-TIREGTID      PIC X(5).                                    
010300*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
010400        05 MOD-FLKLAR        PIC X.                                       
010500*                                 AVSLUTNINGSMARKERING                    
010600     03 MOD-TEMFSINF         PIC X(55).                                   
010700*                                 INFORMATIONSMEDDELANDE                  
010800*** END OF VILMAII-COPY LENGTH= 807 BYTES                                 
