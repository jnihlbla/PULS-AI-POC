000100* GENERATION OF COBOL HOST STRUCTURE FROM T01FCUW-TAB                     
000200  01 T01FCUW.                                                             
000300*              T01FCUW                                                    
000400   03 IDLEGSEL                          PIC X(4).                         
000500*              FAKTURERANDE FÖRETAG TEX VCCS                              
000600   03 IDPARTNR                          PIC X(9).                         
000700*              PARTNERNUMMER                                              
000800   03 KDSTATUS                          PIC S9(3) COMP-3.                 
000900*              STATUSKOD          KDSTATUS-002                            
001000   03 IDALPHA                           PIC X(10).                        
001100*              ALFANUMERISK SÖKNYCKEL                                     
001200   03 BEBET-NAME1                       PIC X(35).                        
001300*              DEL AV BETALNINGSANSVARIGS NAMN                            
001400   03 BEBET-NAME2                       PIC X(35).                        
001500*              DEL AV BETALNINGSANSVARIGS NAMN                            
001600   03 BEBET-NAME3                       PIC X(35).                        
001700*              DEL AV BETALNINGSANSVARIGS NAMN                            
001800   03 BEBET-NAME4                       PIC X(35).                        
001900*              DEL AV BETALNINGSANSVARIGS NAMN                            
002000   03 ADBET-STREET                      PIC X(35).                        
002100*              BETALARENS GATUADRESS                                      
002200   03 ADBET-BOX                         PIC X(10).                        
002300*              BOXADRESS BETALNINGSANSVARIG                               
002400   03 ADBET-CITY                        PIC X(35).                        
002500*              BETALARENS STADSADRESS                                     
002600   03 ADBET-PCODE                       PIC X(10).                        
002700*              BETALARENS STADSADRESS POSTNR                              
002800   03 IDLANDX3                          PIC X(3).                         
002900*              3-STÄLLIG LANDSBETECKNINGSKOD                              
003000   03 IDSPRAK                           PIC X(2).                         
003100*              2-STÄLLIG ISO SPRÅKKOD                                     
003200   03 IDTFN                             PIC X(20).                        
003300*              TELEFONNUMMER EXTERNT                                      
003400   03 IDTFX                             PIC X(20).                        
003500*              TELEFAXNUMMER                                              
003600   03 IDMAIL                            PIC X(60).                        
003700*              MAIL ADRESS                                                
003800   03 IDLEVNR-AP                        PIC X(10).                        
003900*              LEVERANTÖRNUMMER                                           
004000   03 IDVAT                             PIC X(17).                        
004100*              MOMSREGISTRERINGSNUMMER                                    
004200   03 KDVALISO                          PIC X(3).                         
004300*              VALUTAKOD ENLIGT ISO-STANDARD.                             
004400   03 KDTRADP                           PIC X(4).                         
004500*              TRADING PARTNER                                            
004600   03 KDBETALV                          PIC X(4).                         
004700*              BETALNINGSVILLKOR KUNDRESKONTRA                            
004800   03 KDKREDSP                          PIC X(1).                         
004900*              KREDITSPÄRR PÅ BETALARE                                    
005000   03 KDPARTTY                          PIC X(3).                         
005100*              TYP AV BETALARE                                            
005200   03 KDPARTGR                          PIC X(15).                        
005300*              GRUPP AV BETALARE                                          
005400   03 DAREGDAT                          PIC X(8).                         
005500*              REGISTRERINGSDATUM (ÅÅÅÅMMDD)                              
005600   03 DAUPPDAT                          PIC X(8).                         
005700*              UPPDATERINGSDATUM  (ÅÅÅÅMMDD)                              
005800   03 DADELDAT                          PIC X(8).                         
005900*              BORTTAGSDATUM      (ÅÅÅÅMMDD)                              
006000   03 IDUSER                            PIC X(8).                         
006100*              ANVÄNDARENS SÄKERHETS ID                                   
006200   03 FLRATE                            PIC X(1).                         
006300*              A RATE BETWEEN TWO LOCAL CURRENCIES                        
006400*                                                                         
006500*** END OF VILMAII-COPY LENGTH= 450 OLD LENGTH=                           
