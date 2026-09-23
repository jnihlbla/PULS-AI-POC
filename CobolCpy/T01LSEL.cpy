000100* GENERATION OF COBOL HOST STRUCTURE FROM T01LSEL-TAB                     
000200  01 T01LSEL.                                                             
000300*              T01LSEL                                                    
000400   03 IDLEGSEL       PIC X(4).                                            
000500*              FAKTURERANDE FÖRETAG TEX VCCS                              
000600   03 KDSTATUS       PIC S9(3) COMP-3.                                    
000700*              STATUSKOD          KDSTATUS-002                            
000800   03 BELEGRAD-1     PIC X(35).                                           
000900*              DEL AV LEGAL SELLER NAMN                                   
001000   03 BELEGRAD-2     PIC X(35).                                           
001100*              DEL AV LEGAL SELLER NAMN                                   
001200   03 ADLEG-STREET   PIC X(35).                                           
001300*              LEGAL SELLER GATUADRESS                                    
001400   03 ADLEG-BOX      PIC X(10).                                           
001500*              BOXADRESS LEGAL SELLER                                     
001600   03 ADLEG-CITY     PIC X(35).                                           
001700*              LEGAL SÄLJARES ADRESS STAD                                 
001800   03 ADLEG-PCODE    PIC X(10).                                           
001900*              LEGAL SELLER ADRESS POSTNR                                 
002000   03 IDLANDX3       PIC X(3).                                            
002100*              3-STÄLLIG LANDSBETECKNINGSKOD                              
002200   03 IDTFN          PIC X(20).                                           
002300*              TELEFONNUMMER EXTERNT                                      
002400   03 IDTFX          PIC X(20).                                           
002500*              TELEFAXNUMMER                                              
002600   03 IDMAIL         PIC X(60).                                           
002700*              MAIL ADRESS                                                
002800   03 BECONT         PIC X(35).                                           
002900*              KONTAKTPERSON                                              
003000   03 IDVAT          PIC X(17).                                           
003100*              MOMSREGISTRERINGSNUMMER                                    
003200   03 IDBG           PIC X(15).                                           
003300*              BANKGIRO                                                   
003400   03 IDPG           PIC X(15).                                           
003500*              POSTGIRO                                                   
003600   03 KDAPPEND       PIC X(4).                                            
003700*              APPENDIX KOD                                               
003800   03 KDINVFRQ       PIC X(4).                                            
003900*              FAKTURERINGSFREKVENS                                       
004000   03 FLSLUT         PIC X(1).                                            
004100*              AVSLUTNINGSFLAGGA                                          
004200   03 DAREGDAT       PIC X(8).                                            
004300*              REGISTRERINGSDATUM (ÅÅÅÅMMDD)                              
004400   03 DAUPPDAT       PIC X(8).                                            
004500*              UPPDATERINGSDATUM  (ÅÅÅÅMMDD)                              
004600   03 IDUSER         PIC X(8).                                            
004700*              ANVÄNDARENS SÄKERHETS ID                                   
004800   03 FLVATUPD       PIC X(1).                                            
004900*              VAT UPPDATERING                                            
005000   03 FLCUSUPD       PIC X(1).                                            
005100*              FINANSIELL KUNDUPPDATERING                                 
005200   03 FLCURRCL       PIC X(1).                                            
005300*              VALUTAN RENSAS                                             
005400   03 FLINTODO       PIC X(1).                                            
005500*              FÖRSÄKRINGSTEXT PÅ DOKUMENT                                
005600   03 KDVALISO       PIC X(3).                                            
005700*              VALUTAKOD ENLIGT ISO-STANDARD.                             
005800   03 KDTRADP        PIC X(4).                                            
005900*              TRADING PARTNER                                            
006000*                                                                         
006100*** END OF VILMAII-COPY LENGTH= 395 OLD LENGTH=                           
