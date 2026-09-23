000100* GENERATION OF COBOL HOST STRUCTURE FROM T01RECO-TAB                     
000200  01 T01RECO.                                                             
000300*              T01RECO                                                    
000400   03 IDLEGSEL                          PIC X(4).                         
000500*              FAKTURERANDE F÷RETAG TEX VCCS                              
000600   03 IDLANDX3                          PIC X(3).                         
000700*              3-STƒLLIG LANDSBETECKNINGSKOD                              
000800   03 KDSTATUS                          PIC S9(3) COMP-3.                 
000900*              STATUSKOD          KDSTATUS-002                            
001000   03 IDSPRAK                           PIC X(2).                         
001100*              2-STƒLLIG ISO SPR≈KKOD                                     
001200   03 BERESPRA-1                        PIC X(35).                        
001300*              DEL AV ANSV AVDELNINGS NAMN                                
001400   03 BERESPRA-2                        PIC X(35).                        
001500*              DEL AV ANSV AVDELNINGS NAMN                                
001600   03 ADRESP-STREET                     PIC X(35).                        
001700*              ANSVARIG AVDELNINGS GATUADRESS                             
001800   03 ADRESP-BOX                        PIC X(10).                        
001900*              BOX ADRESS ANSVARIG AVDELNING                              
002000   03 ADRESP-CITY                       PIC X(35).                        
002100*              ANSVARIG AVDELNINGS STAD (ELLER LIKNANDE)                  
002200   03 ADRESP-PCODE                      PIC X(10).                        
002300*              ANSVARIG AVDELNINGS POSTNUMMER                             
002400   03 IDTFN                             PIC X(20).                        
002500*              TELEFONNUMMER EXTERNT                                      
002600   03 IDTFX                             PIC X(20).                        
002700*              TELEFAXNUMMER                                              
002800   03 IDMAIL                            PIC X(60).                        
002900*              MAIL ADRESS                                                
003000   03 BECONT                            PIC X(35).                        
003100*              KONTAKTPERSON                                              
003200   03 IDVAT                             PIC X(17).                        
003300*              MOMSREGISTRERINGSNUMMER                                    
003400   03 IDBG                              PIC X(15).                        
003500*              BANKGIRO                                                   
003600   03 IDPG                              PIC X(15).                        
003700*              POSTGIRO                                                   
003800   03 DAREGDAT                          PIC X(8).                         
003900*              REGISTRERINGSDATUM (≈≈≈≈MMDD)                              
004000   03 DAUPPDAT                          PIC X(8).                         
004100*              UPPDATERINGSDATUM  (≈≈≈≈MMDD)                              
004200   03 DADELDAT                          PIC X(8).                         
004300*              BORTTAGSDATUM      (≈≈≈≈MMDD)                              
004400   03 IDUSER                            PIC X(8).                         
004500*              ANVƒNDARENS SƒKERHETS ID                                   
004600   03 SUDOCLIM                          PIC S9(11) COMP-3.                
004700*              MINIMUM VALUE FOR PPRINTOUT OF FINANCIAL DOCUMENT (        
004800   03 IDVAT-AGENT                       PIC X(17).                        
004900*              MOMSREGISTRERINGSNUMMER AGENT                              
005000*                                                                         
005100*** END OF VILMAII-COPY LENGTH= 408 OLD LENGTH=                           
