000100* GENERATION OF COBOL HOST STRUCTURE FROM T01SDEV-TAB                     
000200  01 T01SDEV.                                                             
000300*              T01SDEV                                                    
000400   03 IDLEGSEL          PIC X(4).                                         
000500*              FAKTURERANDE F÷RETAG TEX VCCS                              
000600   03 DAREGDAT          PIC X(8).                                         
000700*              REGISTRERINGSDATUM (≈≈≈≈MMDD)                              
000800   03 IDLOPNR           PIC S9(5) COMP-3.                                 
000900*              L÷PNUMMER          IDLOPNR-002                             
001000   03 BETEXT            PIC X(20).                                        
001100   03 IDFINDOC          PIC S9(9) COMP-3.                                 
001200*              FINANSIELLT DOKUMENT ID                                    
001300   03 IDARTNR-FINANCE   PIC X(50).                                        
001400*              ARTIKELNUMMER F÷R FINANSIELL BRUK                          
001500   03 BEART             PIC X(25).                                        
001600*              ARTIKELBENƒMNING                                           
001700   03 IDPARTNR          PIC X(9).                                         
001800*              PARTNERNUMMER                                              
001900   03 IDEXCUST-1        PIC X(15).                                        
002000*              EXTERNT KUNDID                                             
002100   03 IDEXCUST-2        PIC X(15).                                        
002200*              EXTERNT KUNDID                                             
002300   03 IDREF             PIC X(15).                                        
002400*              REFERENS ID                                                
002500   03 KDFINDOC          PIC X(4).                                         
002600*              TYP FINANSIELLT DOKUMENT                                   
002700   03 FLSOFT            PIC X(1).                                         
002800*              FLAGGA SOFTVARA                                            
002900   03 FLFREE            PIC X(1).                                         
003000*              GRATISFATURA                                               
003100   03 REARTRAB          PIC S9(2)V9(2) COMP-3.                            
003200*              ARTIKELRABATT                                              
003300   03 PRARTNTO          PIC S9(7)V9(2) COMP-3.                            
003400*              ARTIKELPRIS NETTO                                          
003500   03 SUNTO             PIC S9(11)V9(2) COMP-3.                           
003600*              TOTAL SALES AMOUNT EXCL. VAT                               
003700   03 KDVALISO          PIC X(3).                                         
003800*              VALUTAKOD ENLIGT ISO-STANDARD.                             
003900   03 PRARTNTO-SEK      PIC S9(7)V9(2) COMP-3.                            
004000*              ARTIKELPRIS NETTO                                          
004100   03 SUNTO-SEK         PIC S9(11)V9(2) COMP-3.                           
004200*              TOTAL SALES AMOUNT EXCL. VAT                               
004300   03 KVLEVART          PIC S9(7) COMP-3.                                 
004400*              LEVERERAT ANTAL STYCK                                      
004500   03 IDSTATNR          PIC S9(9) COMP-3.                                 
004600*              STATISTISKT NUMMER                                         
004700*              1 = NORSKT                                                 
004800*              2 = ENGELSKT                                               
004900*              3 = BELGISKT                                               
005000*              4 = PERUANSKT                                              
005100*              5 = SVENSKT                                                
005200*              6 =                                                        
005300   03 IDLEVNR           PIC X(5).                                         
005400*              LEVERANT÷RNUMMER                                           
005500   03 DAFINDOC          PIC X(8).                                         
005600*              DOKUMENT DATUM (≈≈≈≈MMDD)                                  
005700*                                                                         
005800*** END OF VILMAII-COPY LENGTH= 227 OLD LENGTH=                           
