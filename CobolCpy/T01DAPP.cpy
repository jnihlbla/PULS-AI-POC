000100* GENERATION OF COBOL HOST STRUCTURE FROM T01DAPP-TAB                     
000200  01 T01DAPP.                                                             
000300*              T01DAPP                                                    
000400   03 IDLEGSEL                          PIC X(4).                         
000500*              FAKTURERANDE FÖRETAG TEX VCCS                              
000600   03 DAEXDAT                           PIC X(8).                         
000700*              EXEKVERINGSDATUM (ÅÅÅÅMMDD)                                
000800   03 TIEXTID                           PIC S9(7) COMP-3.                 
000900*              EXEKVERINGSTIDPUNKT                                        
001000   03 KDVALISO                          PIC X(3).                         
001100*              VALUTAKOD ENLIGT ISO-STANDARD.                             
001200   03 IDLANDX3-SEND                     PIC X(3).                         
001300*              LANDKOD SÄNDANDE LAND                                      
001400   03 IDLEVNR                           PIC X(5).                         
001500*              LEVERANTÖRNUMMER                                           
001600   03 IDPARTNR                          PIC X(9).                         
001700*              PARTNERNUMMER                                              
001800   03 KDFINDOC                          PIC X(4).                         
001900*              TYP FINANSIELLT DOKUMENT                                   
002000   03 FLSOFT                            PIC X(1).                         
002100*              FLAGGA SOFTVARA                                            
002200   03 FLFREE                            PIC X(1).                         
002300*              GRATISFATURA                                               
002400   03 FLPRIV                            PIC X(1).                         
002500*              KÖPARE ÄR EN PRIVATPERSON                                  
002600   03 IDBREAK-1                         PIC X(8).                         
002700*              BRYTVÄRDE                                                  
002800   03 IDBREAK-2                         PIC X(8).                         
002900*              BRYTVÄRDE                                                  
003000   03 KDAPPEND                          PIC X(4).                         
003100*              APPENDIX KOD                                               
003200   03 IDAPPEND                          PIC X(8).                         
003300*              APPENDIXVÄRDE                                              
003400   03 SUNTO-APP                         PIC S9(11)V9(2) COMP-3.           
003500*              SALES AMOUNT FOR APPENDIX EXCL. VAT                        
003600   03 SUVAT-BILLIT-APP                  PIC S9(11)V9(2) COMP-3.           
003700*              MOMSVÄRDE FÖR APPENDIX                                     
003800   03 SUBTO-APP                         PIC S9(11)V9(2) COMP-3.           
003900*              SALES AMOUNT FOR APPENDIX INCL. VAT                        
004000*                                                                         
004100*** END OF VILMAII-COPY LENGTH= 92 OLD LENGTH=                            
