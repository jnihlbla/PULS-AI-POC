000100* GENERATION OF COBOL HOST STRUCTURE FROM T01PDEV-TAB                     
000200  01 T01PDEV.                                                             
000300*              T01PDEV                                                    
000400   03 IDLEGSEL                          PIC X(4).                         
000500*              FAKTURERANDE F÷RETAG TEX VCCS                              
000600   03 KDBEHX                            PIC X(1).                         
000700*              BEHANDLINGSKOD-X                                           
000800   03 FLPAYTE                           PIC X(1).                         
000900*              FLAGGA PAYTE                                               
001000   03 FLDELTE                           PIC X(1).                         
001100*              FLAGGA DELTE                                               
001200   03 REARTRAB                          PIC S9(2)V9(2) COMP-3.            
001300*              ARTIKELRABATT                                              
001400   03 PRARTNTO-MIN                      PIC S9(7)V9(2) COMP-3.            
001500*              ARTIKELPRIS NETTO                                          
001600   03 PRARTNTO-MAX                      PIC S9(7)V9(2) COMP-3.            
001700*              ARTIKELPRIS NETTO                                          
001800   03 SUNTO-MIN                         PIC S9(11)V9(2) COMP-3.           
001900*              TOTAL SALES AMOUNT EXCL. VAT                               
002000   03 SUNTO-MAX                         PIC S9(11)V9(2) COMP-3.           
002100*              TOTAL SALES AMOUNT EXCL. VAT                               
002200   03 FLSOFT                            PIC X(1).                         
002300*              FLAGGA SOFTVARA                                            
002400   03 FLFREE                            PIC X(1).                         
002500*              GRATISFATURA                                               
002600   03 FLSERV                            PIC X(1).                         
002700*              FLAGGA SERVICE                                             
002800   03 FLINVOIC                          PIC X(1).                         
002900*              FLAGGA INVOICE                                             
003000   03 KVMANAD                           PIC S9(3) COMP-3.                 
003100*              ANTAL M≈NADER                                              
003200   03 DAREGDAT                          PIC X(8).                         
003300*              REGISTRERINGSDATUM (≈≈≈≈MMDD)                              
003400   03 DAUPPDAT                          PIC X(8).                         
003500*              UPPDATERINGSDATUM  (≈≈≈≈MMDD)                              
003600   03 IDUSER                            PIC X(8).                         
003700*              ANVƒNDARENS SƒKERHETS ID                                   
003800*                                                                         
003900*** END OF VILMAII-COPY LENGTH= 64 OLD LENGTH=                            
