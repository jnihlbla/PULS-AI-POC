000100 01  RESP-WF0291O1.                                                       
000200*                                 RESPONS-COPYTEXT F÷R PGM WF0291         
000300*                                 SENDING COUNTRY MAINTENANCE             
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-KDBEHX-KEY      PIC X.                                       
000800*                                 BEHANDLINGSKOD-X                        
000900     03 RESP-BELEGRAD-1      PIC X(35).                                   
001000*                                 DEL AV LEGAL SELLER NAMN                
001100*                                 PART OF LEGAL SELLER NAME               
001200     03 RESP-FLPAYTE         PIC X.                                       
001300*                                 FLAGGA PAYTE                            
001400*                                 PAYTE FLAG                              
001500     03 RESP-FLDELTE         PIC X.                                       
001600*                                 FLAGGA DELTE                            
001700*                                 DELTE FLAG                              
001800     03 RESP-REARTRAB        PIC Z9.9(2).                                 
001900*                                 ARTIKELRABATT                           
002000*                                 PARTS DISCOUNT PERCENT                  
002100     03 RESP-PRARTNTO-MIN    PIC Z(6)9.9(2).                              
002200*                                 ARTIKELPRIS NETTO                       
002300*                                 NET PRICE EACH   (FOB NET)              
002400     03 RESP-PRARTNTO-MAX    PIC Z(6)9.9(2).                              
002500*                                 ARTIKELPRIS NETTO                       
002600*                                 NET PRICE EACH   (FOB NET)              
002700     03 RESP-SUNTO-MIN       PIC Z(10)9.9(2).                             
002800*                                 TOTAL SALES AMOUNT EXCL. VAT            
002900     03 RESP-SUNTO-MAX       PIC Z(10)9.9(2).                             
003000*                                 TOTAL SALES AMOUNT EXCL. VAT            
003100     03 RESP-FLSOFT          PIC X.                                       
003200*                                 FLAGGA SOFTVARA                         
003300*                                 SOFTWARE MARK                           
003400     03 RESP-FLFREE          PIC X.                                       
003500*                                 GRATISFATURA                            
003600*                                 FREE INVOICE                            
003700     03 RESP-FLSERV          PIC X.                                       
003800*                                 FLAGGA SERVICE                          
003900*                                 SERVICE FLAG                            
004000     03 RESP-FLINVOIC        PIC X.                                       
004100*                                 FLAGGA INVOICE                          
004200*                                 INVOICE DLAG                            
004300     03 RESP-DAREGDAT        PIC Z(8).                                    
004400*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
004500*                                 REGISTRATION DATE (YYYYMMDD)            
004600     03 RESP-DAUPPDAT        PIC Z(8).                                    
004700*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
004800*                                                                         
004900*                                 UPDATING DATE     (YYYYMMDD)            
005000*                                                                         
005100     03 RESP-IDUSER          PIC X(8).                                    
005200*                                 ANVƒNDARENS SƒKERHETS ID                
005300*                                 USER SECURITY-IDENTITY                  
005400*** END OF VILMAII-COPY LENGTH= 123 BYTES                                 
