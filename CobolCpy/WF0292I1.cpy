000100 01  REQU-WF0292I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0292             
000300*                                 SENDING COUNTRY MAINTENANCE             
000400     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE F÷RETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 REQU-KDBEHX-KEY      PIC X.                                       
000800*                                 BEHANDLINGSKOD-X                        
000900     03 REQU-FLPAYTE         PIC X.                                       
001000*                                 FLAGGA PAYTE                            
001100*                                 PAYTE FLAG                              
001200     03 REQU-FLDELTE         PIC X.                                       
001300*                                 FLAGGA DELTE                            
001400*                                 DELTE FLAG                              
001500     03 REQU-REARTRAB        PIC X(5).                                    
001600*                                 ARTIKELRABATT                           
001700*                                 PARTS DISCOUNT PERCENT                  
001800     03 REQU-PRARTNTO-MIN    PIC X(10).                                   
001900*                                 ARTIKELPRIS NETTO                       
002000*                                 NET PRICE EACH   (FOB NET)              
002100     03 REQU-PRARTNTO-MAX    PIC X(10).                                   
002200*                                 ARTIKELPRIS NETTO                       
002300*                                 NET PRICE EACH   (FOB NET)              
002400     03 REQU-SUNTO-MIN       PIC X(14).                                   
002500*                                 TOTAL SALES AMOUNT EXCL. VAT            
002600     03 REQU-SUNTO-MAX       PIC X(14).                                   
002700*                                 TOTAL SALES AMOUNT EXCL. VAT            
002800     03 REQU-FLSOFT          PIC X.                                       
002900*                                 FLAGGA SOFTVARA                         
003000*                                 SOFTWARE MARK                           
003100     03 REQU-FLFREE          PIC X.                                       
003200*                                 GRATISFATURA                            
003300*                                 FREE INVOICE                            
003400     03 REQU-FLSERV          PIC X.                                       
003500*                                 FLAGGA SERVICE                          
003600*                                 SERVICE FLAG                            
003700     03 REQU-FLINVOIC        PIC X.                                       
003800*                                 FLAGGA INVOICE                          
003900*                                 INVOICE DLAG                            
004000     03 REQU-DAREGDAT        PIC X(8).                                    
004100*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
004200*                                 REGISTRATION DATE (YYYYMMDD)            
004300     03 REQU-DAUPPDAT        PIC X(8).                                    
004400*                                 UPPDATERINGSDATUM  (≈≈≈≈MMDD)           
004500*                                                                         
004600*                                 UPDATING DATE     (YYYYMMDD)            
004700*                                                                         
004800*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
