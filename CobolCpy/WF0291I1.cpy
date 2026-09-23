000100 01  REQU-WF0291I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0291             
000300*                                 ABNORMAL VALUES SELECTION MAINT         
000400*                                 ENANCE                                  
000500     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000600*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000700*                                 LEGAL SELLER IDENTITY                   
000800     03 REQU-KDBEHX-KEY      PIC X.                                       
000900*                                 BEHANDLINGSKOD-X                        
001000     03 REQU-FLPAYTE         PIC X.                                       
001100*                                 FLAGGA PAYTE                            
001200*                                 PAYTE FLAG                              
001300     03 REQU-FLDELTE         PIC X.                                       
001400*                                 FLAGGA DELTE                            
001500*                                 DELTE FLAG                              
001600     03 REQU-REARTRAB        PIC X(5).                                    
001700*                                 ARTIKELRABATT                           
001800*                                 PARTS DISCOUNT PERCENT                  
001900     03 REQU-PRARTNTO-MIN    PIC X(10).                                   
002000*                                 ARTIKELPRIS NETTO                       
002100*                                 NET PRICE EACH   (FOB NET)              
002200     03 REQU-PRARTNTO-MAX    PIC X(10).                                   
002300*                                 ARTIKELPRIS NETTO                       
002400*                                 NET PRICE EACH   (FOB NET)              
002500     03 REQU-SUNTO-MIN       PIC X(14).                                   
002600*                                 TOTAL SALES AMOUNT EXCL. VAT            
002700     03 REQU-SUNTO-MAX       PIC X(14).                                   
002800*                                 TOTAL SALES AMOUNT EXCL. VAT            
002900     03 REQU-FLSOFT          PIC X.                                       
003000*                                 FLAGGA SOFTVARA                         
003100*                                 SOFTWARE MARK                           
003200     03 REQU-FLFREE          PIC X.                                       
003300*                                 GRATISFATURA                            
003400*                                 FREE INVOICE                            
003500     03 REQU-FLSERV          PIC X.                                       
003600*                                 FLAGGA SERVICE                          
003700*                                 SERVICE FLAG                            
003800     03 REQU-FLINVOIC        PIC X.                                       
003900*                                 FLAGGA INVOICE                          
004000*                                 INVOICE DLAG                            
004100*** END OF VILMAII-COPY LENGTH= 64 BYTES                                  
