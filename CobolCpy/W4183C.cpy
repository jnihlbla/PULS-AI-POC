000100 01  W4183C-VIPS.                                                         
000200*                                 HANDLING FEE DATA TO SYSTEM VIP         
000300*                                 S                                       
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 IDFINDOC             PIC 9(9).                                    
000800*                                 FINANSIELLT DOKUMENT ID                 
000900*                                 FINANCIAL DOCUMENT ID                   
001000     03 DAFINDOC             PIC 9(8).                                    
001100*                                 DOKUMENT DATUM (≈≈≈≈MMDD)               
001200*                                 INVOICING DATE   (YYYYMMDD)             
001300     03 IDPARTNR             PIC X(10).                                   
001400*                                 PARTNERNUMMER                           
001500*                                 PARTNER NO                              
001600     03 IDDISTR              PIC X(4).                                    
001700*                                 DISTRIKTNUMMER                          
001800*                                 DISTRICT NUMBER                         
001900     03 IDKUNDNR             PIC X(6).                                    
002000*                                 KUNDNUMMER                              
002100*                                 CUSTOMER NO                             
002200     03 BEART                PIC X(25).                                   
002300*                                 ARTIKELBENƒMNING                        
002400*                                 PART DESCRIPTION                        
002500     03 KVLEVART-VIPS        PIC 9(5).                                    
002600*                                 LEVERERAT ANTAL STYCK                   
002700*                                 DELIVERED QUANTITY                      
002800     03 PRARTNTO-VIPS        PIC 9(9)V9(2).                               
002900*                                 ARTIKELPRIS NETTO                       
003000*                                 NET PRICE EACH   (FOB NET)              
003100     03 SUNTO-VIPS           PIC 9(9)V9(2).                               
003200*                                 TOTAL SALES AMOUNT EXCL. VAT            
003300     03 SUVAT-VIPS           PIC 9(9)V9(2).                               
003400*                                 VAT SALES AMOUNT                        
003500     03 SUBTO-VIPS           PIC 9(9)V9(2).                               
003600*                                 TOTAL SALES AMOUNT INCL. VAT            
003700     03 KDVAT                PIC X(2).                                    
003800*                                 MOMSKOD                                 
003900*                                 VAT CODE                                
004000     03 KDVALISO-LOC         PIC X(3).                                    
004100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004200*                                 CURRENCY CODE BY ISO-STANDARD.          
004300     03 PRKURS               PIC 9(6)V9(5).                               
004400*                                 VALUTAKURS                              
004500*                                 CURRENCY EXCHANGE RATE                  
004600     03 IDREF                PIC X(15).                                   
004700*                                 REFERENS ID                             
004800*                                 REFERENCE ID                            
004900*** END OF VILMAII-COPY LENGTH= 145 BYTES                                 
