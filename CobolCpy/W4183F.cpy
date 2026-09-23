000100 01  W4183C-VIPS.                                                         
000200*                                 HANDLING FEE DATA TO SYSTEM VIP         
000300*                                 S                                       
000400     03 WZ01REQU.                                                         
000500*                                 THE FIRST FIELDS IN AN REQUEST          
000600*                                 SENT FROM ONE SYSTEM COMPONENT          
000700*                                 TO ANOTHER.                             
000800        05 IDMSGVER          PIC 9(3).                                    
000900*                                 VERSIONSNUMMER PÅ MEDDELANDE            
001000*                                 VERSION NUMBER OF THE MESSAGE           
001100        05 KDPGMACT          PIC X.                                       
001200*                                 TYP AV PROGRAMBEARBETNING               
001300*                                 TYPE OF PROGRAM ACTION                  
001400        05 IDUSER            PIC X(8).                                    
001500*                                 ANVÄNDARENS SÄKERHETS ID                
001600*                                 USER SECURITY-IDENTITY                  
001700     03 IDPTYP               PIC X(3).                                    
001800*                                 POSTTYP                                 
001900*                                 RECORD TYPE                             
002000     03 IDFINDOC             PIC 9(9).                                    
002100*                                 FINANSIELLT DOKUMENT ID                 
002200*                                 FINANCIAL DOCUMENT ID                   
002300     03 DAFINDOC             PIC 9(8).                                    
002400*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
002500*                                 INVOICING DATE   (YYYYMMDD)             
002600     03 IDPARTNR             PIC X(10).                                   
002700*                                 FINANCIELL KUND                         
002800*                                 FINANCIAL CUST                          
002900     03 IDDISTR              PIC X(4).                                    
003000*                                 DISTRIKTNUMMER                          
003100*                                 DISTRICT NUMBER                         
003200     03 IDKUNDNR             PIC X(6).                                    
003300*                                 KUNDNUMMER                              
003400*                                 CUSTOMER NO                             
003500     03 BEART                PIC X(25).                                   
003600*                                 ARTIKELBENÄMNING                        
003700*                                 PART DESCRIPTION                        
003800     03 KVLEVART-VIPS        PIC 9(5).                                    
003900*                                 LEVERERAT ANTAL STYCK                   
004000*                                 DELIVERED QUANTITY                      
004100     03 PRARTNTO-VIPS        PIC 9(9)V9(2).                               
004200*                                 ARTIKELPRIS NETTO                       
004300*                                 NET PRICE EACH   (FOB NET)              
004400     03 SUNTO-VIPS           PIC 9(9)V9(2).                               
004500*                                 TOTAL SALES AMOUNT EXCL. VAT            
004600     03 SUVAT-VIPS           PIC 9(9)V9(2).                               
004700*                                 VAT SALES AMOUNT                        
004800     03 SUBTO-VIPS           PIC 9(9)V9(2).                               
004900*                                 TOTAL SALES AMOUNT INCL. VAT            
005000     03 KDVAT                PIC X(2).                                    
005100*                                 MOMSKOD                                 
005200*                                 VAT CODE                                
005300     03 KDVALISO-LOC         PIC X(3).                                    
005400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005500*                                 CURRENCY CODE BY ISO-STANDARD.          
005600     03 PRKURS               PIC 9(6)V9(5).                               
005700*                                 VALUTAKURS                              
005800*                                 CURRENCY EXCHANGE RATE                  
005900     03 IDREF                PIC X(15).                                   
006000*                                 REFERENS ID                             
006100*                                 REFERENCE ID                            
006200*** END OF VILMAII-COPY LENGTH= 157 BYTES                                 
