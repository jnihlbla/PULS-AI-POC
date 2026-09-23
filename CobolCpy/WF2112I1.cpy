000100 01  WF2112.                                                              
000200*                                 FEEDBACK DATA SYSTEM PULS-W418          
000300*                                 (HANDLING FEES)                         
000400     03 DAFINDOC             PIC 9(8).                                    
000500*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
000600*                                 INVOICING DATE   (YYYYMMDD)             
000700     03 IDFINDOC             PIC 9(9).                                    
000800*                                 FINANSIELLT DOKUMENT ID                 
000900*                                 FINANCIAL DOCUMENT ID                   
001000     03 KDVALISO             PIC X(3).                                    
001100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001200*                                 CURRENCY CODE BY ISO-STANDARD.          
001300     03 PRKURS               PIC 9(6)V9(5).                               
001400*                                 VALUTAKURS                              
001500*                                 CURRENCY EXCHANGE RATE                  
001600     03 IDDC                 PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800*                                 WAREHOUSE IDENTIFIER                    
001900     03 SUNTO-TOT            PIC 9(11)V9(2).                              
002000*                                 TOTAL SALES AMOUNT EXCL. VAT            
002100     03 SUVAT-BILLIT-TOT     PIC 9(11)V9(2).                              
002200*                                 SUMMERAT MOMSVÄRDE                      
002300*                                 TOTAL VAT VALUE                         
002400     03 SUBTO-TOT            PIC 9(11)V9(2).                              
002500*                                 TOTAL SALES AMOUNT INCL. VAT            
002600     03 IDEXCUST-1           PIC X(15).                                   
002700*                                 EXTERNT KUNDID                          
002800*                                 EXTERNAL CUSTOMER ID                    
002900     03 IDEXCUST-2           PIC X(15).                                   
003000*                                 EXTERNT KUNDID                          
003100*                                 EXTERNAL CUSTOMER ID                    
003200     03 IDREF                PIC X(15).                                   
003300*                                 REFERENS ID                             
003400*                                 REFERENCE ID                            
003500     03 IDARTNR-FINANCE      PIC X(50).                                   
003600*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
003700*                                 UK                                      
003800*                                 PART NUMBER FOR FINANCIAL USE           
003900     03 BEART                PIC X(25).                                   
004000*                                 ARTIKELBENÄMNING                        
004100*                                 PART DESCRIPTION                        
004200     03 PRARTNTO             PIC 9(7)V9(2).                               
004300*                                 ARTIKELPRIS NETTO                       
004400*                                 NET PRICE EACH   (FOB NET)              
004500     03 IDOPTION-1           PIC X(15).                                   
004600*                                 BRYTBEGREPP                             
004700*                                 OPTIONAL ID                             
004800     03 IDOPTION-2           PIC X(15).                                   
004900*                                 BRYTBEGREPP                             
005000*                                 OPTIONAL ID                             
005100     03 IDOPTION-3           PIC X(15).                                   
005200*                                 BRYTBEGREPP                             
005300*                                 OPTIONAL ID                             
005400     03 KVLEVART             PIC 9(7).                                    
005500*                                 LEVERERAT ANTAL STYCK                   
005600*                                 DELIVERED QUANTITY                      
005700     03 KDVAT                PIC X(2).                                    
005800*                                 MOMSKOD                                 
005900*                                 VAT CODE                                
006000     03 SUNTO                PIC 9(11)V9(2).                              
006100*                                 TOTAL SALES AMOUNT EXCL. VAT            
006200     03 SUBTO                PIC 9(11)V9(2).                              
006300*                                 TOTAL SALES AMOUNT INCL. VAT            
006400     03 SUVAT-BILLIT         PIC 9(11)V9(2).                              
006500*                                 SUMMERAT MOMSVÄRDE PER RAD              
006600*                                 TOTAL VAT VALUE PER LINE                
006700     03 KDTRADP              PIC X(4).                                    
006800*                                 TRADING PARTNER                         
006900*                                 TRADING PARTNER                         
007000     03 KDVALISO-BET         PIC X(3).                                    
007100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007200*                                 CURRENCY CODE BY ISO-STANDARD.          
007300     03 PRKURS-BET           PIC 9(6)V9(5).                               
007400*                                 VALUTAKURS                              
007500*                                 CURRENCY EXCHANGE RATE                  
007600     03 PRKURS-FAKBET        PIC 9(6)V9(5).                               
007700*                                 VALUTAKURS                              
007800*                                 CURRENCY EXCHANGE RATE                  
007900     03 IDPARTNR             PIC X(9).                                    
008000*                                 PARTNERNUMMER                           
008100*                                 PARTNER NO                              
008200     03 DAREGDAT             PIC X(8).                                    
008300*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
008400*                                 REGISTRATION DATE (YYYYMMDD)            
008500*** END OF VILMAII-COPY LENGTH= 340 BYTES                                 
