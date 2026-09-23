000100 01  WF2110.                                                              
000200*                                 FEEDBACK DATA TO SYSTEM VIPS            
000300     03 IDPARTNR             PIC X(9).                                    
000400*                                 PARTNERNUMMER                           
000500*                                 PARTNER NO                              
000600     03 KDVALISO             PIC X(3).                                    
000700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
000800*                                 CURRENCY CODE BY ISO-STANDARD.          
000900     03 PRKURS               PIC 9(6)V9(5).                               
001000*                                 VALUTAKURS                              
001100*                                 CURRENCY EXCHANGE RATE                  
001200     03 DAFINDOC             PIC 9(8).                                    
001300*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
001400*                                 INVOICING DATE   (YYYYMMDD)             
001500     03 IDFINDOC             PIC 9(9).                                    
001600*                                 FINANSIELLT DOKUMENT ID                 
001700*                                 FINANCIAL DOCUMENT ID                   
001800     03 KDBETALV             PIC X(4).                                    
001900*                                 BETALNINGSVILLKOR KUNDRESKONTRA         
002000*                                 TERMS OF PAYMENT                        
002100     03 SUNTO-TOT            PIC 9(11)V9(2).                              
002200*                                 TOTAL SALES AMOUNT EXCL. VAT            
002300     03 SUBTO-TOT            PIC 9(11)V9(2).                              
002400*                                 TOTAL SALES AMOUNT INCL. VAT            
002500     03 SUVAT-BILLIT-TOT     PIC 9(11)V9(2).                              
002600*                                 SUMMERAT MOMSVÄRDE                      
002700*                                 TOTAL VAT VALUE                         
002800     03 IDEXCUST             PIC X(15).                                   
002900*                                 EXTERNT KUNDID                          
003000*                                 EXTERNAL CUSTOMER ID                    
003100     03 DAREFDAT             PIC 9(8).                                    
003200*                                 REFERENSDATUM (ÅÅÅÅMMDD)                
003300*                                 REFERENCE DATE(YYYYMMDD)                
003400     03 IDREF                PIC X(15).                                   
003500*                                 REFERENS ID                             
003600*                                 REFERENCE ID                            
003700     03 IDREFRAD             PIC 9(5).                                    
003800*                                 REFERENSRADSNR                          
003900*                                 REFERENCE LINE NUMBER                   
004000     03 KDVAT                PIC X(2).                                    
004100*                                 MOMSKOD                                 
004200*                                 VAT CODE                                
004300     03 IDARTNR-FINANCE      PIC X(50).                                   
004400*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
004500*                                 UK                                      
004600*                                 PART NUMBER FOR FINANCIAL USE           
004700     03 KVLEVART             PIC 9(7).                                    
004800*                                 LEVERERAT ANTAL STYCK                   
004900*                                 DELIVERED QUANTITY                      
005000     03 PRARTBTO             PIC 9(7)V9(2).                               
005100*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
005200*                                 GROSS SALES PRICE (SEK)                 
005300     03 PRARTNTO             PIC 9(7)V9(2).                               
005400*                                 ARTIKELPRIS NETTO                       
005500*                                 NET PRICE EACH   (FOB NET)              
005600     03 SUNTO                PIC 9(11)V9(2).                              
005700*                                 TOTAL SALES AMOUNT EXCL. VAT            
005800     03 SUBTO                PIC 9(11)V9(2).                              
005900*                                 TOTAL SALES AMOUNT INCL. VAT            
006000     03 SUVAT-BILLIT         PIC 9(11)V9(2).                              
006100*                                 SUMMERAT MOMSVÄRDE PER RAD              
006200*                                 TOTAL VAT VALUE PER LINE                
006300*** END OF VILMAII-COPY LENGTH= 242 BYTES                                 
