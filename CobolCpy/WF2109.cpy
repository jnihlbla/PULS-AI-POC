000100 01  WF2109.                                                              
000200*                                 WEBSHOP DATA TO SYSTEM VIPS             
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500*                                 RECORD TYPE                             
000600     03 IDREF                PIC X(15).                                   
000700*                                 REFERENS ID                             
000800*                                 REFERENCE ID                            
000900     03 IDPARTNR             PIC X(10).                                   
001000*                                 PARTNERNUMMER                           
001100*                                 PARTNER NO                              
001200     03 BEANST               PIC X(25).                                   
001300*                                 ANSTÄLLDS NAMN                          
001400*                                 NAME OF EMPLOYED                        
001500     03 DAREFDAT             PIC 9(8).                                    
001600*                                 REFERENSDATUM (ÅÅÅÅMMDD)                
001700*                                 REFERENCE DATE(YYYYMMDD)                
001800     03 SUNTO-TOT-VIPS       PIC 9(9)V9(2).                               
001900*                                 TOTAL SALES AMOUNT EXCL. VAT            
002000     03 KVORDRAD             PIC 9(4).                                    
002100*                                 ANTAL ORDERRADER                        
002200*                                 NUMBER OF ORDER LINES                   
002300     03 IDREFRAD             PIC 9(5).                                    
002400*                                 REFERENSRADSNR                          
002500*                                 REFERENCE LINE NUMBER                   
002600     03 IDARTNR20            PIC X(20).                                   
002700*                                 20-STÄLLIGT ARTIKELNUMMER FÖR A         
002800*                                 S400 (VIPS)                             
002900*                                 FORMATET ÄR HÖGERJUSTERAT MED I         
003000*                                 NLEDANDE                                
003100*                                 BLANKTECKEN, OCH UTAN INLEDANDE         
003200*                                  NOLLOR.                                
003300*                                 20 CHARACTER PART NUMBER FOR AS         
003400*                                 400 (VIPS)                              
003500*                                 THE FORMAT IS RIGHT JUSTIFIED W         
003600*                                 ITH LEADING                             
003700*                                 SPACES. NO LEADING ZEROES.              
003800*                                                                         
003900     03 BEART                PIC X(25).                                   
004000*                                 ARTIKELBENÄMNING                        
004100*                                 PART DESCRIPTION                        
004200     03 REARTRAB-VIPS        PIC 9(2)V9(1).                               
004300*                                 ARTIKELRABATT                           
004400*                                 PARTS DISCOUNT PERCENT                  
004500     03 KVLEVART-VIPS        PIC 9(5).                                    
004600*                                 LEVERERAT ANTAL STYCK                   
004700*                                 DELIVERED QUANTITY                      
004800     03 PRARTBTO-VIPS        PIC 9(9)V9(2).                               
004900*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
005000*                                 GROSS SALES PRICE (SEK)                 
005100     03 SUNTO-VIPS           PIC 9(9)V9(2).                               
005200*                                 TOTAL SALES AMOUNT EXCL. VAT            
005300     03 PRARTNTO-VIPS        PIC 9(9)V9(2).                               
005400*                                 ARTIKELPRIS NETTO                       
005500*                                 NET PRICE EACH   (FOB NET)              
005600     03 KDVAT                PIC X(2).                                    
005700*                                 MOMSKOD                                 
005800*                                 VAT CODE                                
005900     03 KDVALISO-LOC         PIC X(3).                                    
006000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006100*                                 CURRENCY CODE BY ISO-STANDARD.          
006200     03 PRKURS               PIC 9(6)V9(5).                               
006300*                                 VALUTAKURS                              
006400*                                 CURRENCY EXCHANGE RATE                  
006500     03 IDFINDOC             PIC 9(9).                                    
006600*                                 FINANSIELLT DOKUMENT ID                 
006700*                                 FINANCIAL DOCUMENT ID                   
006800     03 BELEVVIL             PIC X(35).                                   
006900*                                 LEVERANSVILLKOR                         
007000*                                 DELIVERY TERMS                          
007100*** END OF VILMAII-COPY LENGTH= 227 BYTES                                 
