000010*** EDIT ALLOWED                                                          
000100*                     FILE TO SAP R/3 (LINE RECORD)                       
000200*                                                                         
000300*                     ENGLISH EXPLANATION ACCORDING TO SAP R/3            
000400*                                                                         
000500 01  LINE-R3.                                                             
000600     03  LINE-RECORD-TYPE          PIC X(3).                              
000700*                          210 = WITH VENDOR NO                           
000800*                          310 = WITH CUSTOMER NO                         
000900*                          610 = GL ACCOUNTS                              
001000     03  LINE-COMPANY-CODE         PIC X(4).                              
001100*                          COMPANY CODE                                   
001200     03  LINE-DOCUMENT-NO          PIC X(10).                             
001300*                          DOCUMENT NUMBER                                
001400     03  LINE-DOCUMENT-NO-REF      PIC X(16).                             
001500*                          REFERENCE DOCUMENT NUMBER                      
001600     03  LINE-POSTING-KEY          PIC X(2).                              
001700*                          IF...                                          
001800*                          VENDOR INVOICE, 31 (NEGATIVE VALUE)            
001900*                          VENDOR CREDIT NOTE, 21 (POSITIVE VALUE)        
002000*                          CUSTOMER INVOICE, 01 (NEGATIVE VALUE)          
002100*                          CUST. CREDIT NOTE, 11 (POSITIVE VALUE)         
002200*                          ALL OTHER GL TRANS:                            
002300*                          IF POSITIVE VALUE - 40                         
002400*                          IF NEGATIVE VALUE - 50                         
002500     03  LINE-ACCOUNT              PIC X(10).                             
002600*                          G/L ACCOUNT, INCLUDES VENDOR NO,               
002700*                          CUSTOMER NO                                    
002800     03  LINE-GL-IND               PIC X(1).                              
002900*                          G/L INDICATOR                                  
003000     03  LINE-AMOUNT               PIC 9(13)V9(2).                        
003100*                          AMOUNT WITHOUT +/-                             
003200     03  LINE-AMOUNT-SIGN          PIC X(1).                              
003300*                          +/- ONLY FOR INFORMATION, DEBIT OR             
003400*                          CREDIT DETERMINES BY POSTING-KEY               
003500     03  LINE-AMOUNT-LC            PIC 9(13)V9(2).                        
003600*                          AMOUNT LOCAL CURRENCY                          
003700     03  LINE-TAX-CODE             PIC X(2).                              
003800*                          VAT TAX CODE                                   
003900     03  LINE-TAX-AMOUNT           PIC 9(13)V9(2).                        
004000     03  LINE-TAX-AMOUNT-X REDEFINES LINE-TAX-AMOUNT                      
004100                                   PIC X(15).                             
004200*                          VAT AMOUNT                                     
004300     03  LINE-TAX-AMOUNT-LC        PIC 9(13)V9(2).                        
004400     03  LINE-TAX-AMOUNT-LC-X REDEFINES LINE-TAX-AMOUNT-LC                
004500                                   PIC X(15).                             
004600*                          VAT AMOUNT LOCAL CURRENCY                      
004700     03  LINE-TAX-AMOUNT-IND       PIC X(1).                              
004800*                          INDICATOR CALCULATE TAX                        
004900     03  LINE-DUE-DATE             PIC 9(8).                              
005000*                          THE DATE WHEN INVOICE IS DUE                   
005100*                          FORMAT = YYYYMMDD                              
005200     03  LINE-NUMBER-OF-DAYS       PIC 9(3).                              
005300*                          NUMBER OF DAYS - INSTEAD OF DUE DATE           
005400     03  LINE-PAYTERMS             PIC X(4).                              
005500*                          PAYMENT TERMS                                  
005600     03  LINE-PAYBLOCK             PIC X(1).                              
005700*                          PAYMENT BLOCK                                  
005800     03  LINE-PAYMETHOD            PIC X(1).                              
005900*                          PAYMENT METHOD                                 
006000     03  LINE-COST-CENTER          PIC X(10).                             
006100*                          COST CENTER                                    
006200     03  LINE-PROFIT-CENTER        PIC X(10).                             
006300*                          PROFIT CENTER                                  
006400     03  LINE-ORDER                PIC X(12).                             
006500*                          INTERNAL ORDER                                 
006600     03  LINE-QUANTITY             PIC 9(10)V9(3).                        
006700*                          THE QUANTITY OF ITEMS                          
006800     03  LINE-UNIT                 PIC X(3).                              
006900*                          UNIT OF MEASURE                                
007000     03  LINE-ALLOCATE             PIC X(18).                             
007100*                          ALLOCATION                                     
007200     03  LINE-TRADING-PARTNER      PIC X(4).                              
007300*                          TRADING PARTNERS                               
007400     03  LINE-VALUE-DATE           PIC 9(8).                              
007500*                          VALUE DATE                                     
007600     03  LINE-TEXT                 PIC X(50).                             
007700*                          FREE TEXT                                      
007800     03  LINE-MORE-IND             PIC X(1).                              
007900*                          INDICATES MORE INFORMATION                     
008000     03  LINE-NEW-COMPANY-CODE     PIC X(4).                              
008100*                          NEW COMPANY CODE                               
008200     03  FILLER                    PIC X(20).                             
008300*                          ******************                             
008400     03  COPA-PART                 PIC X(240).                            
008500*                          COPA  INFORMATION.                             
008600     03  FILLER REDEFINES COPA-PART.                                      
008700         05  LINE-PA-BAREA         PIC X(6).                              
008800*                          VOLVO BUSINESS AREA                            
008900         05  LINE-PA-PRODVAR       PIC X(18).                             
009000*                          PRODUCT VARIANT                                
009100         05  LINE-PA-PRODSUB       PIC X(18).                             
009200*                          PRODUCT SUB VARIANT                            
009300         05  LINE-PA-SERIALNO      PIC X(18).                             
009400*                          SERIAL NUMBER                                  
009500         05  LINE-PA-MARKET        PIC X(6).                              
009600*                          MARKET                                         
009700         05  LINE-PA-REGION        PIC X(6).                              
009800*                          GEOGRAPHIC REGION                              
009900         05  LINE-PA-SALESREG      PIC X(8).                              
010000*                          SALES REGION                                   
010100         05  LINE-PA-SALESMAN      PIC X(12).                             
010200*                          CUSTOMER TYPE                                  
010300         05  LINE-PA-CUSTOMER      PIC X(10).                             
010400*                          INVOICED CUSTOMER                              
010500         05  LINE-PA-ENDUSER       PIC X(6).                              
010600*                          END USER SEGMENT                               
010700         05  LINE-PA-CONTRACT      PIC X(18).                             
010800*                          CONTRACT NUMBER                                
010900         05  LINE-PA-CAMPAIGN      PIC X(8).                              
011000*                          CAMPAIGN                                       
011100         05  LINE-PA-AAREA         PIC X(6).                              
011200*                          AREA OF APPLICATION                            
011300         05  LINE-PA-CUSTTYPE      PIC X(10).                             
011310*                          CUSTOMER TYPE                                  
011320         05  LINE-PA-PRODMOD       PIC X(11).                             
011330*                          PRODUCT MODEL                                  
011340         05  LINE-PA-CHAR1         PIC X(18).                             
011350*                          GENERAL CHARACTERISTIC 1                       
011360         05  LINE-PA-CHAR2         PIC X(18).                             
011370*                          GENERAL CHARACTERISTIC 2                       
011380         05  LINE-PA-CHAR3         PIC X(14).                             
011390*                          GENERAL CHARACTERISTIC 3                       
011391         05  LINE-PA-CHAR4         PIC X(14).                             
011392*                          GENERAL CHARACTERISTIC 4                       
011393         05  LINE-PA-FILLER        PIC X(15).                             
011394*                          FILLER                                         
011395     03  LINE-PAYREF           PIC X(30).                                 
011396*                          PAYMENT REFERENCE                              
011397     03  LINE-REFKEY1          PIC X(12).                                 
011398*                          CUSTOMER/VENDOR REFERENCE KEY                  
011399     03  LINE-REFKEY2          PIC X(12).                                 
011400*                          CUSTOMER/VENDOR REFERENCE KEY                  
011401     03  LINE-REFKEY3          PIC X(20).                                 
011402*                          CUSTOMER/VENDOR REFERENCE KEY                  
011403     03  LINE-DUNAREA          PIC X(02).                                 
011404*                          DUNNING AREA                                   
011410     03  LINE-WBS              PIC X(24).                                 
011411*                          WBS ELEMENT                                    
011420     03  LINE-SD-ORDER         PIC X(10).                                 
011421*                          SALES ORDER                                    
011430     03  LINE-SD-POSNR         PIC X(06).                                 
011431*                          SALES ORDER POSITION                           
011440     03  LINE-PA-CHAR5         PIC X(18).                                 
011441*                          GENERAL CHARACTERISTIC 5                       
011442     03  LINE-PA-CHAR6         PIC X(18).                                 
011443*                          GENERAL CHARACTERISTIC 6                       
011444     03  LINE-PA-CHAR7         PIC X(18).                                 
011445*                          GENERAL CHARACTERISTIC 7                       
011450     03  LINE-ASSET-TTYPE      PIC X(03).                                 
011451*                          ASSET TRANSACTION TYPE                         
011460     03  LINE-PAYER-PAYEE      PIC X(10).                                 
011461*                          PAYER/PAYEE                                    
011470     03  LINE-SAMNR            PIC 9(08).                                 
011471*                          INVOICE LIST NUMBER                            
011480     03  LINE-PURCH-ORDER      PIC X(10).                                 
011481*                          PURCHASING DOCUMENT NUMBER                     
011490     03  LINE-PURCH-POSNR      PIC X(05).                                 
011491*                          ITEM NUMBER OF PURCHASING DOCUMENT             
011492     03  LINE-TRPART-COPA      PIC X(04).                                 
011493*                          TRADING PARTNER'S BUSINESS AREA                
011494     03  LINE-LZBKZ            PIC X(03).                                 
011495*                          STATE CENTRAL BANK INDICATOR                   
011496     03  LINE-MATNR            PIC X(18).                                 
011497*                          MATERIAL NUMBER                                
011500*** END OF VILMAII-COPY LENGTH= 751 OLD LENGTH= 430                       
