000100 01  OLIN-W41283.                                                         
000200*                                 VALIDATED ORDER LINES FROM              
000300*                                 WEB UPLOAD.                             
000400*                                 IDSYSTEM IS "SPX" OR "XCEL"             
000500     03 OLIN-IDSYSTEM        PIC X(4).                                    
000600*                                 VOLVO VCCS SYSTEMNUMMER                 
000700*                                 VOLVO VCCS SYSTEM NUMBER                
000800     03 OLIN-BEMARKN         PIC X(24).                                   
000900*                                 MARKNADSBENÄMNING                       
001000*                                 MARKET DESCRIPTION                      
001100     03 OLIN-IDDISTR         PIC 9(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 OLIN-IDKUNDNR        PIC 9(6).                                    
001500*                                 KUNDNUMMER                              
001600*                                 CUSTOMER NO                             
001700     03 OLIN-KDORDKL         PIC 9.                                       
001800*                                 ORDERKLASS                              
001900*                                 ORDER CLASS                             
002000     03 OLIN-KDFRAKT         PIC 9(2).                                    
002100*                                 FRAKTSÄTT DC TILL KUND                  
002200*                                 FREIGHT CODE                            
002300     03 OLIN-IDDC            PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500*                                 WAREHOUSE IDENTIFIER                    
002600     03 OLIN-FLFORBI         PIC X.                                       
002700*                                 FÖRBIORDERFLAGGA                        
002800*                                                                         
002900     03 OLIN-BEKUNDRF        PIC X(15).                                   
003000*                                 KUNDENS REFERENS                        
003100*                                 CUSTOMERS REFERENCE                     
003200     03 OLIN-KDTPOTYP        PIC 9.                                       
003300*                                 TYP AV TIDPLANERAD ORDER                
003400*                                 TYPE OF TIME PLANNED ORDER              
003500     03 OLIN-TITPO           PIC 9(6).                                    
003600*                                 PLANERAD ORDERDATUM                     
003700*                                 PLANNED ORDER DATE                      
003800     03 OLIN-TIREPDAT        PIC 9(6).                                    
003900*                                 REPAIR DATE                             
004000*                                 REPAIR DATE                             
004100     03 OLIN-BEGMT-RAD1      PIC X(35).                                   
004200*                                 GODSMOTTAGARNAMN RAD 1                  
004300*                                 GOODS RECEIVER NAME LINE 1              
004400     03 OLIN-BEGMT-RAD2      PIC X(35).                                   
004500*                                 GODSMOTTAGARNAMN RAD 2                  
004600*                                 GOODS RECEIVER NAME LINE 2              
004700     03 OLIN-ADGMT-GATA      PIC X(35).                                   
004800*                                 GODSMOTTAGARADRESS GATA                 
004900*                                 GOODS RECEIVER ADDRESS STREET           
005000     03 OLIN-ADGMT-PADR      PIC X(35).                                   
005100*                                 GODSMOTTAGARADRESS POSTADRESS           
005200*                                 GOODS RECEIVER ADDRESS TOWN             
005300     03 OLIN-ADGMT-LAND      PIC X(35).                                   
005400*                                 GODSMOTTAGARADRESS LAND                 
005500*                                 GOODS RECEIVER ADDRESS COUNTRY          
005600     03 OLIN-IDARTNR         PIC 9(9).                                    
005700*                                 ARTIKELNUMMER                           
005800*                                 PART NUMBER                             
005900     03 OLIN-REKSIFFR        PIC 9.                                       
006000*                                 KONTROLLSIFFRA                          
006100*                                 PART NO CHECK DIGIT                     
006200     03 OLIN-KVBEART         PIC 9(6).                                    
006300*                                 BESTÄLLT ANTAL STYCKEN                  
006400*                                 ORDERED QUANTITY                        
006500     03 OLIN-BERADREF        PIC X(10).                                   
006600*                                 KUNDENS RADREFERENS                     
006700*                                 CUSTOMERS ITEM REF.                     
006800     03 OLIN-BEVARREF        PIC X(10).                                   
006900*                                 VÅR REFERENS                            
007000*                                 OUR REFERENCE                           
007100     03 OLIN-KDPROTYP        PIC X.                                       
007200*                                 TYP AV PROFORMA                         
007300*                                 TYPE OF PRO FORMA                       
007400     03 OLIN-KDFAKTYP        PIC X.                                       
007500*                                 FAKTURATYP                              
007600*                                 INVOICE TYPE                            
007700     03 OLIN-IDKONTO         PIC 9(10).                                   
007800*                                 KONTO                                   
007900*                                 ACCOUNT                                 
008000     03 OLIN-IDSKYLT         PIC X(3).                                    
008100      88 OLIN-GODK-IDSKYLT   VALUE 'CZ '                                  
008200                             'D  '                                        
008300                             'DK '                                        
008400                             'E  '                                        
008500                             'FB '                                        
008600                             'GB '                                        
008700                             'GR '                                        
008800                             'H  '                                        
008900                             'I  '                                        
009000                             'IR '                                        
009100                             'J  '                                        
009200                             'KOR'                                        
009300                             'MAL'                                        
009400                             'NL '                                        
009500                             'P  '                                        
009600                             'PL '                                        
009700                             'RC '                                        
009800                             'RCN'                                        
009900                             'RO '                                        
010000                             'RUS'                                        
010100                             'S  '                                        
010200                             'SF '                                        
010300                             'T  '                                        
010400                             'TR '                                        
010500                             'USA'                                        
010600                             'YU '.                                       
010700*                                 NATIONALITETSTECKEN                     
010800*                                 SPRÅKIDENTIFIKATION                     
010900*                                 NATIONALITY SIGN                        
011000*                                 LANGUAGE IDENTIFIER                     
011100     03 OLIN-FORFDAT         PIC X(6).                                    
011200     03 OLIN-BEBET.                                                       
011300*                                 BETALNINGSANSVARIG NAMN                 
011400*                                 NAME OF PAYER                           
011500        05 OLIN-BEBETRAD-1   PIC X(35).                                   
011600*                                 DEL AV BETALNINGSANSVARIGS NAMN         
011700*                                 PART OF FINANCIAL CUSTOMER NAME         
011800        05 OLIN-BEBETRAD-2   PIC X(35).                                   
011900*                                 DEL AV BETALNINGSANSVARIGS NAMN         
012000*                                 PART OF FINANCIAL CUSTOMER NAME         
012100     03 OLIN-ADBET.                                                       
012200*                                 BETALNINGSANSVARIG ADRESS               
012300*                                 ADDRESS OF PAYER                        
012400        05 OLIN-ADBETRAD-1   PIC X(35).                                   
012500*                                 ADRESSRAD BETALNINGSANSVARIG            
012600*                                 PART OF FINANCIAL CUSTOMER ADDR         
012700*                                 ESS                                     
012800        05 OLIN-ADBETRAD-2   PIC X(35).                                   
012900*                                 ADRESSRAD BETALNINGSANSVARIG            
013000*                                 PART OF FINANCIAL CUSTOMER ADDR         
013100*                                 ESS                                     
013200        05 OLIN-ADBETRAD-3   PIC X(35).                                   
013300*                                 ADRESSRAD BETALNINGSANSVARIG            
013400*                                 PART OF FINANCIAL CUSTOMER ADDR         
013500*                                 ESS                                     
013600*** END OF VILMAII-COPY LENGTH= 479 BYTES                                 
