000100 01  RADB-WDQ501.                                                         
000200*                                 ORDERRAD/BEKRÄFTELSE FRÅN VIPS          
000300*                                 FYSISK NYCKEL: WDQ501KY:                
000400*                                 IDORDER  + IDARTNR + IDLOPNR +          
000500*                                 IDSEKVNR + IDDC    + KDORDBEK           
000600     03 RADB-IDORDER         PIC S9(7)           COMP-3.                  
000700*                                 VOLVO PARTS ORDERNUMMER                 
000800*                                 VOLVO PARTS ORDER NUMBER                
000900     03 RADB-IDARTNR         PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200     03 RADB-IDLOPNR         PIC S9(3)           COMP-3.                  
001300*                                 LÖPNUMMER                               
001400*                                 SEQUENCE NUMBER                         
001500     03 RADB-IDSEKVNR        PIC S9(3)           COMP-3.                  
001600*                                 GENERELLT SEKVENSNUMMER                 
001700*                                 GENERAL SEQUENCE NUMBER                 
001800     03 RADB-IDDC            PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000*                                 WAREHOUSE IDENTIFIER                    
002100     03 RADB-KDORDBEK        PIC 9(2).                                    
002200*                                 ORDERBEKRÄFTELSEKOD                     
002300*                                 ORDERCONFIMATIONCODE                    
002400     03 RADB-ADGMT.                                                       
002500*                                 GODSMOTTAGARADRESS                      
002600*                                 GOODS RECEIVER ADDRESS                  
002700        05 RADB-ADGMT-GATA   PIC X(35).                                   
002800*                                 GODSMOTTAGARADRESS GATA                 
002900*                                 GOODS RECEIVER ADDRESS STREET           
003000        05 RADB-ADGMT-PADR   PIC X(35).                                   
003100*                                 GODSMOTTAGARADRESS POSTADRESS           
003200*                                 GOODS RECEIVER ADDRESS TOWN             
003300        05 RADB-ADPOST-PNRORT REDEFINES RADB-ADGMT-PADR.                  
003400*                                 POSTNUMMER + ORT                        
003500*                                 POSTAL CODE + CITY                      
003600           07 RADB-ADPOSTNR  PIC X(10).                                   
003700*                                 POSTNUMMER I ADRESS                     
003800*                                 POSTAL CODE IN ADDRESS                  
003900           07 RADB-ADCITY    PIC X(25).                                   
004000*                                 BENÄMNING PÅ STAD                       
004100*                                 CITY                                    
004200        05 RADB-ADPOST-ORTPNR REDEFINES RADB-ADGMT-PADR.                  
004300*                                 ORT + POSTNUMMER                        
004400*                                 CITY + POSTAL CODE                      
004500           07 RADB-ADCITY    PIC X(25).                                   
004600*                                 BENÄMNING PÅ STAD                       
004700*                                 CITY                                    
004800           07 RADB-ADPOSTNR  PIC X(10).                                   
004900*                                 POSTNUMMER I ADRESS                     
005000*                                 POSTAL CODE IN ADDRESS                  
005100        05 RADB-ADGMT-LAND   PIC X(35).                                   
005200*                                 GODSMOTTAGARADRESS LAND                 
005300*                                 GOODS RECEIVER ADDRESS COUNTRY          
005400     03 RADB-BEART-USA       PIC X(25).                                   
005500*                                 AMERIKANSK ART.BENÄMNING                
005600     03 RADB-BEGMT.                                                       
005700*                                 GODSMOTTAGARNAMN                        
005800*                                 GOODS RECEIVER NAME                     
005900        05 RADB-BEGMT-RAD1   PIC X(35).                                   
006000*                                 GODSMOTTAGARNAMN RAD 1                  
006100*                                 GOODS RECEIVER NAME LINE 1              
006200        05 RADB-BEGMT-RAD2   PIC X(35).                                   
006300*                                 GODSMOTTAGARNAMN RAD 2                  
006400*                                 GOODS RECEIVER NAME LINE 2              
006500     03 RADB-BEKUNDRF        PIC X(15).                                   
006600*                                 KUNDENS REFERENS                        
006700*                                 CUSTOMERS REFERENCE                     
006800     03 RADB-BERADREF        PIC X(10).                                   
006900*                                 KUNDENS RADREFERENS                     
007000*                                 CUSTOMERS ITEM REF.                     
007100     03 RADB-FLDIRLEV        PIC X.                                       
007200*                                 DIREKTLEVERANS ?                        
007300*                                 DIRECT DELIVERY ?                       
007400     03 RADB-FLTILLK         PIC X.                                       
007500*                                 TILLKOMMANDE ARTIKEL ?                  
007600*                                 REPLACEMENT PART FLAG                   
007700     03 RADB-IDARTNR-TILLK   PIC S9(9)           COMP-3.                  
007800*                                 TILLKOMMANDE ARTIKELNUMMER              
007900*                                 REPLACEMENT PART NO.                    
008000     03 RADB-IDKOLLI         PIC S9(5)           COMP-3.                  
008100*                                 KOLLINUMMER                             
008200*                                 CASE NUMBER                             
008300     03 RADB-IDKUNDRF-RO     PIC X(10).                                   
008400*                                 KUND REF PÅ RO                          
008500*                                 CUST REF RO                             
008600     03 RADB-KVLEVART-TOT    PIC S9(7)           COMP-3.                  
008700*                                 TOTALT LEVERERAT ANTAL ARTIKLAR         
008800*                                 TOTAL NUMBER OF DELIVERED QTY           
008900     03 RADB-IDPRODNR        PIC S9(7)           COMP-3.                  
009000*                                 PRODUKTIONSNUMMER                       
009100*                                 PRODUCTION NUMBER                       
009200     03 RADB-IDGMTREF.                                                    
009300*                                 GODSMOTTAGAREREFERENS                   
009400*                                 GOODS RECEIVER REFERENS                 
009500        05 RADB-IDDISTR      PIC S9(5)           COMP-3.                  
009600*                                 DISTRIKTNUMMER                          
009700*                                 DISTRICT NUMBER                         
009800        05 RADB-IDKUNDNR     PIC S9(7)           COMP-3.                  
009900*                                 KUNDNUMMER                              
010000*                                 CUSTOMER NO                             
010100        05 RADB-IDKUNDRF-GRP.                                             
010200*                                 KUNDENS REFERENS (ORDERID)              
010300*                                 CUSTOMER REFERENCE (ORDER ID)           
010400           07 RADB-IDKUNDRF  PIC X(10).                                   
010500*                                 KUNDENS REFERENS (ORDERID)              
010600*                                 CUSTOMER REFERENCE (ORDER ID)           
010700           07 RADB-IDORDNR5-FILLER REDEFINES RADB-IDKUNDRF.               
010800              09 RADB-IDORDNR5                                            
010900                             PIC 9(5).                                    
011000*                                 ORDERNUMMER                             
011100*                                 ORDER NUMBER                            
011200              09 FILLER      PIC X(5).                                    
011300           07 RADB-IDORDNR7-FILLER REDEFINES RADB-IDKUNDRF.               
011400              09 RADB-IDORDNR7                                            
011500                             PIC 9(7).                                    
011600*                                 ORDERNUMMER                             
011700*                                 ORDER NUMBER                            
011800              09 FILLER      PIC X(3).                                    
011900     03 RADB-KDFRAKT         PIC S9(3)           COMP-3.                  
012000*                                 FRAKTSÄTT DC TILL KUND                  
012100*                                 FREIGHT CODE                            
012200     03 RADB-KDORDKL         PIC S9              COMP-3.                  
012300*                                 ORDERKLASS                              
012400*                                 ORDER CLASS                             
012500     03 RADB-KVLEVART        PIC S9(7)           COMP-3.                  
012600*                                 LEVERERAT ANTAL STYCK                   
012700*                                 DELIVERED QUANTITY                      
012800     03 RADB-KVBEART         PIC S9(7)           COMP-3.                  
012900*                                 BESTÄLLT ANTAL STYCKEN                  
013000*                                 ORDERED QUANTITY                        
013100     03 RADB-KVBEART-Q       PIC S9(7)           COMP-3.                  
013200*                                 BESTÄLLT KVANTANPASSAT ANTAL            
013300*                                 ORDERED QUANTITY ADAPTED                
013400*                                  ITEMS                                  
013500     03 RADB-REKSIFFR        PIC S9              COMP-3.                  
013600*                                 KONTROLLSIFFRA                          
013700*                                 PART NO CHECK DIGIT                     
013800     03 RADB-TIREGDAT        PIC S9(7)           COMP-3.                  
013900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
014000*                                 REGISTRATION DATE (YYMMDD)              
014100     03 RADB-TIREGTID        PIC S9(7)           COMP-3.                  
014200*                                 REGISTRERINGSTID                        
014300*                                 GENERAL REGISTRATION TIME               
014400     03 RADB-TIUPPDAT        PIC S9(7)           COMP-3.                  
014500*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
014600*                                 UPDATING DATE     (YYMMDD)              
014700     03 RADB-IDPURAD         PIC S9(5)           COMP-3.                  
014800*                                 RADNUMMER PÅ PACKUNDERLAG               
014900*                                 LINENO IN PACKINGDOCUMENT               
015000     03 RADB-FILLER          PIC X(8).                                    
015100*** END OF VILMAII-COPY LENGTH= 326 BYTES                                 
