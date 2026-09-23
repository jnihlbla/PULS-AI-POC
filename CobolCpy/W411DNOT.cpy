000100 01  DNOT-W411DNOT.                                                       
000200*                                 LÄNKAREA TILL W411DNOT -                
000300*                                 UPPDATERAR WDQ5                         
000400*                                 ORDERRAD/BEKRÄFTELSE FRÅN               
000500*                                 VIPS                                    
000600*                                 DELIVERY NOTE                           
000700     03 DNOT-IDPGM           PIC X(8).                                    
000800*                                 PROGRAM IDENTITET                       
000900*                                 PROGRAM INTENTITY                       
001000     03 DNOT-FL-OHUV-OK      PIC X.                                       
001100*                                 ALLMÄN FLAGGA                           
001200*                                 GENERAL FLAG                            
001300     03 DNOT-FL-ORAD-LAST    PIC X.                                       
001400*                                 ALLMÄN FLAGGA                           
001500*                                 GENERAL FLAG                            
001600     03 DNOT-FL-IDPLOCK-OK   PIC X.                                       
001700*                                 ALLMÄN FLAGGA                           
001800*                                 GENERAL FLAG                            
001900     03 DNOT-ORDER-INFO.                                                  
002000        05 DNOT-IDORDER      PIC S9(7)           COMP-3.                  
002100*                                 VOLVO PARTS ORDERNUMMER                 
002200*                                 VOLVO PARTS ORDER NUMBER                
002300        05 DNOT-IDARTNR      PIC S9(9)           COMP-3.                  
002400*                                 ARTIKELNUMMER                           
002500*                                 PART NUMBER                             
002600        05 DNOT-IDLOPNR      PIC S9(3)           COMP-3.                  
002700*                                 LÖPNUMMER                               
002800*                                 SEQUENCE NUMBER                         
002900        05 DNOT-IDSEKVNR     PIC S9(3)           COMP-3.                  
003000*                                 GENERELLT SEKVENSNUMMER                 
003100*                                 GENERAL SEQUENCE NUMBER                 
003200        05 DNOT-IDDC         PIC X(2).                                    
003300*                                 IDENTIFIERARE LAGER                     
003400*                                 WAREHOUSE IDENTIFIER                    
003500        05 DNOT-KDORDBEK     PIC 9(2).                                    
003600*                                 ORDERBEKRÄFTELSEKOD                     
003700*                                 ORDERCONFIMATIONCODE                    
003800        05 DNOT-ADGMT.                                                    
003900*                                 GODSMOTTAGARADRESS                      
004000*                                 GOODS RECEIVER ADDRESS                  
004100           07 DNOT-ADGMT-GATA                                             
004200                             PIC X(35).                                   
004300*                                 GODSMOTTAGARADRESS GATA                 
004400*                                 GOODS RECEIVER ADDRESS STREET           
004500           07 DNOT-ADGMT-PADR                                             
004600                             PIC X(35).                                   
004700*                                 GODSMOTTAGARADRESS POSTADRESS           
004800*                                 GOODS RECEIVER ADDRESS TOWN             
004900           07 DNOT-ADPOST-PNRORT REDEFINES DNOT-ADGMT-PADR.               
005000*                                 POSTNUMMER + ORT                        
005100*                                 POSTAL CODE + CITY                      
005200              09 DNOT-ADPOSTNR                                            
005300                             PIC X(10).                                   
005400*                                 POSTNUMMER I ADRESS                     
005500*                                 POSTAL CODE IN ADDRESS                  
005600              09 DNOT-ADCITY PIC X(25).                                   
005700*                                 BENÄMNING PÅ STAD                       
005800*                                 CITY                                    
005900           07 DNOT-ADPOST-ORTPNR REDEFINES DNOT-ADGMT-PADR.               
006000*                                 ORT + POSTNUMMER                        
006100*                                 CITY + POSTAL CODE                      
006200              09 DNOT-ADCITY PIC X(25).                                   
006300*                                 BENÄMNING PÅ STAD                       
006400*                                 CITY                                    
006500              09 DNOT-ADPOSTNR                                            
006600                             PIC X(10).                                   
006700*                                 POSTNUMMER I ADRESS                     
006800*                                 POSTAL CODE IN ADDRESS                  
006900           07 DNOT-ADGMT-LAND                                             
007000                             PIC X(35).                                   
007100*                                 GODSMOTTAGARADRESS LAND                 
007200*                                 GOODS RECEIVER ADDRESS COUNTRY          
007300        05 DNOT-BEART-USA    PIC X(25).                                   
007400*                                 AMERIKANSK ART.BENÄMNING                
007500        05 DNOT-BEGMT.                                                    
007600*                                 GODSMOTTAGARNAMN                        
007700*                                 GOODS RECEIVER NAME                     
007800           07 DNOT-BEGMT-RAD1                                             
007900                             PIC X(35).                                   
008000*                                 GODSMOTTAGARNAMN RAD 1                  
008100*                                 GOODS RECEIVER NAME LINE 1              
008200           07 DNOT-BEGMT-RAD2                                             
008300                             PIC X(35).                                   
008400*                                 GODSMOTTAGARNAMN RAD 2                  
008500*                                 GOODS RECEIVER NAME LINE 2              
008600        05 DNOT-BEKUNDRF     PIC X(15).                                   
008700*                                 KUNDENS REFERENS                        
008800*                                 CUSTOMERS REFERENCE                     
008900        05 DNOT-BERADREF     PIC X(10).                                   
009000*                                 KUNDENS RADREFERENS                     
009100*                                 CUSTOMERS ITEM REF.                     
009200        05 DNOT-FLDIRLEV     PIC X.                                       
009300*                                 DIREKTLEVERANS ?                        
009400*                                 DIRECT DELIVERY ?                       
009500        05 DNOT-FLTILLK      PIC X.                                       
009600*                                 TILLKOMMANDE ARTIKEL ?                  
009700*                                 REPLACEMENT PART FLAG                   
009800        05 DNOT-IDDC-RO      PIC X(2).                                    
009900*                                 LAGER DÄR RESTORDER FÅR SKE             
010000*                                 WAREHOUSE FOR BACKORDERS                
010100        05 DNOT-IDDC-PRIM    PIC X(2).                                    
010200*                                 PRIMÄRT LEVERERANDE LAGER               
010300*                                 PRIMARY DELIVERING DC                   
010400        05 DNOT-IDKOLLI-BORT PIC S9(5)           COMP-3.                  
010500*                                 KOLLINUMMER                             
010600*                                 CASE NUMBER                             
010700        05 DNOT-IDKOLLI      PIC S9(5)           COMP-3.                  
010800*                                 KOLLINUMMER                             
010900*                                 CASE NUMBER                             
011000        05 DNOT-IDKUNDRF-RO  PIC X(10).                                   
011100*                                 KUND REF PÅ RO                          
011200*                                 CUST REF RO                             
011300        05 DNOT-KVLEVART-TOT PIC S9(7)           COMP-3.                  
011400*                                 TOTALT LEVERERAT ANTAL ARTIKLAR         
011500*                                 TOTAL NUMBER OF DELIVERED QTY           
011600        05 DNOT-IDPRODNR     PIC S9(7)           COMP-3.                  
011700*                                 PRODUKTIONSNUMMER                       
011800*                                 PRODUCTION NUMBER                       
011900        05 DNOT-IDGMTREF.                                                 
012000*                                 GODSMOTTAGAREREFERENS                   
012100*                                 GOODS RECEIVER REFERENS                 
012200           07 DNOT-IDDISTR   PIC S9(5)           COMP-3.                  
012300*                                 DISTRIKTNUMMER                          
012400*                                 DISTRICT NUMBER                         
012500           07 DNOT-IDKUNDNR  PIC S9(7)           COMP-3.                  
012600*                                 KUNDNUMMER                              
012700*                                 CUSTOMER NO                             
012800           07 DNOT-IDKUNDRF-GRP.                                          
012900*                                 KUNDENS REFERENS (ORDERID)              
013000*                                 CUSTOMER REFERENCE (ORDER ID)           
013100              09 DNOT-IDKUNDRF                                            
013200                             PIC X(10).                                   
013300*                                 KUNDENS REFERENS (ORDERID)              
013400*                                 CUSTOMER REFERENCE (ORDER ID)           
013500              09 DNOT-IDORDNR5-FILLER REDEFINES DNOT-IDKUNDRF.            
013600                 11 DNOT-IDORDNR5                                         
013700                             PIC 9(5).                                    
013800*                                 ORDERNUMMER                             
013900*                                 ORDER NUMBER                            
014000                 11 FILLER   PIC X(5).                                    
014100              09 DNOT-IDORDNR7-FILLER REDEFINES DNOT-IDKUNDRF.            
014200                 11 DNOT-IDORDNR7                                         
014300                             PIC 9(7).                                    
014400*                                 ORDERNUMMER                             
014500*                                 ORDER NUMBER                            
014600                 11 FILLER   PIC X(3).                                    
014700        05 DNOT-IDPURAD      PIC S9(5)           COMP-3.                  
014800*                                 RADNUMMER PÅ PACKUNDERLAG               
014900*                                 LINENO IN PACKINGDOCUMENT               
015000        05 DNOT-KDFRAKT      PIC S9(3)           COMP-3.                  
015100*                                 FRAKTSÄTT DC TILL KUND                  
015200*                                 FREIGHT CODE                            
015300        05 DNOT-KDORDKL      PIC S9              COMP-3.                  
015400*                                 ORDERKLASS                              
015500*                                 ORDER CLASS                             
015600        05 DNOT-KVLEVART     PIC S9(7)           COMP-3.                  
015700*                                 LEVERERAT ANTAL STYCK                   
015800*                                 DELIVERED QUANTITY                      
015900        05 DNOT-KVBEART      PIC S9(7)           COMP-3.                  
016000*                                 BESTÄLLT ANTAL STYCKEN                  
016100*                                 ORDERED QUANTITY                        
016200        05 DNOT-KVBEART-Q    PIC S9(7)           COMP-3.                  
016300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
016400*                                 ORDERED QUANTITY ADAPTED                
016500*                                  ITEMS                                  
016600        05 DNOT-REKSIFFR     PIC S9              COMP-3.                  
016700*                                 KONTROLLSIFFRA                          
016800*                                 PART NO CHECK DIGIT                     
016900        05 DNOT-TIREGDAT     PIC S9(7)           COMP-3.                  
017000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
017100*                                 REGISTRATION DATE (YYMMDD)              
017200        05 DNOT-TIREGTID     PIC S9(7)           COMP-3.                  
017300*                                 REGISTRERINGSTID                        
017400*                                 GENERAL REGISTRATION TIME               
017500     03 DNOT-KDSVAR          PIC X.                                       
017600*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
017700*                                 RETURN CODE FROM PROGRAM                
017800*** END OF VILMAII-COPY LENGTH= 328 BYTES                                 
