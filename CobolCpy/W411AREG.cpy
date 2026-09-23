000100 01  AREG-W411AREG.                                                       
000200*                                 LÄNKAREA TILL W411AREG -                
000300*                                 LÄSNING ARTIKELREGISTER                 
000400     03 AREG-IDARTNR         PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 AREG-IDDC            PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 AREG-W411AREG-001.                                                
001100        05 AREG-CDC-UNIK-INFO.                                            
001200           07 AREG-ADART.                                                 
001300*                                 ARTIKELADRESS I LAGRET                  
001400*                                 PARTS-ADRESS                            
001500              09 AREG-ADLAGOMR                                            
001600                             PIC S9(3)           COMP-3.                  
001700*                                 LAGEROMRÅDE                             
001800*                                 AREA                                    
001900              09 AREG-ADGANG PIC S9(3)           COMP-3.                  
002000*                                 GÅNG                                    
002100*                                 AISLE                                   
002200              09 AREG-ADPLATS                                             
002300                             PIC S9(5)           COMP-3.                  
002400*                                 LAGERPLATSNUMMER                        
002500*                                 LOCATION                                
002600           07 AREG-FILLER1   PIC X(7).                                    
002700           07 AREG-FLAVRART  PIC X.                                       
002800*                                 AVROPSARTIKEL                           
002900           07 AREG-FLIART    PIC X.                                       
003000*                                 ARTIKELN INGÅR I SATS                   
003100*                                 PART IN KIT                             
003200           07 AREG-FLLSRDEL  PIC X.                                       
003300*                                 LEVERERAS SOM RESDEL                    
003400           07 AREG-FLMARKSP  PIC X.                                       
003500*                                 MARKNADSSPÄRR                           
003600*                                 MARKET BLOCKING CODE                    
003700           07 AREG-FLRADREF  PIC X.                                       
003800*                                 KOMPLETTERANDE INFO. KRÄVS              
003900*                                 ADDITIONAL INFORMATION REQUIRED         
004000           07 AREG-FLREFILL  PIC X.                                       
004100*                                 REFILLARTIKEL                           
004200*                                 REFILLPART                              
004300           07 AREG-FLTPO1    PIC X.                                       
004400*                                 ARTIKELN GODKÄND FÖR TPO1               
004500*                                 TPO1 ALLOWED FOR ARTICLE                
004600           07 AREG-IDFKNGRP  PIC S9(5)           COMP-3.                  
004700*                                 FUNKTIONSGRUPP                          
004800*                                 FUNCTION GROUP                          
004900           07 AREG-IDLEVNR   PIC X(5).                                    
005000*                                 LEVERANTÖRNUMMER                        
005100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005200           07 AREG-IDLKTO    PIC S9(7)           COMP-3.                  
005300*                                 LAGERKONTO (FFHHHUU)                    
005400*                                 STOCK ACCOUNT (CCMMMSS)                 
005500           07 AREG-IDPSN     PIC 9(3).                                    
005600*                                 PROPER SHIPPING NAME                    
005700*                                 PROPER SHIPPING NAME                    
005800           07 AREG-KDERS     PIC S9(3)           COMP-3.                  
005900*                                 ERSÄTTNINGSKOD                          
006000*                                 SUPERSESSION CODE                       
006100           07 AREG-KDERS-UTG PIC S9(3)           COMP-3.                  
006200*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
006300*                                 OBSOLETION SUPERSESSION CODE            
006400           07 AREG-KDFARLIG  PIC S9              COMP-3.                  
006500*                                 KOD FÖR FARLIGT GODS                    
006600*                                 DANGEROUS GOODS CODE                    
006700           07 AREG-KDLEVSP   PIC S9(3)           COMP-3.                  
006800*                                 SPÄRRKOD LEVERANS                       
006900*                                 DELIVERY BLOCKING CODE                  
007000           07 AREG-KDPRODSL  PIC S9(3)           COMP-3.                  
007100*                                 PRODUKTSLAG                             
007200*                                 PRODUCT GROUP                           
007300           07 AREG-KDSORT    PIC X(2).                                    
007400*                                 SORT-KOD                                
007500*                                 UNIT OF MEASURE                         
007600           07 AREG-KDSPEEMB  PIC 9.                                       
007700*                                 SPECIALEMBALLAGEKOD                     
007800*                                 SPECIAL PACKING CODE                    
007900           07 AREG-KDUART    PIC X.                                       
008000*                                 UNDANTAGSARTIKEL                        
008100*                                 EXECPTION PARTS                         
008200           07 AREG-KDVVKL    PIC S9              COMP-3.                  
008300*                                 VOLYMVÄRDESKLASS                        
008400*                                 VOLUME VALUE CLASS                      
008500           07 AREG-KVAKS-CDC PIC S9(7)           COMP-3.                  
008600*                                 DEL AV AK SOM LIGGER I CDC              
008700*                                 PART OF AK IN THE CDC                   
008800           07 AREG-KVAKS-PAV PIC S9(7)           COMP-3.                  
008900*                                 DEL AV AK PÅ VÄG                        
009000*                                 PART OF AK ON ITS WAY                   
009100           07 AREG-KVFRYSTI  PIC S9(3)           COMP-3.                  
009200*                                 FRYSTID FÖR TPO-ORDER                   
009300*                                 FREEZTIME FOR TPO                       
009400           07 AREG-KVLS      PIC S9(7)           COMP-3.                  
009500*                                 LAGERSALDO                              
009600*                                 STOCK BALANCE                           
009700           07 AREG-KVLS-SVS  PIC S9(7)           COMP-3.                  
009800*                                 LAGERSALDO SVS                          
009900*                                 STOCK BALANCE SVS                       
010000           07 AREG-KDVSOP    PIC S9(3)           COMP-3.                  
010100*                                 VSOP-KOD                                
010200*                                 VSOP-CODE                               
010300           07 AREG-FILLER2   PIC X(2).                                    
010400           07 AREG-KVPB-SATS PIC S9(6)V9(1)      COMP-3.                  
010500*                                 SATS-PERIODBEHOV                        
010600*                                 KIT PERIOD REQUIREMENTS                 
010700           07 AREG-KVPB-SEP  PIC S9(6)V9(1)      COMP-3.                  
010800*                                 SEPARAT PERIODBEHOV                     
010900*                                 SEPARATE PERIOD REQUIREMENTS            
011000           07 AREG-KVPB-TPO  PIC S9(6)V9(1)      COMP-3.                  
011100*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
011200*                                 PERIODICAL DEMAND TPO1 AND TPO2         
011300*                                                                         
011400           07 AREG-KVQPACK-0 PIC S9(5)           COMP-3.                  
011500*                                 ANTAL I Q0 FÖRPACKNING                  
011600           07 AREG-KVQPACK-1 PIC S9(5)           COMP-3.                  
011700*                                 ANTAL I Q1 FÖRPACKNING                  
011800*                                 QUANTITY IN BULK PACK Q1                
011900           07 AREG-KVQPACK-3 PIC S9(5)           COMP-3.                  
012000*                                 ANTAL I Q3 FÖRPACKNING                  
012100*                                 QUANTITY IN BULK PACK Q3                
012200           07 AREG-KVQPACK-4 PIC S9(5)           COMP-3.                  
012300*                                 ANTAL I Q4 FÖRPACKNING                  
012400*                                 QUANTITY IN BULK PACK Q4                
012500           07 AREG-KVRESS    PIC S9(7)           COMP-3.                  
012600*                                 RESERVERAT ANTAL ARTIKLAR               
012700*                                 QUANTITY RESERVED ITEMS                 
012800           07 AREG-KVROS     PIC S9(7)           COMP-3.                  
012900*                                 RESTORDERSALDO                          
013000*                                 BACKORDER QTY                           
013100           07 AREG-KVUTRS    PIC S9(7)           COMP-3.                  
013200*                                 UTREDNINGSSALDO                         
013300*                                 INVESTIGATION BALANCE                   
013400           07 AREG-PRARTSTD  PIC S9(7)V9(2)      COMP-3.                  
013500*                                 ARTIKELSTANDARDPRIS                     
013600*                                 STANDARD PRICE                          
013700           07 AREG-REDIRLEV  PIC S9V9(2)         COMP-3.                  
013800*                                 DIREKTLEVERANSANDEL                     
013900           07 AREG-REKSIFFR  PIC S9              COMP-3.                  
014000*                                 KONTROLLSIFFRA                          
014100*                                 PART NO CHECK DIGIT                     
014200           07 AREG-TIDISPIN  PIC S9(7)           COMP-3.                  
014300*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
014400*                                 NEXT CONSIGNMENT AVAIL.(YYMMDD)         
014500           07 AREG-TIFINLV   PIC S9(5)           COMP-3.                  
014600*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
014700*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
014800           07 AREG-FLRELSP   PIC X.                                       
014900*                                 RELEASEBLOCKAD ARTIKEL .                
015000*                                 BLOCKED PART                            
015100           07 AREG-KVSPARR-KVAL                                           
015200                             PIC S9(7)           COMP-3.                  
015300*                                 SPÄRRAT ANTAL KVALITETSFEL              
015400*                                 BLOCKED QUANTITY QUALITY ERROR          
015500           07 AREG-IDUSER-SPKVAL                                          
015600                             PIC X(8).                                    
015700*                                 ANVÄNDAR-ID KVALITETSPÄRR               
015800*                                 USER ID QUALITY ERROR                   
015900           07 AREG-FLCCC     PIC X.                                       
016000*                                 CHINA COMPULSORY CERTIFICATE            
016100*                                 CHINA COMPULSORY CERTIFICATE            
016200           07 AREG-FILLER3   PIC X.                                       
016300        05 AREG-DC-UNIK-INFO.                                             
016400           07 AREG-FLCDCBEH  PIC X.                                       
016500*                                 REFILLBEHOV                             
016600*                                 REFILL NEEDED                           
016700           07 AREG-IDDC-REC  PIC X(2).                                    
016800*                                 MOTTAGANDE LAGER                        
016900*                                 RECEIVING WAREHOUSE                     
017000        05 AREG-MULTIPEL-INFO.                                            
017100           07 AREG-IDANSK    PIC S9(3)           COMP-3.                  
017200*                                 ANSKAFFARNUMMER                         
017300*                                 PROCURER NO.                            
017400           07 AREG-KDARTURS  PIC X(2).                                    
017500*                                 ARTIKELURSPRUNGSKOD                     
017600*                                 COUNTRY OF ORIGIN                       
017700           07 AREG-KVSLUTKP  PIC S9(7)           COMP-3.                  
017800*                                 SLUTKÖPSSALDO                           
017900           07 AREG-KVSPANT   PIC S9(7)           COMP-3.                  
018000*                                 SPÄRRAT ANTAL                           
018100*                                 BLOCKED QTY                             
018200           07 AREG-VKART     PIC S9(7)           COMP-3.                  
018300*                                 ARTIKELVIKT (G)                         
018400*                                 PART WEIGHT (G)                         
018500           07 AREG-VKART-NTO PIC S9(9)           COMP-3.                  
018600*                                 ARTIKELNS NETTOVIKT                     
018700*                                 PART NET WEIGHT                         
018800           07 AREG-VLARTNTO  PIC S9(8)V9(1)      COMP-3.                  
018900*                                 ARTIKELVOLYM (CM3)                      
019000*                                 PART VOLUME    (CM3)                    
019100     03 AREG-KDORDBEK        PIC 9(2).                                    
019200*                                 ORDERBEKRÄFTELSEKOD                     
019300*                                 ORDERCONFIMATIONCODE                    
019400*** END OF VILMAII-COPY LENGTH= 176 BYTES                                 
