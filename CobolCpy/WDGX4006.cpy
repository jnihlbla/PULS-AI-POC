000100 01  4006-WDGX4006.                                                       
000200*                                 PLOCKSATSER UNDER UTSKRIFT              
000300*                                 PLOCKETIKETTER                          
000400*                                 FYSISK NYCKEL: KY4006                   
000500*                                 (KDPRT,   KDSS-PLE, ADLAGOMR            
000600*                                  ADGANG   ADPLATS,  IDARTNR,            
000700*                                  IDLOPNR                                
000800     03 4006-KDPRT           PIC X(3).                                    
000900*                                 PRINTERKOD                              
001000*                                 PRINTERCODE                             
001100     03 4006-KDSS-PLE        PIC X.                                       
001200*                                 SIDOSKIPSKOD                            
001300*                                 CODE FOR PAGESKIP                       
001400     03 4006-ADLAGOMR        PIC S9(3)           COMP-3.                  
001500*                                 LAGEROMRÅDE                             
001600*                                 AREA                                    
001700     03 4006-ADGANG          PIC S9(3)           COMP-3.                  
001800*                                 GÅNG                                    
001900*                                 AISLE                                   
002000     03 4006-ADPLATS         PIC S9(5)           COMP-3.                  
002100*                                 LAGERPLATSNUMMER                        
002200*                                 LOCATION                                
002300     03 4006-IDARTNR         PIC S9(9)           COMP-3.                  
002400*                                 ARTIKELNUMMER                           
002500*                                 PART NUMBER                             
002600     03 4006-IDLOPNR         PIC S9(3)           COMP-3.                  
002700*                                 LÖPNUMMER                               
002800*                                 SEQUENCE NUMBER                         
002900     03 4006-ADLAGOMR-ORD    PIC S9(3)           COMP-3.                  
003000*                                 LAGEROMRÅDE                             
003100*                                 AREA                                    
003200     03 4006-ADPLATS-ORD     PIC S9(5)           COMP-3.                  
003300*                                 LAGERPLATSNUMMER                        
003400*                                 LOCATION                                
003500     03 4006-BEART           PIC X(25).                                   
003600*                                 ARTIKELBENÄMNING                        
003700*                                 PART DESCRIPTION                        
003800     03 4006-BERADREF        PIC X(10).                                   
003900*                                 KUNDENS RADREFERENS                     
004000*                                 CUSTOMERS ITEM REF.                     
004100     03 4006-FLAKPLOC        PIC X.                                       
004200*                                 ORDERRADEN SKA PLOCKAS PÅ AK            
004300*                                 ORDER LINE FROM "AK" QUEUE              
004400     03 4006-IDBORD          PIC X(3).                                    
004500*                                 PACK-BORD                               
004600*                                 PACKING TABLE                           
004700     03 4006-IDGMTREF.                                                    
004800*                                 GODSMOTTAGAREREFERENS                   
004900*                                 GOODS RECEIVER REFERENS                 
005000        05 4006-IDDISTR      PIC S9(5)           COMP-3.                  
005100*                                 DISTRIKTNUMMER                          
005200*                                 DISTRICT NUMBER                         
005300        05 4006-IDKUNDNR     PIC S9(7)           COMP-3.                  
005400*                                 KUNDNUMMER                              
005500*                                 CUSTOMER NO                             
005600        05 4006-IDKUNDRF-GRP.                                             
005700*                                 KUNDENS REFERENS (ORDERID)              
005800*                                 CUSTOMER REFERENCE (ORDER ID)           
005900           07 4006-IDKUNDRF  PIC X(10).                                   
006000*                                 KUNDENS REFERENS (ORDERID)              
006100*                                 CUSTOMER REFERENCE (ORDER ID)           
006200           07 4006-IDORDNR5-FILLER REDEFINES 4006-IDKUNDRF.               
006300              09 4006-IDORDNR5                                            
006400                             PIC 9(5).                                    
006500*                                 ORDERNUMMER                             
006600*                                 ORDER NUMBER                            
006700              09 FILLER      PIC X(5).                                    
006800           07 4006-IDORDNR7-FILLER REDEFINES 4006-IDKUNDRF.               
006900              09 4006-IDORDNR7                                            
007000                             PIC 9(7).                                    
007100*                                 ORDERNUMMER                             
007200*                                 ORDER NUMBER                            
007300              09 FILLER      PIC X(3).                                    
007400     03 4006-IDLOPNR-ORD     PIC S9(3)           COMP-3.                  
007500*                                 ORDERNS ORDNINGSNUMMER INOM             
007600*                                 EN PLOCKSATS                            
007700*                                 SEQUENCE-NUMBER FOR AN ORDER            
007800*                                 WITHIN A PICKING UNIT                   
007900     03 4006-IDLOPNR-PL      PIC S9(3)           COMP-3.                  
008000*                                 PLOCKSATSENS LÖPNUMMER INOM             
008100*                                 PRC-GRUPP                               
008200*                                 SEQUENCE-NUMBER FOR THE                 
008300*                                 PICKING UNIT WITHIN PRC-GROUP           
008400     03 4006-IDPLKLST        PIC S9(3)           COMP-3.                  
008500*                                 PLOCKLISTNUMMER                         
008600*                                 PICKING LIST NUMBER                     
008700     03 4006-IDPRC.                                                       
008800*                                 PRODUKTIONSKANAL                        
008900*                                 PRODUCTION CHANNEL                      
009000        05 4006-IDPRCBAS     PIC X(3).                                    
009100*                                 PRC-BAS                                 
009200*                                 PRC-BASIC                               
009300        05 4006-IDPRCVAR     PIC X.                                       
009400*                                 PRC-VARIANT                             
009500*                                 PRC-VARIANT                             
009600     03 4006-IDPRODNR        PIC S9(7)           COMP-3.                  
009700*                                 PRODUKTIONSNUMMER                       
009800*                                 PRODUCTION NUMBER                       
009900     03 4006-IDPSN           PIC 9(3).                                    
010000*                                 PROPER SHIPPING NAME                    
010100*                                 PROPER SHIPPING NAME                    
010200     03 4006-IDRADNR         PIC S9(5)           COMP-3.                  
010300*                                 RADNUMMER                               
010400*                                 LINE NO                                 
010500     03 4006-IDSPECEMB       PIC 9(4).                                    
010600*                                 SPECIALEMBALLAGEID                      
010700*                                 SPECIAL PACKING ID                      
010800     03 4006-IDZON           PIC X(2).                                    
010900*                                 TRANSPORTVÄG (RUTT,ZON)                 
011000*                                 TRANSPORT ROUTE (ZONE)                  
011100     03 4006-KDARTHNT        PIC S9(7)           COMP-3.                  
011200*                                 HANTERINGSKOD                           
011300*                                 HANDLING CODE                           
011400     03 4006-KDARTURS        PIC X(2).                                    
011500*                                 ARTIKELURSPRUNGSKOD                     
011600*                                 COUNTRY OF ORIGIN                       
011700     03 4006-KDEMBAL         PIC X.                                       
011800*                                 KOD FÖR ATT TALA OM EMBALLAGE-T         
011900*                                 YP                                      
012000*                                 CODE TO DESCRIBE TYPE OF PACKIN         
012100*                                 G                                       
012200     03 4006-KDFARLIG        PIC S9              COMP-3.                  
012300*                                 KOD FÖR FARLIGT GODS                    
012400*                                 DANGEROUS GOODS CODE                    
012500     03 4006-KDFRAKT         PIC S9(3)           COMP-3.                  
012600*                                 FRAKTSÄTT DC TILL KUND                  
012700*                                 FREIGHT CODE                            
012800     03 4006-KDORDKL         PIC S9              COMP-3.                  
012900*                                 ORDERKLASS                              
013000*                                 ORDER CLASS                             
013100     03 4006-KDSORT          PIC X(2).                                    
013200*                                 SORT-KOD                                
013300*                                 UNIT OF MEASURE                         
013400     03 4006-KVAVBART        PIC S9(7)           COMP-3.                  
013500*                                 AVBOKAT ANTAL ARTIKLAR                  
013600*                                 ALLOCATED QUANTITY                      
013700     03 4006-KDORDTYP-LDC    PIC X(2).                                    
013800*                                 ORDERTYP HOS DEALER                     
013900     03 4006-IDKUNDRF-WIP    PIC X(10).                                   
014000*                                 REPARATIONS ORDERNR, LDC KUND           
014100*                                 WORK ORDER NUMBER, LDC DEALER           
014200     03 4006-TIREPDAT        PIC S9(7)           COMP-3.                  
014300*                                 REPAIR DATE                             
014400*                                 REPAIR DATE                             
014500     03 4006-IDDEPT          PIC 9(2).                                    
014600*                                 AVDELNING I VERKSTAD                    
014700*                                 DEPARTMENT IN GARRAGE                   
014800     03 4006-TIRFSDAT        PIC 9(6).                                    
014900*                                 KLART FÖR TRANSPORT ÅÅMMDD              
015000*                                 READY FOR SHIPMENT  YYMMDD              
015100     03 4006-FLLDCKND        PIC X.                                       
015200*                                 FL LDC-KUND                             
015300*                                 FL LDC CUSTOMER                         
015400     03 4006-IDSYSTEM        PIC X(4).                                    
015500*                                 VOLVO VCCS SYSTEMNUMMER                 
015600*                                 VOLVO VCCS SYSTEM NUMBER                
015700     03 4006-FILLER          PIC X.                                       
015800*** END OF VILMAII-COPY LENGTH= 152 BYTES                                 
