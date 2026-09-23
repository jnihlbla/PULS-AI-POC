000100 01  WHEV-W403WHEV.                                                       
000200*                                 PART TRACEBILITY       -                
000300     03 WHEV-KDFUNC          PIC X(10).                                   
000400*                                 FUNKTIONSKOD                            
000500*                                 FUNCTION CODE                           
000600     03 WHEV-IDORIGSYS       PIC X(8).                                    
000700*                                 SYSTEMET MED URSPRUNGETS IDENTI         
000800*                                 TET                                     
000900*                                 IDENTITY OF ORIGIN SYSTEM               
001500     03 WHEV-EVENTLOCATION   PIC X(10).                                   
001600     03 WHEV-USERID          PIC X(10).                                   
001700     03 WHEV-USERID-NEW      PIC X(10).                                   
001800     03 WHEV-IDPRODNR        PIC 9(7).                                    
001900*                                 PRODUKTIONSNUMMER                       
002000*                                 PRODUCTION NUMBER                       
002100     03 WHEV-IDPLKLST        PIC 9(3).                                    
002200*                                 PLOCKLISTNUMMER                         
002300*                                 PICKING LIST NUMBER                     
002400     03 WHEV-IDKOLLI         PIC 9(5).                                    
002500*                                 KOLLINUMMER                             
002600*                                 CASE NUMBER                             
002700     03 WHEV-IDKOLLI-NEW     PIC 9(5).                                    
002800*                                 KOLLINUMMER                             
002900*                                 CASE NUMBER                             
003000     03 WHEV-KDKOLLI         PIC X(8).                                    
003100*                                 KOLLIKOD                                
003200*                                 KOLLI CODE                              
003300     03 WHEV-IDPURAD         PIC 9(4).                                    
003400*                                 RADNUMMER PÅ PACKUNDERLAG               
003500*                                 LINENO IN PACKINGDOCUMENT               
003600     03 WHEV-KVLEVART        PIC 9(7).                                    
003700*                                 LEVERERAT ANTAL STYCK                   
003800*                                 DELIVERED QUANTITY                      
003900     03 WHEV-KDEMBTYP        PIC 9.                                       
004000*                                 EMBALLAGETYP                            
004100*                                 PACKAGE TYPE                            
004200     03 WHEV-DIKOLLIL        PIC 9(4).                                    
004300*                                 KOLLI-LÄNGD                             
004400*                                 CASE LENGTH                             
004500     03 WHEV-DIKOLLIB        PIC 9(4).                                    
004600*                                 KOLLI-BREDD                             
004700*                                 CASE WIDTH                              
004800     03 WHEV-DIKOLLIH        PIC 9(4).                                    
004900*                                 KOLLI-HÖJD                              
005000*                                 CASE HEIGHT                             
005100     03 WHEV-KDARTURS        PIC X(2).                                    
005200*                                 ARTIKELURSPRUNGSKOD                     
005300*                                 COUNTRY OF ORIGIN                       
005400     03 WHEV-IDARTNR         PIC X(30).                                   
005500     03 WHEV-IDLEVNR         PIC X(5).                                    
005600*                                 LEVERANTÖRNUMMER                        
005700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005800     03 WHEV-IDLOPNR-ORD     PIC 9(3).                                    
005900*                                 ORDERNS ORDNINGSNUMMER INOM             
006000*                                 EN PLOCKSATS                            
006100*                                 SEQUENCE-NUMBER FOR AN ORDER            
006200*                                 WITHIN A PICKING UNIT                   
006300     03 WHEV-EVENT-TIMESTAMP PIC X(26).                                   
006400     03 WHEV-ADLAGOMR        PIC 9(2).                                    
006500*                                 LAGEROMRÅDE                             
006600*                                 AREA                                    
006700     03 WHEV-ADGANG          PIC 9(2).                                    
006800*                                 GÅNG                                    
006900*                                 AISLE                                   
007000     03 WHEV-ADPLATS         PIC 9(5).                                    
007100*                                 LAGERPLATSNUMMER                        
007200*                                 LOCATION                                
007300     03 WHEV-IDPSN           PIC 9(3).                                    
007400*                                 PROPER SHIPPING NAME                    
007500*                                 PROPER SHIPPING NAME                    
007600     03 WHEV-IDPRC           PIC X(4).                                    
007700     03 WHEV-IDLOTNR-PLK     PIC 9(3).                                    
007800*                                 VAGN-NUMMER FÖR PLOCKRUNDA              
007900*                                 ORDERLOT NUMBER FOR A PICKLIST          
008000     03 WHEV-IDDISTR         PIC 9(4).                                    
008100*                                 DISTRIKTNUMMER                          
008200*                                 DISTRICT NUMBER                         
008300     03 WHEV-IDKUNDNR        PIC 9(6).                                    
008400*                                 KUNDNUMMER                              
008500*                                 CUSTOMER NO                             
008600     03 WHEV-IDORDNR7        PIC 9(7).                                    
008700*                                 ORDERNUMMER                             
008800*                                 ORDER NUMBER                            
008900     03 WHEV-BEART           PIC X(25).                                   
009000*                                 ARTIKELBENÄMNING                        
009100*                                 PART DESCRIPTION                        
009200     03 WHEV-TIRFSDAT        PIC X(10).                                   
009300*                                 KLART FÖR TRANSPORT ÅÅMMDD              
009400*                                 READY FOR SHIPMENT  YYMMDD              
009500     03 WHEV-TIRFSTID        PIC X(5).                                    
009600*                                 KLART FÖR TRANSPORT (TTMM)              
009700*                                 READY FOR SHIPMENT  (HHMM)              
009800     03 WHEV-KDSORT          PIC X(2).                                    
009900*                                 SORT-KOD                                
010000*                                 UNIT OF MEASURE                         
010100     03 WHEV-TOT-IDLOPNR-ORD PIC 9(3).                                    
010200*                                 ORDERNS ORDNINGSNUMMER INOM             
010300*                                 EN PLOCKSATS                            
010400*                                 SEQUENCE-NUMBER FOR AN ORDER            
010500*                                 WITHIN A PICKING UNIT                   
010600     03 WHEV-KVRADER-TOT     PIC 9(5).                                    
010700*                                 TOTALT ANTAL RADER                      
010800*                                 TOTAL NUMBER OF LINES                   
010900     03 WHEV-KVAKS-PAV       PIC -(7)9.                                   
011000*                                 DEL AV AK PÅ VÄG                        
011100*                                 PART OF AK ON ITS WAY                   
011200     03 WHEV-KVBEART         PIC 9(6).                                    
011300*                                 BESTÄLLT ANTAL STYCKEN                  
011400*                                 ORDERED QUANTITY                        
011500     03 WHEV-KDLEVSP         PIC 9(2).                                    
011600*                                 SPÄRRKOD LEVERANS                       
011700*                                 DELIVERY BLOCKING CODE                  
011800     03 WHEV-KVAKS           PIC -(7)9.                                   
011900*                                 ANKOMSTSALDO                            
012000*                                 ADVICED,NOT BINNED,QTY                  
012100     03 WHEV-KVPB-REF        PIC 9(6)V9(1).                               
012200*                                 PERIODBEHOV REFILLING                   
012300*                                 FORECAST REFILLING                      
012400     03 WHEV-TEKVAINF-EXT    PIC X(79).                                   
012500*                                 KVALITETS INFORMATION EXTERNT           
012600*                                 QUALITY INFORMATION PART NUMBER         
012700*                                  EXTERNAL                               
012800     03 WHEV-IDEVENT         PIC X(30).                                   
012900*                                 EVENT NAMN                              
013000*                                 EVENT NAME                              
013100     03 WHEV-IDEVENTTYP      PIC X(20).                                   
013200*                                 EVENT TYPE                              
013300*                                 EVENT TYPE                              
013400     03 WHEV-VLARTNTO        PIC S9(8)V9(1).                              
013500*                                 ARTIKELVOLYM (CM3)                      
013600*                                 PART VOLUME    (CM3)                    
013700     03 WHEV-VKARTNTO        PIC S9(4)V9(3).                              
013800*                                 ARTIKELVIKT NETTO (KG) MED EMB          
013900*                                 PART NET WEIGHT (KG) W/ PACKAGE         
014000     03 WHEV-KDFARLIG        PIC X.                                       
014100*                                 KOD FÖR FARLIGT GODS                    
014200*                                 DANGEROUS GOODS CODE                    
014300     03 WHEV-TEARTLBL        PIC X(512).                                  
014400*                                 PART LABEL INFORMATION                  
014500*                                 PART LABEL INFORMATION                  
014100*** END OF VILMAII-COPY LENGTH= 941 BYTES                                 
