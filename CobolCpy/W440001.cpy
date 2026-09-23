000100 01  FOR-W440001.                                                         
000200*                                 FÖRÄNDRINGAR MOT RADDATABAS             
000300     03 FOR-IDPTYP           PIC X(3).                                    
000400*                                 POSTTYP                                 
000500*                                 RECORD TYPE                             
000600     03 FOR-BERADREF         PIC X(10).                                   
000700*                                 KUNDENS RADREFERENS                     
000800*                                 CUSTOMERS ITEM REF.                     
000900     03 FOR-DIERS-ERS        PIC S9(4)V9(3)      COMP-3.                  
001000*                                 ERSATT ARTIKELANTAL                     
001100*                                 NUMBER OF SUPERSEDED                    
001200     03 FOR-DIERS-TILLK      PIC S9(4)V9(3)      COMP-3.                  
001300*                                 TILLKOMMANDE ARTIKELANTAL               
001400*                                 NUMBER OF SUPERSEDING                   
001500     03 FOR-IDANSK           PIC S9(3)           COMP-3.                  
001600*                                 ANSKAFFARNUMMER                         
001700*                                 PROCURER NO.                            
001800     03 FOR-IDARTNR          PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000*                                 PART NUMBER                             
002100     03 FOR-IDDISTR          PIC S9(5)           COMP-3.                  
002200*                                 DISTRIKTNUMMER                          
002300*                                 DISTRICT NUMBER                         
002400     03 FOR-IDKONTO          PIC S9(11)          COMP-3.                  
002500*                                 KONTO                                   
002600*                                 ACCOUNT                                 
002700     03 FOR-IDKORTNR-ERS     PIC S9(3)           COMP-3.                  
002800*                                 KORTNUMMER                              
002900*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
003000*                                 SEQUENCE NUMBER FOR EACH RECORD         
003100*                                  IN A SUPERSESSION                      
003200     03 FOR-IDKUNDNR         PIC S9(7)           COMP-3.                  
003300*                                 KUNDNUMMER                              
003400*                                 CUSTOMER NO                             
003500     03 FOR-IDKUNDRF         PIC X(10).                                   
003600*                                 KUNDENS REFERENS (ORDERID)              
003700*                                 CUSTOMER REFERENCE (ORDER ID)           
003800     03 FOR-IDKST            PIC X(10).                                   
003900*                                 KOSTNADSSTÄLLE                          
004000*                                 COST CENTRE                             
004100     03 FOR-IDLOPNRE         PIC S9(3)           COMP-3.                  
004200*                                 LÖPNUMMER ERSÄTTNING                    
004300*                                 SEQUENCE NUMBER FOR EACH SUPER-         
004400*                                 SESSION                                 
004500     03 FOR-IDDC             PIC X(2).                                    
004600*                                 IDENTIFIERARE LAGER                     
004700*                                 WAREHOUSE IDENTIFIER                    
004800     03 FOR-IDDC-RO          PIC X(2).                                    
004900*                                 LAGER DÄR RESTORDER FÅR SKE             
005000*                                 WAREHOUSE FOR BACKORDERS                
005100     03 FOR-KDOI             PIC X(2).                                    
005200*                                 ORDERINGÅNGSTYP                         
005300*                                 TYPE OF INCOMING ORDER                  
005400     03 FOR-CLEARGROUP.                                                   
005500*                                 CLEARINGAREA FÖR ORDERINGÅNG            
005600        05 FOR-CLEARAREA     OCCURS 7 TIMES.                              
005700*                                 CLEARINGAREA FÖR ORDERINGÅNG            
005800           07 FOR-IDDC-CLEAR PIC X(2).                                    
005900*                                 LAGERPRIORITERING VID                   
006000*                                 ORDERCLEARING                           
006100*                                 WAREHOUSE PRIORITY FOR ORDER            
006200*                                 CLEARING                                
006300           07 FOR-FLLF       PIC X.                                       
006400*                                 ARTIKEL LAGERFÖRES                      
006500*                                 PART IN STOCK                           
006600           07 FOR-FLCLEAR    PIC X.                                       
006700*                                 ORDERRAD CLEAR FLAGGA                   
006800*                                 CLEARING FLAG FOR ORDER LINE            
006900     03 FOR-KDDSP            PIC S9              COMP-3.                  
007000*                                 PÅVERKAN PÅ DSP                         
007100*                                 AFFECT ON DSP                           
007200     03 FOR-KDERS            PIC S9(3)           COMP-3.                  
007300*                                 ERSÄTTNINGSKOD                          
007400*                                 SUPERSESSION CODE                       
007500     03 FOR-KDFAKTYP         PIC X.                                       
007600*                                 FAKTURATYP                              
007700*                                 INVOICE TYPE                            
007800     03 FOR-KDFRAKT          PIC S9(3)           COMP-3.                  
007900*                                 FRAKTSÄTT DC TILL KUND                  
008000*                                 FREIGHT CODE                            
008100     03 FOR-KDKVBRYT         PIC S9              COMP-3.                  
008200*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
008300*                                 BREAK BULKPACK CODE                     
008400     03 FOR-KDLEVSP          PIC S9(3)           COMP-3.                  
008500*                                 SPÄRRKOD LEVERANS                       
008600*                                 DELIVERY BLOCKING CODE                  
008700     03 FOR-KDORDKL          PIC S9              COMP-3.                  
008800*                                 ORDERKLASS                              
008900*                                 ORDER CLASS                             
009000     03 FOR-KDPRODSL         PIC S9(3)           COMP-3.                  
009100*                                 PRODUKTSLAG                             
009200*                                 PRODUCT GROUP                           
009300     03 FOR-KDRAPRIO         PIC S9(3)           COMP-3.                  
009400*                                 PRIORITETSKOD PÅ RADEN                  
009500*                                 PRIORITY CODE ON THE LINE               
009600     03 FOR-KDRESTR          PIC S9(3)           COMP-3.                  
009700*                                 RESTRIKTIONSKOD                         
009800*                                 RESTRICTION CODE                        
009900     03 FOR-KDROO            PIC S9              COMP-3.                  
010000*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
010100*                                 REASON CODE FOR WAITING LINE            
010200     03 FOR-FLROSTYR         PIC X.                                       
010300*                                 RESTORDERSTYRNINGSFLAGGA                
010400*                                 BACK ORDER CLEARING FLAG                
010500     03 FOR-KDSTARAD         PIC X.                                       
010600*                                 RADSTATUSKOD                            
010700*                                 LINE STATUS CODE                        
010800     03 FOR-KDTPOTYP         PIC S9              COMP-3.                  
010900*                                 TYP AV TIDPLANERAD ORDER                
011000*                                 TYPE OF TIME PLANNED ORDER              
011100     03 FOR-KDVRINFO         PIC S9              COMP-3.                  
011200*                                 PÅVERKAN I VR/DSP SYSTEM                
011300*                                 VR/DSP UP-DATE                          
011400     03 FOR-KVART            PIC S9(7)           COMP-3.                  
011500*                                 ANTAL ARTNR PER BRYTBEGREPP             
011600*                                 NO OF PARTNOS PER TYPE                  
011700     03 FOR-KVRO             PIC S9(7)           COMP-3.                  
011800*                                 ANTAL RESTNOTERADE ARTIKLAR             
011900*                                 BACKORDERED QTY                         
012000     03 FOR-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
012100*                                 ARTIKELPRIS NETTO                       
012200*                                 NET PRICE EACH   (FOB NET)              
012300     03 FOR-PRARTSTD         PIC S9(7)V9(2)      COMP-3.                  
012400*                                 ARTIKELSTANDARDPRIS                     
012500*                                 STANDARD PRICE                          
012600     03 FOR-REKSIFFR         PIC S9              COMP-3.                  
012700*                                 KONTROLLSIFFRA                          
012800*                                 PART NO CHECK DIGIT                     
012900     03 FOR-TIREGDAT         PIC S9(7)           COMP-3.                  
013000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
013100*                                 REGISTRATION DATE (YYMMDD)              
013200     03 FOR-TIRES            PIC S9(7)           COMP-3.                  
013300*                                 RESERVATIONSDATUM                       
013400*                                 RESERVATION DATE                        
013500     03 FOR-TIRODAT          PIC S9(7)           COMP-3.                  
013600*                                 RESTORDERDATUM         (ÅÅMMDD)         
013700*                                 BACK ORDER DATE        (YYMMDD)         
013800     03 FOR-TITPO            PIC S9(7)           COMP-3.                  
013900*                                 PLANERAD ORDERDATUM                     
014000*                                 PLANNED ORDER DATE                      
014100     03 FOR-FLVR             PIC X.                                       
014200*                                 ANSLUTEN TILL VR-SYST                   
014300*                                 ASSOCIATED TO VR SYST                   
014400     03 FOR-KVQPACK-1        PIC S9(5)           COMP-3.                  
014500*                                 ANTAL I Q1 FÖRPACKNING                  
014600*                                 QUANTITY IN BULK PACK Q1                
014700     03 FOR-BEERS            PIC X(20).                                   
014800*                                 ERSÄTTNINGSTEXT                         
014900*                                 REPLACEMENT TEXT                        
015000     03 FOR-BEART            PIC X(25).                                   
015100*                                 ARTIKELBENÄMNING                        
015200*                                 PART DESCRIPTION                        
015300     03 FOR-KDSORT           PIC X(2).                                    
015400*                                 SORT-KOD                                
015500*                                 UNIT OF MEASURE                         
015600     03 FOR-KDRADERS         PIC S9              COMP-3.                  
015700*                                 HUR RADEN ÄR ERSATT                     
015800     03 FOR-KDTILLK          PIC S9              COMP-3.                  
015900*                                 TILLKOMMANDE-KOD                        
016000     03 FOR-IDLOPNR          PIC S9(3)           COMP-3.                  
016100*                                 LÖPNUMMER                               
016200*                                 SEQUENCE NUMBER                         
016300     03 FOR-KDORDING         PIC S9              COMP-3.                  
016400*                                 UPPDATERING ORDERINGÅNG                 
016500*                                 ORDER STATISTICS                        
016600     03 FOR-KDPRTYP          PIC X.                                       
016700*                                 TYP AV PRISTILLÄMPNING                  
016800*                                 TYPE OF PRICING                         
016900     03 FOR-BEVOLREF         PIC X(10).                                   
017000*                                 VOLVO REFERENS                          
017100*                                 VOLVO REFERENCE                         
017200     03 FOR-FLINVEST         PIC X.                                       
017300*                                 BYTES INVENTERINGSFLAGGA                
017400*                                 EXCHANGE INVESTMENT FLAG                
017500     03 FOR-FLPRTILL         PIC X.                                       
017600*                                 PRISTILLÄGGS FLAGGA                     
017700*                                 PRICE PENALTY FLAG                      
017800     03 FOR-FLTPOBEK         PIC X.                                       
017900*                                 TPO-RAD BEKRÄFTAD                       
018000*                                 TPO ORDER LINE CONFIRMED                
018100     03 FOR-BEKUNDRF         PIC X(15).                                   
018200*                                 KUNDENS REFERENS                        
018300*                                 CUSTOMERS REFERENCE                     
018400     03 FOR-IDKAMPRF         PIC S9(7)           COMP-3.                  
018500*                                 KAMPANJREFERENS                         
018600*                                 CAMPAIGN REFERENCE                      
018700     03 FOR-IDLEVNR          PIC X(5).                                    
018800*                                 LEVERANTÖRNUMMER                        
018900*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
019000     03 FOR-IDSYSTEM         PIC X(4).                                    
019100*                                 VOLVO VCCS SYSTEMNUMMER                 
019200*                                 VOLVO VCCS SYSTEM NUMBER                
019300     03 FOR-KVBEART-Q        PIC S9(7)           COMP-3.                  
019400*                                 BESTÄLLT KVANTANPASSAT ANTAL            
019500*                                 ORDERED QUANTITY ADAPTED                
019600*                                  ITEMS                                  
019700     03 FOR-TIREGTID         PIC S9(7)           COMP-3.                  
019800*                                 REGISTRERINGSTID                        
019900*                                 GENERAL REGISTRATION TIME               
020000     03 FOR-TISENBEK.                                                     
020100*                                 SENASTE BEKRÄFTELSETIDPUNKT             
020200*                                 LATEST CONFIRMATION DATE                
020300        05 FOR-TISENBEK-DAG  PIC S9(7)           COMP-3.                  
020400*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
020500*                                 YEAR - MONTH - DAY  (YYMMDD)            
020600        05 FOR-TISENBEK-KL   PIC S9(7)           COMP-3.                  
020700*                                 TIM - MIN - SEK   (HHMMSS)              
020800*                                 HOUR - MINUTE - SEC (HHMMSS)            
020900     03 FOR-BEVARREF         PIC X(10).                                   
021000*                                 VÅR REFERENS                            
021100*                                 OUR REFERENCE                           
021200     03 FOR-IDORDER          PIC S9(7)           COMP-3.                  
021300*                                 VOLVO PARTS ORDERNUMMER                 
021400*                                 VOLVO PARTS ORDER NUMBER                
021500     03 FOR-TIDISPIN         PIC S9(7)           COMP-3.                  
021600*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
021700*                                 NEXT CONSIGNMENT AVAIL.(YYMMDD)         
021800     03 FOR-TIORDREG         PIC S9(7)           COMP-3.                  
021900*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
022000*                                 ORDER REGISTRATION DATE  YYMMDD         
022100     03 FOR-REDIRLEV         PIC S9V9(2)         COMP-3.                  
022200*                                 DIREKTLEVERANSANDEL                     
022300     03 FOR-KDUART           PIC X.                                       
022400*                                 UNDANTAGSARTIKEL                        
022500*                                 EXECPTION PARTS                         
022600     03 FOR-KDUART-URS       PIC X.                                       
022700*                                 UNDANTAGSARTIKEL                        
022800*                                 EXECPTION PARTS                         
022900     03 FOR-IDANSK-LARM      PIC S9(3)           COMP-3.                  
023000*                                 LARMMOTTAGANDE ANSKAFFARENUMMER         
023100*                                 ALARMRECEIVEING PROCURER NO.            
023200     03 FOR-KDLARM           PIC S9(3)           COMP-3.                  
023300*                                 LARMORSAKSKOD                           
023400*                                 ALARM REASON CODE                       
023500     03 FOR-FLNYLARM         PIC X.                                       
023600*                                 ANGER OM ARTIKELLARMET ÄR NYTT          
023700*                                 NEW PARTALARM REGISTRATED               
023800     03 FOR-TIREGDAT-LARM    PIC S9(7)           COMP-3.                  
023900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
024000*                                 REGISTRATION DATE (YYMMDD)              
024100     03 FOR-IDARTNR-URS      PIC S9(9)           COMP-3.                  
024200*                                 ARTIKELNUMMER                           
024300*                                 PART NUMBER                             
024400     03 FOR-KVKORT           PIC S9(3)           COMP-3.                  
024500*                                 ANTAL KORT (ERSÄTTNINGS-RADER)          
024600     03 FOR-IDANALYS         PIC X(12).                                   
024700*                                 ANALYSNUMMER                            
024800*                                 ANALYSIS NUMBER                         
024900     03 FOR-KDORDTYP-LDC     PIC X(2).                                    
025000*                                 ORDERTYP HOS DEALER                     
025100     03 FOR-TIREPDAT         PIC S9(7)           COMP-3.                  
025200*                                 REPAIR DATE                             
025300*                                 REPAIR DATE                             
025400     03 FOR-IDKUNDRF-WIP     PIC X(10).                                   
025500*                                 REPARATIONS ORDERNR, LDC KUND           
025600*                                 WORK ORDER NUMBER, LDC DEALER           
025700     03 FOR-DEAL-PR-LINE.                                                 
025800*                                 DEALERPRIS (RAD)                        
025900        05 FOR-IDPRQUES      PIC 9(7).                                    
026000*                                 PRISFRÅGA NR                            
026100*                                 PRICE QUESTION NO                       
026200        05 FOR-PRARTNTO-LOC  PIC S9(7)V9(2)      COMP-3.                  
026300*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
026400*                                 NET PRICE EACH LOCAL CURRENCY           
026500        05 FOR-PRARTNTO-LOCPREL                                           
026600                             PIC S9(7)V9(2)      COMP-3.                  
026700*                                 PREL NETTO SLUTKUNDSPRIS I              
026800*                                 LOKAL VALUTA                            
026900*                                 PREL NET PRICE - LOCAL CURRENCY         
027000        05 FOR-PRARTBTO-LOC  PIC S9(7)V9(2)      COMP-3.                  
027100*                                 PRIS I LOKAL VALUTA                     
027200*                                 LOCAL GROSS SALES PRICE                 
027300        05 FOR-KDVALISO      PIC X(3).                                    
027400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
027500*                                 CURRENCY CODE BY ISO-STANDARD.          
027600        05 FOR-KDVAT         PIC X(2).                                    
027700*                                 MOMSKOD                                 
027800*                                 VAT CODE                                
027900        05 FOR-RERAB         PIC S9(2)V9(1)      COMP-3.                  
028000*                                 RABATTSATS (PROCENT)                    
028100        05 FOR-KDRAB         PIC X(5).                                    
028200*                                 RABATTKOD                               
028300        05 FOR-BEART-VIPS    PIC X(25).                                   
028400*                                 VIPS ARTIKELBENÄMNING                   
028500*                                 PÅ DEALERNS SPRÅK                       
028600     03 FOR-PRAVCOST         PIC S9(7)V9(2)      COMP-3.                  
028700*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
028800*                                 AVERAGE COST FOREIGN CURRENCY           
028900     03 FOR-KDROPACK         PIC X.                                       
029000*                                 FRISLÄPPNINGSKOD RO/DO                  
029100*                                 CONSOLIDATION BO/DO                     
029200     03 FOR-IDARBREF         PIC X(10).                                   
029300*                                 ARBETSORDER SOFT/DÖSKALLE               
029400*                                 WORK ORDER SOFT/DUMMY ORDERHEAD         
029500*** END OF VILMAII-COPY LENGTH= 414 BYTES                                 
