000100 01  RESP-W4W27AO1.                                                       
000200*                                 RESPONSE FROM PGM W4W27A                
000300*                                                                         
000400     03 RESP-IDARTNR-KEY     PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 RESP-IDDISTR-KEY     PIC X(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 RESP-IDKUNDNR-KEY    PIC X(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 RESP-KVLS            PIC -(7)9.                                   
001100*                                 LAGERSALDO                              
001200     03 RESP-BEART           PIC X(25).                                   
001300*                                 ARTIKELBENÄMNING                        
001400     03 RESP-ADLAGOMR        PIC Z9.                                      
001500*                                 LAGEROMRÅDE                             
001600     03 RESP-ADGANG          PIC Z9.                                      
001700*                                 GÅNG                                    
001800     03 RESP-ADPLATS         PIC Z(4)9.                                   
001900*                                 LAGERPLATSNUMMER                        
002000     03 RESP-IDLEVNR         PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200     03 RESP-VKART-OLD       PIC Z(6)9.                                   
002300*                                 ARTIKELVIKT (G)                         
002400     03 RESP-VLARTNTO-OLD    PIC Z(7)9.9.                                 
002500*                                 ARTIKELVOLYM NETTO (CM3)                
002600     03 RESP-SPARRKOD        PIC Z9.                                      
002700*                                 SPÄRRKOD LEVERANS                       
002800     03 RESP-KDFARLIG        PIC 9.                                       
002900*                                 KOD FÖR FARLIGT GODS                    
003000     03 RESP-KVAKS-PAV-CDC   PIC Z(6)9.                                   
003100*                                 DEL AV AK PÅ VÄG                        
003200     03 RESP-KVART-FORAVIS   PIC -(7)9.                                   
003300*                                 ANKOMSTSALDO                            
003400     03 RESP-FILLER          PIC X(18).                                   
003500     03 RESP-IDANSK          PIC Z9(2).                                   
003600*                                 ANSKAFFARNUMMER                         
003700     03 RESP-KDERS           PIC Z9(2).                                   
003800*                                 ERSÄTTNINGSKOD                          
003900     03 RESP-KVAKS-LAGER     PIC -(7)9.                                   
004000*                                 ANKOMSTSALDO                            
004100     03 RESP-KVROS           PIC -(7)9.                                   
004200*                                 RESTORDERSALDO                          
004300     03 RESP-KVRESS          PIC -(7)9.                                   
004400*                                 RESERVERAT ANTAL ARTIKLAR               
004500     03 RESP-KVVORKO         PIC -(6)9.                                   
004600*                                 VOR-KÖ KVANT                            
004700     03 RESP-KVANTAL         PIC Z(5)9.                                   
004800*                                 ANTAL                                   
004900     03 RESP-KVSPARR-KVAL-CDC                                             
005000                             PIC Z(6)9.                                   
005100*                                 SPÄRRAT ANTAL KVALITETSFEL              
005200     03 RESP-KVUTRS-CDC      PIC -(6)9.                                   
005300*                                 UTREDNINGSSALDO                         
005400     03 RESP-KVSPANT         PIC -(6)9.                                   
005500*                                 SPÄRRAT ANTAL                           
005600     03 RESP-TELEVBSK-EXT    PIC X(80).                                   
005700*                                 LEVERANSBESKED FÖR EXTERNT              
005800     03 RESP-TELEVBSK-EXT2   PIC X(80).                                   
005900*                                 LEVERANSBESKED FÖR EXTERNT              
006000     03 RESP-TELEVBSK-EXT3   PIC X(80).                                   
006100*                                 LEVERANSBESKEDSINFORMATION              
006200     03 RESP-TELEVBSK-EXT4   PIC X(80).                                   
006300*                                 LEVERANSBESKEDSINFORMATION              
006400     03 RESP-TEARTNOT-1      PIC X(40).                                   
006500*                                 ARTIKEL NOTERING                        
006600     03 RESP-TIURPROD        PIC 9(4).                                    
006700*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
006800     03 RESP-TEARTNOT-2      PIC X(40).                                   
006900*                                 ARTIKEL NOTERING                        
007000     03 RESP-ADDRESS.                                                     
007100*                                                                         
007200        05 RESP-IDNAMN-ANSK  PIC X(40).                                   
007300*                                 NAMN                                    
007400        05 RESP-IDTFN-ANSK   PIC X(20).                                   
007500*                                 TELEFONNUMMER EXTERNT                   
007600        05 RESP-IDMAIL-ANSK  PIC X(60).                                   
007700*                                 MAIL ADRESS                             
007800        05 RESP-IDNAMN-BEREDARE                                           
007900                             PIC X(40).                                   
008000*                                 NAMN                                    
008100        05 RESP-IDTFN-BEREDARE                                            
008200                             PIC X(20).                                   
008300*                                 TELEFONNUMMER EXTERNT                   
008400        05 RESP-IDMAIL-BEREDARE                                           
008500                             PIC X(60).                                   
008600*                                 MAIL ADRESS                             
008700        05 RESP-IDNAMN-KVAL  PIC X(40).                                   
008800*                                 NAMN                                    
008900        05 RESP-IDTFN-KVAL   PIC X(20).                                   
009000*                                 TELEFONNUMMER EXTERNT                   
009100        05 RESP-IDMAIL-KVAL  PIC X(60).                                   
009200*                                 MAIL ADRESS                             
009300     03 RESP-IDPROENH        OCCURS 3 TIMES                               
009400                             PIC X(8).                                    
009500*                                 PRODUKTIONSENHET                        
009600     03 RESP-BYPRO-IDARTNR   OCCURS 18 TIMES                              
009700                             PIC Z(8)9.                                   
009800*                                 ARTIKELNUMMER                           
009900     03 RESP-INFO-DC-4413    OCCURS 6 TIMES.                              
010000*                                 RADINFORMATION                          
010100        05 RESP-IDDC-BULK    PIC X(2).                                    
010200*                                 IDENTIFIERARE BULKORDERLAGER            
010300        05 RESP-KDGENFRA-MO  PIC Z9.                                      
010400*                                 NORMAL FRAKT MÅNADSORDER KL 2-4         
010500        05 RESP-IDDC-DAY     PIC X(2).                                    
010600*                                 IDENTIFIERARE DAGORDERLAGER             
010700        05 RESP-KDGENFRA-DO  PIC Z9.                                      
010800*                                 NORMAL FRAKT DAGORDER                   
010900        05 RESP-IDDC-VOR     PIC X(2).                                    
011000*                                 IDENTIFIERARE VORORDERLAGER             
011100        05 RESP-KDGENFRA-VOR PIC Z9.                                      
011200*                                 NORMAL FRAKT VOR-ORDER                  
011300     03 RESP-TVSVOR-4412-GRP OCCURS 16 TIMES.                             
011400*                                 GODKÄNDA DC FÖR TVS VOR-SLÄPP           
011500        05 RESP-IDDC-TVSVOR  PIC X(2).                                    
011600*                                 TVÅNGSSTYRNING AV VOR-SLÄPP             
011700*                                 FRÅN DC                                 
011800     03 RESP-KVRADER-MAX1    PIC 9(5).                                    
011900*                                 MAX INDEX KOPPLAT TILL OCCURS N         
012000*                                 EDAN.                                   
012100     03 RESP-KVRADER-MAX2    PIC 9(5).                                    
012200*                                 MAX INDEX KOPPLAT TILL OCCURS N         
012300*                                 EDAN.                                   
012400     03 RESP-KVRADER-MAX3    PIC 9(5).                                    
012500*                                 MAX INDEX KOPPLAT TILL OCCURS N         
012600*                                 EDAN.                                   
012700     03 RESP-KVRADER-MAX4    PIC 9(5).                                    
012800*                                 MAX INDEX KOPPLAT TILL OCCURS N         
012900*                                 EDAN.                                   
013000     03 RESP-KVRADER-MAX5    PIC 9(5).                                    
013100*                                 MAX INDEX KOPPLAT TILL OCCURS N         
013200*                                 EDAN.                                   
013300     03 RESP-KVRADER-MAX6    PIC 9(5).                                    
013400*                                 MAX INDEX KOPPLAT TILL OCCURS N         
013500*                                 EDAN.                                   
013600     03 RESP-KVRADER-MAX7    PIC 9(5).                                    
013700*                                 MAX INDEX KOPPLAT TILL OCCURS N         
013800*                                 EDAN.                                   
013900     03 RESP-KVRADER-MAX8    PIC 9(5).                                    
014000*                                 MAX INDEX KOPPLAT TILL OCCURS N         
014100*                                 EDAN.                                   
014200     03 RESP-RADER           OCCURS 1 TO 100 TIMES                        
014300                             DEPENDING ON RESP-KVRADER-MAX1.              
014400*                                                                         
014500        05 RESP-IDARTNR      PIC Z(8)9.                                   
014600*                                 ARTIKELNUMMER                           
014700        05 RESP-DIERS-ERS    PIC -(6)9.9(3).                              
014800*                                 ANTAL FÖR ERSATT  DIERS-ERS-002         
014900     03 RESP-KOLUMNER        OCCURS 1 TO 100 TIMES                        
015000                             DEPENDING ON RESP-KVRADER-MAX2.              
015100*                                                                         
015200        05 RESP-FLTEXT       PIC X.                                       
015300*                                 FINNS TEXTINFORMATION ?                 
015400        05 RESP-TILLK-KOLUMNER.                                           
015500*                                                                         
015600           07 RESP-IDARTNR-TILLK                                          
015700                             PIC Z(8)9.                                   
015800*                                 TILLKOMMANDE ARTIKELNUMMER              
015900           07 RESP-DIERS-TILLK                                            
016000                             PIC -(4)9.9(3).                              
016100*                                 TILLKOMMANDE ARTIKELANTAL               
016200           07 RESP-FILLER    PIC X(3).                                    
016300     03 RESP-INFO-RAD-5107   OCCURS 1 TO 500 TIMES                        
016400                             DEPENDING ON RESP-KVRADER-MAX3.              
016500*                                 RADINFORMATION                          
016600        05 RESP-5107-IDPTYP  PIC X(3).                                    
016700*                                 POSTTYP                                 
016800        05 RESP-5107-IDLOPNRM                                             
016900                             PIC Z(7)9.                                   
017000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
017100*                                 (0VVDLLLLK)                             
017200        05 RESP-5107-TIAAVVD PIC 9(5).                                    
017300*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
017400        05 RESP-5107-IDLEVNR PIC X(5).                                    
017500*                                 LEVERANTÖRNUMMER                        
017600        05 RESP-5107-KDRT    PIC Z9.                                      
017700*                                 REDOVISNINGSTYP                         
017800        05 RESP-5107-TIAVSDAT                                             
017900                             PIC 9(6).                                    
018000*                                 AVISERINGSDATUM (YYMMDD)                
018100        05 RESP-5107-IDAVINR PIC Z(6)9.                                   
018200*                                 AVI-NUMMER                              
018300        05 RESP-5107-KVAVIS  PIC -(6)9.                                   
018400*                                 AVISERAT ANTAL                          
018500        05 RESP-5107-KVANTMOT                                             
018600                             PIC -(6)9.                                   
018700*                                 ANTAL MOTTAGET                          
018800     03 RESP-SDC-INFO-0510   OCCURS 1 TO 100 TIMES                        
018900                             DEPENDING ON RESP-KVRADER-MAX4.              
019000        05 RESP-IDDC-SDC     PIC X(2).                                    
019100*                                 IDENTIFIERARE LAGER                     
019200        05 RESP-KVLS-SDC     PIC -(6)9.                                   
019300*                                 LAGERSALDO                              
019400        05 RESP-KVDISP-SDC   PIC -(6)9.                                   
019500*                                 DISPONIBELT LAGER                       
019600        05 RESP-KVOKS-DAG-SDC                                             
019700                             PIC -(6)9.                                   
019800*                                 ORDERKÖSALDO, KLASS 1                   
019900        05 RESP-KVOKS-BULK-SDC                                            
020000                             PIC -(6)9.                                   
020100*                                 ORDERKÖSALDO, KLASS 2-4                 
020200        05 RESP-KVSPARR-KVAL-SDC                                          
020300                             PIC Z(6)9.                                   
020400*                                 SPÄRRAT ANTAL KVALITETSFEL              
020500        05 RESP-KVUTRS-SDC   PIC -(6)9.                                   
020600*                                 UTREDNINGSSALDO                         
020700        05 RESP-KVAKS-SDC    PIC Z(6)9.                                   
020800*                                 DEL AV AK SOM LIGGER I SDC              
020900        05 RESP-KVAKS-PAV-SDC                                             
021000                             PIC -(7)9.                                   
021100*                                 DEL AV AK PÅ VÄG                        
021200        05 RESP-TIBERANK-SDC PIC 9(6).                                    
021300*                                 BERÄKNAD ANKOMSTDATUM                   
021400        05 RESP-KDLEVSP-SDC  PIC Z9.                                      
021500*                                 SPÄRRKOD LEVERANS                       
021600     03 RESP-BUFF-ADR-4108   OCCURS 1 TO 500 TIMES                        
021700                             DEPENDING ON RESP-KVRADER-MAX5.              
021800        05 RESP-4108-PLATSTYP                                             
021900                             PIC X.                                       
022000        05 RESP-4108-ADBUFFOMR                                            
022100                             PIC Z9.                                      
022200*                                 BUFFERTOMRÅDE                           
022300        05 RESP-4108-ADBUFFGANG                                           
022400                             PIC Z9.                                      
022500*                                 BUFFERT GÅNG                            
022600        05 RESP-4108-ADBUFFPL                                             
022700                             PIC Z(4)9.                                   
022800*                                 BUFFERPLATSNUMMER                       
022900        05 RESP-4108-KVBUFF-F                                             
023000                             PIC -(7)9.                                   
023100*                                 FÖRÄDLAT BUFFERSALDO                    
023200        05 RESP-4108-KVBUFF-OF                                            
023300                             PIC -(7)9.                                   
023400*                                 BUFFERSALDO OFÖRÄDLAT GODS              
023500     03 RESP-RAD-6123        OCCURS 1 TO 500 TIMES                        
023600                             DEPENDING ON RESP-KVRADER-MAX6.              
023700*                                 LINES                                   
023800        05 RESP-6123-IDLOPNRM                                             
023900                             PIC X(9).                                    
024000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
024100*                                 (0VVDLLLLK)                             
024200        05 RESP-6123-IDRADNR PIC X(3).                                    
024300*                                 RADNUMMER                               
024400        05 RESP-6123-IDLEVNR PIC X(5).                                    
024500*                                 LEVERANTÖRNUMMER                        
024600        05 RESP-6123-IDOKOLLI                                             
024700                             PIC X(9).                                    
024800*                                 ODETTE KOLLINUMMER                      
024900        05 RESP-6123-KVINLART                                             
025000                             PIC Z(5)9.                                   
025100*                                 ANTAL I PARTIRAD                        
025200        05 RESP-6123-ADINLOMR                                             
025300                             PIC X(4).                                    
025400*                                 INLEVERANSOMRÅDE                        
025500        05 RESP-6123-IDINLVGN                                             
025600                             PIC X(3).                                    
025700*                                 VAGNSIDENTITET                          
025800        05 RESP-6123-ADINLOMR-NXT                                         
025900                             PIC X(4).                                    
026000*                                 INLEVERANSOMRÅDE NÄSTA                  
026100        05 RESP-6123-KDINLSTA                                             
026200                             PIC X(3).                                    
026300*                                 SYSTEMSTATUS INLEVERANS                 
026400     03 RESP-RAD-2106        OCCURS 1 TO 50 TIMES                         
026500                             DEPENDING ON RESP-KVRADER-MAX7.              
026600*                                 LINES                                   
026700        05 RESP-TILEVBSK-AVS-UT                                           
026800                             PIC 9(5).                                    
026900*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
027000        05 RESP-TILEVBSK-INL-C1-UT                                        
027100                             PIC 9(5).                                    
027200*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
027300        05 RESP-FLFORAVI-C1-UT                                            
027400                             PIC X.                                       
027500*                                 FÖRAVISERAD INLEVERANS                  
027600        05 RESP-KVAVIS-UT    PIC Z(6)9.                                   
027700*                                 AVISERAT ANTAL                          
027800     03 RESP-RAD-0553        OCCURS 1 TO 50 TIMES                         
027900                             DEPENDING ON RESP-KVRADER-MAX8.              
028000*                                 LINES                                   
028100        05 RESP-TEINFO       PIC X(1200).                                 
028200*                                 ALLMÄN TEXT INFO                        
028300*** END OF VILMAII-COPY LENGTH= 134077 BYTES                              
