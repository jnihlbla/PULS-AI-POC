000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5132000.                                                
000300 AUTHOR.         BOO HAMMARIN, GDC-GROUP.                                 
000400 DATE-WRITTEN.   APRIL 1997.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION: UPPDATERAR                                                 
000800*              - WDH7 OCH WDK611                                          
000900*                                                                         
001000*    ANVÄNDA SEGMENT: WDH101                                              
001100*                     WDH111                                              
001200*                     WDK601                                              
001300*                     WDK611                                              
001400*                     WDK627                                              
001500*                     WDK701                                              
001600*                     WDK711                                              
001700*                     WDH701                                              
001800*                     WDH711                                              
001900                                                                          
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300                                                                          
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600*    -- CHECKED BY WY2000                                                 
002700                                                                          
002800 77  IDPGM                   PIC X(8)            VALUE 'W5132000'.        
002900 77  JA                      PIC X               VALUE 'J'.               
003000 77  NEJ                     PIC X               VALUE 'N'.               
003100                                                                          
003200*    --- GENERELL ARBETSAREA                                              
003300                                                                          
003400 01  CHKP-VAR.                                                            
003500     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
003600     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
003700     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
003800     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
003900     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004000     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
004100                                                                          
004200 01  ARBETS-AREA.                                                         
004300     03  RETURKOD                PIC S9(5)   VALUE +0   COMP-3.           
004400     03  SKROT-SEGM-FINNS        PIC X       VALUE 'N'.                   
004500                                                                          
004600 01  FELTEXT.                                                             
004700     03  FILLER                  PIC X(8)   VALUE 'FELTEXT'.              
004800     03  FELTEXT-STR             PIC X(72)  VALUE SPACE.                  
004900                                                                          
005000 01  W-IDUSER-CRE                PIC X(8).                                
005100 01  W-IDUSER-PR1                PIC X(8).                                
005200 01  W-IDUSER-PR2                PIC X(8).                                
005300 01  W-IDUSER-PR3                PIC X(8).                                
005400                                                                          
005500 01  W-ADARTADR                  PIC 9(9).                                
005600                                                                          
005700 01  WS-DASKROT                  PIC 9(8)        VALUE ZERO .             
005800                                                                          
005900 01  WS-TISEGKEYAREA.                                                     
006000     03  WS-TIAAAAMMDDL      PIC 9(9) VALUE ZERO.                         
006100     03  FILLER REDEFINES WS-TIAAAAMMDDL.                                 
006200         05  WS-AAR          PIC 9(2).                                    
006300         05  WS-TIAAMMDD     PIC 9(6).                                    
006400         05  WS-LOPNR        PIC 9(1).                                    
006500                                                                          
006600     03  WS-TISEGKEY         PIC S9(9)  VALUE ZERO COMP-3.                
006700                                                                          
006800 01  MINUS-AAVV              PIC 9(4).                                    
006900 01  FILLER REDEFINES MINUS-AAVV.                                         
007000     03 MINUS-AA             PIC 9(2).                                    
007100     03 MINUS-VV             PIC 9(2).                                    
007200     EJECT                                                                
007300 01  FILLER PIC X(16) VALUE '*****NYCKLAR****'.                           
007400                                                                          
007500 01  W-IDARTNR-X.                                                         
007600     03  W-IDARTNR               PIC S9(9)   COMP-3  VALUE ZERO.          
007700                                                                          
007800 01  W-KDERS-X.                                                           
007900     03  FILLER                  PIC S9(3)   COMP-3  VALUE +0.            
008000                                                                          
008100 01  KEYWDH1-MIN.                                                         
008200     03  IDDC-SEARCH-MIN     PIC X(2)  VALUE '00'.                        
008300     03  INVKAT-SEARCH-MIN   PIC S9(3) VALUE ZERO COMP-3.                 
008400     03  TISEGKEY-SEARCH-MIN PIC S9(9) VALUE ZERO COMP-3.                 
008500     03  DAREGDAT-9KOMP-MIN  PIC 9(8)  VALUE ZERO.                        
008600                                                                          
008700 01  KEYWDH1-MAX.                                                         
008800     05  IDDC-SEARCH-MAX     PIC X(2)  VALUE '99'.                        
008900     03  INVKAT-SEARCH-MAX   PIC S9(3) VALUE +999       COMP-3.           
009000     03  TISEGKEY-SEARCH-MAX PIC S9(9) VALUE +999999999 COMP-3.           
009100     03  DAREGDAT-9KOMP-MAX  PIC 9(8)  VALUE 99999999.                    
009200                                                                          
009300 01  W-DASKROT-X.                                                         
009400     03  W-DASKROT               PIC 9(8).                                
009500                                                                          
009600 01  W-IDDC-X.                                                            
009700     03  W-IDDC                  PIC X(2)      VALUE SPACE.               
009800                                                                          
009900 01  W-IDDC-B6-X.                                                         
010000      03 W-IDDC-B6               PIC X(2).                                
010100     EJECT                                                                
010200                                                                          
010300 01  GENERELLA-SUBPROGRAM.                                                
010400     05  DATKORT                 PIC X(8)      VALUE 'DATKORT '.          
010500     03  FELLOG                  PIC X(8)      VALUE 'FELLOG  '.          
010600     03  CBLTDLI                 PIC X(8)      VALUE 'CBLTDLI '.          
010700     03  ABEND                   PIC X(8)      VALUE 'ABEND   '.          
010800     03  WDATKONV                PIC X(8)      VALUE 'WDATKONV'.          
010900                                                                          
011000*--------------------------------------- PARAMETRAR TILL DATKORT          
011100                                                                          
011200 01  DATUMKORT-ID            PIC X(8)    VALUE 'WDATUM'.                  
011300                                                                          
011400*01  -COPY WDATKORT                                                       
011500     EJECT                                                                
011600*---- PARAMETRAR TILL ABEND                                               
011700 01  RETURKODER.                                                          
011800     03  RKOD                    PIC S9(4) COMP SYNC VALUE ZERO.          
011900     03  RKOD-ABEND-UTAN-DUMP    PIC S9(4) COMP SYNC VALUE +16.           
012000     03  RKOD-ABEND-MED-DUMP     PIC S9(4) COMP SYNC VALUE +1000.         
012100                                                                          
012200     EJECT                                                                
012300*   --- VALID IDDC CODES                                                  
012400*                                                                         
012500*01  -COPY WWDC99                                                         
012600                                                                          
012700*-----------------------------------------PARAMETRAR TILL                 
012800*                                         SUBPROGRAM WDATKONV             
012900 01  FILLER             PIC X(8)   VALUE 'WDATKONV'.                      
013000*01  -COPY WDATAREA.                                                      
013100     EJECT                                                                
013200 01  FILLER                  PIC X(16)   VALUE 'WLARTC01'.                
013300*01  WLARTC01  -COPY WDK601 -PRE BE01-.                                   
013400                                                                          
013500 01  FILLER                  PIC X(16)   VALUE 'WLARTC11'.                
013600*01  WLARTC11  -COPY WDK611 -PRE BE11-.                                   
013700                                                                          
013800 01  FILLER                  PIC X(16)   VALUE 'WLARTC27'.                
013900*01  WLARTC27  -COPY WDK627 -PRE BE27-.                                   
014000                                                                          
014100 01  FILLER                  PIC X(16)   VALUE 'WLARTS01'.                
014200*01  WLARTS01  -COPY WDK701 -PRE SE01-.                                   
014300                                                                          
014400 01  FILLER                  PIC X(16)   VALUE 'WLARTS11'.                
014500*01  WLARTS11  -COPY WDK711 -PRE SE11-.                                   
014600                                                                          
014700 01  FILLER                  PIC X(16)   VALUE 'WDH101'.                  
014800*01  WDH101    -COPY WDH101                                               
014900                                                                          
015000 01  FILLER                  PIC X(16)   VALUE 'WDH111'.                  
015100*01  WDH111    -COPY WDH111                                               
015200                                                                          
015300 01  FILLER                  PIC X(16)   VALUE 'WDH121'.                  
015400*01  WDH121    -COPY WDH121                                               
015500                                                                          
015600 01  FILLER                  PIC X(16)   VALUE 'WLINVC01'.                
015700*01  WLINVC01  -COPY WDH701                                               
015800                                                                          
015900 01  FILLER                  PIC X(16)   VALUE 'WLINVC11'.                
016000*01  WLINVC11  -COPY WDH711                                               
016100                                                                          
016200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
016300 01   DLI-IO-AREA-B601.                                                   
016400*     03  -COPY WDB601                                                    
016500                                                                          
016600     EJECT                                                                
016700*****                                                                     
016800*****    IN-AREA TILL IMS-SEKTIONERNA                                     
016900*****                                                                     
017000 01  IMS-WORKAREOR.                                                       
017100     03  FILLER          PIC X(16)   VALUE '*-*-*IMS-WS*-*-*'.            
017200     03  STATUS-WS       PIC XX.                                          
017300         88  SEGMENT-FINNS       VALUE '  '.                              
017400         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
017500         88  BASEN-SLUT          VALUE 'GB'.                              
017600         88  SEGMENT-FINNS-REDAN VALUE 'II'.                              
017700         88  IMS-EJ-OK           VALUE 'XD'.                              
017800     SKIP3                                                                
017900     03  GODK-STATUSKODER.                                                
018000         05  GODK-STATUS OCCURS 3 INDEXED BY STATUS-IX PIC XX.            
018100     SKIP3                                                                
018200 01      SSA1            PIC X(128).                                      
018300     EJECT                                                                
018400*                                                                         
018500*        IMS FUNKTIONSKODER                                               
018600*                                                                         
018700*01      -COPY W0003                                                      
018800     EJECT                                                                
018900 LINKAGE SECTION.                                                         
019000*01  -COPY W0009     -PRE MSG-                                            
019100                                                                          
019200*01  -COPY W0008     -PRE WDH1-                                           
019300     05  WDH1-KONKAT-KEY PIC X(10).                                       
019400     EJECT                                                                
019500*01  -COPY W0008     -PRE WLARTC-.                                        
019600     05  WLARTC-KONKAT-KEY PIC X(10).                                     
019700                                                                          
019800*01  -COPY W0008  -PRE WLARTS-.                                           
019900     05  WLARTS-KONKAT-KEY PIC X(30).                                     
020000     EJECT                                                                
020100*01  -COPY W0008  -PRE WLINVC-.                                           
020200     05  WLINVC-KONKAT-KEY PIC X(30).                                     
020300                                                                          
020400*01  -COPY W0008  -PRE WDB6-                                              
020500     05  FILLER            PIC X.                                         
020600                                                                          
020700     EJECT                                                                
020800 PROCEDURE DIVISION USING MSG-PCB                                         
020900                          WDH1-PCB                                        
021000                          WLARTC-PCB                                      
021100                          WLARTS-PCB                                      
021200                          WLINVC-PCB                                      
021300                          WDB6-PCB.                                       
021400 MAIN SECTION.                                                            
021500     ENTRY 'DLITCBL' USING MSG-PCB                                        
021600                           WDH1-PCB                                       
021700                           WLARTC-PCB                                     
021800                           WLARTS-PCB                                     
021900                           WLINVC-PCB                                     
022000                           WDB6-PCB.                                      
022100                                                                          
022200     PERFORM A-INITIERING                                                 
022300                                                                          
022400     PERFORM B-BEARBETNING                                                
022500                                                                          
022600     MOVE +0 TO RETURN-CODE                                               
022700     GOBACK                                                               
022800     .                                                                    
022900     EJECT                                                                
023000 A-INITIERING SECTION.                                                    
023100                                                                          
023200     PERFORM IMS-RESTART                                                  
023300                                                                          
023400     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
023500                                                                          
023600     IF D-VECKA = 01                                                      
023700       COMPUTE MINUS-AA = D-AAR - 1                                       
023800       COMPUTE MINUS-VV = 52                                              
023900     ELSE                                                                 
024000       COMPUTE MINUS-AA = D-AAR                                           
024100       COMPUTE MINUS-VV = D-VECKA - 1                                     
024200     END-IF                                                               
024300     DISPLAY ' MINUS VECKA = ' MINUS-AAVV                                 
024400                                                                          
024500     MOVE 'AAVV '     TO DAT-KDDATFORM                                    
024600     MOVE MINUS-AAVV  TO DAT-I-TIDATUM                                    
024700                                                                          
024800     CALL WDATKONV USING DAT-KDDATFORM                                    
024900                         DAT-I-TIDATUM                                    
025000                         DAT-O-TIDATUM                                    
025100                         DAT-KDSVAR                                       
025200                                                                          
025300     IF DAT-KDSVAR = ' '                                                  
025400       MOVE DAT-TISEKEL  TO WS-DASKROT(1:2)                               
025500       MOVE DAT-TIAAMMDD TO WS-DASKROT(3:6)                               
025600       MOVE WS-DASKROT   TO W-DASKROT                                     
025700       DISPLAY ' DATUMGRÄNS FÖR SKROT = ' W-DASKROT                       
025800     ELSE                                                                 
025900       DISPLAY '**** TIAAVV' DAT-I-TIDATUM                                
026000       MOVE +1000 TO RETURKOD                                             
026100       CALL ABEND USING RETURKOD                                          
026200     END-IF                                                               
026300     .                                                                    
026400     EJECT                                                                
026500 B-BEARBETNING SECTION.                                                   
026600                                                                          
026700     PERFORM IMS-GET-INVENTERINGSROT                                      
026800                                                                          
026900     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
027000       MOVE ART-IDARTNR             TO W-IDARTNR                          
027100                                                                          
027200       PERFORM IMS-GET-INVENTERINGSINF                                    
027300       IF SEGMENT-FINNS                                                   
027400         PERFORM UNTIL SEGMENT-SAKNAS                                     
027500           MOVE INV-IDDC             TO WS-IDDC                           
027600                                        W-IDDC                            
027700                                        W-IDDC-B6                         
027800           PERFORM IMS-GU-WDB601                                          
027900                                                                          
028000           IF INV-KDINVKAT = +3 AND INV-FLINVBEH = JA                     
028100             PERFORM BC-HAMTA-WDH121                                      
028200             IF DCS-CDC                                                   
028300               PERFORM BA-BEARBETA-CDC                                    
028400             ELSE                                                         
028500               IF DCS-SDC OR DCS-AUSTRALIA OR DCS-JAPAN                   
028600                  PERFORM BB-BEARBETA-SDN-NDC-PAC                         
028700               END-IF                                                     
028800             END-IF                                                       
028900                                                                          
029000           END-IF                                                         
029100           PERFORM IMS-GET-INVENTERINGSINF                                
029200                                                                          
029300           IF CHKP-ANT > CHKP-MAX                                         
029400             PERFORM X-TAG-CHECKPOINT                                     
029500           END-IF                                                         
029600         END-PERFORM                                                      
029700       END-IF                                                             
029800       PERFORM IMS-GET-INVENTERINGSROT                                    
029900     END-PERFORM                                                          
030000     .                                                                    
030100     EJECT                                                                
030200 BA-BEARBETA-CDC SECTION.                                                 
030300                                                                          
030400     PERFORM IMS-GET-ARTIKEL                                              
030500                                                                          
030600     IF SEGMENT-FINNS                                                     
030700                                                                          
030800       PERFORM IMS-GET-CLAGERINFO                                         
030900                                                                          
031000       IF SEGMENT-FINNS                                                   
031100         PERFORM IMS-GET-SKROT-INFO                                       
031200         IF SEGMENT-FINNS                                                 
031300           MOVE JA                   TO SKROT-SEGM-FINNS                  
031400         ELSE                                                             
031500           MOVE NEJ                  TO SKROT-SEGM-FINNS                  
031600         END-IF                                                           
031700                                                                          
031800         IF BE11-CLAG-PRARTSTD > +0                                       
031900                                                                          
032000           IF BE01-ART-KDERS-UTG = +0                                     
032100             MOVE BE11-CLAG-ADLAGOMR TO W-ADARTADR(1:2)                   
032200             MOVE BE11-CLAG-ADGANG   TO W-ADARTADR(3:2)                   
032300             MOVE BE11-CLAG-ADPLATS  TO W-ADARTADR(5:5)                   
032400           ELSE                                                           
032500             MOVE ZERO               TO W-ADARTADR                        
032600           END-IF                                                         
032700                                                                          
032800           IF (SKROT-SEGM-FINNS = JA OR W-ADARTADR = +0) AND              
032900              BE11-CLAG-KVLS = +0                                         
033000             PERFORM BB01-SKAPA-INVHIST-SEG                               
033100             PERFORM BB02-UPPD-WDK611-SEG                                 
033200           END-IF                                                         
033300         ELSE                                                             
033400           PERFORM BB01-SKAPA-INVHIST-SEG                                 
033500           PERFORM BB02-UPPD-WDK611-SEG                                   
033600         END-IF                                                           
033700       END-IF                                                             
033800     END-IF                                                               
033900     .                                                                    
034000     EJECT                                                                
034100 BB-BEARBETA-SDN-NDC-PAC SECTION.                                         
034200                                                                          
034300     PERFORM IMS-GET-ARTIKEL                                              
034400     IF SEGMENT-FINNS                                                     
034500                                                                          
034600       PERFORM IMS-GET-CLAGERINFO                                         
034700       IF SEGMENT-FINNS                                                   
034800         PERFORM IMS-GET-SKROT-INFO                                       
034900         IF SEGMENT-FINNS                                                 
035000           MOVE JA                   TO SKROT-SEGM-FINNS                  
035100         ELSE                                                             
035200           MOVE NEJ                  TO SKROT-SEGM-FINNS                  
035300         END-IF                                                           
035400                                                                          
035500         PERFORM IMS-GU-WDK701                                            
035600         IF SEGMENT-FINNS                                                 
035700                                                                          
035800           IF BE11-CLAG-PRARTSTD > +0                                     
035900             PERFORM IMS-GNP-WDK711                                       
036000             IF NOT SEGMENT-FINNS                                         
036100               MOVE ZERO             TO SE11-SLAG-KVLS                    
036200             END-IF                                                       
036300*                                                                         
036400***LDC BORTTAGET FÖREKOMMER EJ HÄR ENL. KJH                               
036500*            IF (SKROT-SEGM-FINNS = JA AND SE11-SLAG-KVLS = +0) OR        
036600*                LDC                                                      
036700             IF  SKROT-SEGM-FINNS = JA AND SE11-SLAG-KVLS = +0            
036800              PERFORM BB01-SKAPA-INVHIST-SEG                              
036900            END-IF                                                        
037000           END-IF                                                         
037100         END-IF                                                           
037200                                                                          
037300         IF BE11-CLAG-PRARTSTD = +0                                       
037400           PERFORM BB01-SKAPA-INVHIST-SEG                                 
037500         END-IF                                                           
037600       END-IF                                                             
037700     END-IF                                                               
037800     .                                                                    
037900     EJECT                                                                
038000 BB01-SKAPA-INVHIST-SEG SECTION.                                          
038100                                                                          
038200     PERFORM IMS-GU-INVHIST-ROT                                           
038300     IF SEGMENT-SAKNAS                                                    
038400       MOVE W-IDARTNR      TO INVA-IDARTNR                                
038500       PERFORM IMS-ISRT-INVHIST-ROT                                       
038600     END-IF                                                               
038700                                                                          
038800     MOVE 20               TO WS-AAR                                      
038900     MOVE 9                TO WS-LOPNR                                    
039000     MOVE FUNCTION CURRENT-DATE(3:6) TO WS-TIAAMMDD                       
039100     COMPUTE WS-TISEGKEY = 999999999 - WS-TIAAAAMMDDL                     
039200     MOVE WS-TISEGKEY        TO INVH-TISEGKEY                             
039300                                                                          
039400     MOVE FUNCTION CURRENT-DATE(1:8) TO INVH-DAREGDAT-CLO                 
039500     MOVE WS-IDDC            TO INVH-IDDC                                 
039600     MOVE +0                 TO INVH-KVJUSTKV                             
039700     MOVE +3                 TO INVH-KDJUSTYP                             
039800     MOVE 'W5132000'         TO INVH-IDUSER-CLO                           
039900     MOVE NEJ                TO INVH-FLAUTLSJ                             
040000     MOVE SPACE              TO INVH-IDPW                                 
040100     MOVE BE11-CLAG-PRARTSTD TO INVH-PRARTSTD                             
040200     MOVE INV-DAREGDAT-CRE   TO INVH-DAREGDAT-CRE                         
040300     MOVE INV-DAREGDAT-PR1   TO INVH-DAREGDAT-PR1                         
040400     MOVE INV-DAREGDAT-PR2   TO INVH-DAREGDAT-PR2                         
040500     MOVE INV-DAREGDAT-PR3   TO INVH-DAREGDAT-PR3                         
040600     MOVE W-IDUSER-CRE       TO INVH-IDUSER-CRE                           
040700     MOVE W-IDUSER-PR1       TO INVH-IDUSER-PR1                           
040800     MOVE W-IDUSER-PR2       TO INVH-IDUSER-PR2                           
040900     MOVE W-IDUSER-PR3       TO INVH-IDUSER-PR3                           
041000     MOVE +0                 TO INVH-KVANTAL                              
041100                                                                          
041200     PERFORM IMS-ISRT-INVHIST-SEGM                                        
041300                                                                          
041400     PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                                
041500       IF SEGMENT-FINNS-REDAN                                             
041600         SUBTRACT 1 FROM INVH-TISEGKEY                                    
041700         PERFORM IMS-ISRT-INVHIST-SEGM                                    
041800       END-IF                                                             
041900     END-PERFORM                                                          
042000     .                                                                    
042100     EJECT                                                                
042200 BB02-UPPD-WDK611-SEG SECTION.                                            
042300                                                                          
042400     PERFORM IMS-GET-ARTIKEL                                              
042500                                                                          
042600     IF SEGMENT-FINNS                                                     
042700                                                                          
042800       PERFORM IMS-GHNP-WDK611                                            
042900       MOVE ZERO           TO BE11-CLAG-KVINVS                            
043000       PERFORM IMS-REPL-WDK611                                            
043100     END-IF                                                               
043200     .                                                                    
043300     EJECT                                                                
043400 BC-HAMTA-WDH121 SECTION.                                                 
043500     MOVE SPACE TO W-IDUSER-CRE                                           
043600                   W-IDUSER-PR1                                           
043700                   W-IDUSER-PR2                                           
043800                   W-IDUSER-PR3                                           
043900     PERFORM IMS-GNP-WDH121                                               
044000     IF SEGMENT-FINNS                                                     
044100       PERFORM UNTIL SEGMENT-SAKNAS                                       
044200         IF INVL-KDSEGKEY = '0'                                           
044300           MOVE INVL-IDUSER TO W-IDUSER-CRE                               
044400         END-IF                                                           
044500         IF INVL-KDSEGKEY = '1'                                           
044600           MOVE INVL-IDUSER TO W-IDUSER-PR1                               
044700         END-IF                                                           
044800         IF INVL-KDSEGKEY = '2'                                           
044900           MOVE INVL-IDUSER TO W-IDUSER-PR2                               
045000         END-IF                                                           
045100         IF INVL-KDSEGKEY = '3'                                           
045200           MOVE INVL-IDUSER TO W-IDUSER-PR3                               
045300         END-IF                                                           
045400         PERFORM IMS-GNP-WDH121                                           
045500                                                                          
045600       END-PERFORM                                                        
045700     END-IF                                                               
045800                                                                          
045900     .                                                                    
046000     EJECT                                                                
046100 X-TAG-CHECKPOINT   SECTION.                                              
046200                                                                          
046300     PERFORM IMS-CHECKPOINT                                               
046400     MOVE ZERO TO CHKP-ANT                                                
046500     PERFORM IMS-GET-INVENTERINGSROT-0                                    
046600     .                                                                    
046700     EJECT                                                                
046800* - - - - - - - - - - - - - *                                             
046900*    OPERATIONER MOT WDH1   *                                             
047000* - - - - - - - - - - - - - *                                             
047100 IMS-GET-INVENTERINGSROT SECTION.                                         
047200                                                                          
047300     MOVE 'WDH101  ' TO SSA1                                              
047400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
047500     CALL CBLTDLI USING GN WDH1-PCB WDH101 SSA1                           
047600     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
047700     PERFORM IMS-STATUSKONTROLL                                           
047800     .                                                                    
047900     SKIP2                                                                
048000 IMS-GET-INVENTERINGSROT-0 SECTION.                                       
048100                                                                          
048200     STRING  'WDH101  (IDARTNR  =' W-IDARTNR-X ')'                        
048300              DELIMITED BY SIZE INTO SSA1                                 
048400     MOVE '  GE' TO GODK-STATUSKODER                                      
048500     CALL CBLTDLI USING GHU WDH1-PCB WDH101 SSA1                          
048600     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
048700     PERFORM IMS-STATUSKONTROLL                                           
048800     .                                                                    
048900     EJECT                                                                
049000 IMS-GET-INVENTERINGSINF SECTION.                                         
049100                                                                          
049200     STRING  'WDH111  (WDH111KY>=' KEYWDH1-MIN                            
049300                     '&WDH111KY<=' KEYWDH1-MAX ')'                        
049400              DELIMITED BY SIZE INTO SSA1                                 
049500     MOVE '  GE' TO GODK-STATUSKODER                                      
049600     CALL CBLTDLI USING GHNP WDH1-PCB WDH111 SSA1                         
049700     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
049800     PERFORM IMS-STATUSKONTROLL                                           
049900                                                                          
050000     .                                                                    
050100 IMS-GNP-WDH121 SECTION.                                                  
050200     MOVE 'WDH121  ' TO SSA1                                              
050300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
050400     CALL CBLTDLI USING GNP WDH1-PCB WDH121 SSA1                          
050500     MOVE WDH1-STATUS-CODE TO STATUS-WS                                   
050600     PERFORM IMS-STATUSKONTROLL                                           
050700     .                                                                    
050800* - - - - - - - - - - - - - *                                             
050900*    OPERATIONER MOT WDK6   *                                             
051000* - - - - - - - - - - - - - *                                             
051100 IMS-GET-ARTIKEL SECTION.                                                 
051200*                            *** LÄSER AKTUELL ARTIKEL                    
051300*                            *** EJ UTGÅNGNA                              
051400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X                             
051500     '&KDERS    =' W-KDERS-X ')'                                          
051600             DELIMITED BY SIZE INTO SSA1                                  
051700     MOVE '  GE' TO GODK-STATUSKODER                                      
051800     CALL CBLTDLI USING GU WLARTC-PCB BE01-WLARTC01  SSA1                 
051900     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
052000     PERFORM IMS-STATUSKONTROLL                                           
052100     .                                                                    
052200     SKIP2                                                                
052300 IMS-GET-CLAGERINFO SECTION.                                              
052400                                                                          
052500     MOVE 'WLARTC11 ' TO SSA1                                             
052600     MOVE '  GE' TO GODK-STATUSKODER                                      
052700     CALL CBLTDLI USING GNP WLARTC-PCB BE11-WLARTC11 SSA1                 
052800     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
052900     PERFORM IMS-STATUSKONTROLL                                           
053000     .                                                                    
053100     EJECT                                                                
053200 IMS-GET-SKROT-INFO SECTION.                                              
053300                                                                          
053400     STRING  'WLARTC27(DASKROT >=' W-DASKROT-X ')'                        
053500              DELIMITED BY SIZE INTO SSA1                                 
053600     MOVE '  GE' TO GODK-STATUSKODER                                      
053700     CALL CBLTDLI USING GNP WLARTC-PCB BE27-WLARTC27 SSA1                 
053800     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
053900     PERFORM IMS-STATUSKONTROLL                                           
054000     .                                                                    
054100     SKIP2                                                                
054200 IMS-GHNP-WDK611 SECTION.                                                 
054300                                                                          
054400     MOVE 'WLARTC11 ' TO SSA1                                             
054500     MOVE '  GE' TO GODK-STATUSKODER                                      
054600     CALL CBLTDLI USING GHNP WLARTC-PCB BE11-WLARTC11 SSA1                
054700     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
054800     PERFORM IMS-STATUSKONTROLL                                           
054900     .                                                                    
055000     SKIP2                                                                
055100 IMS-REPL-WDK611 SECTION.                                                 
055200                                                                          
055300     MOVE '  ' TO GODK-STATUSKODER                                        
055400     CALL CBLTDLI USING REPL WLARTC-PCB BE11-WLARTC11                     
055500     MOVE WLARTC-STATUS-CODE TO STATUS-WS                                 
055600     PERFORM IMS-STATUSKONTROLL                                           
055700     .                                                                    
055800     EJECT                                                                
055900* - - - - - - - - - - - - - *                                             
056000*    OPERATIONER MOT WDK7   *                                             
056100* - - - - - - - - - - - - - *                                             
056200                                                                          
056300 IMS-GU-WDK701 SECTION.                                                   
056400                                                                          
056500     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
056600            DELIMITED BY SIZE INTO SSA1                                   
056700     MOVE '  GE' TO GODK-STATUSKODER                                      
056800     CALL CBLTDLI USING GU WLARTS-PCB SE01-WLARTS01 SSA1                  
056900     MOVE WLARTS-STATUS-CODE TO STATUS-WS                                 
057000     PERFORM IMS-STATUSKONTROLL                                           
057100     .                                                                    
057200     SKIP2                                                                
057300 IMS-GNP-WDK711 SECTION.                                                  
057400                                                                          
057500     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
057600            DELIMITED BY SIZE INTO SSA1                                   
057700     MOVE '  GE' TO GODK-STATUSKODER                                      
057800     CALL CBLTDLI USING GNP WLARTS-PCB SE11-WLARTS11 SSA1                 
057900     MOVE WLARTS-STATUS-CODE TO STATUS-WS                                 
058000     PERFORM IMS-STATUSKONTROLL                                           
058100     .                                                                    
058200     EJECT                                                                
058300* - - - - - - - - - - - - - *                                             
058400*    OPERATIONER MOT WDH7   *                                             
058500* - - - - - - - - - - - - - *                                             
058600                                                                          
058700 IMS-GU-INVHIST-ROT SECTION.                                              
058800                                                                          
058900     STRING 'WLINVC01(IDARTNR  =' W-IDARTNR-X ')'                         
059000            DELIMITED BY SIZE INTO SSA1                                   
059100     MOVE '  GE' TO GODK-STATUSKODER                                      
059200     CALL CBLTDLI USING GU WLINVC-PCB WLINVC01 SSA1                       
059300     MOVE WLINVC-STATUS-CODE TO STATUS-WS                                 
059400     PERFORM IMS-STATUSKONTROLL                                           
059500     .                                                                    
059600     EJECT                                                                
059700 IMS-ISRT-INVHIST-ROT SECTION.                                            
059800                                                                          
059900     ADD +1   TO CHKP-ANT                                                 
060000                                                                          
060100     MOVE 'WLINVC01 ' TO SSA1                                             
060200     MOVE '  ' TO GODK-STATUSKODER                                        
060300     CALL CBLTDLI USING ISRT WLINVC-PCB WLINVC01 SSA1                     
060400     MOVE WLINVC-STATUS-CODE TO STATUS-WS                                 
060500     PERFORM IMS-STATUSKONTROLL                                           
060600     .                                                                    
060700     SKIP2                                                                
060800 IMS-ISRT-INVHIST-SEGM SECTION.                                           
060900                                                                          
061000     ADD +1   TO CHKP-ANT                                                 
061100                                                                          
061200     MOVE 'WLINVC11*F' TO SSA1                                            
061300     MOVE '  II' TO GODK-STATUSKODER                                      
061400     CALL CBLTDLI USING ISRT WLINVC-PCB WLINVC11 SSA1                     
061500     MOVE WLINVC-STATUS-CODE TO STATUS-WS                                 
061600     PERFORM IMS-STATUSKONTROLL                                           
061700     .                                                                    
061800     EJECT                                                                
061900 IMS-RESTART SECTION.                                                     
062000                                                                          
062100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
062200     MOVE '  ' TO GODK-STATUSKODER                                        
062300     CALL CBLTDLI USING XRST MSG-PCB                                      
062400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
062500                        CHKP-AREA-LENGTH CHKP-AREA                        
062600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
062700     PERFORM IMS-STATUSKONTROLL                                           
062800     .                                                                    
062900     EJECT                                                                
063000                                                                          
063100 IMS-GU-WDB601    SECTION.                                                
063200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
063300          DELIMITED BY SIZE INTO SSA1                                     
063400     MOVE '  ' TO GODK-STATUSKODER                                        
063500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
063600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
063700     PERFORM IMS-STATUSKONTROLL                                           
063800     .                                                                    
063900     EJECT                                                                
064000                                                                          
064100 IMS-CHECKPOINT SECTION.                                                  
064200                                                                          
064300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
064400     MOVE '  XD' TO GODK-STATUSKODER                                      
064500     CALL CBLTDLI USING CHKP MSG-PCB                                      
064600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
064700                        CHKP-AREA-LENGTH CHKP-AREA                        
064800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
064900     PERFORM IMS-STATUSKONTROLL                                           
065000                                                                          
065100     IF IMS-EJ-OK                                                         
065200       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
065300       DISPLAY FELTEXT                                                    
065400       CALL FELLOG                                                        
065500     END-IF                                                               
065600     .                                                                    
065700     EJECT                                                                
065800 IMS-STATUSKONTROLL SECTION.                                              
065900                                                                          
066000     SET STATUS-IX TO 1                                                   
066100     SEARCH GODK-STATUS                                                   
066200       AT END                                                             
066300         CALL FELLOG                                                      
066400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
066500         CONTINUE                                                         
066600     END-SEARCH                                                           
066700     .                                                                    
