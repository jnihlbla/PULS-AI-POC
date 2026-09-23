000100 ID DIVISION.                                                             
000200 PROGRAM-ID. W5010200.                                                    
000300*AUTHOR. S.OHLSSON - M-A EVERBÄCK.                                        
000400                                                                          
000500*DATE-WRITTEN. APRIL 1979.                                                
000600                                                                          
000700                                                                          
000800*    FUNKTION.                                                            
000900                                                                          
001000*        TP-PROGRAM FÖR EKONOMI (LAGERBOKFÖRINGSBILD)                     
001100                                                                          
001200*        PROGRAMMET LÄSER DATABASERNA WDK6 OCH WDD3.                      
001300*        VISAR VÄRDEN FÖR CDC OCH TERMINAL DÄR VÄRDEN FINNS.              
001400                                                                          
001500*        OM KDERS > 0 SKRIVS FEL-4.                                       
001600*    INDATA.                                                              
001700*        TRANSAKTION:  W5T102                                             
001800*        MID:  W5I10201                                                   
001900*    UTDATA.                                                              
002000*        MOD:  W5O10201                                                   
002100*    SUBPROGRAM.                                                          
002200*        FELLOG                                                           
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600 DATA DIVISION.                                                           
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000 77  IDPGM               PIC X(8)    VALUE 'W5010200'.                    
003100                                                                          
003200 77  IDARTNR-WS          PIC X(9)    VALUE SPACE.                         
003300 77  INDX                PIC S9(9)   VALUE ZERO COMP SYNC.                
003400 77  PRARTSTD            PIC S9(7)V9(2) COMP-3.                           
003500     SKIP2                                                                
003600 77  DAGENS-AAAAMMDD     PIC 9(8)  VALUE ZERO.                            
003700 77  WS-PRARTBES         PIC X       VALUE 'N'.                           
003800                                                                          
003900 01  DYNAMISKA-SUBPROGRAM.                                                
004000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
004300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
004400*01 -COPY WMSGINIT                                                        
004500                                                                          
004600 01  KONSTANTER.                                                          
004700     03  JA              PIC X       VALUE 'J'.                           
004800     03  NEJ             PIC X       VALUE 'N'.                           
004900     03  ARTIKEL-RETT    PIC X.                                           
005000     03  ARTIKEL-FINNS   PIC X.                                           
005100                                                                          
005200 01  W-IDARTNR-X.                                                         
005300     03  W-IDARTNR       PIC S9(9)   COMP-3.                              
005400                                                                          
005500 01  W-DAPRLIST-X.                                                        
005600     03  W-DAPRLIST      PIC 9(8)  VALUE ZERO.                            
005700                                                                          
005800 01  W-KDSEGKEY-X.                                                        
005900     03  W-KDSEGKEY      PIC X(1)    VALUE '1'.                           
006000     SKIP2                                                                
006100 01  W-IDSKYLT-X.                                                         
006200     03  W-IDSKYLT       PIC X(3).                                        
006300                                                                          
006400 01  W-IDDC-B6-X.                                                         
006500     03 W-IDDC-B6        PIC X(2).                                        
006600                                                                          
006700     SKIP2                                                                
006800 01  FELMED.                                                              
006900     03  FEL-1           PIC X(26)   VALUE 'PART NUMBER IS NOT NUM        
007000-                                          'ERIC'.                        
007100     03  FEL-2           PIC X(35)   VALUE 'THIS ARTICLE IS NOT IN        
007200-                                          ' THE DATABASE'.               
007300     03  FEL-3           PIC X(29)   VALUE 'THIS ARTICLE HAS BEEN         
007400-                                          'DELETED'.                     
007500     03  FEL-4           PIC X(30)   VALUE 'THIS ARTICLE HAS BEEN         
007600-                                          'REPLACED'.                    
007700     EJECT                                                                
007800*****  TP-AREOR                                                           
007900*                                                                         
008000*****  MIDEN                                                              
008100*  03    MID -COPY W5I10201  -PRE MID-.                                   
008200     EJECT                                                                
008300*01  -COPY WMSGAREA.                                                      
008400     EJECT                                                                
008500*****  MODEN                                                              
008600*  03    MOD -COPY W5O10201 -PRE MOD- -RED MSG-AREA.                      
008700     EJECT                                                                
008800*01  -COPY WMFSAREA.                                                      
008900     EJECT                                                                
009000*****  ARBETSAREOR FÖR IMS-SEKTIONERNA.                                   
009100*                                                                         
009200 01  IMS-WS.                                                              
009300     03  FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
009400     SKIP2                                                                
009500*****  STATUSKOD FRÅN IMS                                                 
009600     03  STATUS-WS       PIC XX.                                          
009700      88 SEGMENT-FINNS               VALUE '  '.                          
009800      88 SEGMENT-SAKNAS              VALUE 'GE'.                          
009900     SKIP2                                                                
010000     03  GODK-STATUSKODER.                                                
010100      04 GODK-STATUS  OCCURS 5  INDEXED BY STATUS-IX  PIC XX.             
010200     SKIP3                                                                
010300     03  SSA1              PIC X(50).                                     
010400     03  SSA2              PIC X(50).                                     
010500     EJECT                                                                
010600*****  IMS-FUNKTIONSKODER                                                 
010700     SKIP2                                                                
010800*    03  -COPY  W0003.                                                    
010900     SKIP2                                                                
011000*****  DLI I/O-AREA                                                       
011100 01  DLI-IO-AREA.                                                         
011200*03  WLARTC01 -COPY WDK601                                                
011300     SKIP3                                                                
011400 01  DLI-IO-AREA2.                                                        
011500*03  WLARTC11 -COPY WDK611                                                
011600     SKIP3                                                                
011700 01  DLI-IO-K621.                                                         
011710*03  WLARTC21 -COPY WDK621                                                
011720     SKIP3                                                                
011800 01  FILLER.                                                              
011900 03  DLI-IO-AREA3        PIC X(200)  VALUE SPACE.                         
012000     SKIP3                                                                
012100*03  WLBENA11 -COPY WDD311 -PRE BEN-   -RED DLI-IO-AREA3                  
012200                                                                          
012300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
012400 01   DLI-IO-AREA-B601.                                                   
012500*     03  -COPY WDB601                                                    
012600                                                                          
012700     EJECT                                                                
012800 LINKAGE SECTION.                                                         
012900*    -COPY W0009 -PRE MSG-                                                
013000     EJECT                                                                
013100*01  -COPY W0008     -PRE USEA-.                                          
013200         05  FILLER           PIC X.                                      
013300     EJECT                                                                
013400*01  -COPY W0008  -PRE ARTC-                                              
013500       05  ARTC-KONKAT-KEY PIC X.                                         
013600     EJECT                                                                
013700*01  -COPY W0008  -PRE BEN-                                               
013800       05  BEN-KONKAT-KEY PIC X.                                          
013900     EJECT                                                                
014000*01  -COPY W0008     -PRE WDB6-.                                          
014100         05  FILLER           PIC X.                                      
014200     EJECT                                                                
014300 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
014400                                  ARTC-PCB BEN-PCB WDB6-PCB.              
014500     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
014600                                   ARTC-PCB BEN-PCB WDB6-PCB.             
014700     SKIP1                                                                
014800     PERFORM IMS-GET-MSG                                                  
014900     IF SEGMENT-FINNS                                                     
015000       PERFORM A-KOLLA-NYCKLAR                                            
015100       IF ARTIKEL-RETT = JA                                               
015200         PERFORM C-INFORMATIONSBILD                                       
015300       END-IF                                                             
015400       PERFORM IMS-ISRT-MSG                                               
015500     END-IF                                                               
015600     MOVE ZERO TO RETURN-CODE                                             
015700     GOBACK                                                               
015800     .                                                                    
015900     EJECT                                                                
016000 A-KOLLA-NYCKLAR SECTION.                                                 
016100*                                                                         
016200     IF MSG-DUBBLA-TRANSKODER                                             
016300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I10201                 
016400       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
016500       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
016600     ELSE                                                                 
016700       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W5I10201                   
016800       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
016900       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
017000     END-IF                                                               
017100     SKIP2                                                                
017200                                                                          
017300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
017400     MOVE '001'             TO MSGI-KDCALL                                
017500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
017600     MOVE '5102'               TO MSGI-IDTRANS                            
017700     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
017800     IF MFS-IDTRANS = '5102'                                              
017900     OR (MID-IDARTNR-IN NUMERIC                                           
018000     AND MID-IDARTNR-IN > ZERO)                                           
018100         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
018200     END-IF                                                               
018300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
018400     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
018500     INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                   
018600                                                                          
018700     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
018800         MOVE 'S  ' TO W-IDSKYLT                                          
018900     ELSE                                                                 
019000         MOVE 'GB ' TO W-IDSKYLT                                          
019100     END-IF                                                               
019200                                                                          
019300     SKIP1                                                                
019400     IF IDARTNR-WS NUMERIC                                                
019500       MOVE JA TO ARTIKEL-RETT                                            
019600     ELSE                                                                 
019700       MOVE NEJ TO ARTIKEL-RETT                                           
019800       MOVE FEL-1 TO MOD-MESSAGE                                          
019900     END-IF                                                               
020000                                                                          
020100     MOVE MSGI-IDDC     TO W-IDDC-B6                                      
020200     PERFORM IMS-GU-WDB601                                                
020300                                                                          
020400     SKIP2                                                                
020500     MOVE LOW-VALUE TO MOD-W5O10201                                       
020600     MOVE 'W5O102N1' TO MFS-IDMOD                                         
020700     MOVE '5102' TO MOD-TRANS-NUMMER                                      
020800     SKIP1                                                                
020900     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
021000     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
021100     SKIP1                                                                
021200     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
021300                                                                          
021400     COMPUTE MSG-KVLL = LENGTH OF MOD-W5O10201 + 4                        
021500     .                                                                    
021600     EJECT                                                                
021700****************************************************************          
021800*    INFORMATIONSVÄRDEN PÅ ÖNSKAT ARTIKELNR LÄGGS UT                      
021900*    INFORMATIONEN HÄMTAS FRÅN WDD3 OCH WDK6                              
022000*                                                                         
022100 C-INFORMATIONSBILD SECTION.                                              
022200*                                                                         
022300     MOVE NEJ TO ARTIKEL-FINNS                                            
022400     MOVE IDARTNR-WS TO W-IDARTNR                                         
022500     MOVE +1 TO INDX                                                      
022600                                                                          
022700     PERFORM IMS-GET-ARTIKEL-WDK601                                       
022800*                                                                         
022900     IF SEGMENT-FINNS                                                     
023000       MOVE '-'           TO MOD-BINDESTRECK                              
023100       MOVE ART-IDLEVNR  TO MOD-IDLEVNR                                   
023200       MOVE ART-REKSIFFR TO MOD-REKSIFFR                                  
023300       MOVE ART-KDPRODSL TO MOD-KDPRODSL                                  
023400       IF ART-KDERS-UTG > 0                                               
023500         MOVE FEL-3 TO MOD-MESSAGE                                        
023600       END-IF                                                             
023700*                                                                         
023800*                                          WDK611                         
023900*                                                                         
024000*                                                                         
024100       PERFORM IMS-GNP-WDK611                                             
024200       IF SEGMENT-FINNS                                                   
024300         MOVE CLAG-KDVVKL   TO MOD-KDVVKL                                 
024400         MOVE CLAG-IDANSK   TO MOD-IDANSK                                 
024500         MOVE CLAG-IDLEVNR-SEN TO MOD-IDLEVNR-SEN (INDX)                  
024600         MOVE CLAG-PRHEMTAG TO MOD-PRHEMTAG                               
024700         MOVE CLAG-PRINK    TO MOD-PRINK                                  
024800         MOVE CLAG-PRARTSTD TO MOD-PRARTSTD                               
024900         MOVE CLAG-PRARTSTD TO PRARTSTD                                   
024901                                                                          
024910         MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD               
024920         COMPUTE W-DAPRLIST = 99999999 - DAGENS-AAAAMMDD                  
024930         PERFORM IMS-GNP-WDK621                                           
024940         IF SEGMENT-SAKNAS                                                
024950           MOVE CLAG-PRARTSTD       TO MOD-PRARTBES                       
024960         ELSE                                                             
024970           MOVE NEJ                 TO WS-PRARTBES                        
024980           PERFORM UNTIL  SEGMENT-SAKNAS                                  
024990             IF PRL-SUINLEV-PR > ZERO                                     
024991               MOVE PRL-PRARTBES-PR  TO MOD-PRARTBES                      
024992               SET SEGMENT-SAKNAS TO TRUE                                 
024993             ELSE                                                         
024994               IF WS-PRARTBES = NEJ                                       
024995                 MOVE PRL-PRARTBES-PR TO MOD-PRARTBES                     
024996                 MOVE JA              TO WS-PRARTBES                      
024997               END-IF                                                     
024998               PERFORM IMS-GNP-WDK621                                     
024999             END-IF                                                       
025000           END-PERFORM                                                    
025001         END-IF                                                           
025010                                                                          
025100         MOVE CLAG-PRARTSJK TO MOD-PRARTSJK                               
025200         MOVE CLAG-KDVTH    TO MOD-KDVTH                                  
025300         MOVE CLAG-KDKG     TO MOD-KDKG                                   
025400         MOVE CLAG-PRDIRLON TO MOD-PRDIRLON                               
025500         MOVE CLAG-PRDMTRL  TO MOD-PRDMTRL                                
025600         MOVE CLAG-PROVRPAL TO MOD-PROVRPAL                               
025700         MOVE CLAG-KDTIPPR  TO MOD-KDTIPPR                                
025800         MOVE CLAG-KDGK     TO MOD-KDGK                                   
025900         MOVE CLAG-KDLTK    TO MOD-KDLTK                                  
026000         IF DCS-NDC-NA                                                    
026100           MOVE  'LPC          ' TO MOD-HEADING                           
026200           MOVE  CLAG-KDPSLLOC    TO MOD-KDPSLLOC                         
026300         ELSE                                                             
026400           MOVE  SPACE TO MOD-HEADING                                     
026500                          MOD-KDPSLLOC                                    
026600         END-IF                                                           
026700         MOVE CLAG-KDERS TO MOD-KDERS (INDX)                              
026800         IF CLAG-KDERS > 0                                                
026900           MOVE FEL-4       TO MOD-MESSAGE                                
027000         END-IF                                                           
027100         MOVE CLAG-KVUTRS TO MOD-KVUTRS (INDX)                            
027200         MOVE CLAG-KVAKS-CDC TO MOD-KVAKS (INDX)                          
027300         MOVE CLAG-KVEFRS TO MOD-KVEFRS (INDX)                            
027400         MOVE CLAG-KVRESS TO MOD-KVRESS (INDX)                            
027500         COMPUTE MOD-KVDISP (INDX) =                                      
027600             CLAG-KVLS - CLAG-KVRESS - CLAG-KVUTRS                        
027700         MULTIPLY CLAG-KVAKS-CDC BY PRARTSTD GIVING MOD-VAERDE1           
027800         (INDX)                                                           
027900         MULTIPLY CLAG-KVEFRS BY PRARTSTD GIVING MOD-VAEWDE2              
028000         (INDX)                                                           
028100         MULTIPLY CLAG-KVLS BY PRARTSTD GIVING MOD-VAERDE3                
028200         (INDX)                                                           
028300         COMPUTE MOD-VAEWDE4 (INDX) =                                     
028400        (CLAG-KVAKS-CDC + CLAG-KVEFRS + CLAG-KVLS) * PRARTSTD             
028500       END-IF                                                             
028600                                                                          
028700*                                          WDD311, BENÄMNING              
028800*                                                                         
028900       PERFORM IMS-GU-BEN                                                 
029000       IF SEGMENT-FINNS                                                   
029100         MOVE BEN-TEXT-BEART TO MOD-BEART                                 
029200       ELSE                                                               
029300         MOVE SPACE TO MOD-BEART                                          
029400       END-IF                                                             
029500     ELSE                                                                 
029600       MOVE FEL-2         TO MOD-MESSAGE                                  
029700     END-IF                                                               
029800     .                                                                    
029900     EJECT                                                                
030000*****  IMS-SEKTIONER.                                                     
030100*                                                                         
030200*****  CALL MOT MSG                                                       
030300 IMS-GET-MSG SECTION.                                                     
030400     MOVE '  QC' TO GODK-STATUSKODER                                      
030500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
030600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
030700     PERFORM IMS-STATUS-KONTROLL                                          
030800     .                                                                    
030900     SKIP3                                                                
031000 IMS-ISRT-MSG SECTION.                                                    
031100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
031200       MOVE '0' TO MFS-KDHUVOMR                                           
031300     END-IF                                                               
031400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
031500     MOVE SPACE TO GODK-STATUSKODER                                       
031600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
031700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
031800     PERFORM IMS-STATUS-KONTROLL                                          
031900     .                                                                    
032000*****  CALL MOT DLI                                                       
032100     SKIP3                                                                
032200 IMS-GET-ARTIKEL-WDK601 SECTION.                                          
032300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
032400     DELIMITED BY SIZE INTO SSA1                                          
032500     MOVE '  GE' TO GODK-STATUSKODER                                      
032600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
032700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
032800     PERFORM IMS-STATUS-KONTROLL                                          
032900     .                                                                    
033000     EJECT                                                                
033100 IMS-GNP-WDK611 SECTION.                                                  
033200     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
033300     DELIMITED BY SIZE INTO SSA1                                          
033400     MOVE '  GE' TO GODK-STATUSKODER                                      
033500     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA2 SSA1                    
033600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
033700     PERFORM IMS-STATUS-KONTROLL                                          
033800     .                                                                    
033900     SKIP3                                                                
033910 IMS-GNP-WDK621 SECTION.                                                  
033920     STRING 'WLARTC21(DAPRLIST=>' W-DAPRLIST-X ')'                        
033930     DELIMITED BY SIZE INTO SSA1                                          
033940     MOVE '  GE' TO GODK-STATUSKODER                                      
033950     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-K621 SSA1                     
033960     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
033970     PERFORM IMS-STATUS-KONTROLL                                          
033980     .                                                                    
033990     SKIP3                                                                
034000 IMS-GU-BEN  SECTION.                                                     
034100                                                                          
034200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X  ')'                        
034300     DELIMITED BY SIZE INTO SSA1                                          
034400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X  ')'                        
034500     DELIMITED BY SIZE INTO SSA2                                          
034600     MOVE '  GE' TO GODK-STATUSKODER                                      
034700     CALL CBLTDLI USING GU    BEN-PCB DLI-IO-AREA3 SSA1 SSA2              
034800     MOVE BEN-STATUS-CODE TO STATUS-WS                                    
034900     PERFORM IMS-STATUS-KONTROLL                                          
035000     .                                                                    
035100     EJECT                                                                
035200 IMS-GU-WDB601    SECTION.                                                
035300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
035400          DELIMITED BY SIZE INTO SSA1                                     
035500     MOVE '  GE' TO GODK-STATUSKODER                                      
035600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
035700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
035800     PERFORM IMS-STATUS-KONTROLL                                          
035900     IF SEGMENT-SAKNAS                                                    
036000         MOVE SPACE TO DCS-KDDC                                           
036100     END-IF                                                               
036200     .                                                                    
036300 IMS-STATUS-KONTROLL SECTION.                                             
036400     SET STATUS-IX TO 1                                                   
036500     SEARCH GODK-STATUS                                                   
036600       AT END                                                             
036700         CALL FELLOG                                                      
036800     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
036900     END-SEARCH                                                           
037000     .                                                                    
