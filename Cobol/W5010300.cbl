000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5010300.                                                
000300*AUTHOR.         MARIE-ANN EVERBÄCK.                                      
000400*DATE-COMPILED.                                                           
000500*DATE-WRITTEN.   APRIL 1979.                                              
000600*    FUNKTION.   TP-FRÅGE-PROGRAM FÖR ANSKAFFNINGEN                       
000700*                (FRÅGECENTRAL 34000)                                     
000800*                                                                         
000900*                VISAR INFORMATION FÖR CDC OCH ST                         
001000*                                                                         
001100*                                                                         
001200                                                                          
001300                                                                          
001400     SKIP2                                                                
001500*    INDATA.                                                              
001600*        TRANSAKTION: W5T103                                              
001700*        MID:         W5I10301                                            
001800*    UTDATA.                                                              
001900*        MOD:         W5O10301                                            
002000*    SUBPROGRAM.                                                          
002100*        FELLOG                                                           
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP3                                                                
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900 77  IDPGM               PIC X(8)    VALUE 'W5010300'.                    
003000 77  DAGENS-AAAAMMDD     PIC 9(8)    VALUE ZERO.                          
003010 77  WS-PRARTBES         PIC X       VALUE 'N'.                           
003020 77      IDARTNR-WS      PIC X(9).                                        
003100 77      W-SUMMA1        PIC S9(7)     COMP-3.                            
003200 77      W-KDCLPOST      PIC S9(1)     COMP-3.                            
003300 77      W-KDERS-UTG     PIC S9(3)     COMP-3.                            
003400 77      INDX            PIC S9(4)     COMP SYNC.                         
003500                                                                          
003600 01  GENERELLA-SUBPROGRAM.                                                
003700     03  CBLTDLI         PIC X(8)    VALUE 'CBLTDLI '.                    
003800     03  FELLOG          PIC X(8)    VALUE 'FELLOG  '.                    
003900     03  W005INIT        PIC X(8)    VALUE 'W005INIT'.                    
004000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
004100*01 -COPY WMSGINIT                                                        
004200                                                                          
004300 01  FILLER.                                                              
004400     03  W-SUMMA2        PIC S9(8)V9   COMP-3 OCCURS 2.                   
004500                                                                          
004600 01  KONSTANTER.                                                          
004700     03  JA                  PIC X       VALUE 'J'.                       
004800     03  NEJ                 PIC X       VALUE 'N'.                       
004900     03  ARTIKEL-RETT        PIC X.                                       
005000     03  ARTIKEL-FINNS       PIC X.                                       
005100                                                                          
005200 01      W-IDARTNR-X.                                                     
005300   03    W-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.                  
005310                                                                          
005320 01  W-DAPRLIST-X.                                                        
005330     03  W-DAPRLIST      PIC   9(8)  VALUE ZERO.                          
005400                                                                          
005500 01      W-WDD901KY-X.                                                    
005600   03    W-IDARTNR-D9    PIC S9(9)   VALUE ZERO  COMP-3.                  
005700   03    W-IDDC-D9       PIC  X(2)   VALUE SPACE.                         
005800                                                                          
005900 01      W-KDSEGKEY-X.                                                    
006000   03    W-KDSEGKEY      PIC X(1)    VALUE '1'.                           
006100                                                                          
006200 01      W-IDLEVNR-X.                                                     
006300   03    W-IDLEVNR       PIC  X(5)   VALUE SPACE.                         
006400                                                                          
006500 01      W-IDSKYLT-X.                                                     
006600   03    W-IDSKYLT       PIC X(3)    VALUE 'S  '.                         
006700     SKIP2                                                                
006800 01      FELMEDDELANDE.                                                   
006900   03    MEDDELANDE-1    PIC X(40)                                        
007000      VALUE 'ARTIKELN FINNS EJ I ARTIKELREGISTRET'.                       
007100                                                                          
007200   03    MEDDELANDE-2    PIC X(40)                                        
007300      VALUE 'ARTIKELN ÄR ERSATT'.                                         
007400   03    MEDDELANDE-3    PIC X(40)                                        
007500      VALUE 'ARTIKELN ÄR BORTTAGEN UR ARTIKELREG'.                        
007600     EJECT                                                                
007700*                        ****    TP-AREOR                                 
007800 01  FILLER  PIC X(16)   VALUE '    TP-AREAOR   '.                        
007900     SKIP2                                                                
008000*01      MID -COPY W5I10301 -PRE MID-.                                    
008100     EJECT                                                                
008200*01      -COPY WMSGAREA                                                   
008300     EJECT                                                                
008400*  03    MOD -COPY W5O10301 -PRE MOD- -RED MSG-AREA.                      
008500     EJECT                                                                
008600*01  -COPY WMFSAREA.                                                      
008700     EJECT                                                                
008800******************************************************************        
008900*****                                                                     
009000*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009100*****                                                                     
009200 01  IMS-WS.                                                              
009300   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
009400     SKIP3                                                                
009500*****                    **** STATUS-KOD FRÅN IMS                         
009600   03    STATUS-WS       PIC XX.                                          
009700         88  SEGMENT-FINNS       VALUE '  '.                              
009800         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
009900     SKIP3                                                                
010000   03    GODK-STATUSKODER.                                                
010100     05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.                
010200     SKIP3                                                                
010300 01      SSA1            PIC X(50).                                       
010400 01      SSA2            PIC X(50).                                       
010500     EJECT                                                                
010600*                            IMS FUNKTIONSKODER                           
010700*01      -COPY W0003                                                      
010800     EJECT                                                                
010900*                            DLI INPUT-OUTPUT AREA                        
011000 01      DLI-IO-AREA.                                                     
011100     SKIP3                                                                
011200*-------- WDK6-ARTIKELREG                                                 
011300                                                                          
011400*03  WLARTC01     -COPY WDK601                                            
011500     SKIP3                                                                
011600*                            DLI INPUT-OUTPUT AREA                        
011700 01      DLI-IO-AREA2.                                                    
011800     SKIP3                                                                
011900*03  WLARTC11     -COPY WDK611                                            
011910     SKIP3                                                                
011911 01  DLI-IO-K621.                                                         
011920*03  WLARTC21     -COPY WDK621                                            
012000     EJECT                                                                
012100*------ WDD9-LEVERANSPLANEREGISTER--------------                          
012200 01      FILLER.                                                          
012300 03      DLI-IO-AREA3    PIC X(200)  VALUE SPACE.                         
012400     SKIP3                                                                
012500                                                                          
012600*03  WLINLB11     -COPY WDD902 -PRE LEV- -RED DLI-IO-AREA3.               
012700     EJECT                                                                
012800*------ WDD7-ERSÄTTNINGSREGISTER-------------------                       
012900                                                                          
013000*03  WLERSA11     -COPY WDD702 -PRE ERS- -RED DLI-IO-AREA3.               
013100     EJECT                                                                
013200*------ WDD3-BENÄMNINGSREGISTER--------------------                       
013300                                                                          
013400*03  WLBENA11     -COPY WDD311 -PRE BEN- -RED DLI-IO-AREA3.               
013500     EJECT                                                                
013600 LINKAGE SECTION.                                                         
013700                                                                          
013800*01  -COPY W0009     -PRE MSG-                                            
013900     EJECT                                                                
014000*01  -COPY W0008     -PRE USEA-.                                          
014100         05  FILLER           PIC X.                                      
014200                                                                          
014300*01  -COPY W0008     -PRE ARTC-.                                          
014400         05  FILLER           PIC X.                                      
014500     EJECT                                                                
014600                                                                          
014700*01  -COPY W0008     -PRE ERS-.                                           
014800         05  FILLER           PIC X.                                      
014900                                                                          
015000*01  -COPY W0008     -PRE BEN-.                                           
015100         05  FILLER           PIC X.                                      
015200                                                                          
015300*01  -COPY W0008     -PRE INLB-.                                          
015400         05  FILLER           PIC X.                                      
015500     EJECT                                                                
015600 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
015700                                  ARTC-PCB                                
015800                                  ERS-PCB                                 
015900                                  BEN-PCB                                 
016000                                  INLB-PCB.                               
016100                                                                          
016200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
016300                                   ARTC-PCB                               
016400                                   ERS-PCB                                
016500                                   BEN-PCB                                
016600                                   INLB-PCB.                              
016700     SKIP2                                                                
016800     PERFORM IMS-GET-MSG                                                  
016900     IF SEGMENT-FINNS                                                     
017000       PERFORM A-KOLLA-NYCKLAR                                            
017100       IF ARTIKEL-RETT = JA                                               
017200         PERFORM C-ARTIKEL-FINNS                                          
017300         IF ARTIKEL-FINNS = JA                                            
017400           PERFORM D-INFORMATIONSBILD                                     
017500         END-IF                                                           
017600       END-IF                                                             
017700       PERFORM IMS-INSERT-MSG                                             
017800     END-IF                                                               
017900     MOVE ZERO TO RETURN-CODE                                             
018000     GOBACK                                                               
018100     .                                                                    
018200     EJECT                                                                
018300 A-KOLLA-NYCKLAR   SECTION.                                               
018400     SKIP2                                                                
018500     IF MSG-DUBBLA-TRANSKODER                                             
018600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I10301                 
018700       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
018800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
018900     ELSE                                                                 
019000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I10301                  
019100       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
019200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
019300     END-IF                                                               
019400     SKIP2                                                                
019500                                                                          
019600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
019700     MOVE '001'             TO MSGI-KDCALL                                
019800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
019900     MOVE '5103'               TO MSGI-IDTRANS                            
020000     MOVE MSG-LTERM-NAME       TO MSGI-IDLTERM-USER                       
020100     IF MFS-IDTRANS = '5103'                                              
020200     OR (MID-IDARTNR-IN NUMERIC                                           
020300     AND MID-IDARTNR-IN > ZERO)                                           
020400         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
020500     END-IF                                                               
020600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
020700     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
020800        MOVE 'S  '  TO  W-IDSKYLT                                         
020900     ELSE                                                                 
021000        MOVE 'GB '  TO  W-IDSKYLT                                         
021100     END-IF                                                               
021200     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
021300     INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                   
021400                                                                          
021500     IF IDARTNR-WS NUMERIC                                                
021600       MOVE JA TO ARTIKEL-RETT                                            
021700     ELSE                                                                 
021800       MOVE NEJ TO ARTIKEL-RETT                                           
021900       MOVE MEDDELANDE-1 TO MOD-MESSAGE-1                                 
022000     END-IF                                                               
022100     SKIP2                                                                
022200     MOVE LOW-VALUE TO MOD-W5O10301                                       
022300     MOVE 'W5O103N1' TO MFS-IDMOD                                         
022400     MOVE '-' TO MOD-STRECK                                               
022500     MOVE '5103' TO MOD-TRANS-NUMMER                                      
022600     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
022700     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
022800                                                                          
022900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
023000                                                                          
023100     COMPUTE MSG-KVLL = LENGTH OF MOD-W5O10301 + 4                        
023200     .                                                                    
023300     EJECT                                                                
023400******************************************************************        
023500*    KONTROLL ATT ARTIKEL FINNS PÅ WDK6                                   
023600*                                                                         
023700*                                                                         
023800 C-ARTIKEL-FINNS SECTION.                                                 
023900     MOVE NEJ TO ARTIKEL-FINNS                                            
024000     MOVE IDARTNR-WS TO W-IDARTNR                                         
024100                                                                          
024200*                                          WDK601, ARTIKEL-INFO           
024300                                                                          
024400     PERFORM IMS-LAES-ARTIKEL-WDK601                                      
024500     IF SEGMENT-FINNS                                                     
024600       MOVE JA TO ARTIKEL-FINNS                                           
024700       MOVE ART-KDERS-UTG TO W-KDERS-UTG                                  
024800       MOVE ART-REKSIFFR  TO MOD-REKSIFFR                                 
024900       MOVE ART-KDSORT    TO MOD-KDSORT                                   
025000       IF ART-KDERS-UTG > 0                                               
025100         MOVE MEDDELANDE-3 TO MOD-MESSAGE-1                               
025200       END-IF                                                             
025300     ELSE                                                                 
025400       MOVE MEDDELANDE-1   TO MOD-MESSAGE-1                               
025500     END-IF                                                               
025600     .                                                                    
025700     EJECT                                                                
025800*****************************************************************         
025900*    INFORMATIONSVÄRDEN PÅ ÖNSKAT ARTIKELNR LÄGGS UT                      
026000*    INFORMATIONEN HÄMTAS FRÅN WDD1, WDK6, WDD7 OCH WDD9                  
026100*                                                                         
026200 D-INFORMATIONSBILD SECTION.                                              
026300*                                          WDK601, ARTIKEL-INFO           
026400*                                          FRÅN C-ARTIKEL-FINNS           
026500*                                          SECTION.                       
026600*                                                                         
026700*                                                                         
026800     MOVE ART-IDLEVNR TO W-IDLEVNR                                        
026900                          MOD-IDLEVNR                                     
027000*                                                                         
027100*                                                                         
027200     SKIP2                                                                
027300*                                          WDD311,BENÄMNING               
027400                                                                          
027500     PERFORM IMS-GU-BEN                                                   
027600     IF SEGMENT-FINNS                                                     
027700       MOVE BEN-TEXT-BEART TO MOD-BEART-SVE                               
027800     ELSE                                                                 
027900       MOVE SPACE          TO MOD-BEART-SVE                               
028000     END-IF                                                               
028100                                                                          
028200     MOVE +1 TO INDX                                                      
028300     PERFORM IMS-LAES-WDK611                                              
028400     IF SEGMENT-FINNS                                                     
028500       MOVE CLAG-IDANSK    TO MOD-IDANSK                                  
028600       MOVE CLAG-IDINK     TO MOD-IDINK                                   
028700       MOVE CLAG-KDHF      TO MOD-KDHF                                    
028800       MOVE CLAG-TIAVIDAT-SEN TO MOD-TIAVIDAT-SEN (INDX)                  
028900       COMPUTE MOD-KVPB = CLAG-KVPB-SATS + CLAG-KVPB-SEP                  
029000       MOVE CLAG-PRINK    TO MOD-PRINK                                    
029100       MOVE CLAG-PRARTSTD TO MOD-PRARTSTD                                 
029110                                                                          
029120       MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD                 
029130       COMPUTE W-DAPRLIST = 99999999 - DAGENS-AAAAMMDD                    
029140       PERFORM IMS-GNP-WDK621                                             
029150       IF SEGMENT-SAKNAS                                                  
029160         MOVE CLAG-PRARTSTD       TO MOD-PRARTBES                         
029170       ELSE                                                               
029180         MOVE NEJ                 TO WS-PRARTBES                          
029190         PERFORM UNTIL  SEGMENT-SAKNAS                                    
029191           IF PRL-SUINLEV-PR > ZERO                                       
029192             MOVE PRL-PRARTBES-PR  TO MOD-PRARTBES                        
029193             SET SEGMENT-SAKNAS TO TRUE                                   
029194           ELSE                                                           
029195             IF WS-PRARTBES = NEJ                                         
029196               MOVE PRL-PRARTBES-PR TO MOD-PRARTBES                       
029197               MOVE JA              TO WS-PRARTBES                        
029198             END-IF                                                       
029199             PERFORM IMS-GNP-WDK621                                       
029200           END-IF                                                         
029201         END-PERFORM                                                      
029202       END-IF                                                             
029203                                                                          
029300       MOVE CLAG-PRARTSJK TO MOD-PRARTSJK                                 
029400       MOVE CLAG-KDTIPPR  TO MOD-KDTIPPR                                  
029500       MOVE CLAG-KDKG     TO MOD-KDKG                                     
029600       MOVE CLAG-PRHEMTAG TO MOD-PRHEMTAG                                 
029700       MOVE CLAG-FLLSRDEL TO MOD-FLLSRDEL                                 
029800       MOVE CLAG-KVQPACK-1 TO MOD-KVQPACK-1                               
029900       MOVE CLAG-VKART    TO MOD-VKART                                    
030000       PERFORM CA-SPARRKOD                                                
030100         IF W-KDERS-UTG > +0                                              
030200           MOVE W-KDERS-UTG  TO MOD-KDERS (INDX)                          
030300         ELSE                                                             
030400           MOVE CLAG-KDERS   TO MOD-KDERS (INDX)                          
030500         END-IF                                                           
030600         MOVE CLAG-KVAKS-CDC TO MOD-KVAKS (INDX)                          
030700         MOVE CLAG-KVSPANT   TO MOD-KVSPANT (INDX)                        
030800         MOVE CLAG-KVRESS    TO MOD-KVRESS (INDX)                         
030900         MOVE CLAG-KVROS     TO MOD-KVROS (INDX)                          
031000         MOVE CLAG-KVEFRS    TO MOD-KVEFRS (INDX)                         
031100         MOVE CLAG-ADLAGOMR TO MOD-ADLAGOMR (INDX)                        
031200         MOVE CLAG-ADGANG    TO MOD-ADGANG   (INDX)                       
031300         MOVE CLAG-ADPLATS   TO MOD-ADPLATS  (INDX)                       
031400         COMPUTE W-SUMMA1 = CLAG-KVLS - CLAG-KVRESS -                     
031500         CLAG-KVUTRS                                                      
031600         MOVE W-SUMMA1      TO MOD-KVDISP (INDX)                          
031700                                                                          
031800         IF CLAG-KDERS > +9                                               
031900           MOVE MEDDELANDE-2 TO MOD-MESSAGE-1                             
032000         END-IF                                                           
032100     END-IF                                                               
032200*                                                                         
032300*                                         WDD902,LEVERANTÖRSINFO          
032400                                                                          
032500     MOVE W-IDARTNR    TO W-IDARTNR-D9                                    
032510     MOVE MSGI-IDDC    TO W-IDDC-D9                                       
032600     PERFORM IMS-LAES-LEV-WDD902                                          
032700     IF SEGMENT-FINNS                                                     
032800       MOVE LEV-KVBR TO MOD-KVBR                                          
032900     END-IF                                                               
033000     SKIP2                                                                
033100*                                         WDD701,TILLK ARTIKLAR           
033200                                                                          
033300     PERFORM IMS-LAES-ERS-WDD702                                          
033400     IF SEGMENT-FINNS                                                     
033500     AND ERS-FLTEXT = 'N'                                                 
033600       MOVE ERS-IDARTNR-TILLK TO MOD-IDARTNR-TILLK                        
033700       MOVE ERS-DIERS-TILLK   TO MOD-DIERS-TILLK                          
033800     END-IF                                                               
033900     .                                                                    
034000     EJECT                                                                
034100 CA-SPARRKOD SECTION.                                                     
034200                                                                          
034300********************* DENNA BÖR TAS BORT VID SDC ÄNDRINGAR                
034400************  NOLLSTÄLLS TILLS VIDARE                                     
034500**************  940414 *                                                  
034600     MOVE 0 TO MOD-KDSPARR (1)                                            
034700               MOD-KDSPARR (2)                                            
034800     .                                                                    
034900     EJECT                                                                
035000* IMS SEKTIONER                                                           
035100     SKIP3                                                                
035200 IMS-GET-MSG SECTION.                                                     
035300     MOVE '  QC' TO GODK-STATUSKODER                                      
035400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
035500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
035600     PERFORM IMS-STATUSKONTROLL                                           
035700     .                                                                    
035800     SKIP3                                                                
035900 IMS-INSERT-MSG SECTION.                                                  
036000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
036100        MOVE '0' TO MFS-KDHUVOMR                                          
036200     END-IF                                                               
036300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
036400     MOVE SPACE TO GODK-STATUSKODER                                       
036500     CALL CBLTDLI USING ISRT MSG-PCB                                      
036600                          MSG-IO-AREA MFS-IDMOD                           
036700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
036800     PERFORM IMS-STATUSKONTROLL                                           
036900     .                                                                    
037000     EJECT                                                                
037100 IMS-LAES-ARTIKEL-WDK601 SECTION.                                         
037200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
037300     DELIMITED BY SIZE INTO SSA1                                          
037400     MOVE '  GE' TO GODK-STATUSKODER                                      
037500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
037600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
037700     PERFORM IMS-STATUSKONTROLL                                           
037800     .                                                                    
037900     SKIP2                                                                
038000 IMS-LAES-WDK611 SECTION.                                                 
038100     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
038200     DELIMITED BY SIZE INTO SSA1                                          
038300     MOVE '  GE' TO GODK-STATUSKODER                                      
038400     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA2 SSA1                    
038500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
038600     PERFORM IMS-STATUSKONTROLL                                           
038700     .                                                                    
038800     EJECT                                                                
038810 IMS-GNP-WDK621 SECTION.                                                  
038820     STRING 'WLARTC21(DAPRLIST=>' W-DAPRLIST-X ')'                        
038830     DELIMITED BY SIZE INTO SSA1                                          
038840     MOVE '  GE' TO GODK-STATUSKODER                                      
038850     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-K621 SSA1                     
038860     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
038870     PERFORM IMS-STATUSKONTROLL                                           
038880     .                                                                    
038890     EJECT                                                                
038900 IMS-LAES-LEV-WDD902 SECTION.                                             
039000     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
039100     DELIMITED BY SIZE INTO SSA1                                          
039200     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
039300     DELIMITED BY SIZE INTO SSA2                                          
039400     MOVE '  GE' TO GODK-STATUSKODER                                      
039500     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA3 SSA1 SSA2                
039600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
039700     PERFORM IMS-STATUSKONTROLL                                           
039800     .                                                                    
039900     EJECT                                                                
040000 IMS-LAES-ERS-WDD702 SECTION.                                             
040100     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
040200     DELIMITED BY SIZE INTO SSA1                                          
040300     MOVE 'WLERSA11 ' TO SSA2                                             
040400     MOVE '  GE' TO GODK-STATUSKODER                                      
040500     CALL CBLTDLI USING GU ERS-PCB DLI-IO-AREA3 SSA1 SSA2                 
040600     MOVE ERS-STATUS-CODE TO STATUS-WS                                    
040700     PERFORM IMS-STATUSKONTROLL                                           
040800     .                                                                    
040900     SKIP3                                                                
041000 IMS-GU-BEN  SECTION.                                                     
041100                                                                          
041200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
041300     DELIMITED BY SIZE INTO SSA1                                          
041400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
041500     DELIMITED BY SIZE INTO SSA2                                          
041600     MOVE '  GE' TO GODK-STATUSKODER                                      
041700     CALL CBLTDLI USING GU BEN-PCB DLI-IO-AREA3 SSA1 SSA2                 
041800     MOVE BEN-STATUS-CODE TO STATUS-WS                                    
041900     PERFORM IMS-STATUSKONTROLL                                           
042000     .                                                                    
042100     SKIP3                                                                
042200 IMS-STATUSKONTROLL SECTION.                                              
042300     SET STATUS-IX TO 1                                                   
042400     SEARCH GODK-STATUS AT END CALL FELLOG                                
042500     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
042600     END-SEARCH                                                           
042700     .                                                                    
