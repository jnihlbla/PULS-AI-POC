000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.         W2712500.                                            
000400 AUTHOR.             STEFAN ÅSGÅRDEN.                                     
000500 DATE-WRITTEN.       JULI 2002.                                           
000600     SKIP2                                                                
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION.                                                            
001000*        PROGRAMMET ÄR ETT SUBPROGRAM FÖR                                 
001100*        ATT SKAPA REFILLORDER                                            
001200*    SUBPROGRAM.                                                          
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP1                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900 DATA DIVISION.                                                           
002000 FILE SECTION.                                                            
002100     EJECT                                                                
002200 WORKING-STORAGE SECTION.                                                 
002300     SKIP2                                                                
002400 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
002500     SKIP3                                                                
002600 01  KONSTANTER.                                                          
002700     03  JA                  PIC X       VALUE 'J'.                       
002800     03  NEJ                 PIC X       VALUE 'N'.                       
002900     SKIP3                                                                
003000 01  FILLER                      PIC X(16)   VALUE 'ARBETSAREOR'.         
003100 01  ARBETSAREOR.                                                         
003200     SKIP1                                                                
003300*                                                                         
003400     03 IX-K7                    PIC S9(9)   VALUE ZERO COMP-3.           
003500     03 IX-K7-MAX                PIC S9(9)   VALUE ZERO COMP-3.           
003600     03 WS-K7 OCCURS 15.                                                  
003700        05 WS-K7-IDDC            PIC  X(2)   VALUE SPACE.                 
003800        05 WS-K7-CD-OMRADE       PIC S9(3)   VALUE ZERO COMP-3.           
003900        05 WS-K7-KVDAGAR-BEHOV   PIC S9(3)   VALUE ZERO COMP-3.           
004000        05 WS-K7-FL-CD-RELEASE   PIC  X(1)   VALUE SPACE.                 
004100     03  WS-ANTAL-VECKOR         PIC  9(3)   VALUE ZERO COMP-3.           
004200     03  WS-TIAAVV-NUM           PIC  9(4)   VALUE ZERO.                  
004300     03  WS-CD-TILLGANG          PIC S9(7)   VALUE ZERO COMP-3.           
004400     03  WS-IDDISTR              PIC 9(5)    VALUE ZERO.                  
004500     03  WS-TILLGANG             PIC S9(7)   VALUE ZERO COMP-3.           
004600     03  WS-HELTAL-BEST          PIC S9(7)   VALUE ZERO COMP-3.           
004700     03  WS-HELTAL-SALDO         PIC S9(7)   VALUE ZERO COMP-3.           
004800     03  WS-SALDO                PIC S9(7)   VALUE ZERO COMP-3.           
004900     03  WS-KVANTAL              PIC S9(7)   VALUE ZERO COMP-3.           
005000     03  WS-KVBEHOV              PIC S9(7)   VALUE ZERO COMP-3.           
005100     SKIP3                                                                
005200     03 IX-CD-OMR                PIC S9(9)   VALUE ZERO COMP-3.           
005300     03 IX-IDDC                  PIC S9(9)   VALUE ZERO COMP-3.           
005400     03 IX                       PIC S9(4)   VALUE ZERO COMP-3.           
005500     03 IX-2                     PIC S9(4)   VALUE ZERO COMP-3.           
005600     03 IX-3                     PIC S9(4)   VALUE ZERO COMP-3.           
005700     03 MAX-IX                   PIC S9(4)   VALUE +20  COMP-3.           
005800                                                                          
005900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000                                                                          
006100 77  SW-TRAEFF                   PIC X       VALUE 'J'.                   
006200     88  SW-TRAEFF-JA                        VALUE 'J'.                   
006300     88  SW-TRAEFF-NEJ                       VALUE 'N'.                   
006400                                                                          
006500                                                                          
006600 01  FILLER                      PIC X(16)   VALUE 'WWDC99     '.         
006700*      --- VALID IDDC CODES                                               
006800*                                                                         
006900*01    -COPY WWDC99                                                       
007000                                                                          
007100                                                                          
007200 01  FELTEXT.                                                             
007300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007500 01  FELTEXT2.                                                            
007600     03  FILLER                  PIC X(9)    VALUE 'FELTEXT2'.            
007700     03  FELTEXT2-STR      PIC X(71)   VALUE SPACE.                       
007800                                                                          
007900     EJECT                                                                
008000 01  DYNAMISKA-SUBPROGRAM.                                                
008100     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
008200     03  W009VADD            PIC X(8)    VALUE 'W009VADD'.                
008300     03  PERADD              PIC X(8)    VALUE 'PERADD'.                  
008400     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
008500     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
008600     03  WDAGKONV            PIC X(8)    VALUE 'WDAGKONV'.                
008700     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
008800     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
008900     03  WORKDAY             PIC X(8)    VALUE 'WORKDAY'.                 
009000     EJECT                                                                
009100*    ---PARAMETRAR TILL DATKONV                                           
009200*01  -COPY WDATAREA                                                       
009300     EJECT                                                                
009400*    ---PARAMETRAR TILL WORKDAY                                           
009500*01  -COPY WORKAREA                                                       
009600     EJECT                                                                
009700*    --- PARAMETRAR TILL ABEND                                            
009800                                                                          
009900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010100     SKIP3                                                                
010200     SKIP2                                                                
010300*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
010400*                                                                         
010500 01      IMS-WS.                                                          
010600   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
010700     SKIP3                                                                
010800*                            *** STATUSKOD FRÅN IMS                       
010900   03    STATUS-WS       PIC XX.                                          
011000     88  SEGMENT-FINNS               VALUE '  '.                          
011100     88  SEGMENT-SAKNAS              VALUE 'GE'                           
011200                                           'GB'.                          
011300     SKIP3                                                                
011400   03    SSA1            PIC X(50).                                       
011500   03    SSA2            PIC X(50).                                       
011600   03    SSA3            PIC X(50).                                       
011700     SKIP3                                                                
011800 01  NYCKLAR-TILL-DLI.                                                    
011900                                                                          
012000     03 W-IDDC-X.                                                         
012100         05 W-IDDC           PIC  X(2).                                   
012200     03  W-IDDC-B6-X.                                                     
012300         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
012400     03 W-IDARTNR-X.                                                      
012500         05 W-IDARTNR        PIC S9(9)               COMP-3.              
012600     03 W-KDERS-0-X.                                                      
012700         05 W-KDERS-0        PIC S9(3)   COMP-3  VALUE ZERO.              
012800                                                                          
012900     03  W-KDSEGKEY-X.                                                    
013000         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
013100                                                                          
013200     03 W-WDE301-X.                                                       
013300         05  W-IDDC-301          PIC X(2)  VALUE SPACE.                   
013400         05  W-IDPERSON-BUY      PIC S9(3) VALUE ZERO COMP-3.             
013500         05  W-KDREFTYP          PIC X     VALUE SPACE.                   
013600         05  W-IDARTNR-301       PIC S9(9) VALUE ZERO COMP-3.             
013700         05  W-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
013800   03    GODK-STATUSKODER.                                                
013900     05  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC XX.               
014000     SKIP3                                                                
014100*01      -COPY W0003                                                      
014200     EJECT                                                                
014300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
014400     SKIP3                                                                
014500 01  DLI-IO-AREA-WDK601.                                                  
014600*        05  -COPY WDK601                                                 
014700     EJECT                                                                
014800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
014900     SKIP3                                                                
015000 01  DLI-IO-AREA-WDK611.                                                  
015100*        05  -COPY WDK611                                                 
015200     EJECT                                                                
015300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK701'.             
015400     SKIP3                                                                
015500 01  DLI-IO-AREA-WDK701.                                                  
015600*        05  -COPY WDK701                                                 
015700     EJECT                                                                
015800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
015900     SKIP3                                                                
016000 01  DLI-IO-AREA-WDK711.                                                  
016100*        05  -COPY WDK711                                                 
016200     EJECT                                                                
016300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDE301'.             
016400     SKIP3                                                                
016500 01  DLI-IO-AREA-WDE301.                                                  
016600*        05  -COPY WDE301                                                 
016700     EJECT                                                                
016800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDE301OLD'.          
016900     SKIP3                                                                
017000 01  DLI-IO-AREA-WDE301-OLD.                                              
017100*        05  -COPY WDE301    -PRE OLD-                                    
017200     EJECT                                                                
017300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
017400 01   DLI-IO-AREA-B601.                                                   
017500*     03  -COPY WDB601                                                    
017600                                                                          
017700     EJECT                                                                
017800 LINKAGE SECTION.                                                         
017900     SKIP3                                                                
018000*01  AREA  -COPY W27125      -PRE LINK-.                                  
018100     EJECT                                                                
018200*01  -COPY W0008  -PRE WDK6-.                                             
018300     05  FILLER              PIC X.                                       
018400     EJECT                                                                
018500*01  -COPY W0008  -PRE WDK7-.                                             
018600     05  FILLER              PIC X.                                       
018700     EJECT                                                                
018800*01  -COPY W0008  -PRE WDE3-                                              
018900     05  FILLER                  PIC X.                                   
019000     EJECT                                                                
019100*01  -COPY W0008      -PRE WDB6-                                          
019200     05  FILLER                  PIC X.                                   
019300                                                                          
019400     EJECT                                                                
019500 PROCEDURE DIVISION USING LINK-AREA                                       
019600                          WDK6-PCB WDK7-PCB WDE3-PCB WDB6-PCB.            
019700     PERFORM A-INITIERA                                                   
019800                                                                          
019900     MOVE LINK-IDARTNR       TO W-IDARTNR                                 
020000                                                                          
020100     PERFORM IMS-GHU-K611                                                 
020200                                                                          
020300     IF LINK-ANROPSTYP = 1                                                
020400       PERFORM B-BEHANDLA-ANROP1                                          
020500     ELSE                                                                 
020600       PERFORM C-BEHANDLA-ANROP2                                          
020700     END-IF                                                               
020800                                                                          
020900     PERFORM IMS-REPL-K611                                                
021000                                                                          
021100     MOVE ZERO TO RETURN-CODE                                             
021200     GOBACK                                                               
021300     .                                                                    
021400     EJECT                                                                
021500 A-INITIERA SECTION.                                                      
021600******************************************************************        
021700*                                                                *        
021800*    BERÄKNING AV START- OCH SLUT-TIDPUNKTER (ÅR OCH VECKA)      *        
021900*    FÖR BERÄKNING                                               *        
022000*    NOLLSTÄLLNING AV TABELLER                                   *        
022100*                                                                *        
022200******************************************************************        
022300     SKIP1                                                                
022400     ACCEPT DAGENS-DATUM FROM DATE                                        
022500                                                                          
022600     .                                                                    
022700     EJECT                                                                
022800 B-BEHANDLA-ANROP1 SECTION.                                               
022900                                                                          
023000     MOVE 1                  TO IX                                        
023100                                                                          
023200     PERFORM UNTIL IX > MAX-IX                                            
023300     OR LINK-IDDC (IX) = SPACE                                            
023400                                                                          
023500       PERFORM BA-SKAPA-REFILLPOST                                        
023600                                                                          
023700       ADD 1                 TO IX                                        
023800                                                                          
023900     END-PERFORM                                                          
024000     .                                                                    
024100     EJECT                                                                
024200 BA-SKAPA-REFILLPOST SECTION.                                             
024300                                                                          
024400     MOVE LINK-IDDC (IX)     TO W-IDDC                                    
024500                                WS-IDDC                                   
024600     PERFORM IMS-GHU-K711                                                 
024700                                                                          
024800     MOVE 1                  TO IX-CD-OMR                                 
024900     PERFORM UNTIL IX-CD-OMR > 4                                          
025000     OR SLAG-ADLAGOMR-CD = CLAG-ADLAGOMR-CD (IX-CD-OMR)                   
025100       ADD 1                 TO IX-CD-OMR                                 
025200     END-PERFORM                                                          
025300                                                                          
025400*                                                                         
025500*                                                                         
025600*     LINK-KVANTAL-CD (IX-CD-OMR) ÄR DET ANTALET SOM SKA                  
025700*     LÄGGAS IN PÅ AKTUELLT CD OMRÅDE FRÅN DET PARTI SOM                  
025800*     BEHANDLAS I ANROPANDE PROGRAM 6115                                  
025900*     SKAPA BARA REFILLORDER FÖR DC SOM FÅR INLEVERANS TILL               
026000*     SITT CD-OMRÅDE                                                      
026100*                                                                         
026200*                                                                         
026300     IF  IX-CD-OMR <= 4                                                   
026400     AND LINK-KVANTAL-CD (IX-CD-OMR) > ZERO                               
026500       COMPUTE WS-SALDO = CLAG-KVLS-CD   (IX-CD-OMR)                      
026600                        - CLAG-KVRESS-CD (IX-CD-OMR)                      
026700       IF WS-SALDO < ZERO                                                 
026800         MOVE ZERO           TO WS-SALDO                                  
026900       END-IF                                                             
027000       COMPUTE WS-SALDO = WS-SALDO                                        
027100                        + LINK-KVANTAL-CD (IX-CD-OMR)                     
027200       DIVIDE WS-SALDO       BY CLAG-KVQPACK-3                            
027300                             GIVING WS-HELTAL-SALDO                       
027400       DIVIDE LINK-KVBEHOV-DC (IX)                                        
027500                             BY CLAG-KVQPACK-3                            
027600                             GIVING WS-HELTAL-BEST                        
027700       IF WS-HELTAL-BEST > WS-HELTAL-SALDO                                
027800         COMPUTE WS-KVANTAL =                                             
027900                           WS-HELTAL-SALDO * CLAG-KVQPACK-3               
028000       ELSE                                                               
028100         COMPUTE WS-KVANTAL =                                             
028200                           WS-HELTAL-BEST * CLAG-KVQPACK-3                
028300       END-IF                                                             
028400                                                                          
028500       IF WS-KVANTAL > ZERO                                               
028600*                                                                         
028700*                                                                         
028800*     SKAPA BARA REFILLORDRAR SOM GÅR MOT CROSS DOCKING OMRÅDET           
028900*     (ÅTMINSTÅENDE DELVIS)                                               
029000*                                                                         
029100*                                                                         
029200                                                                          
029300         MOVE 'B'            TO REF-KDREFTYP                              
029400         MOVE WS-IDDC        TO REF-IDDC                                  
029500         MOVE LINK-IDARTNR   TO REF-IDARTNR                               
029600         MOVE ZERO           TO REF-IDKUNDNR                              
029700         MOVE CLAG-ADLAGOMR  TO REF-ADLAGOMR-CDC                          
029800         MOVE CLAG-ADGANG    TO REF-ADGANG-CDC                            
029900         MOVE CLAG-ADPLATS   TO REF-ADPLATS-CDC                           
030000         MOVE SLAG-ADLAGOMR  TO REF-ADLAGOMR-SDC                          
030100         MOVE SLAG-ADGANG    TO REF-ADGANG-SDC                            
030200         MOVE SLAG-ADPLATS   TO REF-ADPLATS-SDC                           
030300                                                                          
030400         MOVE CLAG-ADLAGOMR-CD (IX-CD-OMR)                                
030500                               TO REF-ADLAGOMR-CD                         
030600         MOVE CLAG-ADGANG-CD (IX-CD-OMR)                                  
030700                               TO REF-ADGANG-CD                           
030800         MOVE CLAG-ADPLATS-CD (IX-CD-OMR)                                 
030900                               TO REF-ADPLATS-CD                          
031000         PERFORM BAA-HAMTA-REFILLDISTRIKT                                 
031100         MOVE WS-IDDISTR     TO REF-IDDISTR                               
031200*                                                                         
031300*                                                                         
031400*                                                                         
031500*       EN REFILLORDER SKAPAS MED EN KVANT SOM TÄCKER HELA BEHOVET        
031600*         INOM DEN SIKT (ANTALET DAGAR) SOM FRAMGÅR PÅ 2344               
031700*         FINNS DET INTE TÄCKNING PÅ CD-OMRÅDET TAS RESTERANDE            
031800*         FRÅN NORMAL PLOCKPLATS                                          
031900*        (REF-KVBEART  = TOTALA ANTALET I REFILLORDERN                    
032000*        REF-KVBEART-CD = HUR MYCKET SOM SKA HÄMTAS FRÅN CD-OMRÅDE        
032100*                          AV DET TOTALA ANTALET DVS EN DELMÄNGD          
032200*                          DVS EN DELMÄNGD AV REF-KVBEART)                
032300*                                                                         
032400*                                                                         
032500*                                                                         
032600         MOVE LINK-KVBEHOV-DC (IX)                                        
032700                               TO REF-KVBEART                             
032800         MOVE WS-KVANTAL     TO REF-KVBEART-CD                            
032900         COMPUTE LINK-KVANTAL-CD (IX-CD-OMR) =                            
033000                 LINK-KVANTAL-CD (IX-CD-OMR) -                            
033100                 WS-KVANTAL                                               
033200         MOVE 'O'            TO REF-KDREFORS                              
033300         MOVE SLAG-IDLEVNR   TO REF-IDLEVNR                               
033400         MOVE '99'           TO REF-KDREFTXT                              
033500         MOVE ZERO           TO REF-KDFRAKT                               
033600         MOVE SLAG-IDDC-REF  TO REF-IDDC-REF                              
033700         MOVE SLAG-IDPERSON-BUY                                           
033800                             TO REF-IDPERSON-BUY                          
033900                                                                          
034000         PERFORM IMS-ISRT-E301                                            
034100                                                                          
034200         IF STATUS-WS = 'II'                                              
034300                                                                          
034400            MOVE REF-IDDC    TO W-IDDC-301                                
034500            MOVE REF-IDPERSON-BUY                                         
034600                             TO W-IDPERSON-BUY                            
034700            MOVE REF-KDREFTYP                                             
034800                             TO W-KDREFTYP                                
034900            MOVE REF-IDARTNR                                              
035000                             TO W-IDARTNR-301                             
035100            MOVE REF-IDDISTR TO W-IDDISTR                                 
035200                                                                          
035300            PERFORM IMS-GHU-E301-OLD                                      
035400*                                                                         
035500*     KOLLA BEFINTLIG POST, LÄGG POSTEN PÅ OLD-                           
035600*                                                                         
035700                                                                          
035800            IF SEGMENT-FINNS                                              
035900            AND OLD-REF-KDREFORS = 'P'                                    
036000*                                                                         
036100*     ETT ORDERFÖRSLAG FINNS REDAN, SKRIV ÖVER                            
036200*     SKAPA CROSSDOCKINGORDER                                             
036300*                                                                         
036400                                                                          
036500               PERFORM IMS-REPL-E301                                      
036600                                                                          
036700               COMPUTE SLAG-KVBEART = SLAG-KVBEART +                      
036800                                      LINK-KVBEHOV-DC (IX)                
036900               MOVE DAGENS-DATUM TO SLAG-TIORDREG                         
037000               PERFORM IMS-REPL-K711                                      
037100                                                                          
037200               COMPUTE CLAG-KVRESS-CD (IX-CD-OMR) =                       
037300                       CLAG-KVRESS-CD (IX-CD-OMR) + WS-KVANTAL            
037400                                                                          
037500            END-IF                                                        
037600                                                                          
037700         ELSE                                                             
037800*                                                                         
037900*     INSERTEN AV WDE3 GICK BRA                                           
038000*                                                                         
038100           COMPUTE SLAG-KVBEART = SLAG-KVBEART +                          
038200                                  LINK-KVBEHOV-DC (IX)                    
038300           MOVE DAGENS-DATUM TO SLAG-TIORDREG                             
038400           PERFORM IMS-REPL-K711                                          
038500                                                                          
038600           COMPUTE CLAG-KVRESS-CD (IX-CD-OMR) =                           
038700                   CLAG-KVRESS-CD (IX-CD-OMR) + WS-KVANTAL                
038800         END-IF                                                           
038900       END-IF                                                             
039000     END-IF                                                               
039100     .                                                                    
039200     EJECT                                                                
039300 BAA-HAMTA-REFILLDISTRIKT SECTION.                                        
039400                                                                          
039500     MOVE LINK-IDDC (IX)     TO W-IDDC-B6                                 
039600     PERFORM IMS-GU-WDB601                                                
039700     MOVE DCS-IDDISTR-REFILL TO WS-IDDISTR                                
039800                                                                          
039900*                                                                         
040000*    MOVE ZERO              TO WS-IDDISTR                                 
040100*    MOVE 1                 TO IX-2                                       
040200*                              IX-3                                       
040300*                                                                         
040400*    PERFORM UNTIL IX-2     > DC-MAX                                      
040500*               OR IDDC (IX-2) = LINK-IDDC (IX)                           
040600*                                                                         
040700*        ADD 1              TO IX-2                                       
040800*    END-PERFORM                                                          
040900*                                                                         
041000*    IF  IX-2               NOT > DC-MAX                                  
041100*        PERFORM UNTIL IX-3     > DC-DISTRIKT-MAX                         
041200*                   OR ORDERTYP (IX-2, IX-3) = 'REFILL'                   
041300*                                                                         
041400*            ADD 1              TO IX-3                                   
041500*        END-PERFORM                                                      
041600*                                                                         
041700*        IF  IX-3               NOT > DC-DISTRIKT-MAX                     
041800*            MOVE IDDISTR (IX-2, IX-3)                                    
041900*                               TO WS-IDDISTR                             
042000*        END-IF                                                           
042100*    END-IF                                                               
042200     .                                                                    
042300     EJECT                                                                
042400 C-BEHANDLA-ANROP2 SECTION.                                               
042500                                                                          
042600                                                                          
042700     PERFORM IMS-GU-K701                                                  
042800     IF SEGMENT-FINNS                                                     
042900       PERFORM IMS-GNP-K711                                               
043000     END-IF                                                               
043100                                                                          
043200     PERFORM UNTIL SEGMENT-SAKNAS                                         
043300*                                                                         
043400       IF      SLAG-KDREFSTA = 'A'                                        
043500       AND SLAG-ADLAGOMR-CD > ZERO                                        
043600       AND SLAG-ADLAGOMR-CD = CLAG-ADLAGOMR-CD (LINK-IX)                  
043700       AND LINK-KVANTAL-CD-KOMPL > ZERO                                   
043800         MOVE SLAG-IDDC      TO W-IDDC-301                                
043900                                WS-IDDC                                   
044000         MOVE SLAG-IDPERSON-BUY                                           
044100                             TO W-IDPERSON-BUY                            
044200         MOVE 'B'            TO W-KDREFTYP                                
044300         MOVE LINK-IDARTNR   TO W-IDARTNR-301                             
044400         PERFORM CA-HAMTA-REFILLDISTRIKT                                  
044500         MOVE WS-IDDISTR     TO W-IDDISTR                                 
044600         PERFORM IMS-GHU-E301                                             
044700         IF SEGMENT-FINNS                                                 
044800         AND REF-KDREFORS = 'O'                                           
044900           DIVIDE LINK-KVANTAL-CD-KOMPL                                   
045000                             BY CLAG-KVQPACK-3                            
045100                             GIVING WS-HELTAL-SALDO                       
045200           COMPUTE WS-KVBEHOV =                                           
045300                   REF-KVBEART - REF-KVBEART-CD                           
045400           DIVIDE WS-KVBEHOV BY CLAG-KVQPACK-3                            
045500                             GIVING WS-HELTAL-BEST                        
045600           IF WS-HELTAL-BEST > WS-HELTAL-SALDO                            
045700             COMPUTE WS-KVANTAL =                                         
045800                               WS-HELTAL-SALDO * CLAG-KVQPACK-3           
045900           ELSE                                                           
046000             COMPUTE WS-KVANTAL =                                         
046100                               WS-HELTAL-BEST * CLAG-KVQPACK-3            
046200           END-IF                                                         
046300                                                                          
046400           IF WS-KVANTAL > ZERO                                           
046500*                                                                         
046600*     KOMPLETTERA BEFINTLIG REFILLORDER SOM ÄR DELAD MOT CROSS            
046700*     DOCKING PLATS OCH NORMAL LAGERPLATS                                 
046800*                                                                         
046900             ADD WS-KVANTAL  TO REF-KVBEART-CD                            
047000                                LINK-KVANTAL-CD-RES                       
047100             PERFORM IMS-REPL-E301                                        
047200                                                                          
047300             COMPUTE LINK-KVANTAL-CD-KOMPL =                              
047400                     LINK-KVANTAL-CD-KOMPL -                              
047500                     WS-KVANTAL                                           
047600                                                                          
047700             COMPUTE CLAG-KVRESS-CD (LINK-IX) =                           
047800                     CLAG-KVRESS-CD (LINK-IX) + WS-KVANTAL                
047900           END-IF                                                         
048000         END-IF                                                           
048100       END-IF                                                             
048200       PERFORM IMS-GNP-K711                                               
048300     END-PERFORM                                                          
048400     .                                                                    
048500     EJECT                                                                
048600 CA-HAMTA-REFILLDISTRIKT SECTION.                                         
048700                                                                          
048800     MOVE SLAG-IDDC          TO W-IDDC-B6                                 
048900     PERFORM IMS-GU-WDB601                                                
049000     MOVE DCS-IDDISTR-REFILL TO WS-IDDISTR                                
049100                                                                          
049200*                                                                         
049300*    MOVE ZERO              TO WS-IDDISTR                                 
049400*    MOVE 1                 TO IX-2                                       
049500*                              IX-3                                       
049600*                                                                         
049700*    PERFORM UNTIL IX-2     > DC-MAX                                      
049800*               OR IDDC (IX-2) = SLAG-IDDC                                
049900*                                                                         
050000*        ADD 1              TO IX-2                                       
050100*    END-PERFORM                                                          
050200*                                                                         
050300*    IF  IX-2               NOT > DC-MAX                                  
050400*        PERFORM UNTIL IX-3     > DC-DISTRIKT-MAX                         
050500*                   OR ORDERTYP (IX-2, IX-3) = 'REFILL'                   
050600*                                                                         
050700*            ADD 1              TO IX-3                                   
050800*        END-PERFORM                                                      
050900*                                                                         
051000*        IF  IX-3               NOT > DC-DISTRIKT-MAX                     
051100*            MOVE IDDISTR (IX-2, IX-3)                                    
051200*                               TO WS-IDDISTR                             
051300*        END-IF                                                           
051400*    END-IF                                                               
051500                                                                          
051600     .                                                                    
051700     EJECT                                                                
051800                                                                          
051900                                                                          
052000                                                                          
052100* --- IMS SEKTIONER ---                                                   
052200     SKIP3                                                                
052300                                                                          
052400     EJECT                                                                
052500                                                                          
052600 IMS-GHU-K611 SECTION.                                                    
052700                                                                          
052800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
052900          DELIMITED BY SIZE INTO SSA1                                     
053000     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
053100          DELIMITED BY SIZE INTO SSA2                                     
053200     MOVE '  ' TO GODK-STATUSKODER                                        
053300     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-AREA-WDK611 SSA1 SSA2         
053400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
053500     PERFORM IMS-STATUSKONTROLL                                           
053600     .                                                                    
053700     EJECT                                                                
053800                                                                          
053900 IMS-REPL-K611 SECTION.                                                   
054000                                                                          
054100     MOVE '  ' TO GODK-STATUSKODER                                        
054200     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-AREA-WDK611                  
054300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
054400     PERFORM IMS-STATUSKONTROLL                                           
054500     .                                                                    
054600     EJECT                                                                
054700                                                                          
054800 IMS-GU-K701      SECTION.                                                
054900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
055000            DELIMITED BY SIZE INTO SSA1                                   
055100     MOVE '  GE' TO GODK-STATUSKODER                                      
055200     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK701 SSA1               
055300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
055400     PERFORM IMS-STATUSKONTROLL                                           
055500     .                                                                    
055600     SKIP3                                                                
055700 IMS-GNP-K711  SECTION.                                                   
055800     MOVE 'WDK711   '  TO SSA1                                            
055900     MOVE '  GE'        TO GODK-STATUSKODER                               
056000     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-WDK711 SSA1              
056100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
056200     PERFORM IMS-STATUSKONTROLL                                           
056300     .                                                                    
056400     EJECT                                                                
056500                                                                          
056600 IMS-GHU-K711 SECTION.                                                    
056700                                                                          
056800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
056900          DELIMITED BY SIZE INTO SSA1                                     
057000     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
057100          DELIMITED BY SIZE INTO SSA2                                     
057200     MOVE '  ' TO GODK-STATUSKODER                                        
057300     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2         
057400     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
057500     PERFORM IMS-STATUSKONTROLL                                           
057600     .                                                                    
057700     EJECT                                                                
057800                                                                          
057900 IMS-REPL-K711 SECTION.                                                   
058000                                                                          
058100     MOVE '  ' TO GODK-STATUSKODER                                        
058200     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-AREA-WDK711                  
058300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
058400     PERFORM IMS-STATUSKONTROLL                                           
058500     .                                                                    
058600     EJECT                                                                
058700                                                                          
058800 IMS-GHU-E301 SECTION.                                                    
058900                                                                          
059000     STRING 'WDE301  (WDE301KY =' W-WDE301-X ')'                          
059100          DELIMITED BY SIZE INTO SSA1                                     
059200     MOVE '  GE' TO GODK-STATUSKODER                                      
059300     CALL CBLTDLI USING GHU WDE3-PCB DLI-IO-AREA-WDE301 SSA1              
059400     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
059500     PERFORM IMS-STATUSKONTROLL                                           
059600     .                                                                    
059700     SKIP3                                                                
059800                                                                          
059900                                                                          
060000 IMS-GHU-E301-OLD SECTION.                                                
060100                                                                          
060200     STRING 'WDE301  (WDE301KY =' W-WDE301-X ')'                          
060300          DELIMITED BY SIZE INTO SSA1                                     
060400     MOVE '  GE' TO GODK-STATUSKODER                                      
060500     CALL CBLTDLI USING GHU WDE3-PCB DLI-IO-AREA-WDE301-OLD SSA1          
060600     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
060700     PERFORM IMS-STATUSKONTROLL                                           
060800     .                                                                    
060900     SKIP3                                                                
061000                                                                          
061100                                                                          
061200 IMS-REPL-E301 SECTION.                                                   
061300                                                                          
061400     MOVE '  ' TO GODK-STATUSKODER                                        
061500     CALL CBLTDLI USING REPL WDE3-PCB DLI-IO-AREA-WDE301                  
061600     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
061700     PERFORM IMS-STATUSKONTROLL                                           
061800     .                                                                    
061900     EJECT                                                                
062000                                                                          
062100 IMS-ISRT-E301 SECTION.                                                   
062200                                                                          
062300     MOVE 'WDE301   ' TO SSA1                                             
062400     MOVE '  II' TO GODK-STATUSKODER                                      
062500     CALL CBLTDLI USING ISRT WDE3-PCB DLI-IO-AREA-WDE301 SSA1             
062600     MOVE WDE3-STATUS-CODE TO STATUS-WS                                   
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
064000 IMS-STATUSKONTROLL SECTION.                                              
064100     SET STATUS-IX TO 1                                                   
064200     SEARCH GODK-STATUS  AT END CALL FELLOG                               
064300     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
064400          CONTINUE                                                        
064500     END-SEARCH                                                           
064600     .                                                                    
