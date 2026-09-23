000100 ID DIVISION.                                                             
000200 PROGRAM-ID.             W4403300.                                        
000300 AUTHOR.                 BO SVENSSON.                                     
000400 DATE-WRITTEN.           JAN 1997.                                        
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*      SÖKER FRAM RESTORDER-TÄCKNING FÖR NDC:ER   MHA GENOM-              
001000*      LÄSNING AV NDC-ERNAS LAGERBAND.                                    
001100*                                                                         
001200*      HUVUDLOGIK:                                                        
001300*        OM EN ARTIKEL                                                    
001400*              OM  KVROS > 0 OCH DISPONIBELT > 0                          
001500*        ELLER ERSÄTTNINGSKOD > 10                                        
001600*            SÅ SKAPAS EN HTR TILL RO-TÄCKNING (WL4505)                   
001700*            MED KDTAKORS 21, RESP FILEN W44033 TILL RO-ERS.              
001800*                                                                         
001900*      PROGRAMMET KÖRS SOM BMP.                                           
002000*      BMP-STYR-HTR UPPDATERAS MED CHKP-RÄKNARE MM.                       
002100*      VID ÅTERSTART LÄSES INFILEN FRAM TILL SENASTE CHKP-LÄGE.           
002200*   ÄT LASSI 020125:                                                      
002300*      CHKP TAS FÖR VARJE UPPLÄGG AV RO-TÄCKNINGSRAD PÅ 4506!!            
002400*      DETTA FÖR ATT INTE BLOCKERA R4 FÖR ONLINE!                         
002500*                                                                         
002600*      HTR 4551 WL4551/WDR4 UPPDATERAS. (BMP-STYR-HTR)                    
002700*      HTR 4505 WL4505/WDR4 UPPDATERAS. (RO-TÄCKNINGS-HTR).               
002800*                                                                         
002900*    ABENDKODER:                                                          
003000*      U0016 - INFIL OCH CHKP-HÄNDELSETRANS STÄMMER EJ ÖVERENS.           
003100*                                                                         
003100*    STORY 2217565: RECOMPILING PGM FOR WWDCLAND COPYBOOK CHANGE          
003200     EJECT                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400                                                                          
003500 INPUT-OUTPUT SECTION.                                                    
003600                                                                          
003700 FILE-CONTROL.                                                            
003800     SKIP2                                                                
003900*    ---- INFIL: LAGERBAND NDC-ER                                         
004000     SELECT  LBFIL         ASSIGN  W44033D1.                              
004100     SKIP2                                                                
004200*    ---- UTFIL: ARTIKLAR FÖR ERSÄTTNING I RO                             
004300     SELECT  W44031        ASSIGN  W44033D2.                              
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600                                                                          
004700 FILE SECTION.                                                            
004800     SKIP3                                                                
004900 FD  LBFIL                                                                
005000     LABEL RECORD STANDARD                                                
005100     RECORDING  F                                                         
005200     BLOCK CONTAINS 0.                                                    
005300                                                                          
005400*01  LBPOST -COPY W01184        -L.                                       
005500     SKIP3                                                                
005600 FD  W44031                                                               
005700     LABEL RECORD STANDARD                                                
005800     RECORDING  F                                                         
005900     BLOCK CONTAINS 0.                                                    
006000                                                                          
006100*01  UTPOST -COPY W440004 -PRE W44031-  -L.                               
006200     EJECT                                                                
006300 WORKING-STORAGE SECTION.                                                 
006400                                                                          
006500                                                                          
006600*    -- CHECKED BY WY2000                                                 
006700 77  PROGRAM-NAMN            PIC X(8)    VALUE 'W4403300'.                
006800 77  FELTEXT                 PIC X(80)   VALUE SPACE.                     
006900                                                                          
007000 77  CHKP-ID                 PIC X(8)    VALUE 'W44033  '.                
007100 77  MSG-IO-AREA-LENGTH      PIC S9(9)   VALUE +32  COMP SYNC.            
007200 77  MSG-IO-AREA             PIC X(32)   VALUE SPACE.                     
007300 77  CHKP-AREA-1-LENGTH      PIC S9(9)   VALUE +32  COMP SYNC.            
007400 77  CHKP-AREA-1             PIC X(32)   VALUE SPACE.                     
007500                                                                          
007510 77  W-IDLAND                PIC X(3)    VALUE SPACE.                     
007520                                                                          
007600 77  LBFIL-EOF               PIC X       VALUE 'N'.                       
007700     EJECT                                                                
007800 01      FILLER              PIC X(16)   VALUE                            
007900                                         'W***************'.              
008000 01      W.                                                               
008100*                                                                         
008200  02     W-ANT-POST-LB       PIC S9(7)   VALUE ZERO  COMP-3.              
008300  02     W-DISP              PIC S9(7)   VALUE ZERO  COMP-3.              
008400                                                                          
008500  02     W-AAMMDD            PIC 9(6).                                    
008600  02     FILLER              REDEFINES W-AAMMDD.                          
008700   03    W-AAMMDD-AA         PIC 9(2).                                    
008800   03    W-AAMMDD-MM         PIC 9(2).                                    
008900   03    W-AAMMDD-DD         PIC 9(2).                                    
009000                                                                          
009100  02     W-AAVVD             PIC 9(5).                                    
009200  02     FILLER              REDEFINES W-AAVVD.                           
009300   03    W-AAVVD-AA          PIC 9(2).                                    
009400   03    W-AAVVD-VV          PIC 9(2).                                    
009500   03    W-AAVVD-D           PIC 9(1).                                    
009600     SKIP1                                                                
009700*    TIDS-STÄMPEL FÖR CHKP-HTR                                            
009800  02     W-TS.                                                            
009900   03    W-TS-TIAAMMDD       PIC 9(6).                                    
010000   03    W-TS-TIKLOCK        PIC 9(8).                                    
010100     EJECT                                                                
010200 01      FILLER              PIC X(16)   VALUE                            
010300                                         'ARTW************'.              
010400*        SPARAD INFO FRÅN AKTUELL ARTIKEL.                                
010500 01      ARTW.                                                            
010600*                                                                         
010700   03    ARTW-IDARTNR        PIC S9(9)   VALUE ZERO  COMP-3.              
010800   03    ARTW-KVDISP         PIC S9(7)               COMP-3.              
010900   03    ARTW-KVROS          PIC S9(7)               COMP-3.              
011000   03    ARTW-KVAKS          PIC S9(7)               COMP-3.              
011100   03    ARTW-IDDC           PIC X(2)    VALUE SPACE.                     
011200     SKIP3                                                                
011300 01      FILLER              PIC X(16)   VALUE                            
011400                                         'K-KONSTANTER****'.              
011500 01      K-KONSTANTER.                                                    
011600*                                                                         
011700  02     K-KDTAKORS          PIC S9(3)   VALUE +21   COMP-3.              
011800  02     JA                  PIC X(1)    VALUE 'J'.                       
011900  02     NEJ                 PIC X(1)    VALUE 'N'.                       
012000* 02     -COPY WWDCLAND                                                   
012100     SKIP3                                                                
012200 01      FILLER              PIC X(16)   VALUE                            
012300                                         'SW-SWITCHAR*****'.              
012400 01      SW-SWITCHAR.                                                     
012500*                                                                         
012600  02     SW-SPARR-OK         PIC X(1).                                    
012700     EJECT                                                                
012800 01      FILLER              PIC X(16)   VALUE                            
012900                                         'DYNAM-SUBPGM****'.              
013000 01      DYNAM-SUBPGM.                                                    
013100*                                                                         
013200  02     ABEND               PIC X(8)    VALUE 'ABEND   '.                
013300  02     FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
013400  02     POSTSUM             PIC X(8)    VALUE 'POSTSUM '.                
013500  02     CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
013600  02     DATKORT             PIC X(8)    VALUE 'DATKORT '.                
013700     EJECT                                                                
013800 01      FILLER              PIC X(16)   VALUE                            
013900                                         'LBW*************'.              
014000*01  POST   -COPY W01184       -PRE LBW-                                  
014100     EJECT                                                                
014200 01      FILLER              PIC X(16)   VALUE                            
014300                                         'UTFIL***********'.              
014400*01  AREA   -COPY W440004      -PRE UT-                                   
014500     EJECT                                                                
014600*    ----  PARAMETRAR TILL ABEND                                          
014700     SKIP1                                                                
014800 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4) VALUE +16 COMP SYNC.               
014900     EJECT                                                                
015000*    ----  PARAMETRAR TILL POSTSUM                                        
015100                                                                          
015200*01  -COPY W0005       -PRE POSTSUM-.                                     
015300     EJECT                                                                
015400*    ----  PARAMETRAR TILL DATUMKORT                                      
015500                                                                          
015600 01  DATUMKORT-ID            PIC X(6)   VALUE 'WDATUM'.                   
015700     SKIP3                                                                
015800*01  -COPY WDATKORT                                                       
015900     EJECT                                                                
016000*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
016100                                                                          
016200 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
016300     SKIP3                                                                
016400*    ---- STATUSKOD FRÅN IMS                                              
016500                                                                          
016600 01  STATUS-WS               PIC XX.                                      
016700     88  SEGMENT-FINNS                    VALUE '  '.                     
016800     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
016900     88  SEGMENT-FINNS-REDAN              VALUE 'II'.                     
017000     88  IMS-EJ-OK                        VALUE 'XD'.                     
017100     SKIP3                                                                
017200 01  GODK-STATUSKODER.                                                    
017300   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
017400     SKIP3                                                                
017500 01  SSA1                    PIC X(64).                                   
017600 01  SSA2                    PIC X(32).                                   
017700     EJECT                                                                
017800*    ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                   
017900                                                                          
018000 01      FILLER              PIC X(16)   VALUE                            
018100                                         'NYCKAR-TILL-DLI*'.              
018200 01      NYCKLAR-TILL-DLI.                                                
018300*                                                                         
018400  02     W-IDHTYP-4551-X.                                                 
018500   03    FILLER              PIC X(4)    VALUE '4551'.                    
018600   03    FILLER              PIC X(26)   VALUE LOW-VALUE.                 
018700*                                                                         
018800  02     W-IDHTYP-4505-X.                                                 
018900   03    FILLER              PIC X(4)    VALUE '4505'.                    
019000   03    IDDC-4505           PIC X(2)    VALUE SPACE.                     
019100   03    FILLER              PIC X(24)   VALUE LOW-VALUE.                 
019200                                                                          
019300  02     W-IDARTNR-K6-X.                                                  
019400   03    W-IDARTNR-K6        PIC S9(9)   COMP-3 VALUE ZERO.               
019500                                                                          
019600     EJECT                                                                
019700*01  -COPY W0003                                                          
019800     EJECT                                                                
019900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-AREA'.               
020000     SKIP1                                                                
020100 01  DLI-IO-AREA.                                                         
020200  03 IO-AREA                 PIC X(300).                                  
020300     SKIP1                                                                
020400*03  WL455111 -COPY WDGX4552    -PRE 4551-     -RED IO-AREA               
020500     EJECT                                                                
020600*03  WL450511 -COPY WDGX4506    -PRE 4505-     -RED IO-AREA               
020700     EJECT                                                                
020800                                                                          
020900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-ARTC11'.             
021000 01  DLI-IO-ARTC11.                                                       
021100*  03  WLARTC11 -COPY WDK611                                              
021200     EJECT                                                                
021300 LINKAGE SECTION.                                                         
021400     SKIP2                                                                
021500*01  -COPY W0009      -PRE  MSG-                                          
021600     EJECT                                                                
021700*01  -COPY W0008      -PRE  4551-                                         
021800       05  FILLER                PIC X.                                   
021900     SKIP2                                                                
022000*01  -COPY W0008      -PRE  4505-                                         
022100       05  FILLER                PIC X.                                   
022200     SKIP2                                                                
022300*01  -COPY W0008      -PRE  ARTC-                                         
022400       05  FILLER                PIC X.                                   
022500     EJECT                                                                
022600 PROCEDURE DIVISION  USING  MSG-PCB 4551-PCB 4505-PCB ARTC-PCB.           
022700     ENTRY 'DLITCBL' USING  MSG-PCB 4551-PCB 4505-PCB ARTC-PCB.           
022800     SKIP2                                                                
022900     PERFORM A-INIT                                                       
023000                                                                          
023100     PERFORM S01-LAS-LBFIL                                                
023200                                                                          
023300     PERFORM UNTIL (LBFIL-EOF = JA)                                       
023400                                                                          
023500       PERFORM B-INIT-ART                                                 
023600                                                                          
023700       IF LBW-SLAG-IDARTNR NOT = ARTW-IDARTNR                             
023800                                                                          
023900         MOVE LBW-SLAG-IDARTNR TO W-IDARTNR-K6                            
024000         PERFORM IMS-GU-ARTC11                                            
024100                                                                          
024200       END-IF                                                             
024300                                                                          
024400** FIX                                                                    
024500      IF SEGMENT-SAKNAS                                                   
024600        DISPLAY W-IDARTNR-K6                                              
024700      ELSE                                                                
024800       PERFORM D-UTVARD-LBPOST                                            
024900                                                                          
024900       DISPLAY 'LBW-SLAG-IDARTNR: ' LBW-SLAG-IDARTNR                      
024900       DISPLAY 'ARTW-IDARTNR: ' ARTW-IDARTNR                              
024900       DISPLAY 'ARTW-KVROS: ' ARTW-KVROS                                  
024900       DISPLAY 'ARTW-KVDISP ' ARTW-KVDISP                                 
024900       DISPLAY 'CLAG-KDERS: ' CLAG-KDERS                                  
025000       IF ARTW-KVROS  > ZERO                                              
025100         IF  CLAG-KDERS = 22                                              
025200         OR  CLAG-KDERS = 23                                              
025300         OR  CLAG-KDERS = 25                                              
025400         OR  CLAG-KDERS = 26                                              
025500             MOVE LBW-SLAG-IDARTNR     TO UT-IDARTNR                      
025600             MOVE LBW-SLAG-IDDC        TO UT-IDDC                         
025700             PERFORM S03-SKRIV-W44031                                     
025800         ELSE                                                             
025900             IF ARTW-KVDISP > ZERO                                        
026000*        * ARTIKELN HAR BÅDE RESTORDER OCH DISPONIBELT SALDO.             
026100                PERFORM E-GEN-TACKN-HTR                                   
026200                PERFORM F-TAG-CHECKPOINT                                  
026300             ELSE                                                         
026400               IF CLAG-KDERS > +10                                        
026500                  MOVE LBW-SLAG-IDARTNR     TO UT-IDARTNR                 
026600                  MOVE LBW-SLAG-IDDC        TO UT-IDDC                    
026700                  PERFORM S03-SKRIV-W44031                                
026800               END-IF                                                     
026900             END-IF                                                       
027000         END-IF                                                           
027100       END-IF                                                             
027200                                                                          
027300       MOVE LBW-SLAG-IDARTNR TO ARTW-IDARTNR                              
027400       MOVE LBW-SLAG-IDDC    TO ARTW-IDDC                                 
027500                                                                          
027600** SLUT FIX                                                               
027700      END-IF                                                              
027800       PERFORM S01-LAS-LBFIL                                              
027900                                                                          
028000     END-PERFORM                                                          
028100                                                                          
028200     PERFORM Z-FINIT                                                      
028300     MOVE ZERO TO RETURN-CODE                                             
028400     GOBACK                                                               
028500     .                                                                    
028600     EJECT                                                                
028700 A-INIT SECTION.                                                          
028800                                                                          
028900     OPEN INPUT  LBFIL                                                    
029000          OUTPUT W44031                                                   
029100     SKIP1                                                                
029200     MOVE NEJ                TO LBFIL-EOF                                 
029300     MOVE ZERO               TO W-ANT-POST-LB                             
029400     MOVE ZERO               TO ARTW-IDARTNR                              
029500     MOVE SPACE              TO ARTW-IDDC                                 
029600                                                                          
029700     PERFORM AA-ATERSTART                                                 
029800                                                                          
029900     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
030000                                                                          
030100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
030200     MOVE D-AAR    TO W-AAMMDD-AA                                         
030300     MOVE D-MAANAD TO W-AAMMDD-MM                                         
030400     MOVE D-DAG    TO W-AAMMDD-DD                                         
030500     MOVE D-AAR    TO W-AAVVD-AA                                          
030600     MOVE D-VECKA  TO W-AAVVD-VV                                          
030700     MOVE D-DAGNR  TO W-AAVVD-D                                           
030800     .                                                                    
030900     EJECT                                                                
031000 AA-ATERSTART SECTION.                                                    
031100                                                                          
031200     PERFORM IMS-RESTART                                                  
031300                                                                          
031400     PERFORM IMS-GHU-4551-4552                                            
031500                                                                          
031600     IF SEGMENT-FINNS                                                     
031700                                                                          
031800       IF  4551-4552-KVPOST > ZERO                                        
031900                                                                          
032000*--------------------------- LBFIL LÄSES                                  
032100*                            FRAM TILL CHECKPOINT-LÄGE                    
032200                                                                          
032300         PERFORM S01-LAS-LBFIL                                            
032400                                                                          
032500         PERFORM UNTIL (LBFIL-EOF = JA                                    
032600                    OR  W-ANT-POST-LB >= 4551-4552-KVPOST)                
032700           PERFORM S01-LAS-LBFIL                                          
032800         END-PERFORM                                                      
032900                                                                          
033000         IF  LBFIL-EOF       = JA                                         
033100         OR  W-ANT-POST-LB   NOT = 4551-4552-KVPOST                       
033200         OR  LBW-SLAG-IDARTNR     NOT = 4551-4552-IDARTNR                 
033300           DISPLAY 'W4403300: FEL I ÅTERSTARTEN, LB-FIL'                  
033400           CALL ABEND USING  RKOD-ABEND-UTAN-DUMP                         
033500         END-IF                                                           
033600       END-IF                                                             
033700     END-IF                                                               
033800     .                                                                    
033900     EJECT                                                                
034000 B-INIT-ART SECTION.                                                      
034100                                                                          
034200     MOVE ZERO        TO ARTW-KVDISP                                      
034300     MOVE ZERO        TO ARTW-KVROS                                       
034400     MOVE ZERO        TO ARTW-KVAKS                                       
034500     .                                                                    
034600     EJECT                                                                
034700 D-UTVARD-LBPOST SECTION.                                                 
034710                                                                          
034720     SEARCH ALL DC-LAND                                                   
034730        AT END                                                            
034740           MOVE SPACE          TO W-IDLAND                                
034750        WHEN DCLAND-IDDC (DCLAND-IX) = LBW-SLAG-IDDC                      
034760           MOVE DCLAND-IDLANDX2 (DCLAND-IX)                               
034770                               TO W-IDLAND                                
034780     END-SEARCH                                                           
034800                                                                          
034900     ADD  LBW-SLAG-KVROS-DAG                                              
035000          LBW-SLAG-KVROS-BULK  GIVING ARTW-KVROS                          
035100                                                                          
035200     ADD  LBW-SLAG-KVAKS-SDC                                              
035300          LBW-SLAG-KVAKS-PAV   GIVING ARTW-KVAKS                          
035400                                                                          
035500     IF  ARTW-KVROS > 0                                                   
035600                                                                          
035700         MOVE JA                 TO SW-SPARR-OK                           
035800                                                                          
035900         IF LBW-SLAG-KDLEVSP > 0   OR                                     
036000            LBW-SLAG-FLORDSP = JA                                         
036100           MOVE NEJ              TO SW-SPARR-OK                           
036200         END-IF                                                           
036300                                                                          
036400         IF  SW-SPARR-OK = JA                                             
036500           IF LBW-SLAG-KVRESS < ZERO                                      
036600             MOVE ZERO TO LBW-SLAG-KVRESS                                 
036700           END-IF                                                         
036800           COMPUTE W-DISP        =  LBW-SLAG-KVLS                         
036900                                 -  LBW-SLAG-KVOKS-DAG                    
037000                                 -  LBW-SLAG-KVSPARR-KVAL                 
037100                                 -  LBW-SLAG-KVUTRS                       
037200                                 -  LBW-SLAG-KVRESS                       
037210           IF DCLAND-CHINA (DCLAND-IX)                                    
037220              COMPUTE W-DISP = W-DISP - LBW-SLAG-KVSPANT                  
037230           END-IF                                                         
037300                                                                          
037400           IF  W-DISP > ZERO                                              
037500             IF  CLAG-PRARTSTD > ZERO                                     
037600               MOVE W-DISP       TO ARTW-KVDISP                           
037700             END-IF                                                       
037800           END-IF                                                         
037900         ELSE                                                             
038000            MOVE ZERO            TO W-DISP                                
038100         END-IF                                                           
038200     END-IF                                                               
038300     .                                                                    
038400     EJECT                                                                
038500 E-GEN-TACKN-HTR SECTION.                                                 
038600                                                                          
038700     MOVE LBW-SLAG-IDDC      TO IDDC-4505                                 
038800     MOVE SPACE              TO 4505-4506-WDGX4506                        
038900     MOVE LBW-SLAG-IDARTNR   TO 4505-4506-IDARTNR                         
039000     MOVE K-KDTAKORS         TO 4505-4506-KDTAKORS                        
039100     MOVE ZERO               TO 4505-4506-KVANTMOT                        
039200     PERFORM IMS-ISRT-4505-4506                                           
039300     .                                                                    
039400     EJECT                                                                
039500 F-TAG-CHECKPOINT SECTION.                                                
039600                                                                          
039700     PERFORM IMS-GHU-4551-4552                                            
039800                                                                          
039900     MOVE SPACE              TO   4551-4552-WDGX4552                      
040000     MOVE '1'                TO   4551-4552-KDSEGKEY                      
040100                                                                          
040200     MOVE W-ANT-POST-LB      TO   4551-4552-KVPOST                        
040300     MOVE LBW-SLAG-IDARTNR   TO   4551-4552-IDARTNR                       
040400                                                                          
040500     ACCEPT W-TS-TIAAMMDD    FROM DATE                                    
040600     MOVE   W-TS-TIAAMMDD    TO   4551-4552-TIUPPDAT                      
040700     ACCEPT W-TS-TIKLOCK     FROM TIME                                    
040800     MOVE   W-TS-TIKLOCK     TO   4551-4552-TIUPPTID                      
040900                                                                          
041000     IF  SEGMENT-FINNS                                                    
041100       PERFORM IMS-REPL-4551                                              
041200     ELSE                                                                 
041300       PERFORM IMS-ISRT-4551-4552                                         
041400     END-IF                                                               
041500                                                                          
041600     MOVE CHKP-ID            TO MSG-IO-AREA                               
041700     PERFORM IMS-CHECKPOINT                                               
041800     .                                                                    
041900     EJECT                                                                
042000 Z-FINIT SECTION.                                                         
042100                                                                          
042200     CLOSE  LBFIL                                                         
042300            W44031                                                        
042400                                                                          
042500     PERFORM IMS-GHU-4551-4552                                            
042600     IF  SEGMENT-FINNS                                                    
042700       PERFORM IMS-DLET-4551                                              
042800     END-IF                                                               
042900                                                                          
043000     MOVE 'S' TO POSTSUM-OPKOD                                            
043100     CALL POSTSUM USING POSTSUM-PARM                                      
043200     .                                                                    
043300     EJECT                                                                
043400 S01-LAS-LBFIL SECTION.                                                   
043500                                                                          
043600     READ LBFIL INTO LBW-POST                                             
043700       AT  END                                                            
043800         MOVE JA               TO LBFIL-EOF                               
043900     END-READ                                                             
044000                                                                          
044100     IF  LBFIL-EOF = NEJ                                                  
044200       MOVE 'LBFIL'            TO POSTSUM-FDNAMN                          
044300       MOVE 'W44033D1'         TO POSTSUM-DDNAMN2                         
044400       MOVE 'LB'               TO POSTSUM-TRANSTYP                        
044500       CALL POSTSUM USING POSTSUM-PARM                                    
044600       ADD +1                  TO W-ANT-POST-LB                           
044700     END-IF                                                               
044800     .                                                                    
044900     EJECT                                                                
045000 S03-SKRIV-W44031 SECTION.                                                
045100                                                                          
045200     WRITE W44031-UTPOST FROM UT-AREA                                     
045300                                                                          
045400     MOVE 'W44031'           TO POSTSUM-FDNAMN                            
045500     MOVE 'W44033D2'         TO POSTSUM-DDNAMN2                           
045600     MOVE 'ERS-RO'           TO POSTSUM-TRANSTYP                          
045700     CALL POSTSUM USING POSTSUM-PARM                                      
045800     .                                                                    
045900     EJECT                                                                
046000*    ---- IMS SEKTIONER                                                   
046100                                                                          
046200 IMS-RESTART      SECTION.                                                
046300                                                                          
046400     MOVE SPACE TO MSG-IO-AREA                                            
046500     MOVE '  ' TO GODK-STATUSKODER                                        
046600     CALL CBLTDLI USING XRST                                              
046700                        MSG-PCB                                           
046800                        MSG-IO-AREA-LENGTH                                
046900                        MSG-IO-AREA                                       
047000                        CHKP-AREA-1-LENGTH                                
047100                        CHKP-AREA-1                                       
047200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
047300     PERFORM IMS-STATUSKONTROLL                                           
047400     .                                                                    
047500     SKIP3                                                                
047600 IMS-CHECKPOINT   SECTION.                                                
047700                                                                          
047800     MOVE SPACE TO MSG-IO-AREA                                            
047900     MOVE '  XD' TO GODK-STATUSKODER                                      
048000     CALL CBLTDLI USING CHKP                                              
048100                        MSG-PCB                                           
048200                        MSG-IO-AREA-LENGTH                                
048300                        MSG-IO-AREA                                       
048400                        CHKP-AREA-1-LENGTH                                
048500                        CHKP-AREA-1                                       
048600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
048700     PERFORM IMS-STATUSKONTROLL                                           
048800                                                                          
048900     IF  IMS-EJ-OK                                                        
049000       DISPLAY 'IMS-KONTROLLREGIONEN EJ TILLGÄNGLIG'                      
049100       CALL FELLOG                                                        
049200     END-IF                                                               
049300     .                                                                    
049400     EJECT                                                                
049500 IMS-GHU-4551-4552 SECTION.                                               
049600                                                                          
049700     STRING 'WL455101(WDGXKEY  =' W-IDHTYP-4551-X ')'                     
049800            DELIMITED BY SIZE INTO SSA1                                   
049900     STRING 'WL455111(KDSEGKEY =' '1' ')'                                 
050000            DELIMITED BY SIZE INTO SSA2                                   
050100     MOVE '  GE' TO GODK-STATUSKODER                                      
050200     CALL CBLTDLI USING GHU  4551-PCB DLI-IO-AREA SSA1 SSA2               
050300     MOVE 4551-STATUS-CODE TO STATUS-WS                                   
050400     PERFORM IMS-STATUSKONTROLL                                           
050500     .                                                                    
050600     SKIP3                                                                
050700 IMS-REPL-4551 SECTION.                                                   
050800                                                                          
050900     MOVE '  ' TO GODK-STATUSKODER                                        
051000     CALL CBLTDLI USING REPL 4551-PCB DLI-IO-AREA                         
051100     MOVE 4551-STATUS-CODE TO STATUS-WS                                   
051200     PERFORM IMS-STATUSKONTROLL                                           
051300     .                                                                    
051400     EJECT                                                                
051500 IMS-DLET-4551 SECTION.                                                   
051600                                                                          
051700     MOVE '  ' TO GODK-STATUSKODER                                        
051800     CALL CBLTDLI USING DLET 4551-PCB DLI-IO-AREA                         
051900     MOVE 4551-STATUS-CODE TO STATUS-WS                                   
052000     PERFORM IMS-STATUSKONTROLL                                           
052100     .                                                                    
052200     SKIP3                                                                
052300 IMS-ISRT-4551-4552 SECTION.                                              
052400                                                                          
052500     STRING 'WL455101(WDGXKEY  =' W-IDHTYP-4551-X ')'                     
052600            DELIMITED BY SIZE INTO SSA1                                   
052700     MOVE 'WL455111 ' TO SSA2                                             
052800     MOVE '  ' TO GODK-STATUSKODER                                        
052900     CALL CBLTDLI USING ISRT 4551-PCB DLI-IO-AREA SSA1 SSA2               
053000     MOVE 4551-STATUS-CODE TO STATUS-WS                                   
053100     PERFORM IMS-STATUSKONTROLL                                           
053200     .                                                                    
053300     EJECT                                                                
053400 IMS-ISRT-4505-4506 SECTION.                                              
053500                                                                          
053600     STRING 'WL450501(WDGXKEY  =' W-IDHTYP-4505-X ')'                     
053700            DELIMITED BY SIZE INTO SSA1                                   
053800     MOVE 'WL450511 ' TO SSA2                                             
053900     MOVE '  ' TO GODK-STATUSKODER                                        
054000     CALL CBLTDLI USING ISRT 4505-PCB DLI-IO-AREA SSA1 SSA2               
054100     MOVE 4505-STATUS-CODE TO STATUS-WS                                   
054200     PERFORM IMS-STATUSKONTROLL                                           
054300     .                                                                    
054400     EJECT                                                                
054500 IMS-GU-ARTC11 SECTION.                                                   
054600                                                                          
054700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-K6-X ')'                      
054800            DELIMITED BY SIZE INTO SSA1                                   
054900     MOVE 'WLARTC11 '           TO SSA2                                   
055000     MOVE '  GE'                TO GODK-STATUSKODER                       
055100**   MOVE '  '                  TO GODK-STATUSKODER                       
055200     CALL CBLTDLI USING GU  ARTC-PCB DLI-IO-ARTC11 SSA1 SSA2              
055300     MOVE ARTC-STATUS-CODE      TO STATUS-WS                              
055400     PERFORM IMS-STATUSKONTROLL                                           
055500     .                                                                    
055600     EJECT                                                                
055700 IMS-STATUSKONTROLL SECTION.                                              
055800                                                                          
055900     SET STATUS-IX TO 1                                                   
056000     SEARCH GODK-STATUS                                                   
056100       AT  END                                                            
056200         STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                           
056300           DELIMITED BY SIZE INTO FELTEXT                                 
056400         CALL FELLOG                                                      
056500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
056600     END-SEARCH                                                           
056700     .                                                                    
