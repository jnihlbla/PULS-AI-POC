000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4791500.                                                
000400 AUTHOR.         CAMELIA OLGRENER.                                        
000500 DATE-WRITTEN.   91/09/24.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        RENSNINGSPGM FÖR ATT TA BORT ORDERDELAR SOM INTE HAR             
001100*        NÅGRA RADER PÅ SIG.                                              
001200*                                                                         
001300*        BORTTAGNINGSREGLER: ORDERDELEN HAR - STATUS  = 'P'               
001400*                                           - KVRADER =  0                
001500*                                                                         
001600*        OM ALLA ORDERDELAR INOM SAMMA IDDC PÅ SAMMA ORDER                
001700*        TAS BORT, TAS ÄVEN ARBETSTABELLEN BORT.                          
001800*                                                                         
001900*        PROGRAMMET LÄSER OCH UPPDATERAR :                                
002000*                                                                         
002100*        - WDQ301 (ORDERDELSKÖ)                                           
002200*        - WDQ212 (ORDERHUVUDREG-ARBETSTABELLEN)                          
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100     SKIP2                                                                
003200                                                                          
003300*    -- CHECKED BY WY2000                                                 
003400 77  IDPGM                       PIC X(8)    VALUE 'W4791500'.            
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700 77  W-CHKP-RAKNARE              PIC S9(5)   VALUE +0    COMP-3.          
003800 77  W-CHKP-MAX                  PIC S9(5)   VALUE +900  COMP-3.          
003900 77  CHKP-ID                     PIC X(8)    VALUE 'W4791500'.            
004000 77  MSG-IO-AREA-LENGTH-1        PIC S9(9)   VALUE +32  COMP SYNC.        
004100 77  MSG-IO-AREA-1               PIC X(32)   VALUE SPACE.                 
004200 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
004300 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
004301 77  WDQ3-STATUS                 PIC X(2).                                
004302                                                                          
004303 01  WS-AKT-IDORDER-IDDC.                                                 
004304     03  WS-AKT-IDORDER          PIC S9(7)   VALUE ZERO COMP-3.           
004305     03  WS-AKT-IDDC             PIC  X(2)   VALUE SPACE.                 
004306     03  WS-AKT-IDPRODNR         PIC S9(7)   VALUE ZERO COMP-3.           
004307 01  WS-SPAR-IDORDER-IDDC.                                                
004400     03  WS-SPAR-IDORDER         PIC S9(7)   VALUE ZERO COMP-3.           
004500     03  WS-SPAR-IDDC            PIC  X(2)   VALUE SPACE.                 
004600     03  WS-SPAR-IDPRODNR        PIC S9(7)   VALUE ZERO COMP-3.           
004700                                                                          
004800 77  WDQ2-BORTTAG-SW             PIC X.                                   
004900     88  WDQ2-BORTTAG-OK                     VALUE 'J'.                   
005000                                                                          
005100     EJECT                                                                
005200 01  DYNAMISKA-SUBPROGRAM.                                                
005300*                                                                         
005400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005700     SKIP2                                                                
005800*    --- PARAMETRAR TILL ABEND                                            
005900                                                                          
006000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006200     SKIP2                                                                
006300 01  FELTEXT.                                                             
006400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006600     EJECT                                                                
006700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
006800*                                                                         
006900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007000     SKIP2                                                                
007100 01  NYCKLAR-TILL-DLI.                                                    
007200                                                                          
007300*-------- ORDERHUVUDREG KÖ (WDQ2)                                         
007400     03  W-WDQ201-IDORDER-X.                                              
007500         05  W-IDORDER-OHUV      PIC S9(7)   VALUE ZERO COMP-3.           
007600                                                                          
007700     03  W-WDQ212-IDDC-X.                                                 
007800         05  W-IDDC-ARB          PIC  X(2)   VALUE SPACE.                 
007900                                                                          
008000*-------- ORDERDELSKÖ (WDQ3)                                              
008100     03  W-WDQ301KY-X.                                                    
008200         05 W-IDORDER-ODEL       PIC S9(7)   VALUE ZERO COMP-3.           
008300         05 W-IDDC-ODEL          PIC  X(2)   VALUE SPACE.                 
008400         05 W-IDPRODNR-ODEL      PIC S9(7)   VALUE ZERO COMP-3.           
008500         05 W-IDPLKLST-ODEL      PIC S9(3)   VALUE ZERO COMP-3.           
008600                                                                          
008700*    --- STATUS-KOD FRÅN IMS                                              
008800 01  STATUS-WS                   PIC XX.                                  
008900     88  SEGMENT-FINNS                       VALUE '  '.                  
009000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009100     88  BASEN-SLUT                          VALUE 'GB'.                  
009200     SKIP2                                                                
009300 01  GODK-STATUSKODER.                                                    
009400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009500     SKIP2                                                                
009600 01  SSA1                        PIC X(64).                               
009700 01  SSA2                        PIC X(64).                               
009800     EJECT                                                                
009900*    --- IMS FUNKTIONSKODER                                               
010000*01  -COPY W0003                                                          
010100     EJECT                                                                
010200*    ---  DLI INPUT-OUTPUT AREA                                           
010300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-Q201'.         
010400     SKIP3                                                                
010500 01  DLI-IO-AREA-OHUV.                                                    
010600*    03  -COPY WDQ201                                                     
010700     EJECT                                                                
010800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-Q211'.         
010900     SKIP3                                                                
011000 01  DLI-IO-AREA-DIRL.                                                    
011100*    03  -COPY WDQ211                                                     
011200     EJECT                                                                
011300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-Q212'.         
011400     SKIP3                                                                
011500 01  DLI-IO-AREA-ARB.                                                     
011600*    03  -COPY WDQ212                                                     
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-Q301'.         
011900     SKIP3                                                                
012000 01  DLI-IO-AREA-ODEL.                                                    
012100*    03  -COPY WDQ301                                                     
012200     EJECT                                                                
012300 LINKAGE SECTION.                                                         
012400                                                                          
013200*01  -COPY W0009      -PRE MSG-                                           
013210     EJECT                                                                
013211*01  -COPY W0008      -PRE WDQ3-                                          
013212     05  FILLER                  PIC X.                                   
013213     EJECT                                                                
013214*01  -COPY W0008      -PRE WDQ2-                                          
013215     05  FILLER                  PIC X.                                   
013216     EJECT                                                                
013900 PROCEDURE DIVISION  USING   MSG-PCB WDQ3-PCB   WDQ2-PCB.                 
014000     ENTRY 'DLITCBL' USING   MSG-PCB WDQ3-PCB   WDQ2-PCB.                 
014001                                                                          
014002     PERFORM A-INIT                                                       
014003     PERFORM S01-LAES-WDQ301                                              
014004                                                                          
014005     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
014006                   BASEN-SLUT                                             
014007                                                                          
014008       MOVE ODEL-IDORDER  TO WS-SPAR-IDORDER                              
014100       MOVE ODEL-IDDC     TO WS-SPAR-IDDC                                 
014200       MOVE ODEL-IDPRODNR TO WS-SPAR-IDPRODNR                             
014300                                                                          
014400       PERFORM UNTIL (WS-AKT-IDORDER-IDDC  NOT =                          
014500                      WS-SPAR-IDORDER-IDDC ) OR                           
014600                      BASEN-SLUT                                          
014700         PERFORM C-KOLLA-OM-WDQ301-SKA-BORT                               
014800                                                                          
014900         PERFORM S01-LAES-WDQ301                                          
015000       END-PERFORM                                                        
015100                                                                          
015200       PERFORM D-KOLLA-OM-WDQ212-SKA-BORT                                 
016020       IF W-CHKP-RAKNARE              >  W-CHKP-MAX                       
016030          PERFORM IMS-CHECKPOINT                                          
016040          MOVE ZERO          TO W-CHKP-RAKNARE                            
016041          MOVE ODEL-IDORDER  TO W-IDORDER-ODEL                            
016042          MOVE ODEL-IDDC     TO W-IDDC-ODEL                               
016043          MOVE ODEL-IDPRODNR TO W-IDPRODNR-ODEL                           
016044          MOVE ODEL-IDPLKLST TO W-IDPLKLST-ODEL                           
016045          PERFORM IMS-GHU-WDQ301                                          
016050       END-IF                                                             
016051                                                                          
016052     END-PERFORM                                                          
016053                                                                          
016054     MOVE ZERO TO RETURN-CODE                                             
016055     GOBACK                                                               
016056     .                                                                    
016057     EJECT                                                                
016058 A-INIT SECTION.                                                          
016900                                                                          
016910     PERFORM IMS-RESTART                                                  
016911                                                                          
016912     MOVE JA         TO WDQ2-BORTTAG-SW                                   
017010     MOVE ZERO       TO W-CHKP-RAKNARE                                    
017011     MOVE SPACE      TO WDQ3-STATUS                                       
017012     .                                                                    
017013     EJECT                                                                
017014 C-KOLLA-OM-WDQ301-SKA-BORT SECTION.                                      
017015                                                                          
017016     IF ODEL-KDODELSTA = 'P' AND                                          
017017        ODEL-KVRADER   = ZERO                                             
017018                                                                          
017019       IF SEGMENT-FINNS                                                   
017020          PERFORM IMS-DLET-WDQ301                                         
017030       END-IF                                                             
017200                                                                          
017300     ELSE                                                                 
017400       MOVE NEJ TO WDQ2-BORTTAG-SW                                        
017500                                                                          
017600     END-IF                                                               
017700     .                                                                    
017800     EJECT                                                                
017900 D-KOLLA-OM-WDQ212-SKA-BORT SECTION.                                      
018000                                                                          
018100     IF WDQ2-BORTTAG-OK AND ODEL-IDLEVNR = SPACE                          
018200                                                                          
018300       MOVE WS-SPAR-IDORDER  TO W-IDORDER-OHUV                            
018400       MOVE WS-SPAR-IDDC     TO W-IDDC-ARB                                
018500                                                                          
018600       PERFORM IMS-GHU-WDQ212                                             
018700       IF SEGMENT-FINNS                                                   
018800         DISPLAY 'Q212-BORDE-EJ-DLET HÄR: ' W-IDORDER-OHUV                
018900                                            W-IDDC-ARB                    
019000         PERFORM IMS-DLET-WDQ212                                          
019100       END-IF                                                             
019200                                                                          
019300       PERFORM IMS-GU-WDQ211                                              
019400       IF SEGMENT-SAKNAS                                                  
019500          PERFORM IMS-GU-WDQ212-OKVAL                                     
019600          IF SEGMENT-SAKNAS                                               
019700             PERFORM IMS-GHU-WDQ201                                       
019800             IF SEGMENT-SAKNAS                                            
019900                CONTINUE                                                  
020000                DISPLAY 'FIX FIX GE PÅ WDQ201'                            
020100                DISPLAY 'W-IDORDER-OHUV = ' W-IDORDER-OHUV                
020200             ELSE                                                         
020300                MOVE JA         TO OHUV-FLBORT                            
020400                PERFORM IMS-REPL-WDQ201                                   
020500               DISPLAY 'Q201-FLBORT=JA: ' W-IDORDER-OHUV                  
020600             END-IF                                                       
020700          END-IF                                                          
020800*         IF SEGMENT-SAKNAS                                               
020900*            PERFORM IMS-GHU-WDQ201                                       
021000*            MOVE JA         TO OHUV-FLBORT                               
021100*            PERFORM IMS-REPL-WDQ201                                      
021200*           DISPLAY 'Q201-FLBORT=JA: ' W-IDORDER-OHUV                     
021300*         END-IF                                                          
021400       END-IF                                                             
021500                                                                          
021600     END-IF                                                               
021700                                                                          
021800     MOVE WDQ3-STATUS         TO STATUS-WS                                
021900     MOVE JA TO WDQ2-BORTTAG-SW                                           
022000     .                                                                    
022100     EJECT                                                                
022200 S01-LAES-WDQ301 SECTION.                                                 
022300                                                                          
022400     PERFORM IMS-GHN-WDQ301                                               
022500                                                                          
022600     IF SEGMENT-FINNS                                                     
022700       MOVE ODEL-IDORDER  TO WS-AKT-IDORDER                               
022800       MOVE ODEL-IDDC     TO WS-AKT-IDDC                                  
022900       MOVE ODEL-IDPRODNR TO WS-AKT-IDPRODNR                              
023000     ELSE                                                                 
023100       MOVE 0             TO WS-AKT-IDORDER                               
023200       MOVE 0             TO WS-AKT-IDPRODNR                              
023300       MOVE SPACE         TO WS-AKT-IDDC                                  
023400     END-IF                                                               
023500     MOVE STATUS-WS       TO WDQ3-STATUS                                  
023600     .                                                                    
023700     EJECT                                                                
023800* --- IMS SEKTIONER ---                                                   
024610                                                                          
024620 IMS-RESTART           SECTION.                                           
024630                                                                          
024640     MOVE SPACE TO MSG-IO-AREA-1                                          
024650     MOVE '  ' TO GODK-STATUSKODER                                        
024660     CALL CBLTDLI USING XRST MSG-PCB                                      
024670                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
024680                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
024690     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024691     PERFORM IMS-STATUSKONTROLL                                           
024692     .                                                                    
024693                                                                          
024694     SKIP3                                                                
024710 IMS-CHECKPOINT        SECTION.                                           
024720                                                                          
024730     MOVE CHKP-ID TO MSG-IO-AREA-1                                        
024740     MOVE '  XD' TO GODK-STATUSKODER                                      
024750     CALL CBLTDLI USING CHKP MSG-PCB                                      
024760                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
024770                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
024780     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024790     PERFORM IMS-STATUSKONTROLL                                           
024791     .                                                                    
024792     EJECT                                                                
024793 IMS-GHU-WDQ301 SECTION.                                                  
024794     STRING 'WDQ301  (WDQ301KY =' W-WDQ301KY-X ')'                        
024795            DELIMITED BY SIZE INTO SSA1                                   
024796     MOVE '  ' TO GODK-STATUSKODER                                        
024797     CALL CBLTDLI USING GHU WDQ3-PCB DLI-IO-AREA-ODEL SSA1                
024798     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
024799     PERFORM IMS-STATUSKONTROLL                                           
024800     .                                                                    
024801 IMS-GHN-WDQ301 SECTION.                                                  
024802                                                                          
024803     MOVE 'WDQ301  ' TO SSA1                                              
024804     MOVE '  GEGB'    TO GODK-STATUSKODER                                 
024805     CALL CBLTDLI USING GHN WDQ3-PCB DLI-IO-AREA-ODEL SSA1                
024806     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
024807     PERFORM IMS-STATUSKONTROLL                                           
024808     .                                                                    
024810                                                                          
024900 IMS-DLET-WDQ301 SECTION.                                                 
025000                                                                          
025100     MOVE '  ' TO GODK-STATUSKODER                                        
025200     CALL CBLTDLI USING DLET WDQ3-PCB DLI-IO-AREA-ODEL                    
025300     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
025400     PERFORM IMS-STATUSKONTROLL                                           
026210     ADD +5    TO W-CHKP-RAKNARE                                          
026211     .                                                                    
026212     EJECT                                                                
026213 IMS-GHU-WDQ201 SECTION.                                                  
026214                                                                          
026215     STRING 'WDQ201  (IDORDER  =' W-WDQ201-IDORDER-X ')'                  
026216          DELIMITED BY SIZE INTO SSA1                                     
026217     MOVE '  GE'    TO GODK-STATUSKODER                                   
026218     CALL CBLTDLI USING GHU WDQ2-PCB DLI-IO-AREA-OHUV SSA1                
026300     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
026400     PERFORM IMS-STATUSKONTROLL                                           
026500     .                                                                    
026600                                                                          
026700 IMS-REPL-WDQ201       SECTION.                                           
026800                                                                          
026900     MOVE '  '                    TO GODK-STATUSKODER                     
027000     CALL CBLTDLI USING REPL WDQ2-PCB DLI-IO-AREA-OHUV                    
027100     MOVE WDQ2-STATUS-CODE        TO STATUS-WS                            
027200     PERFORM IMS-STATUSKONTROLL                                           
028010     ADD +2    TO W-CHKP-RAKNARE                                          
028011     .                                                                    
028012                                                                          
028013     EJECT                                                                
028014 IMS-GHU-WDQ212 SECTION.                                                  
028015                                                                          
028016     STRING 'WDQ201  (IDORDER  =' W-WDQ201-IDORDER-X ')'                  
028017          DELIMITED BY SIZE INTO SSA1                                     
028018     STRING 'WDQ212  (IDDC     =' W-WDQ212-IDDC-X ')'                     
028100          DELIMITED BY SIZE INTO SSA2                                     
028200     MOVE '  GE' TO GODK-STATUSKODER                                      
028300     CALL CBLTDLI USING GHU WDQ2-PCB DLI-IO-AREA-ARB SSA1 SSA2            
028400     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
028500     PERFORM IMS-STATUSKONTROLL                                           
028600     .                                                                    
028700                                                                          
028800 IMS-GU-WDQ211       SECTION.                                             
028900                                                                          
029000     STRING 'WDQ201  (IDORDER  =' W-WDQ201-IDORDER-X ')'                  
029100          DELIMITED BY SIZE INTO SSA1                                     
029200     MOVE 'WDQ211 ' TO SSA2                                               
029300     MOVE '  GE' TO GODK-STATUSKODER                                      
029400     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-AREA-DIRL SSA1 SSA2            
029500     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
029600     PERFORM IMS-STATUSKONTROLL                                           
029700     .                                                                    
029800     EJECT                                                                
029900 IMS-GU-WDQ212-OKVAL SECTION.                                             
030000                                                                          
030100     STRING 'WDQ201  (IDORDER  =' W-WDQ201-IDORDER-X ')'                  
030200          DELIMITED BY SIZE INTO SSA1                                     
030300     MOVE 'WDQ212 ' TO SSA2                                               
030400     MOVE '  GE' TO GODK-STATUSKODER                                      
030500     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-AREA-ARB SSA1 SSA2             
030600     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
030700     PERFORM IMS-STATUSKONTROLL                                           
030800     .                                                                    
030900     EJECT                                                                
031000 IMS-DLET-WDQ212 SECTION.                                                 
031100                                                                          
031200     MOVE '  ' TO GODK-STATUSKODER                                        
031300     CALL CBLTDLI USING DLET WDQ2-PCB DLI-IO-AREA-ARB                     
031400     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
031500     PERFORM IMS-STATUSKONTROLL                                           
032310     ADD +2    TO W-CHKP-RAKNARE                                          
032311     .                                                                    
032312     EJECT                                                                
032313 IMS-STATUSKONTROLL SECTION.                                              
032314                                                                          
032315     SET STATUS-IX TO 1                                                   
032316     SEARCH GODK-STATUS                                                   
032317       AT END                                                             
032318         MOVE 'FELAKTIG STATUSKOD FRÅN IMS' TO FELTEXT-STR                
032400         DISPLAY FELTEXT                                                  
032500         CALL FELLOG                                                      
032600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
032700     END-SEARCH                                                           
032800     .                                                                    
