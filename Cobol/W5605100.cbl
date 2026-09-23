000100 ID DIVISION.                                                             
000200 PROGRAM-ID.    W5605100.                                                 
000300                                                                          
000400*                                                                         
000500*    AUTHOR.        ANDERS HENRIKSSON.                                    
000600*    DATE-WRITTEN   AUGUSTI 2005.                                         
000700*                                                                         
000800*    FUNKTION:                                                            
000900*               SKAPAR LISTPOSTER KURANSGRUPPERADE EL. NEDAN:             
001000*             - ARTIKLAR MED KDERS > 20     HAMNAR I KURANS 6             
001100*             - ARTIKLAR MED PUBLICERINGSVECKA YNGRE ÄN                   
001200*               KÖRNINGSDATUM - 2 ÅR        HAMNAR I KURANS 1             
001300*             - ARTIKLAR UTAN ORDERINGÅNG   HAMNAR I KURANS 5             
001400*                                                                         
001500*             - ARTIKLAR MED KDERS > 21     HAMNAR I KURANS 5             
001600*    UNDANTAG - ARTIKLAR MED KDERS = 29  HANTERAS ENLIGT SHELFLIFE        
001700*                                                                         
001800*               SKAPAR LISTA KURANSGRUPPERING MED SHELFLIFEMETODEN        
001900*               ÅRSBEHOV TAS FRAM M.H.A. ORDERINGÅNG W01186.              
002000*               KVOI-RULL FÖR 53 VECKOR                                   
002100*               TOT. LAGERSALDO HÄMTAS FRÅN LAGERBANDET W01184.           
002200*               KVLS + KVEFRS                                             
002300*               ÅRSBEHOV STÄLLS I RELATION TILL TOT.LAGERSALDO.           
002400*               ALLT UPP TILL   1  ÅRSBEHOV HAMNAR I KURANS 1             
002500*               FRÅN 1 UPP TILL 3  ÅRSBEHOV HAMNAR I KURANS 2             
002600*               FRÅN 3 UPP TILL 5  ÅRSBEHOV HAMNAR I KURANS 3             
002700*               FRÅN 5 UPP TILL 10 ÅRSBEHOV HAMNAR I KURANS 4             
002800*               FRÅN 10 ÅRBEHOV OCH UPPÅT   HAMNAR I KURANS 5             
002900*                                                                         
003000     EJECT                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200                                                                          
003300 INPUT-OUTPUT SECTION.                                                    
003400 FILE-CONTROL.                                                            
003500                                                                          
003600                                                                          
003700     SELECT W56050  ASSIGN       TO W56051D1.                             
003800                                                                          
003900     SELECT W01186  ASSIGN       TO W56051D2.                             
004000                                                                          
004100     SELECT W56051  ASSIGN       TO W56051D3.                             
004200                                                                          
004300     EJECT                                                                
004400 DATA DIVISION.                                                           
004500 FILE SECTION.                                                            
004600                                                                          
004700 FD  W56050                                                               
004800     RECORDING F                                                          
004900     BLOCK CONTAINS 0.                                                    
005000                                                                          
005100 01  INPOST1.                                                             
005200*    03  -COPY  W56050   -L.                                              
005300     SKIP2                                                                
005400                                                                          
005500 FD  W01186                                                               
005600     RECORDING F                                                          
005700     BLOCK CONTAINS 0.                                                    
005800                                                                          
005900 01  INPOST2.                                                             
006000*    03   -COPY W01186    -L                                              
006100     SKIP2                                                                
006200                                                                          
006300 FD  W56051                                                               
006400     RECORDING F                                                          
006500     BLOCK CONTAINS 0.                                                    
006600                                                                          
006700*01  UTPOST      -COPY W56051    -L                                       
006800     SKIP2                                                                
006900                                                                          
007000 WORKING-STORAGE SECTION.                                                 
007100     SKIP3                                                                
007200 77  IDPGM                   PIC X(8)      VALUE 'W5605100'.              
007300 77  JA                      PIC X         VALUE 'J'.                     
007400 77  NEJ                     PIC X         VALUE 'N'.                     
007500 77  EOF-W56050              PIC X         VALUE 'N'.                     
007600 77  EOF-W01186              PIC X         VALUE 'N'.                     
007700 77  WS-IX                   PIC S9(3)   COMP-3 VALUE ZERO.               
007800 77  MAX-WEEK                PIC S9(3)   COMP-3 VALUE +53.                
007900                                                                          
008000 01  W-TIAAVVD               PIC S9(5).                                   
008100 01  W-TIYYAAVVD             PIC S9(7).                                   
008200 01  W-TIAAVVD-P             PIC S9(5)   VALUE ZERO COMP-3.               
008300                                                                          
008400 01  WS-KVOI-TOT             PIC S9(7)   COMP-3 VALUE ZERO.               
008500 01  WS-KDERS                PIC 9(3)    VALUE ZERO.                      
008600 01  WS-KDPSLLOC             PIC 9(2)    VALUE ZERO.                      
008700 01  WS-TIFINLV              PIC S9(5)   COMP-3 VALUE ZERO.               
008800 01  WS-TIFINLV2             PIC S9(7)   COMP-3 VALUE ZERO.               
008900 01  WS-CHECK                PIC S9(7)   COMP-3 VALUE ZERO.               
009000 01  WS-STOCK-KURANS         PIC S9(7)   COMP-3 VALUE ZERO.               
009100 01  WS-STOCK-KURANS1        PIC S9(7)   COMP-3 VALUE ZERO.               
009200 01  WS-STOCK-KURANS2        PIC S9(7)   COMP-3 VALUE ZERO.               
009300 01  WS-STOCK-KURANS3        PIC S9(7)   COMP-3 VALUE ZERO.               
009400 01  WS-STOCK-KURANS4        PIC S9(7)   COMP-3 VALUE ZERO.               
009500 01  WS-STOCK-KURANS5        PIC S9(7)   COMP-3 VALUE ZERO.               
009600 01  WS-STOCK-KURANS6        PIC S9(7)   COMP-3 VALUE ZERO.               
009700 01  WS-KURANS               PIC S9(1)   COMP-3 VALUE ZERO.               
009800 01  WS-KVPB                 PIC S9(6)V9(1) COMP-3.                       
009900                                                                          
010000 01  WDATUM                  PIC X(6)    VALUE 'WDATUM'.                  
010100                                                                          
010200 01  SUBPROGRAM.                                                          
010300     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
010400     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
010500                                                                          
010600 01  W01184-TRANSID.                                                      
010700     03  FILLER              PIC X(6) VALUE 'W01184'.                     
010800     03  FILLER              PIC X(8) VALUE 'W56051D1'.                   
010900     03  FILLER              PIC X(4) VALUE '1184'.                       
011000                                                                          
011100 01  W01186-TRANSID.                                                      
011200     03  FILLER              PIC X(6) VALUE 'W01186'.                     
011300     03  FILLER              PIC X(8) VALUE 'W56051D2'.                   
011400     03  FILLER              PIC X(4) VALUE '1186'.                       
011500                                                                          
011600 01  W56050-TRANSID.                                                      
011700     03  FILLER              PIC X(6) VALUE 'W56050'.                     
011800     03  FILLER              PIC X(8) VALUE 'W56051D3'.                   
011900     03  FILLER              PIC X(4) VALUE '1160'.                       
012000                                                                          
012100 01  W56051-TRANSID.                                                      
012200     03  FILLER              PIC X(6) VALUE 'W56051'.                     
012300     03  FILLER              PIC X(8) VALUE 'W56051D4'.                   
012400     03  FILLER              PIC X(4) VALUE '  UT'.                       
012500                                                                          
012600     EJECT                                                                
012700*   -COPY W0005  -PRE POSTSUM-                                            
012800     EJECT                                                                
012900*   -COPY WDATKORT                                                        
013000     EJECT                                                                
013100                                                                          
013200*01  -COPY WWDC99                                                         
013300     EJECT                                                                
013400                                                                          
013500 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
013600 01  INAREA1.                                                             
013700*    03  -COPY W56050 -PRE IN-                                            
013800     EJECT                                                                
013900                                                                          
014000 01  FILLER                  PIC X(16)   VALUE 'IN-AREA2   '.             
014100 01  INAREA2.                                                             
014200*    03  -COPY W01186                                                     
014300     EJECT                                                                
014400                                                                          
014500 01  FILLER                  PIC X(16)   VALUE 'UT-AREA1    '.            
014600 01  UTAREA1.                                                             
014700*    03  -COPY W56051 -PRE UT-                                            
014800     EJECT                                                                
014900                                                                          
015000 PROCEDURE DIVISION.                                                      
015100 MAIN SECTION.                                                            
015200                                                                          
015300     PERFORM A-INIT                                                       
015400                                                                          
015500     PERFORM S01-READ-W56050-POST                                         
015600     PERFORM S02-READ-W01186-POST                                         
015700                                                                          
015800     PERFORM UNTIL EOF-W56050 = JA                                        
015900       MOVE IN-IDDC     TO WS-IDDC                                        
016000       PERFORM UNTIL EOF-W01186 = JA                                      
016100         EVALUATE TRUE                                                    
016200           WHEN IN-IDARTNR = DC-IDARTNR                                   
016300             IF IN-IDDC = DC-IDDC                                         
016400               PERFORM B-READ-STOCK                                       
016500               PERFORM C-REDIGERA-PRODUCT-GROUP                           
016600               PERFORM D-CREATE-KURANS-FILE                               
016700               PERFORM S01-READ-W56050-POST                               
016800               PERFORM S02-READ-W01186-POST                               
016900             ELSE                                                         
017000               IF IN-IDDC > DC-IDDC                                       
017100                 PERFORM S02-READ-W01186-POST                             
017200               ELSE                                                       
017300                 IF IN-IDDC < DC-IDDC                                     
017400                   MOVE +0   TO WS-KVOI-TOT                               
017500                   PERFORM C-REDIGERA-PRODUCT-GROUP                       
017600                   PERFORM D-CREATE-KURANS-FILE                           
017700                   PERFORM S01-READ-W56050-POST                           
017800                 END-IF                                                   
017900               END-IF                                                     
018000             END-IF                                                       
018100           WHEN IN-IDARTNR < DC-IDARTNR                                   
018200             MOVE +0    TO WS-KVOI-TOT                                    
018300             PERFORM C-REDIGERA-PRODUCT-GROUP                             
018400             PERFORM E-CREATE-KURANS-FILE                                 
018500             PERFORM S01-READ-W56050-POST                                 
018600           WHEN IN-IDARTNR > DC-IDARTNR                                   
018700             PERFORM S02-READ-W01186-POST                                 
018800         END-EVALUATE                                                     
018900       END-PERFORM                                                        
019000       MOVE +0    TO WS-KVOI-TOT                                          
019100       PERFORM C-REDIGERA-PRODUCT-GROUP                                   
019200       PERFORM E-CREATE-KURANS-FILE                                       
019300       PERFORM S01-READ-W56050-POST                                       
019400     END-PERFORM                                                          
019500                                                                          
019600     PERFORM Z-END                                                        
019700                                                                          
019800     MOVE ZERO TO RETURN-CODE                                             
019900     GOBACK                                                               
020000     .                                                                    
020100     EJECT                                                                
020200                                                                          
020300 A-INIT SECTION.                                                          
020400     OPEN INPUT  W56050                                                   
020500                 W01186                                                   
020600          OUTPUT W56051                                                   
020700                                                                          
020800     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
020900                                                                          
021000     CALL DATKORT  USING IDPGM WDATUM DATUMKORT                           
021100     MOVE D-AAR           TO W-TIAAVVD(1:2)                               
021200     MOVE D-VECKA         TO W-TIAAVVD(3:2)                               
021300     MOVE 1               TO W-TIAAVVD(5:1)                               
021400     COMPUTE W-TIYYAAVVD = 2000000 + W-TIAAVVD                            
021500     .                                                                    
021600     EJECT                                                                
021700                                                                          
021800 B-READ-STOCK SECTION.                                                    
021900     MOVE +0 TO WS-KVOI-TOT                                               
022000     MOVE +1 TO WS-IX                                                     
022100     PERFORM UNTIL WS-IX > MAX-WEEK                                       
022200       IF DC-KVOI-RULL(WS-IX) < +0                                        
022300         IF DC-KVOI-REF-RULL(WS-IX) < +0                                  
022400           COMPUTE WS-KVOI-TOT = WS-KVOI-TOT + 0 + 0                      
022500         ELSE                                                             
022600           COMPUTE WS-KVOI-TOT = WS-KVOI-TOT + 0 +                        
022700                   DC-KVOI-REF-RULL(WS-IX)                                
022800         END-IF                                                           
022900       ELSE                                                               
023000         IF DC-KVOI-REF-RULL(WS-IX) < +0                                  
023100           COMPUTE WS-KVOI-TOT = WS-KVOI-TOT +                            
023200                 DC-KVOI-RULL(WS-IX) + 0                                  
023300         ELSE                                                             
023400           COMPUTE WS-KVOI-TOT = WS-KVOI-TOT +                            
023500                 DC-KVOI-RULL(WS-IX) + DC-KVOI-REF-RULL(WS-IX)            
023600         END-IF                                                           
023700       END-IF                                                             
023800       ADD +1 TO WS-IX                                                    
023900     END-PERFORM                                                          
024000     .                                                                    
024100     EJECT                                                                
024200                                                                          
024300 C-REDIGERA-PRODUCT-GROUP SECTION.                                        
024400                                                                          
024500     MOVE IN-IDDC       TO WS-IDDC                                        
024600     MOVE IN-KDERS      TO WS-KDERS                                       
024700     IF NDC-NA                                                            
024800       MOVE IN-KDPSLLOC TO WS-KDPSLLOC                                    
024900       COMPUTE WS-KVPB = IN-KVPB-REF                                      
025000     ELSE                                                                 
025100       MOVE IN-KDPRODSL TO WS-KDPSLLOC                                    
025200       COMPUTE WS-KVPB = IN-KVPB-REF                                      
025300     END-IF                                                               
025400     MOVE IN-TIFINLV    TO WS-TIFINLV                                     
025500     .                                                                    
025600     EJECT                                                                
025700                                                                          
025800 D-CREATE-KURANS-FILE SECTION.                                            
025900     COMPUTE WS-STOCK-KURANS = IN-KVEFRS + IN-KVLS                        
026000     PERFORM S04-GIVE-KURANS-FIRST                                        
026100     PERFORM S05-GIVE-KURANS-SHELFLIFE                                    
026200     PERFORM S06-CREATE-FILE-W56051                                       
026300     .                                                                    
026400     EJECT                                                                
026500                                                                          
026600 E-CREATE-KURANS-FILE SECTION.                                            
026700     COMPUTE WS-STOCK-KURANS = IN-KVEFRS + IN-KVLS                        
026800     PERFORM S04-GIVE-KURANS-FIRST                                        
026900     PERFORM S07-GIVE-KURANS-SHELFLIFE                                    
027000     PERFORM S06-CREATE-FILE-W56051                                       
027100     .                                                                    
027200     EJECT                                                                
027300                                                                          
027400 Z-END SECTION.                                                           
027500     CLOSE W56050                                                         
027600           W01186                                                         
027700           W56051                                                         
027800                                                                          
027900     MOVE 'S'        TO POSTSUM-OPKOD                                     
028000     CALL POSTSUM USING POSTSUM-PARM                                      
028100     .                                                                    
028200     EJECT                                                                
028300                                                                          
028400 S01-READ-W56050-POST SECTION.                                            
028500     READ W56050 INTO INAREA1                                             
028600     AT END                                                               
028700       MOVE JA TO EOF-W56050                                              
028800     NOT AT END                                                           
028900       MOVE W56050-TRANSID TO POSTSUM-TRANSID                             
029000       CALL POSTSUM USING POSTSUM-PARM                                    
029100     END-READ                                                             
029200     .                                                                    
029300     EJECT                                                                
029400                                                                          
029500 S02-READ-W01186-POST SECTION.                                            
029600     READ W01186 INTO INAREA2                                             
029700     AT END                                                               
029800       MOVE JA TO EOF-W01186                                              
029900     NOT AT END                                                           
030000       MOVE W01186-TRANSID TO POSTSUM-TRANSID                             
030100       CALL POSTSUM USING POSTSUM-PARM                                    
030200     END-READ                                                             
030300     .                                                                    
030400     SKIP2                                                                
030500                                                                          
030600 S04-GIVE-KURANS-FIRST SECTION.                                           
030700     MOVE +0 TO WS-KURANS                                                 
030800     MOVE +0 TO WS-STOCK-KURANS1                                          
030900     MOVE +0 TO WS-STOCK-KURANS2                                          
031000     MOVE +0 TO WS-STOCK-KURANS3                                          
031100     MOVE +0 TO WS-STOCK-KURANS4                                          
031200     MOVE +0 TO WS-STOCK-KURANS5                                          
031300     MOVE +0 TO WS-STOCK-KURANS6                                          
031400     IF WS-KDERS > 20                                                     
031500       IF WS-KVOI-TOT = +0                                                
031600         MOVE +6 TO WS-KURANS                                             
031700         MOVE WS-STOCK-KURANS TO WS-STOCK-KURANS6                         
031800       ELSE                                                               
031900         MOVE +5 TO WS-KURANS                                             
032000         MOVE WS-STOCK-KURANS TO WS-STOCK-KURANS5                         
032100       END-IF                                                             
032200     ELSE                                                                 
032300       IF WS-TIFINLV > +0                                                 
032400         IF WS-TIFINLV > +50000                                           
032500           COMPUTE WS-TIFINLV2 = 1900000 + WS-TIFINLV                     
032600         ELSE                                                             
032700           COMPUTE WS-TIFINLV2 = 2000000 + WS-TIFINLV                     
032800         END-IF                                                           
032900         COMPUTE WS-CHECK = W-TIYYAAVVD - WS-TIFINLV2                     
033000*** NYA ARTIKLAR MINDRE ÄN 2 ÅR                                           
033100         IF WS-CHECK < 2001                                               
033200           MOVE +1 TO WS-KURANS                                           
033300           MOVE WS-STOCK-KURANS TO WS-STOCK-KURANS1                       
033400         ELSE                                                             
033500           IF  WS-KDERS = +0                                              
033600           AND WS-KVPB  = +0                                              
033700               MOVE +5 TO WS-KURANS                                       
033800               MOVE WS-STOCK-KURANS TO WS-STOCK-KURANS5                   
033900           ELSE                                                           
034000             IF WS-KVOI-TOT = +0                                          
034100               MOVE +5 TO WS-KURANS                                       
034200               MOVE WS-STOCK-KURANS TO WS-STOCK-KURANS5                   
034300             END-IF                                                       
034400           END-IF                                                         
034500         END-IF                                                           
034600       ELSE                                                               
034700         MOVE +1 TO WS-KURANS                                             
034800         MOVE WS-STOCK-KURANS TO WS-STOCK-KURANS1                         
034900       END-IF                                                             
035000     END-IF                                                               
035100     .                                                                    
035200     EJECT                                                                
035300                                                                          
035400 S05-GIVE-KURANS-SHELFLIFE SECTION.                                       
035500     IF WS-KURANS = +0                                                    
035600       IF WS-KVOI-TOT > WS-STOCK-KURANS                                   
035700         MOVE WS-STOCK-KURANS TO WS-STOCK-KURANS1                         
035800       ELSE                                                               
035900         IF (WS-KVOI-TOT * 3) > WS-STOCK-KURANS                           
036000           MOVE WS-KVOI-TOT TO WS-STOCK-KURANS1                           
036100           COMPUTE WS-STOCK-KURANS2 = WS-STOCK-KURANS -                   
036200                                      WS-KVOI-TOT                         
036300         ELSE                                                             
036400           IF (WS-KVOI-TOT * 5) > WS-STOCK-KURANS                         
036500             MOVE WS-KVOI-TOT TO WS-STOCK-KURANS1                         
036600             COMPUTE WS-STOCK-KURANS2 = (WS-KVOI-TOT * 3) -               
036700                                         WS-KVOI-TOT                      
036800             COMPUTE WS-STOCK-KURANS3 = WS-STOCK-KURANS -                 
036900                                       (WS-KVOI-TOT * 3)                  
037000           ELSE                                                           
037100             IF (WS-KVOI-TOT * 10) > WS-STOCK-KURANS                      
037200               MOVE WS-KVOI-TOT TO WS-STOCK-KURANS1                       
037300               COMPUTE WS-STOCK-KURANS2 = (WS-KVOI-TOT * 3) -             
037400                                           WS-KVOI-TOT                    
037500               COMPUTE WS-STOCK-KURANS3 = (WS-KVOI-TOT * 5) -             
037600                                          (WS-KVOI-TOT * 3)               
037700               COMPUTE WS-STOCK-KURANS4 =  WS-STOCK-KURANS -              
037800                                          (WS-KVOI-TOT * 5)               
037900             ELSE                                                         
038000               MOVE WS-KVOI-TOT TO WS-STOCK-KURANS1                       
038100               COMPUTE WS-STOCK-KURANS2 = (WS-KVOI-TOT * 3) -             
038200                                           WS-KVOI-TOT                    
038300               COMPUTE WS-STOCK-KURANS3 = (WS-KVOI-TOT * 5) -             
038400                                          (WS-KVOI-TOT * 3)               
038500               COMPUTE WS-STOCK-KURANS4 = (WS-KVOI-TOT * 10) -            
038600                                          (WS-KVOI-TOT * 5)               
038700               COMPUTE WS-STOCK-KURANS5 =  WS-STOCK-KURANS -              
038800                                          (WS-KVOI-TOT * 10)              
038900             END-IF                                                       
039000           END-IF                                                         
039100         END-IF                                                           
039200       END-IF                                                             
039300     END-IF                                                               
039400     .                                                                    
039500     EJECT                                                                
039600                                                                          
039700 S06-CREATE-FILE-W56051 SECTION.                                          
039800     MOVE IN-IDARTNR       TO UT-IDARTNR                                  
039900     MOVE IN-IDDC          TO UT-IDDC                                     
040000     MOVE IN-PRAVCOST      TO UT-PRAVCOST                                 
040100     MOVE WS-KDERS         TO UT-KDERS                                    
040200     MOVE WS-KDPSLLOC      TO UT-KDPSLLOC                                 
040300     MOVE WS-STOCK-KURANS  TO UT-KVLS-TOT                                 
040400     MOVE WS-KVOI-TOT      TO UT-KVOI-TOT                                 
040500     MOVE WS-STOCK-KURANS1 TO UT-STOCK-KURANS1                            
040600     MOVE WS-STOCK-KURANS2 TO UT-STOCK-KURANS2                            
040700     MOVE WS-STOCK-KURANS3 TO UT-STOCK-KURANS3                            
040800     MOVE WS-STOCK-KURANS4 TO UT-STOCK-KURANS4                            
040900     MOVE WS-STOCK-KURANS5 TO UT-STOCK-KURANS5                            
041000     MOVE WS-STOCK-KURANS6 TO UT-STOCK-KURANS6                            
041100                                                                          
041200     WRITE UTPOST FROM UT-W56051                                          
041300     MOVE W56051-TRANSID TO POSTSUM-TRANSID                               
041400     CALL POSTSUM USING POSTSUM-PARM                                      
041500     .                                                                    
041600     EJECT                                                                
041700                                                                          
041800 S07-GIVE-KURANS-SHELFLIFE SECTION.                                       
041900     IF WS-KURANS = +0                                                    
042000       MOVE WS-STOCK-KURANS TO WS-STOCK-KURANS1                           
042100     END-IF                                                               
042200     .                                                                    
042300     EJECT                                                                
042400                                                                          
