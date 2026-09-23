000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W271ERS.                                                 
000400*AUTHOR.         STEFAN KIHLBERG.                                         
000500*DATE-WRITTEN.   94/12/12.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        BEDÖMMER REFILL FÖR ARTIKLAR MED ERSÄTTNINGSKOD                  
001200*        MELLAN 01 OCH 09.                                                
001300*        AVGÖR OM REFILLEN SKALL BLI RETURORDER, REFILLORDER              
001400*        ELLER LÄMMNAS UTAN ÅTGÄRD                                        
001500*        UPPDATERAR WDK7                                                  
001600*                                                                         
001700*    CHANGE LOG:                                                          
001800*        2013-06-24  E'TRACKER 10162003                                   
001900*                    DECREASE NO OF WEEKS AT SUPERSESSION.                
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400     SKIP2                                                                
003500*    -COPY WY2000W2                                                       
003600     SKIP3                                                                
003700 77  IDPGM                       PIC X(8)    VALUE 'W271ERS'.             
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000     SKIP2                                                                
004100 01  FELTEXT.                                                             
004200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004400     EJECT                                                                
004500 01  FILLER                    PIC X(24)  VALUE 'ARBETSAREOR'.            
004600                                                                          
004700 01  ARBETSAREOR.                                                         
004800                                                                          
004900     03 AKTUELLT-XDC-NUM        PIC 9(2)    VALUE ZERO.                   
005000     03 AKT-XDC-IX              PIC 9(1)    VALUE ZERO.                   
005001                                                                          
005010     03 WS-TIERSDAT-CDC-REFILL  PIC S9(5)    VALUE ZERO COMP-3.           
005100                                                                          
005200     03 WS-ANTAL-AKT-XDC     PIC S9(7)      VALUE ZERO COMP-3.            
005300     03 WS-MAX-PAFYLLNAD     PIC S9(7)      VALUE ZERO COMP-3.            
005400     03 WS-ANTAL-VECKOR-KVAR PIC S9(7)      VALUE ZERO COMP-3.            
005600     03 WS-ANTAL-Q1          PIC S9(7)      VALUE ZERO COMP-3.            
005700     03 WS-ANTAL-DAGAR       PIC S9(1)      VALUE ZERO COMP-3.            
005800     03 WS-ANTAL-DAGAR-RETUR-                                             
005900-                TIERS       PIC S9(3)      VALUE ZERO COMP-3.            
006000     03 WS-ANTAL-DAGAR-TIERS  PIC S9(3)      VALUE ZERO COMP-3.           
006100     03 WS-TIERSDAT-PREL-C1-                                              
006200-                     AAMMDD PIC 9(6)      VALUE ZERO COMP-3.             
006300     03 WS-TIERSDAT-AAMMDD   PIC  9(6)      VALUE ZERO COMP-3.            
006400     03 WS-KVDISP-CDC        PIC S9(7)      VALUE ZERO COMP-3.            
006500     03 WS-KVDISP-XDC        PIC S9(7)      VALUE ZERO COMP-3.            
006600     03 WS-REFILLPOINT       PIC S9(7)      VALUE ZERO COMP-3.            
006700     03 WS-NASTA-VECKA       PIC S9(5)      VALUE ZERO COMP-3.            
006710     03 WS-SPAR-TIERSDAT     PIC S9(5)      VALUE ZERO COMP-3.            
006800                                                                          
006900     03 WS-KVPB-DAG-CDC      PIC S9(6)V9(2) VALUE ZERO COMP-3.            
007200     03 WS-KVPB-BEHOV-TIERS  PIC S9(6)V9(2) VALUE ZERO COMP-3.            
007300     03 WS-KVPB-VECKA-CDC    PIC S9(6)V9(2) VALUE ZERO COMP-3.            
007500     03 WS-KVPB-DAG-                                                      
007600-               CDC-ALL-XDC  PIC S9(6)V9(2) VALUE ZERO COMP-3.            
007700     03 WS-KVPB-INNEV-9V-                                                 
007800-               CDC-ALL-XDC  PIC S9(6)V9(2) VALUE ZERO COMP-3.            
007900                                                                          
008000     03 WS-KVPB-INNEV-8V-                                                 
008100-               CDC-ALL-XDC  PIC S9(6)V9(2) VALUE ZERO COMP-3.            
008200                                                                          
008300     03 WS-KVPB-INNEV-7V-                                                 
008400-               CDC-ALL-XDC  PIC S9(6)V9(2) VALUE ZERO COMP-3.            
008500                                                                          
008600     03 WS-KVPB-INNEV-6V-                                                 
008700-               CDC-ALL-XDC  PIC S9(6)V9(2) VALUE ZERO COMP-3.            
008800                                                                          
008900     03 WS-KVPB-INNEV-5V-                                                 
009000-               CDC-ALL-XDC  PIC S9(6)V9(2) VALUE ZERO COMP-3.            
009100                                                                          
009200     03 WS-KVPB-INNEV-4V-                                                 
009300-               CDC-ALL-XDC  PIC S9(6)V9(2) VALUE ZERO COMP-3.            
009400                                                                          
009500     03 WS-KVPB-INNEV-3V-                                                 
009600-               CDC-ALL-XDC  PIC S9(6)V9(2) VALUE ZERO COMP-3.            
009700                                                                          
009800     03 WS-KVPB-INNEV-2V-                                                 
009900-               CDC-ALL-XDC  PIC S9(6)V9(2) VALUE ZERO COMP-3.            
010000                                                                          
010100     03 WS-KVPB-INNEV-1V-                                                 
010200-               CDC-ALL-XDC  PIC S9(6)V9(2) VALUE ZERO COMP-3.            
010300                                                                          
010400                                                                          
010500     03 WS-KVPB-VECKA-AKT-XDC PIC S9(6)V9(2) VALUE ZERO COMP-3.           
010600     03 WS-KVPB-DAG-AKT-XDC   PIC S9(6)V9(2) VALUE ZERO COMP-3.           
010700     03 WS-KVPB-8V-AKT-XDC   PIC S9(6)V9(2) VALUE ZERO COMP-3.            
010800                                                                          
011400     03 WS-RETURGRANS        PIC  9(5)      VALUE ZERO.                   
011500     03 WS-RETURGRANS-AAVVD REDEFINES WS-RETURGRANS.                      
011600        05 WS-RETURGRANS-AAVV  PIC  9(4).                                 
011700        05 WS-RETURGRANS-D     PIC  9(1).                                 
011800                                                                          
011900     03 WS-RETURGRANS-AAMMDD PIC  9(6)      VALUE ZERO.                   
012000                                                                          
012100                                                                          
012200     03 WS-EN-VECKOR           PIC  9(5)      VALUE ZERO.                 
012300     03 WS-EN-VECKOR-AAVVD REDEFINES WS-EN-VECKOR.                        
012400        05 WS-EN-VECKOR-AAVV PIC    9(4).                                 
012500        05 WS-EN-VECKOR-D      PIC  9(1).                                 
012600                                                                          
012700     03 WS-EN-VECKOR-AAMMDD PIC 9(6)        VALUE ZERO.                   
012800                                                                          
012900     03 WS-TVA-VECKOR          PIC  9(5)      VALUE ZERO.                 
013000     03 WS-TVA-VECKOR-AAVVD REDEFINES WS-TVA-VECKOR.                      
013100        05 WS-TVA-VECKOR-AAVV PIC   9(4).                                 
013200        05 WS-TVA-VECKOR-D     PIC  9(1).                                 
013300                                                                          
013400     03 WS-TVA-VECKOR-AAMMDD PIC 9(6)       VALUE ZERO.                   
013500                                                                          
013600     03 WS-TRE-VECKOR          PIC  9(5)      VALUE ZERO.                 
013700     03 WS-TRE-VECKOR-AAVVD REDEFINES WS-TRE-VECKOR.                      
013800        05 WS-TRE-VECKOR-AAVV PIC   9(4).                                 
013900        05 WS-TRE-VECKOR-D     PIC  9(1).                                 
014000                                                                          
014100     03 WS-TRE-VECKOR-AAMMDD PIC 9(6)       VALUE ZERO.                   
014200                                                                          
014300     03 WS-FYRA-VECKOR         PIC  9(5)      VALUE ZERO.                 
014400     03 WS-FYRA-VECKOR-AAVVD REDEFINES WS-FYRA-VECKOR.                    
014500        05 WS-FYRA-VECKOR-AAVV PIC  9(4).                                 
014600        05 WS-FYRA-VECKOR-D    PIC  9(1).                                 
014700                                                                          
014800     03 WS-FYRA-VECKOR-AAMMDD PIC 9(6)      VALUE ZERO.                   
014900                                                                          
015000     03 WS-FEM-VECKOR          PIC  9(5)      VALUE ZERO.                 
015100     03 WS-FEM-VECKOR-AAVVD REDEFINES WS-FEM-VECKOR.                      
015200        05 WS-FEM-VECKOR-AAVV PIC   9(4).                                 
015300        05 WS-FEM-VECKOR-D     PIC  9(1).                                 
015400                                                                          
015500     03 WS-FEM-VECKOR-AAMMDD PIC 9(6)       VALUE ZERO.                   
015600                                                                          
015700     03 WS-SEX-VECKOR          PIC  9(5)      VALUE ZERO.                 
015800     03 WS-SEX-VECKOR-AAVVD REDEFINES WS-SEX-VECKOR.                      
015900        05 WS-SEX-VECKOR-AAVV PIC   9(4).                                 
016000        05 WS-SEX-VECKOR-D     PIC  9(1).                                 
016100                                                                          
016200     03 WS-SEX-VECKOR-AAMMDD PIC 9(6)       VALUE ZERO.                   
016300                                                                          
016400     03 WS-SJU-VECKOR          PIC  9(5)      VALUE ZERO.                 
016500     03 WS-SJU-VECKOR-AAVVD REDEFINES WS-SJU-VECKOR.                      
016600        05 WS-SJU-VECKOR-AAVV PIC   9(4).                                 
016700        05 WS-SJU-VECKOR-D     PIC  9(1).                                 
016800                                                                          
016900     03 WS-SJU-VECKOR-AAMMDD PIC 9(6)       VALUE ZERO.                   
017000                                                                          
017100     03 WS-ATTA-VECKOR         PIC  9(5)      VALUE ZERO.                 
017200     03 WS-ATTA-VECKOR-AAVVD REDEFINES WS-ATTA-VECKOR.                    
017300        05 WS-ATTA-VECKOR-AAVV PIC  9(4).                                 
017400        05 WS-ATTA-VECKOR-D    PIC  9(1).                                 
017500                                                                          
017600     03 WS-ATTA-VECKOR-AAMMDD PIC 9(6)      VALUE ZERO.                   
017700                                                                          
017800     03 WS-NIO-VECKOR          PIC  9(5)      VALUE ZERO.                 
017900     03 WS-NIO-VECKOR-AAVVD REDEFINES WS-NIO-VECKOR.                      
018000        05 WS-NIO-VECKOR-AAVV PIC   9(4).                                 
018100        05 WS-NIO-VECKOR-D     PIC  9(1).                                 
018200                                                                          
018300     03 WS-NIO-VECKOR-AAMMDD PIC 9(6)       VALUE ZERO.                   
018400                                                                          
018500     03 WS-TILLG-KVAR-CDC    PIC S9(5)V9(2) VALUE ZERO COMP-3.            
018600                                                                          
018700     03 WS-KVANT-Q1          PIC  9(5)V9(2) VALUE ZERO.                   
018800     03 WS-KVANT-Q1-DELAR    REDEFINES WS-KVANT-Q1.                       
018900        05 WS-KVANT-Q1-HELTAL PIC 9(5).                                   
019000        05 WS-KVANT-Q1-DECTAL PIC 9(2).                                   
019100                                                                          
019200     03 IX                      PIC S9(9) VALUE ZERO COMP-3.              
019300                                                                          
019400     EJECT                                                                
019500                                                                          
019600 01  FILLER                    PIC X(24)  VALUE 'SWITCHAR'.               
019700                                                                          
019800*      --- VALID IDDC CODES                                               
019900*                                                                         
020000*01    -COPY WWDC99                                                       
020100       EJECT                                                              
020200 77  VECKOSLUT-SW                PIC X       VALUE 'N'.                   
020300     88  VECKOSLUT                           VALUE 'J'.                   
020400                                                                          
020500     EJECT                                                                
020600                                                                          
020700 01  FILLER                    PIC X(24)  VALUE 'KONSTANTER'.             
020800                                                                          
020900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
021000 01  FILLER REDEFINES DAGENS-DATUM.                                       
021100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
021200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
021300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
021400                                                                          
021500 01  DAGENS-VECKA                PIC 9(5)    VALUE ZERO.                  
021600 01  FILLER REDEFINES DAGENS-VECKA.                                       
021700     03  DAGENS-VECKA-AAVV       PIC 9(4).                                
021800     03  DAGENS-VECKA-D          PIC 9(1).                                
021900                                                                          
022000 01  ERSATT-VECKA                PIC 9(5)    VALUE ZERO.                  
022100 01  FILLER REDEFINES ERSATT-VECKA.                                       
022200     03  ERSATT-VECKA-AAVV       PIC 9(4).                                
022300     03  ERSATT-VECKA-D          PIC 9(1).                                
022400                                                                          
022500     EJECT                                                                
022600 01  DYNAMISKA-SUBPROGRAM.                                                
022700*                                                                         
022800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
022900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
023000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
023100     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
023200     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
023400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
023410     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
023500*                                                                         
023600     EJECT                                                                
023700*    --- PARAMETRAR TILL ABEND                                            
023800                                                                          
023900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
024000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
024100     SKIP2                                                                
024200*    --- PARAMETRAR TILL DATKONV                                          
024300*                                                                         
024400*01  -COPY WDATAREA                                                       
024500     EJECT                                                                
024600*    --- PARAMETRAR TILL WORKDAY                                          
024700*                                                                         
024800*01  -COPY WORKAREA                                                       
024900     EJECT                                                                
025000*    --- PARAMETRAR TILL W009VADD                                         
025100*                                                                         
025110*    --- PARAMETERS FOR WZ20DAYS SUBPROGRAM                               
025120*01  -COPY WZ20DAYS                                                       
025200 01  W009VADD-AREA.                                                       
025300     03 VADD-DATUM-AAVV          PIC S9(5)  COMP-3.                       
025400     03 VADD-ANTAL               PIC S9(3)  COMP-3.                       
025500     EJECT                                                                
026100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
026200     SKIP3                                                                
026300 01  NYCKLAR-TILL-DLI.                                                    
026400     03  W-IDDC-B6-X.                                                     
026500         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
026510                                                                          
026520     03  W-IDDC-B616-X.                                                   
026530         05  W-IDDC-B616     PIC X(2)   VALUE SPACE.                      
026540                                                                          
026550     03  W-IDARTNR-X.                                                     
026560         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
026600     SKIP2                                                                
026700*    --- STATUS-KOD FRÅN IMS                                              
026800 01  STATUS-WS                   PIC XX.                                  
026900     88  SEGMENT-FINNS                       VALUE '  '.                  
027000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
027100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
027200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
027300     88  IMS-EJ-OK                           VALUE 'XD'.                  
027400     SKIP2                                                                
027500 01  GODK-STATUSKODER.                                                    
027600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027700     SKIP3                                                                
027800 01  SSA1                        PIC X(128).                              
027900 01  SSA2                        PIC X(128).                              
027910 01  SSA3                        PIC X(128).                              
028000     EJECT                                                                
028100*    --- IMS FUNKTIONSKODER                                               
028200*01  -COPY W0003                                                          
028300     EJECT                                                                
028400*    ---  DLI INPUT-OUTPUT AREA                                           
028500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
028600 01   DLI-IO-AREA-B601.                                                   
028700*     03  -COPY WDB601                                                    
028800                                                                          
028900     EJECT                                                                
028910 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
028920 01   DLI-IO-AREA-B616.                                                   
028930*     03  -COPY WDB616                                                    
028940                                                                          
028950     EJECT                                                                
028960 01  FILLER               PIC X(16)   VALUE 'WDK629 AREA'.                
028970 01   DLI-IO-AREA-K629.                                                   
028980*     03  -COPY WDK629                                                    
028990                                                                          
028991     EJECT                                                                
029000                                                                          
029100 LINKAGE SECTION.                                                         
029200                                                                          
029300*01  -COPY W271ERS                                                        
029400                                                                          
029500*01  -COPY W0008  -PRE ARTC-                                              
029600     05  FILLER                  PIC X.                                   
029700     EJECT                                                                
029800*01  -COPY W0008  -PRE ARTS-                                              
029900     05  FILLER                  PIC X.                                   
030000     EJECT                                                                
030100*01  -COPY W0008  -PRE ARTM-                                              
030200     05  FILLER                  PIC X.                                   
030300     EJECT                                                                
030400*01  -COPY W0008  -PRE 2501-                                              
030500     05  FILLER                  PIC X.                                   
030600     EJECT                                                                
030700*01  -COPY W0008      -PRE WDB6-                                          
030800     05  FILLER                  PIC X.                                   
030900     EJECT                                                                
030910*01  -COPY W0008      -PRE WDK6-                                          
030920     05  FILLER                  PIC X.                                   
030930     EJECT                                                                
031000 PROCEDURE DIVISION  USING W271ERS-W271ERS                                
031100                           ARTC-PCB ARTS-PCB ARTM-PCB 2501-PCB            
031200                           WDB6-PCB WDK6-PCB.                             
031300                                                                          
031400                                                                          
031500     SKIP2                                                                
031600     PERFORM A-INIT                                                       
031700     PERFORM B-NOLLSTALL                                                  
031800     EVALUATE TRUE                                                        
031900        WHEN W271ERS-CLAG-KDERS = 01 OR 02 OR 03 OR 05 OR 06 OR 07        
032000           PERFORM I-ERS-EFT-INLEV-EL-VISS-TIDP                           
032100        WHEN W271ERS-CLAG-KDERS = 04 OR 08 OR 09                          
032200           PERFORM S51-REFILL-IN-MAINPGM                                  
032300        WHEN OTHER                                                        
032400           CONTINUE                                                       
032500     END-EVALUATE                                                         
032600                                                                          
032700     MOVE ZERO TO RETURN-CODE                                             
032800     GOBACK                                                               
032900     .                                                                    
033000     EJECT                                                                
033100                                                                          
033200                                                                          
033300 A-INIT SECTION.                                                          
033400     SKIP2                                                                
033500     MOVE 'IDAG'             TO DAT-KDDATFORM                             
033600     CALL WDATKONV           USING DAT-KDDATFORM                          
033700                                   DAT-I-TIDATUM                          
033800                                   DAT-O-TIDATUM                          
033900                                   DAT-KDSVAR                             
034000     IF DAT-KDSVAR-OK                                                     
034100        MOVE DAT-TIAAVVD      TO DAGENS-VECKA                             
034200     ELSE                                                                 
034300        MOVE 'FEL FRÅN WDATKONV I A-INIT SECTION I W271ERS' TO            
034400                                    FELTEXT-STR                           
034500        DISPLAY FELTEXT                                                   
034600        PERFORM S99-ABEND                                                 
034700     END-IF                                                               
034800                                                                          
034900     IF DAGENS-VECKA-D > 4                                                
035000        MOVE JA TO VECKOSLUT-SW                                           
035100     END-IF                                                               
035200                                                                          
035300     MOVE DAGENS-VECKA-AAVV   TO VADD-DATUM-AAVV                          
035400     MOVE +1                  TO VADD-ANTAL                               
035500     CALL W009VADD            USING VADD-DATUM-AAVV                       
035600                                    VADD-ANTAL                            
035700     MOVE VADD-DATUM-AAVV     TO WS-NASTA-VECKA                           
035800                                                                          
035900                                                                          
036000     MOVE W271ERS-AKTUELLT-IDDC TO AKTUELLT-XDC-NUM                       
036100                                                                          
036200     MOVE AKTUELLT-XDC-NUM      TO AKT-XDC-IX                             
036300                                                                          
036301***OM ARTIKEL ÄR REFILLAD TILL CDC SKALL MAN I VISSA FALL                 
036302***TA MED LEDTIDEN FRÅN NDC TILL CDC                                      
036303     MOVE ZERO      TO WS-TIERSDAT-CDC-REFILL                             
036304                       WS-SPAR-TIERSDAT                                   
036310     IF W271ERS-REFILL-CDC = 'J'                                          
036311       MOVE W271ERS-IDARTNR    TO W-IDARTNR                               
036312       PERFORM IMS-GU-WDK629                                              
036313       IF SEGMENT-FINNS                                                   
036320         IF (REF-IDDC-REF = SPACE)                                        
036330         OR (REF-IDDC-REF = ALL '+')                                      
036340         OR (REF-IDDC-REF = LOW-VALUE)                                    
036400           MOVE '11'           TO W-IDDC-B6                               
036500           MOVE CREF-IDDC-REF  TO W-IDDC-B616                             
036600           PERFORM IMS-GU-WDB616                                          
036601         END-IF                                                           
036602*                                                                         
036606         IF W271ERS-ERSA-TIERSDAT-PREL-C1 = ZERO                          
036607           MOVE 49521 TO W271ERS-ERSA-TIERSDAT-PREL-C1                    
036608         END-IF                                                           
036609         MOVE W271ERS-ERSA-TIERSDAT-PREL-C1 TO DAYS-TIDATE1               
036610         MOVE 'YYWWD'                       TO DAYS-KDDATFMT1             
036611         MOVE 'YYWWD'                       TO DAYS-KDDATFMT2             
036612         IF CREF-FLFLYG = 'J'                                             
036613           MOVE REF-KVDLTID-AIRETA          TO DAYS-KVDAYS                
036614         ELSE                                                             
036615           MOVE REF-KVDLTID-TOT             TO DAYS-KVDAYS                
036616         END-IF                                                           
036617         MOVE SPACE                         TO DAYS-TIDATE2               
036618         MOVE SPACE                         TO DAYS-IDCALEND              
036619         CALL WZ20DAYS USING DAYS-WZ20DAYS                                
036620*                                                                         
036621         IF DAYS-KDRC = 8                                                 
036622*          MOVE NEJ TO BEHANDLA-SW                                        
036623           MOVE 49521 TO W271ERS-ERSA-TIERSDAT-PREL-C1                    
036624         ELSE                                                             
036625           MOVE DAYS-TIDATE2(1:5) TO WS-TIERSDAT-CDC-REFILL               
036626         END-IF                                                           
036627*      ELSE                                                               
036628*        MOVE 49521 TO W271ERS-ERSA-TIERSDAT-PREL-C1                      
036629       END-IF                                                             
036630     END-IF                                                               
036700     .                                                                    
036800     EJECT                                                                
036900                                                                          
037000                                                                          
037100 B-NOLLSTALL SECTION.                                                     
037200                                                                          
037300     MOVE ZERO          TO WS-ANTAL-AKT-XDC                               
037400                           WS-ANTAL-DAGAR                                 
037500                           WS-ANTAL-Q1                                    
037600                           WS-ANTAL-DAGAR-RETUR-TIERS                     
037700                           WS-ANTAL-DAGAR-TIERS                           
037800                           WS-TIERSDAT-PREL-C1-AAMMDD                     
037900                           WS-TIERSDAT-AAMMDD                             
038000                           WS-KVDISP-CDC                                  
038100                           WS-KVDISP-XDC                                  
038200                           WS-RETURGRANS                                  
038300                           WS-RETURGRANS-AAMMDD                           
038400                           WS-TILLG-KVAR-CDC                              
038500                                                                          
038600*****                                                                     
038700                           WS-KVPB-DAG-CDC                                
039000                           WS-KVPB-VECKA-CDC                              
039200                           WS-KVPB-INNEV-9V-CDC-ALL-XDC                   
039300                           WS-KVPB-INNEV-8V-CDC-ALL-XDC                   
039400                           WS-KVPB-INNEV-7V-CDC-ALL-XDC                   
039500                           WS-KVPB-INNEV-4V-CDC-ALL-XDC                   
039600                           WS-KVPB-INNEV-3V-CDC-ALL-XDC                   
039700                           WS-KVPB-INNEV-2V-CDC-ALL-XDC                   
039800                           WS-KVPB-INNEV-1V-CDC-ALL-XDC                   
039900                           WS-KVPB-VECKA-AKT-XDC                          
040000                           WS-KVPB-DAG-AKT-XDC                            
040100                           WS-KVPB-8V-AKT-XDC                             
040200                                                                          
040300                           WS-KVDISP-CDC                                  
040900*****                                                                     
041000                           WS-KVANT-Q1                                    
041100                           IX                                             
041200     .                                                                    
041300     EJECT                                                                
041400                                                                          
041500 I-ERS-EFT-INLEV-EL-VISS-TIDP SECTION.                                    
041600                                                                          
041700     PERFORM S31-BER-RETURGRANS                                           
041800     IF W271ERS-ERSA-TIERSDAT-PREL-C1 = ZERO                              
041900       MOVE 49521 TO W271ERS-ERSA-TIERSDAT-PREL-C1                        
042000     END-IF                                                               
042010********************                                                      
042011*****FÖR ARTIKLAR SOM ÄR REFILLADE TILL CDC SKALL FÖR VISSA               
042012*****ERSKODER HÄNSYN TAS TILL LEDTID FÖR NDC => CDC                       
042020     IF W271ERS-REFILL-CDC = JA                                           
042022       IF W271ERS-CLAG-KDERS = 01 OR 02 OR 05 OR 07                       
042023         MOVE W271ERS-ERSA-TIERSDAT-PREL-C1 TO                            
042024              WS-SPAR-TIERSDAT                                            
042025         MOVE WS-TIERSDAT-CDC-REFILL TO                                   
042026              W271ERS-ERSA-TIERSDAT-PREL-C1                               
042028       END-IF                                                             
042030     END-IF                                                               
042100********************                                                      
042200     IF W271ERS-ERSA-TIERSDAT-PREL-C1 > WS-ATTA-VECKOR                    
042300        PERFORM S51-REFILL-IN-MAINPGM                                     
042400     ELSE                                                                 
042500*      PERFORM GA-ERS-AKT-XDC                                             
042600       PERFORM IA-8V-AUTOREF-N                                            
042700       IF W271ERS-ERSA-TIERSDAT-PREL-C1 > WS-TVA-VECKOR                   
042800       AND W271ERS-ERSA-TIERSDAT-PREL-C1 < WS-NIO-VECKOR                  
042900         PERFORM IB-3V-TOM-8V                                             
043000       END-IF                                                             
043100     END-IF                                                               
043110********************                                                      
043111*****FLYTTA TILLBAKA ORDINARIER ERSDAT VID ART REFILLAD TILL CDC          
043120     IF W271ERS-REFILL-CDC = JA                                           
043130       IF W271ERS-CLAG-KDERS = 01 OR 02 OR 05 OR 07                       
043160         MOVE WS-SPAR-TIERSDAT       TO                                   
043170              W271ERS-ERSA-TIERSDAT-PREL-C1                               
043180       END-IF                                                             
043190     END-IF                                                               
043191********************                                                      
043200     IF W271ERS-ERSA-TIERSDAT-PREL-C1 < WS-TRE-VECKOR                     
043501       IF W271ERS-ART-FLIART = NEJ                                        
043502**HÄR PASSIVERAS ARTIKEL OCH FORECAST FLYTTAS                             
043510         PERFORM S55-NO-ORDER-STOP-REF-AKT-XDC                            
043520       END-IF                                                             
043600       IF W271ERS-ERSA-TIERSDAT-PREL-C1 > WS-TVA-VECKOR                   
043700         AND W271ERS-ERSA-TIERSDAT-PREL-C1 < WS-FYRA-VECKOR               
043800       OR ((W271ERS-ERSA-TIERSDAT-PREL-C1 < WS-TRE-VECKOR                 
043900         AND W271ERS-ERSA-TIERSDAT-PREL-C1 > WS-EN-VECKOR))               
044000         IF W271ERS-ART-FLIART = JA                                       
044100            PERFORM S51-REFILL-IN-MAINPGM                                 
044300         END-IF                                                           
044400       END-IF                                                             
044500     END-IF                                                               
044600     .                                                                    
044700     EJECT                                                                
044800                                                                          
044900 IA-8V-AUTOREF-N SECTION.                                                 
045000                                                                          
045100     IF W271ERS-SLAG-FLREFBEO = JA OR NEJ                                 
045200       MOVE 'S'    TO W271ERS-FLREFBEO                                    
045300     END-IF                                                               
045400     .                                                                    
045500     EJECT                                                                
045600                                                                          
045700 IB-3V-TOM-8V SECTION.                                                    
045800                                                                          
045900     PERFORM IBA-REFIL-TILL-ERS                                           
046000     .                                                                    
046100     EJECT                                                                
046200                                                                          
046300 IBA-REFIL-TILL-ERS SECTION.                                              
046400     PERFORM S36B-PB-AKTUELLT-XDC                                         
046500     PERFORM S39-PB-VECKA-CDC                                             
046600     IF W271ERS-ERSA-TIERSDAT-PREL-C1 < WS-NIO-VECKOR                     
046700       MOVE 8        TO WS-ANTAL-VECKOR-KVAR                              
046800     END-IF                                                               
046900     IF W271ERS-ERSA-TIERSDAT-PREL-C1 < WS-ATTA-VECKOR                    
047000       MOVE 7        TO WS-ANTAL-VECKOR-KVAR                              
047100     END-IF                                                               
047200     IF W271ERS-ERSA-TIERSDAT-PREL-C1 < WS-SJU-VECKOR                     
047300       MOVE 6        TO WS-ANTAL-VECKOR-KVAR                              
047400     END-IF                                                               
047500     IF W271ERS-ERSA-TIERSDAT-PREL-C1 < WS-SEX-VECKOR                     
047600       MOVE 5        TO WS-ANTAL-VECKOR-KVAR                              
047700     END-IF                                                               
047800     IF W271ERS-ERSA-TIERSDAT-PREL-C1 < WS-FEM-VECKOR                     
047900       MOVE 4        TO WS-ANTAL-VECKOR-KVAR                              
048000     END-IF                                                               
048100     IF W271ERS-ERSA-TIERSDAT-PREL-C1 < WS-FYRA-VECKOR                    
048200       MOVE 3        TO WS-ANTAL-VECKOR-KVAR                              
048300     END-IF                                                               
048400     IF W271ERS-ERSA-TIERSDAT-PREL-C1 < WS-TRE-VECKOR                     
048500       MOVE 2        TO WS-ANTAL-VECKOR-KVAR                              
048600     END-IF                                                               
048700     COMPUTE WS-MAX-PAFYLLNAD = WS-ANTAL-VECKOR-KVAR                      
048800                                * WS-KVPB-VECKA-AKT-XDC                   
048900     PERFORM S60-TILLG-XDC                                                
049000     IF WS-KVDISP-XDC > WS-MAX-PAFYLLNAD                                  
049100     OR WS-KVDISP-XDC >= W271ERS-SLAG-KVREFPKT                            
049200        PERFORM S54-NO-ORDER-AKT-XDC                                      
049300     ELSE                                                                 
050000       PERFORM IBAA-MINOR-SHORTAGE                                        
050010       PERFORM S37-Q1-ANPASSA                                             
050100       IF W271ERS-CLAG-KVQPACK-1 > +0 AND                                 
050200                         WS-KVANT-Q1-HELTAL = +0                          
050300***       PERFORM S55-NO-ORDER-STOP-REF-AKT-XDC                           
050400          PERFORM S54-NO-ORDER-AKT-XDC                                    
050500       ELSE                                                               
050600          PERFORM S38-TILLG-CDC                                           
050700          COMPUTE WS-TILLG-KVAR-CDC =                                     
050800                  WS-KVDISP-CDC - WS-ANTAL-Q1                             
050900          IF WS-TILLG-KVAR-CDC >= 2 * WS-KVPB-VECKA-CDC                   
051000             PERFORM S52-REFILL-AKT-XDC                                   
051100          ELSE                                                            
051200***          BRIST I CDC                                                  
051300                PERFORM S54-NO-ORDER-AKT-XDC                              
051400          END-IF                                                          
051500       END-IF                                                             
051600     END-IF                                                               
051700     .                                                                    
051800                                                                          
051900 IBAA-MINOR-SHORTAGE SECTION.                                             
051901                                                                          
051902     MOVE WS-KVDISP-XDC    TO WS-ANTAL-AKT-XDC                            
051903*                                                                         
051904     MOVE ZERO TO WS-REFILLPOINT                                          
051905     IF WS-MAX-PAFYLLNAD < W271ERS-SLAG-KVREFPKT                          
051906       MOVE WS-MAX-PAFYLLNAD TO WS-REFILLPOINT                            
051907     ELSE                                                                 
051908       MOVE W271ERS-SLAG-KVREFPKT TO WS-REFILLPOINT                       
051909     END-IF                                                               
051910*                                                                         
051911     IF W271ERS-SLAG-KVREFBER > +0                                        
051912        PERFORM UNTIL WS-ANTAL-AKT-XDC >= WS-REFILLPOINT                  
051913           COMPUTE WS-ANTAL-AKT-XDC =                                     
051914                   WS-ANTAL-AKT-XDC + W271ERS-SLAG-KVREFBER               
051915        END-PERFORM                                                       
051916     ELSE                                                                 
051917        COMPUTE WS-ANTAL-AKT-XDC = WS-ANTAL-AKT-XDC                       
051918                                 + WS-REFILLPOINT                         
051920     END-IF                                                               
051921*                                                                         
051922     IF WS-ANTAL-AKT-XDC > WS-MAX-PAFYLLNAD                               
051923       COMPUTE WS-ANTAL-AKT-XDC = WS-MAX-PAFYLLNAD -                      
051924                                  WS-KVDISP-XDC                           
051925     ELSE                                                                 
051926       COMPUTE WS-ANTAL-AKT-XDC = WS-ANTAL-AKT-XDC - WS-KVDISP-XDC        
051927     END-IF                                                               
051928     .                                                                    
051929                                                                          
051930 S31-BER-RETURGRANS SECTION.                                              
052000                                                                          
052100     MOVE DAGENS-VECKA-D  TO WS-RETURGRANS-D                              
052200                                                                          
052300     MOVE DAGENS-VECKA-AAVV    TO VADD-DATUM-AAVV                         
052700     IF W271ERS-CLAG-KDERS = +02 OR +03 OR +05 OR +06                     
052800        MOVE +1               TO VADD-ANTAL                               
053400     END-IF                                                               
053500                                                                          
053600     CALL W009VADD         USING VADD-DATUM-AAVV                          
053700                                  VADD-ANTAL                              
053800     MOVE VADD-DATUM-AAVV  TO WS-RETURGRANS-AAVV                          
053900                                                                          
054000     MOVE 'AAVVD'            TO DAT-KDDATFORM                             
054100     MOVE WS-RETURGRANS      TO DAT-I-TIDATUM                             
054200     CALL WDATKONV           USING DAT-KDDATFORM                          
054300                                   DAT-I-TIDATUM                          
054400                                   DAT-O-TIDATUM                          
054500                                   DAT-KDSVAR                             
054600     IF DAT-KDSVAR-OK                                                     
054700        MOVE DAT-TIAAMMDD   TO WS-RETURGRANS-AAMMDD                       
054800     ELSE                                                                 
054900        MOVE 'FEL FRÅN DATKONV I S31.1..SECTION I W271ERS'                
055000                       TO FELTEXT-STR                                     
055100        DISPLAY FELTEXT                                                   
055200        PERFORM S99-ABEND                                                 
055300     END-IF                                                               
055400                                                                          
055500*1 VECKOR FRAMMÅT                                                         
055600                                                                          
055700     MOVE DAGENS-VECKA-D  TO WS-EN-VECKOR-D                               
055800                                                                          
055900     MOVE DAGENS-VECKA-AAVV    TO VADD-DATUM-AAVV                         
056000        MOVE +1                TO VADD-ANTAL                              
056100                                                                          
056200     CALL W009VADD         USING VADD-DATUM-AAVV                          
056300                                 VADD-ANTAL                               
056400     MOVE VADD-DATUM-AAVV  TO WS-EN-VECKOR-AAVV                           
056500                                                                          
056600     MOVE 'AAVVD'            TO DAT-KDDATFORM                             
056700     MOVE WS-EN-VECKOR       TO DAT-I-TIDATUM                             
056800     CALL WDATKONV           USING DAT-KDDATFORM                          
056900                                   DAT-I-TIDATUM                          
057000                                   DAT-O-TIDATUM                          
057100                                   DAT-KDSVAR                             
057200     IF DAT-KDSVAR-OK                                                     
057300        MOVE DAT-TIAAMMDD   TO WS-EN-VECKOR-AAMMDD                        
057400     ELSE                                                                 
057500        MOVE 'FEL FRÅN DATKONV I S31.1V..SECTION I W271ERS'               
057600                       TO FELTEXT-STR                                     
057700        DISPLAY FELTEXT                                                   
057800        PERFORM S99-ABEND                                                 
057900     END-IF                                                               
058000*2 VECKOR FRAMMÅT                                                         
058100                                                                          
058200     MOVE DAGENS-VECKA-D TO WS-TVA-VECKOR-D                               
058300                                                                          
058400     MOVE DAGENS-VECKA-AAVV    TO VADD-DATUM-AAVV                         
058500        MOVE +2                TO VADD-ANTAL                              
058600                                                                          
058700     CALL W009VADD         USING VADD-DATUM-AAVV                          
058800                                 VADD-ANTAL                               
058900     MOVE VADD-DATUM-AAVV  TO WS-TVA-VECKOR-AAVV                          
059000                                                                          
059100     MOVE 'AAVVD'            TO DAT-KDDATFORM                             
059200     MOVE WS-TVA-VECKOR      TO DAT-I-TIDATUM                             
059300     CALL WDATKONV           USING DAT-KDDATFORM                          
059400                                   DAT-I-TIDATUM                          
059500                                   DAT-O-TIDATUM                          
059600                                   DAT-KDSVAR                             
059700     IF DAT-KDSVAR-OK                                                     
059800        MOVE DAT-TIAAMMDD   TO WS-TVA-VECKOR-AAMMDD                       
059900     ELSE                                                                 
060000        MOVE 'FEL FRÅN DATKONV I S31.2V..SECTION I W271ERS'               
060100                       TO FELTEXT-STR                                     
060200        DISPLAY FELTEXT                                                   
060300        PERFORM S99-ABEND                                                 
060400     END-IF                                                               
060500*3 VECKOR FRAMMÅT                                                         
060600                                                                          
060700     MOVE DAGENS-VECKA-D TO WS-TRE-VECKOR-D                               
060800                                                                          
060900     MOVE DAGENS-VECKA-AAVV    TO VADD-DATUM-AAVV                         
061000        MOVE +3                TO VADD-ANTAL                              
061100                                                                          
061200     CALL W009VADD         USING VADD-DATUM-AAVV                          
061300                                 VADD-ANTAL                               
061400     MOVE VADD-DATUM-AAVV  TO WS-TRE-VECKOR-AAVV                          
061500                                                                          
061600     MOVE 'AAVVD'            TO DAT-KDDATFORM                             
061700     MOVE WS-TRE-VECKOR      TO DAT-I-TIDATUM                             
061800     CALL WDATKONV           USING DAT-KDDATFORM                          
061900                                   DAT-I-TIDATUM                          
062000                                   DAT-O-TIDATUM                          
062100                                   DAT-KDSVAR                             
062200     IF DAT-KDSVAR-OK                                                     
062300        MOVE DAT-TIAAMMDD   TO WS-TRE-VECKOR-AAMMDD                       
062400     ELSE                                                                 
062500        MOVE 'FEL FRÅN DATKONV I S31.3V..SECTION I W271ERS'               
062600                       TO FELTEXT-STR                                     
062700        DISPLAY FELTEXT                                                   
062800        PERFORM S99-ABEND                                                 
062900     END-IF                                                               
063000*4 VECKOR FRAMMÅT                                                         
063100                                                                          
063200     MOVE DAGENS-VECKA-D TO WS-FYRA-VECKOR-D                              
063300                                                                          
063400     MOVE DAGENS-VECKA-AAVV    TO VADD-DATUM-AAVV                         
063500        MOVE +4                TO VADD-ANTAL                              
063600                                                                          
063700     CALL W009VADD         USING VADD-DATUM-AAVV                          
063800                                 VADD-ANTAL                               
063900     MOVE VADD-DATUM-AAVV  TO WS-FYRA-VECKOR-AAVV                         
064000                                                                          
064100     MOVE 'AAVVD'            TO DAT-KDDATFORM                             
064200     MOVE WS-FYRA-VECKOR     TO DAT-I-TIDATUM                             
064300     CALL WDATKONV           USING DAT-KDDATFORM                          
064400                                   DAT-I-TIDATUM                          
064500                                   DAT-O-TIDATUM                          
064600                                   DAT-KDSVAR                             
064700     IF DAT-KDSVAR-OK                                                     
064800        MOVE DAT-TIAAMMDD   TO WS-FYRA-VECKOR-AAMMDD                      
064900     ELSE                                                                 
065000        MOVE 'FEL FRÅN DATKONV I S31.4V..SECTION I W271ERS'               
065100                       TO FELTEXT-STR                                     
065200        DISPLAY FELTEXT                                                   
065300        PERFORM S99-ABEND                                                 
065400     END-IF                                                               
065500                                                                          
065600*5 VECKOR FRAMMÅT                                                         
065700                                                                          
065800     MOVE DAGENS-VECKA-D TO WS-FEM-VECKOR-D                               
065900                                                                          
066000     MOVE DAGENS-VECKA-AAVV    TO VADD-DATUM-AAVV                         
066100        MOVE +5                TO VADD-ANTAL                              
066200                                                                          
066300     CALL W009VADD         USING VADD-DATUM-AAVV                          
066400                                 VADD-ANTAL                               
066500     MOVE VADD-DATUM-AAVV  TO WS-FEM-VECKOR-AAVV                          
066600                                                                          
066700     MOVE 'AAVVD'            TO DAT-KDDATFORM                             
066800     MOVE WS-FEM-VECKOR      TO DAT-I-TIDATUM                             
066900     CALL WDATKONV           USING DAT-KDDATFORM                          
067000                                   DAT-I-TIDATUM                          
067100                                   DAT-O-TIDATUM                          
067200                                   DAT-KDSVAR                             
067300     IF DAT-KDSVAR-OK                                                     
067400        MOVE DAT-TIAAMMDD   TO WS-FEM-VECKOR-AAMMDD                       
067500     ELSE                                                                 
067600        MOVE 'FEL FRÅN DATKONV I S31.5V..SECTION I W271ERS'               
067700                       TO FELTEXT-STR                                     
067800        DISPLAY FELTEXT                                                   
067900        PERFORM S99-ABEND                                                 
068000     END-IF                                                               
068100                                                                          
068200*6 VECKOR FRAMMÅT                                                         
068300                                                                          
068400     MOVE DAGENS-VECKA-D TO WS-SEX-VECKOR-D                               
068500                                                                          
068600     MOVE DAGENS-VECKA-AAVV    TO VADD-DATUM-AAVV                         
068700        MOVE +6                TO VADD-ANTAL                              
068800                                                                          
068900     CALL W009VADD         USING VADD-DATUM-AAVV                          
069000                                 VADD-ANTAL                               
069100     MOVE VADD-DATUM-AAVV  TO WS-SEX-VECKOR-AAVV                          
069200                                                                          
069300     MOVE 'AAVVD'            TO DAT-KDDATFORM                             
069400     MOVE WS-SEX-VECKOR      TO DAT-I-TIDATUM                             
069500     CALL WDATKONV           USING DAT-KDDATFORM                          
069600                                   DAT-I-TIDATUM                          
069700                                   DAT-O-TIDATUM                          
069800                                   DAT-KDSVAR                             
069900     IF DAT-KDSVAR-OK                                                     
070000        MOVE DAT-TIAAMMDD   TO WS-SEX-VECKOR-AAMMDD                       
070100     ELSE                                                                 
070200        MOVE 'FEL FRÅN DATKONV I S31.6V..SECTION I W271ERS'               
070300                       TO FELTEXT-STR                                     
070400        DISPLAY FELTEXT                                                   
070500        PERFORM S99-ABEND                                                 
070600     END-IF                                                               
070700                                                                          
070800*7 VECKOR FRAMMÅT                                                         
070900                                                                          
071000     MOVE DAGENS-VECKA-D TO WS-SJU-VECKOR-D                               
071100                                                                          
071200     MOVE DAGENS-VECKA-AAVV    TO VADD-DATUM-AAVV                         
071300        MOVE +7                TO VADD-ANTAL                              
071400                                                                          
071500     CALL W009VADD         USING VADD-DATUM-AAVV                          
071600                                 VADD-ANTAL                               
071700     MOVE VADD-DATUM-AAVV  TO WS-SJU-VECKOR-AAVV                          
071800                                                                          
071900     MOVE 'AAVVD'            TO DAT-KDDATFORM                             
072000     MOVE WS-SJU-VECKOR      TO DAT-I-TIDATUM                             
072100     CALL WDATKONV           USING DAT-KDDATFORM                          
072200                                   DAT-I-TIDATUM                          
072300                                   DAT-O-TIDATUM                          
072400                                   DAT-KDSVAR                             
072500     IF DAT-KDSVAR-OK                                                     
072600        MOVE DAT-TIAAMMDD   TO WS-SJU-VECKOR-AAMMDD                       
072700     ELSE                                                                 
072800        MOVE 'FEL FRÅN DATKONV I S31.7V..SECTION I W271ERS'               
072900                       TO FELTEXT-STR                                     
073000        DISPLAY FELTEXT                                                   
073100        PERFORM S99-ABEND                                                 
073200     END-IF                                                               
073300                                                                          
073400*8 VECKOR FRAMMÅT                                                         
073500                                                                          
073600     MOVE DAGENS-VECKA-D TO WS-ATTA-VECKOR-D                              
073700                                                                          
073800     MOVE DAGENS-VECKA-AAVV    TO VADD-DATUM-AAVV                         
073900        MOVE +8                TO VADD-ANTAL                              
074000                                                                          
074100     CALL W009VADD         USING VADD-DATUM-AAVV                          
074200                                 VADD-ANTAL                               
074300     MOVE VADD-DATUM-AAVV  TO WS-ATTA-VECKOR-AAVV                         
074400                                                                          
074500     MOVE 'AAVVD'            TO DAT-KDDATFORM                             
074600     MOVE WS-ATTA-VECKOR     TO DAT-I-TIDATUM                             
074700     CALL WDATKONV           USING DAT-KDDATFORM                          
074800                                   DAT-I-TIDATUM                          
074900                                   DAT-O-TIDATUM                          
075000                                   DAT-KDSVAR                             
075100     IF DAT-KDSVAR-OK                                                     
075200        MOVE DAT-TIAAMMDD   TO WS-ATTA-VECKOR-AAMMDD                      
075300     ELSE                                                                 
075400        MOVE 'FEL FRÅN DATKONV I S31.8V..SECTION I W271ERS'               
075500                       TO FELTEXT-STR                                     
075600        DISPLAY FELTEXT                                                   
075700        PERFORM S99-ABEND                                                 
075800     END-IF                                                               
075900*9 VECKOR FRAMMÅT                                                         
076000                                                                          
076100     MOVE DAGENS-VECKA-D TO WS-NIO-VECKOR-D                               
076200                                                                          
076300     MOVE DAGENS-VECKA-AAVV    TO VADD-DATUM-AAVV                         
076400        MOVE +9                TO VADD-ANTAL                              
076500                                                                          
076600     CALL W009VADD         USING VADD-DATUM-AAVV                          
076700                                 VADD-ANTAL                               
076800     MOVE VADD-DATUM-AAVV  TO WS-NIO-VECKOR-AAVV                          
076900                                                                          
077000     MOVE 'AAVVD'            TO DAT-KDDATFORM                             
077100     MOVE WS-NIO-VECKOR      TO DAT-I-TIDATUM                             
077200     CALL WDATKONV           USING DAT-KDDATFORM                          
077300                                   DAT-I-TIDATUM                          
077400                                   DAT-O-TIDATUM                          
077500                                   DAT-KDSVAR                             
077600     IF DAT-KDSVAR-OK                                                     
077700        MOVE DAT-TIAAMMDD   TO WS-NIO-VECKOR-AAMMDD                       
077800     ELSE                                                                 
077900        MOVE 'FEL FRÅN DATKONV I S31.9V..SECTION I W271ERS'               
078000                       TO FELTEXT-STR                                     
078100        DISPLAY FELTEXT                                                   
078200        PERFORM S99-ABEND                                                 
078300     END-IF                                                               
078400     .                                                                    
078500     EJECT                                                                
078600                                                                          
078700                                                                          
078800 S36B-PB-AKTUELLT-XDC SECTION.                                            
078900                                                                          
079000     COMPUTE WS-KVPB-VECKA-AKT-XDC =                                      
079100             W271ERS-SLAG-KVPB-REF  / 4.33                                
079200                                                                          
079300     COMPUTE WS-KVPB-DAG-AKT-XDC =                                        
079400             WS-KVPB-VECKA-AKT-XDC / 5                                    
079500     .                                                                    
079600     EJECT                                                                
079700                                                                          
079800                                                                          
079900 S37-Q1-ANPASSA SECTION.                                                  
080000                                                                          
080100     IF W271ERS-CLAG-KVQPACK-1 > +0                                       
080200        COMPUTE WS-KVANT-Q1 =                                             
080300                WS-ANTAL-AKT-XDC / W271ERS-CLAG-KVQPACK-1                 
080400        COMPUTE WS-ANTAL-Q1 =                                             
080500                WS-KVANT-Q1-HELTAL * W271ERS-CLAG-KVQPACK-1               
080600     ELSE                                                                 
080700        MOVE WS-ANTAL-AKT-XDC TO WS-ANTAL-Q1                              
080800     END-IF                                                               
080900     .                                                                    
081000     EJECT                                                                
081100                                                                          
081200 S38-TILLG-CDC SECTION.                                                   
081300                                                                          
081400     COMPUTE WS-KVDISP-CDC = W271ERS-CLAG-KVLS        -                   
081500                             W271ERS-CLAG-KVRESS      +                   
081600                             W271ERS-CLAG-KVAKS-CDC   +                   
081700                             W271ERS-CLAG-KVAKS-PAV   +                   
081800                             W271ERS-CLAG-KVAKS-T     -                   
081900                             W271ERS-CLAG-KVROS       -                   
082000                             W271ERS-ARTM-KVOKS-BULK  -                   
082100                             W271ERS-ARTM-KVOKS-DAG   -                   
082200                             W271ERS-ARTM-KVOKS-VOR   +                   
082300                             W271ERS-SUM-KVBR                             
082400     .                                                                    
082500     EJECT                                                                
082600                                                                          
082700 S39-PB-VECKA-CDC SECTION.                                                
082800                                                                          
082900                                                                          
083000     COMPUTE WS-KVPB-VECKA-CDC =                                          
083100             ((W271ERS-CLAG-KVPB-SEP *                                    
083200              (1 - W271ERS-CLAG-REDIRLEV) / 4.33)  +                      
083300             (W271ERS-CLAG-KVPB-SATS / 4.33) +                            
083400             (W271ERS-CLAG-KVPB-TPO  / 4.33))                             
083500     .                                                                    
083600     EJECT                                                                
083700                                                                          
083800                                                                          
083900 S51-REFILL-IN-MAINPGM SECTION.                                           
084000                                                                          
084100        MOVE '1'           TO W271ERS-KDSVAR                              
084200     .                                                                    
084300     EJECT                                                                
084400                                                                          
084500                                                                          
084600 S52-REFILL-AKT-XDC SECTION.                                              
084700                                                                          
084800     MOVE WS-ANTAL-Q1      TO W271ERS-ANTAL                               
084900     MOVE 'O'              TO W271ERS-KDREFTYP                            
085000     MOVE '2'              TO W271ERS-KDSVAR                              
085100     .                                                                    
085200     EJECT                                                                
085300                                                                          
085400                                                                          
085500 S54-NO-ORDER-AKT-XDC SECTION.                                            
085600                                                                          
085700     MOVE ZERO             TO W271ERS-ANTAL                               
085800     MOVE '3'              TO W271ERS-KDSVAR                              
085900     .                                                                    
086000     EJECT                                                                
086100                                                                          
086200                                                                          
086300 S55-NO-ORDER-STOP-REF-AKT-XDC SECTION.                                   
086400**** SKAPAR W27136                                                        
086500     MOVE ZERO             TO W271ERS-ANTAL                               
086600     MOVE '3'              TO W271ERS-KDSVAR                              
086700     MOVE 'P'              TO W271ERS-KDREFSTA                            
086800     .                                                                    
086900     EJECT                                                                
087000                                                                          
087100                                                                          
087200 S60-TILLG-XDC SECTION.                                                   
087300                                                                          
087400     COMPUTE WS-KVDISP-XDC = W271ERS-SLAG-KVLS      +                     
087500                             W271ERS-SLAG-KVBEART      +                  
087600                             W271ERS-SLAG-KVAKS-SDC    +                  
087700                             W271ERS-SLAG-KVAKS-PAV    -                  
087800                             W271ERS-SLAG-KVOKS-DAG    -                  
087900                             W271ERS-SLAG-KVOKS-BULK   -                  
088000                             W271ERS-SLAG-KVROS-DAG    -                  
088100                             W271ERS-SLAG-KVROS-BULK   -                  
088200                             W271ERS-SLAG-KVSPARR-KVAL                    
088300     .                                                                    
088400     EJECT                                                                
088500                                                                          
088600                                                                          
088700                                                                          
088800 S99-ABEND SECTION.                                                       
088900                                                                          
089000     SKIP2                                                                
089100     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
089200     .                                                                    
089300     EJECT                                                                
089400                                                                          
089500                                                                          
089600                                                                          
089700* --- IMS SEKTIONER ---                                                   
089800*                                                                         
089900                                                                          
090000                                                                          
090100 IMS-GU-WDB601    SECTION.                                                
090200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
090300          DELIMITED BY SIZE INTO SSA1                                     
090400     MOVE '  ' TO GODK-STATUSKODER                                        
090500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
090600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
090700     PERFORM IMS-STATUSKONTROLL                                           
090800     .                                                                    
090900     EJECT                                                                
090910 IMS-GU-WDB616    SECTION.                                                
090920     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
090930          DELIMITED BY SIZE INTO SSA1                                     
090940     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
090950          DELIMITED BY SIZE INTO SSA2                                     
090960     MOVE '  ' TO GODK-STATUSKODER                                        
090970     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
090980     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
090990     PERFORM IMS-STATUSKONTROLL                                           
090991     .                                                                    
090992     EJECT                                                                
090993 IMS-GU-WDK629 SECTION.                                                   
090994                                                                          
090995     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
090996          DELIMITED BY SIZE INTO SSA1                                     
090997     MOVE 'WDK611  '         TO SSA2                                      
090998     MOVE 'WDK629  '         TO SSA3                                      
090999     MOVE '  GE' TO GODK-STATUSKODER                                      
091000     CALL CBLTDLI USING GU WDK6-PCB                                       
091001                        DLI-IO-AREA-K629 SSA1 SSA2 SSA3                   
091002     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
091003     PERFORM IMS-STATUSKONTROLL                                           
091004     .                                                                    
091005     EJECT                                                                
091010 IMS-STATUSKONTROLL SECTION.                                              
091100     SKIP2                                                                
091200     SET STATUS-IX TO 1                                                   
091300     SEARCH GODK-STATUS                                                   
091400       AT END                                                             
091500         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
091600         DISPLAY FELTEXT                                                  
091700         CALL FELLOG                                                      
091800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
091900         CONTINUE                                                         
092000     END-SEARCH                                                           
092100     .                                                                    
092200     EJECT                                                                
092300                                                                          
092400                                                                          
092500     EJECT                                                                
092600*    -COPY WY2000P2                                                       
