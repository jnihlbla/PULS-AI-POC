000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W413TAVG.                                                
000500 AUTHOR.         G KJELLSON  GUIDE                                        
000600 DATE-WRITTEN.   OKTOBER      2006                                        
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    BERÄKNING AV PRELIMINÄR TRANSPORTAVGÅNGSTID                          
001100*                                                                         
001200*    FÖR DISTRIKT, KUND               HÄMTAS DC                           
001300*    FÖR DISTRIKT, KUND, DC           HÄMTAS FRAKTKOD                     
001400*    FÖR DISTRIKT, KUND, DC, FRAKTKOD HÄMTAS TRANSPORTID                  
001500*                                                                         
001600*    DÄREFTER BERÄKNAS TRANSPORTAVGÅNGSTID MED HJÄLP AV W411TRAN          
001700*                                                                         
001800                                                                          
001900                                                                          
002000 ENVIRONMENT DIVISION.                                                    
002100 DATA DIVISION.                                                           
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400 01  FILLER                    PIC X(16) VALUE 'KONSTANTER'.              
002500 01  IDPGM                     PIC X(08) VALUE 'W413TAVG'.                
002600                                                                          
002700 01  CURRENT-SECTION           PIC X(16) VALUE 'MAIN'.                    
002800 01  CURRENT-IMS-SECTION       PIC X(16) VALUE SPACE.                     
002900 01  JA                        PIC X     VALUE 'J'.                       
003000 01  NEJ                       PIC X     VALUE 'N'.                       
003100                                                                          
003200 77  INIT-SW                   PIC X.                                     
003300     88  INIT-OK                         VALUE 'J'.                       
003400     88  INIT-FEL                        VALUE 'N'.                       
003500                                                                          
003600 01 WS-IDDC                    PIC  X(2).                                 
003700 01 WS-KDFRAKT                 PIC  S9(3)      COMP-3.                    
003800 01 WS-IDTRP                   PIC  X(5).                                 
003900 01 WS-KDTRPKAT                PIC  X(1).                                 
004000                                                                          
004100*                                                                         
004200 01  FILLER                    PIC X(16) VALUE 'TIDER'.                   
004300                                                                          
004400 01  TIDER.                                                               
004500     03  WS-TIAAMMDD           PIC 9(6)  VALUE ZERO.                      
004600     03  WS-TIHHMMSS           PIC 9(6)  VALUE ZERO.                      
004700 01  WS-TIHHMM-SS    REDEFINES TIDER.                                     
004800     03  TID-UTAN-SEK.                                                    
004900       05  TIAAMMDD            PIC 9(6).                                  
005000       05  TIHHMM              PIC 9(4).                                  
005100     03  SEKUNDER              PIC 9(2).                                  
005200*                                                                         
005300 01  GENERELLA-SUBPROGRAM.                                                
005400     03  CBLTDLI               PIC X(8)  VALUE 'CBLTDLI '.                
005500     03  FELLOG                PIC X(8)  VALUE 'FELLOG  '.                
005600     03  W411TRAN              PIC X(8)  VALUE 'W411TRAN'.                
005700     03  W005INIT              PIC X(8)  VALUE 'W005INIT'.                
005800                                                                          
005900                                                                          
006000*    --- PARAMETRAR TILL GENERELLA SUBPROGRAM                             
006100 01 FILLER                     PIC X(8)     VALUE 'W005INIT'.             
006200*   -COPY WMSGINIT                                                        
006300                                                                          
006400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
006500*                                                                         
006600 01  FILLER                    PIC X(16) VALUE 'IMS-WS'.                  
006700                                                                          
006800*    --- STATUS-KOD FRÅN IMS                                              
006900 01  STATUS-WS                 PIC XX.                                    
007000     88  SEGMENT-FINNS                   VALUE '  '.                      
007100     88  SEGMENT-SAKNAS                  VALUE 'GE'.                      
007200                                                                          
007300 01  GODK-STATUSKODER.                                                    
007400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007500                                                                          
007600 01  SSA1                      PIC X(300).                                
007700                                                                          
007800                                                                          
007900*    --- IMS FUNKTIONSKODER                                               
008000*01  -COPY W0003                                                          
008100                                                                          
008200*                                                                         
008300 01  FILLER                    PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.        
008400 01  NYCKLAR-TILL-DLI.                                                    
008500                                                                          
008600     03 W-IDGMT-X.                                                        
008700        05 W-B201-IDDISTR      PIC S9(5) VALUE ZERO COMP-3.               
008800        05 W-B201-IDKUNDNR     PIC S9(7) VALUE ZERO COMP-3.               
008900                                                                          
009000     03 W-WDB301KY-X.                                                     
009100        05 W-B301-IDDC         PIC X(2).                                  
009200        05 W-B301-IDDISTR      PIC S9(5) COMP-3.                          
009300        05 W-B301-IDKUNDNR     PIC S9(7) COMP-3.                          
009400     03  W-WDB301KY-DEF-X.                                                
009500         05  W-IDDC-DEF          PIC  X(2)   VALUE SPACE.                 
009600         05  W-IDDISTR-DC-DEF    PIC S9(5)   VALUE ZERO COMP-3.           
009700         05  W-IDKUNDNR-DC-DEF   PIC S9(7)  VALUE +9999999 COMP-3.        
009800                                                                          
009900     03  W-WDB501KY-X.                                                    
010000        05  W-B501-IDDC        PIC  X(2) VALUE SPACE.                     
010100        05  W-B501-KDFRAKT     PIC S9(3) VALUE ZERO COMP-3.               
010200        05  W-B501-IDDISTR     PIC S9(5) VALUE ZERO COMP-3.               
010300        05  W-B501-IDKUNDNR    PIC S9(7) VALUE ZERO COMP-3.               
010400     03  W-WDB501KY-DEF-X.                                                
010500         05  W-IDDC-FK-DEF       PIC  X(2)   VALUE SPACE.                 
010600         05  W-KDFRAKT-DEF       PIC S9(3)   VALUE ZERO COMP-3.           
010700         05  W-IDDISTR-FK-DEF    PIC S9(5)   VALUE ZERO COMP-3.           
010800         05  W-IDKUNDNR-FK-DEF   PIC S9(7)  VALUE +9999999 COMP-3.        
010900*                                                                         
011000                                                                          
011100 01  FILLER                    PIC X(16) VALUE 'WDB201-AREA'.             
011200*01  -COPY WDB201                                                         
011300                                                                          
011400 01  FILLER                    PIC X(16) VALUE 'WDB3-AREA'.               
011500*01  -COPY WDB301                                                         
011600                                                                          
011700 01  FILLER                    PIC X(16) VALUE 'WDB5-AREA'.               
011800*01  -COPY WDB501                                                         
011900                                                                          
012000 01  FILLER                    PIC X(16) VALUE 'TRAN-AREA  '.             
012100*01 -COPY W411TRAN                                                        
012200                                                                          
012300 LINKAGE SECTION.                                                         
012400*                                                                         
012500*    -COPY W413TAVG                                                       
012600                                                                          
012700*01  -COPY W0008      -PRE WDB2-                                          
012800     05  FILLER                  PIC X.                                   
012900*01  -COPY W0008      -PRE WDB3-                                          
013000     05  FILLER                  PIC X.                                   
013100*01  -COPY W0008      -PRE WDB5-                                          
013200     05  FILLER                  PIC X.                                   
013300*01  -COPY W0008      -PRE WDP7-                                          
013400     05  FILLER                  PIC X.                                   
013500                                                                          
013600*01  -COPY W0008      -PRE TRAN-XXKB-                                     
013700     05  FILLER                  PIC X.                                   
013800                                                                          
013900 PROCEDURE DIVISION  USING TAVG-W413TAVG                                  
014000                           WDB2-PCB WDB3-PCB                              
014100                           WDB5-PCB WDP7-PCB                              
014200                           TRAN-XXKB-PCB.                                 
014300 MAIN SECTION.                                                            
014400                                                                          
014500     PERFORM A-INIT                                                       
014600                                                                          
014700     IF INIT-OK                                                           
014800        MOVE MSGI-TILOKDAT       TO TRAN-TIREGDAT                         
014900        MOVE MSGI-TILOKTID       TO TRAN-TIHHMM-REG                       
015000        MOVE 'IMS'               TO TRAN-IDSYSTEM                         
015100        MOVE WS-IDTRP            TO TRAN-IDTRP                            
015200        MOVE WS-IDDC             TO TRAN-IDDC                             
015300        MOVE TAVG-KDORDKL        TO TRAN-KDORDKL                          
015400        MOVE WS-KDTRPKAT         TO TRAN-KDTRPKAT                         
015500        MOVE DC-KVLEDTIM-0       TO TRAN-KVLEDTIM-0                       
015600        MOVE DC-KVLEDTIM-1       TO TRAN-KVLEDTIM-1                       
015700        MOVE DC-KVLEDTIM-2       TO TRAN-KVLEDTIM-2                       
015800        MOVE DC-KVLEDTIM-3       TO TRAN-KVLEDTIM-3                       
015900        MOVE DC-KVLEDTIM-4       TO TRAN-KVLEDTIM-4                       
016000        MOVE ZERO                TO TRAN-TIRFS                            
016100        MOVE ZERO                TO TRAN-KDTPOTYP                         
016200                                                                          
016300        CALL   W411TRAN USING TRAN-W411TRAN                               
016400                              TRAN-XXKB-PCB                               
016500        IF TRAN-KDSVAR = '0'                                              
016600          MOVE WS-IDDC           TO TAVG-IDDC                             
016700          MOVE TRAN-TIAAMMDD     TO TAVG-DATRPAVD                         
016800          MOVE TRAN-TIHHMM       TO TAVG-TIHHMM                           
016900        END-IF                                                            
017000     END-IF                                                               
017100     IF TAVG-KDFRAKT NOT NUMERIC                                          
017200        MOVE ZERO TO TAVG-KDFRAKT                                         
017300     END-IF                                                               
017400                                                                          
017500     GOBACK                                                               
017600     .                                                                    
017700                                                                          
017800                                                                          
017900 A-INIT      SECTION.                                                     
018000     MOVE 'A-INIT'         TO CURRENT-SECTION.                            
018100                                                                          
018200     MOVE JA TO INIT-SW                                                   
018300                                                                          
018400     PERFORM AA-LAES-DEFAULT-DC                                           
018500     IF INIT-OK                                                           
018700        PERFORM AB-LAES-DEFAULT-FK                                        
018800     END-IF                                                               
018900     IF INIT-OK                                                           
019000        PERFORM AC-LAES-DEFAULT-TRP                                       
019100     END-IF                                                               
019200                                                                          
019300     MOVE ALL '+'          TO MSGI-WMSGINIT                               
019400     MOVE '013'            TO MSGI-KDCALL                                 
019500     MOVE 'IMS'            TO MSGI-IDTRANS                                
019600     MOVE 'WIDDC   '       TO MSGI-IDUSER                                 
019700     MOVE WS-IDDC          TO MSGI-IDUSER(6:2)                            
019800                                                                          
019900     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
020000                                                                          
020100     MOVE WS-IDDC          TO TAVG-IDDC                                   
020200     MOVE MSGI-TILOKDAT    TO TAVG-DATRPAVD                               
020300     MOVE MSGI-TILOKTID    TO TAVG-TIHHMM                                 
020400     .                                                                    
020500                                                                          
020600                                                                          
020700 AA-LAES-DEFAULT-DC     SECTION.                                          
020800     MOVE 'AA-LAES-DEFAULT'         TO CURRENT-SECTION.                   
020900                                                                          
021000     MOVE TAVG-IDDISTR              TO W-B201-IDDISTR                     
021100     MOVE TAVG-IDKUNDNR             TO W-B201-IDKUNDNR                    
021200                                                                          
021300     PERFORM IMS-01-GU-WDB201                                             
021400                                                                          
021500     IF SEGMENT-FINNS                                                     
021600        IF TAVG-KDORDKL = 0                                               
021700           MOVE GMT-IDDC-VOR(1)     TO WS-IDDC                            
021800        ELSE                                                              
021900           IF TAVG-KDORDKL = 1                                            
022000              MOVE GMT-IDDC-DAY(1)  TO WS-IDDC                            
022100           ELSE                                                           
022200              MOVE GMT-IDDC-BULK(1) TO WS-IDDC                            
022300           END-IF                                                         
022400        END-IF                                                            
022500     ELSE                                                                 
022600        MOVE NEJ TO INIT-SW                                               
022700     END-IF                                                               
022800     .                                                                    
022900                                                                          
023000                                                                          
023100 AB-LAES-DEFAULT-FK    SECTION.                                           
023200     MOVE 'AB-LAES-DEFAULT-FK'    TO CURRENT-SECTION.                     
023300                                                                          
023400     MOVE WS-IDDC                 TO W-B301-IDDC                          
023500                                     W-IDDC-DEF                           
023600     MOVE TAVG-IDDISTR            TO W-B301-IDDISTR                       
023700                                     W-IDDISTR-DC-DEF                     
023800     MOVE TAVG-IDKUNDNR           TO W-B301-IDKUNDNR                      
023900                                                                          
024000     PERFORM IMS-02-GU-WDB301                                             
024100                                                                          
024200     IF SEGMENT-FINNS                                                     
024210        IF TAVG-KDFRAKT = ZERO                                            
024300           IF TAVG-KDORDKL = 0                                            
024400              MOVE DC-KDGENFRA-VOR TO WS-KDFRAKT                          
024500                                        TAVG-KDFRAKT                      
024600           ELSE                                                           
024700              IF TAVG-KDORDKL = 1                                         
024800                 MOVE DC-KDGENFRA-DO TO WS-KDFRAKT                        
024900                                        TAVG-KDFRAKT                      
025000              ELSE                                                        
025100                 MOVE DC-KDGENFRA-MO TO WS-KDFRAKT                        
025200                                        TAVG-KDFRAKT                      
025300              END-IF                                                      
025400           END-IF                                                         
025410        ELSE                                                              
025420           MOVE TAVG-KDFRAKT TO WS-KDFRAKT                                
025430        END-IF                                                            
025500     ELSE                                                                 
025600        MOVE NEJ TO INIT-SW                                               
025700     END-IF                                                               
025800     .                                                                    
025900                                                                          
026000                                                                          
026100 AC-LAES-DEFAULT-TRP   SECTION.                                           
026200     MOVE 'AC-LAES-DEFAULT-TRP'        TO CURRENT-SECTION.                
026300                                                                          
026400     MOVE WS-IDDC                      TO  W-B501-IDDC                    
026500                                           W-IDDC-FK-DEF                  
026600     MOVE WS-KDFRAKT                   TO  W-B501-KDFRAKT                 
026700                                           W-KDFRAKT-DEF                  
026800     MOVE TAVG-IDDISTR                 TO  W-B501-IDDISTR                 
026900                                           W-IDDISTR-FK-DEF               
027000     MOVE TAVG-IDKUNDNR                TO  W-B501-IDKUNDNR                
027100                                                                          
027200     PERFORM IMS-03-GU-WDB501                                             
027300     IF SEGMENT-FINNS                                                     
027400        MOVE FK-KDTRPKAT               TO WS-KDTRPKAT                     
027500        IF TAVG-KDORDKL = 0                                               
027600           MOVE FK-IDTRP-0             TO WS-IDTRP                        
027700        ELSE                                                              
027800           IF TAVG-KDORDKL = 1                                            
027900              MOVE FK-IDTRP-1          TO WS-IDTRP                        
028000           ELSE                                                           
028100              IF TAVG-KDORDKL = 2                                         
028200                 MOVE FK-IDTRP-2       TO WS-IDTRP                        
028300              ELSE                                                        
028400                 IF TAVG-KDORDKL = 3                                      
028500                    MOVE FK-IDTRP-3    TO WS-IDTRP                        
028600                 ELSE                                                     
028700                    IF TAVG-KDORDKL = 4                                   
028800                       MOVE FK-IDTRP-4 TO WS-IDTRP                        
028900                    ELSE                                                  
029000                       MOVE NEJ        TO INIT-SW                         
029100                    END-IF                                                
029200                 END-IF                                                   
029300              END-IF                                                      
029400           END-IF                                                         
029500        END-IF                                                            
029600     ELSE                                                                 
029700        MOVE NEJ TO INIT-SW                                               
029800     END-IF                                                               
029900     .                                                                    
030000                                                                          
030100                                                                          
030200 IMS-01-GU-WDB201            SECTION.                                     
030300     MOVE 'IMS-01'  TO CURRENT-IMS-SECTION.                               
030400                                                                          
030500     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
030600             DELIMITED BY SIZE INTO SSA1                                  
030700     MOVE '  GE'                 TO GODK-STATUSKODER                      
030800                                                                          
030900     CALL CBLTDLI USING GU WDB2-PCB GMT-WDB201  SSA1                      
031000     MOVE WDB2-STATUS-CODE       TO STATUS-WS                             
031100     PERFORM IMS-STATUSKONTROLL                                           
031200     .                                                                    
031300                                                                          
031400 IMS-02-GU-WDB301    SECTION.                                             
031500     MOVE 'IMS-02'  TO CURRENT-IMS-SECTION.                               
031600                                                                          
031700     STRING 'WDB301  (WDB301KY =' W-WDB301KY-X                            
031800                    '!WDB301KY =' W-WDB301KY-DEF-X ')'                    
031900             DELIMITED BY SIZE INTO SSA1                                  
032000     MOVE '  GE'                 TO GODK-STATUSKODER                      
032100                                                                          
032200     CALL CBLTDLI USING GU WDB3-PCB DC-WDB301 SSA1                        
032300     MOVE WDB3-STATUS-CODE       TO STATUS-WS                             
032400     PERFORM IMS-STATUSKONTROLL                                           
032500     .                                                                    
032600                                                                          
032700 IMS-03-GU-WDB501            SECTION.                                     
032800     MOVE 'IMS-03'  TO CURRENT-IMS-SECTION.                               
032900                                                                          
033000     STRING 'WDB501  (WDB501KY =' W-WDB501KY-X                            
033100                    '!WDB501KY =' W-WDB501KY-DEF-X ')'                    
033200             DELIMITED BY SIZE INTO SSA1                                  
033300     MOVE '  GE'                 TO GODK-STATUSKODER                      
033400                                                                          
033500     CALL CBLTDLI USING GU WDB5-PCB FK-WDB501  SSA1                       
033600     MOVE WDB5-STATUS-CODE       TO STATUS-WS                             
033700     PERFORM IMS-STATUSKONTROLL                                           
033800     .                                                                    
033900                                                                          
034000 IMS-STATUSKONTROLL            SECTION.                                   
034100                                                                          
034200     SET STATUS-IX             TO 1                                       
034300     SEARCH GODK-STATUS AT END CALL FELLOG                                
034400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
034500     END-SEARCH                                                           
034600     .                                                                    
