000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000300*                                                                         
000400 ID DIVISION.                                                             
000500 PROGRAM-ID.     WF200300.                                                
000600 AUTHOR.         BO HAMMARIN.                                             
000700 DATE-WRITTEN.   MAR 2002.                                                
000800 DATE-COMPILED.                                                           
000900                                                                          
001000*   PGM                                                                   
001100*   CREATES COMPLETE DOCUMENT LINES                                       
001200*                                                                         
001300*   PGM INSERTS                                                           
001400*   - ROWS IN TABLE T01DLIN                                               
001500*                                                                         
001600*   PGM READS                                                             
001700*   - ROWS IN TABLE T01PROC                                               
001800*   - ROWS IN TABLE T01SLIN                                               
001900*   - ROWS IN TABLE T01INRE                                               
002000*   - ROWS IN TABLE T01CUGR                                               
002100*   - ROWS IN TABLE T01FCUS                                               
002200*   - ROWS IN TABLE T01VAT                                                
002300*   - ROWS IN TABLE T01BURE                                               
002400                                                                          
002500 ENVIRONMENT DIVISION.                                                    
002600                                                                          
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000                                                                          
003100 DATA DIVISION.                                                           
003200                                                                          
003300 FILE SECTION.                                                            
003400                                                                          
003500 WORKING-STORAGE SECTION.                                                 
003600*    -- CHECKED BY WY2000                                                 
003700 77  IDPGM                        PIC X(8)   VALUE 'WF200300'.            
003800                                                                          
003900 77  JA                           PIC X      VALUE 'J'.                   
004000 77  NEJ                          PIC X      VALUE 'N'.                   
004100 77  SLIN-FYLLER                  PIC X(100).                             
004200                                                                          
004300                                                                          
004400                                                                          
004500 01  WS-COMMIT.                                                           
004600     03  WS-COMMIT-FREQUENCY      PIC S9(9)V9(2)  COMP-3.                 
004700     03  WS-COMMIT-COUNT          PIC S9(9)V9(2)  COMP-3.                 
004800                                                                          
004900 01  WS-DIVERSE-MULTIFETCH.                                               
005000     03 WS-MX                    PIC S9(3)  COMP-3.                       
005100     03 WS-MULTIFETCH            PIC S9(3)  COMP-3.                       
005200                                                                          
005300     03 WS-SLIN-IDLEGSEL      OCCURS 100 PIC X(4).                        
005400     03 WS-SLIN-DAEXDAT       OCCURS 100 PIC X(8).                        
005500     03 WS-SLIN-TIEXTID       OCCURS 100 PIC S9(7) COMP-3.                
005600     03 WS-SLIN-KDVALISO      OCCURS 100 PIC X(3).                        
005700     03 WS-SLIN-IDLANDX3-SEND OCCURS 100 PIC X(3).                        
005800     03 WS-SLIN-IDLEVNR       OCCURS 100 PIC X(5).                        
005900     03 WS-SLIN-IDPARTNR      OCCURS 100 PIC X(9).                        
006000     03 WS-SLIN-KDFINDOC      OCCURS 100 PIC X(4).                        
006100     03 WS-SLIN-FLSOFT        OCCURS 100 PIC X(1).                        
006200     03 WS-SLIN-FLFREE        OCCURS 100 PIC X(1).                        
006300     03 WS-SLIN-FLPRIV        OCCURS 100 PIC X(1).                        
006400     03 WS-SLIN-IDBREAK-1     OCCURS 100 PIC X(8).                        
006500     03 WS-SLIN-IDBREAK-2     OCCURS 100 PIC X(8).                        
006600     03 WS-SLIN-IDLOPNR       OCCURS 100 PIC S9(5) COMP-3.                
006700     03 WS-SLIN-IDAPPEND      OCCURS 100 PIC X(8).                        
006800     03 WS-SLIN-IDLANDX3-REC  OCCURS 100 PIC X(3).                        
006900     03 WS-SLIN-IDREF         OCCURS 100 PIC X(15).                       
007000     03 WS-SLIN-DAREFDAT      OCCURS 100 PIC X(8).                        
007100     03 WS-SLIN-BEVOLREF      OCCURS 100 PIC X(10).                       
007200     03 WS-SLIN-IDEXCUST-1    OCCURS 100 PIC X(15).                       
007300     03 WS-SLIN-IDEXCUST-2    OCCURS 100 PIC X(15).                       
007400     03 WS-SLIN-IDEXCUST-3    OCCURS 100 PIC X(15).                       
007500     03 WS-SLIN-IDBUNDLE      OCCURS 100 PIC X(15).                       
007600     03 WS-SLIN-IDOPTION-1    OCCURS 100 PIC X(15).                       
007700     03 WS-SLIN-IDOPTION-2    OCCURS 100 PIC X(15).                       
007800     03 WS-SLIN-IDOPTION-3    OCCURS 100 PIC X(15).                       
007900     03 WS-SLIN-IDOPTION-4    OCCURS 100 PIC X(15).                       
008000     03 WS-SLIN-IDOPTION-5    OCCURS 100 PIC X(15).                       
008100     03 WS-SLIN-IDARTNR-FINANCE OCCURS 100 PIC X(50).                     
008200     03 WS-SLIN-BEART         OCCURS 100 PIC X(25).                       
008300     03 WS-SLIN-IDSTATNR      OCCURS 100 PIC S9(9) COMP-3.                
008400     03 WS-SLIN-KDVAT         OCCURS 100 PIC X(2).                        
008500     03 WS-SLIN-FLSPECPR      OCCURS 100 PIC X(1).                        
008600     03 WS-SLIN-PRARTBTO      OCCURS 100 PIC S9(7)V9(2) COMP-3.           
008700     03 WS-SLIN-PRARTNTO      OCCURS 100 PIC S9(7)V9(2) COMP-3.           
008800     03 WS-SLIN-REARTRAB      OCCURS 100 PIC S9(2)V9(2) COMP-3.           
008900     03 WS-SLIN-KVBEART       OCCURS 100 PIC S9(7) COMP-3.                
009000     03 WS-SLIN-KVLEVART      OCCURS 100 PIC S9(7) COMP-3.                
009100     03 WS-SLIN-KDANMORS      OCCURS 100 PIC X(2).                        
009200     03 WS-SLIN-IDFAKREF      OCCURS 100 PIC S9(9) COMP-3.                
009300     03 WS-SLIN-DAFAKREF      OCCURS 100 PIC X(8).                        
009400     03 WS-SLIN-IDDC          OCCURS 100 PIC X(2).                        
009500     03 WS-SLIN-KDFRAKT       OCCURS 100 PIC S9(3) COMP-3.                
009600     03 WS-SLIN-BELEVVIL      OCCURS 100 PIC X(35).                       
009700     03 WS-SLIN-IDACCNT-1     OCCURS 100 PIC X(15).                       
009800     03 WS-SLIN-IDACCNT-2     OCCURS 100 PIC X(15).                       
009900     03 WS-SLIN-IDACCNT-3     OCCURS 100 PIC X(15).                       
010000     03 WS-SLIN-IDACCNT-4     OCCURS 100 PIC X(15).                       
010100     03 WS-SLIN-VKORDBTO-KOLLI OCCURS 100 PIC S9(6)V9(1) COMP-3.          
010200     03 WS-SLIN-VKARTNTO      OCCURS 100 PIC S9(4)V9(3) COMP-3.           
010300     03 WS-SLIN-KDARTURS      OCCURS 100 PIC X(2).                        
010400     03 WS-SLIN-FYLLER        OCCURS 100 PIC X(100).                      
010500     03 WS-SLIN-IDSYSTEM-SEND OCCURS 100 PIC X(4).                        
010600     03 WS-SLIN-IDSYSTEM-REC  OCCURS 100 PIC X(4).                        
010700     03 WS-SLIN-BEANST        OCCURS 100 PIC X(25).                       
010800     03 WS-SLIN-IDUSER        OCCURS 100 PIC X(8).                        
010900     03 WS-SLIN-BETEXT        OCCURS 100 PIC X(125).                      
011000     03 WS-SLIN-BETEXT-CRE    OCCURS 100 PIC X(100).                      
011100     03 WS-SLIN-IDARTNR-CNTRL OCCURS 100 PIC X(2).                        
011200     03 WS-SLIN-FLPCOO        OCCURS 100 PIC X(1).                        
011210     03 WS-SLIN-IDLEVNR-ART   OCCURS 100 PIC X(5).                        
011220     03 WS-SLIN-IDTRACK-1     OCCURS 100 PIC X(25).                       
011230     03 WS-SLIN-KVANT-TRACK-1 OCCURS 100 PIC S9(7) COMP-3.                
011240     03 WS-SLIN-IDTRACK-2     OCCURS 100 PIC X(25).                       
011250     03 WS-SLIN-KVANT-TRACK-2 OCCURS 100 PIC S9(7) COMP-3.                
011260     03 WS-SLIN-IDTRACK-3     OCCURS 100 PIC X(25).                       
011270     03 WS-SLIN-KVANT-TRACK-3 OCCURS 100 PIC S9(7) COMP-3.                
011280     03 WS-SLIN-IDTRACK-4     OCCURS 100 PIC X(25).                       
011290     03 WS-SLIN-KVANT-TRACK-4 OCCURS 100 PIC S9(7) COMP-3.                
011291     03 WS-SLIN-IDTRACK-5     OCCURS 100 PIC X(25).                       
011292     03 WS-SLIN-KVANT-TRACK-5 OCCURS 100 PIC S9(7) COMP-3.                
011293     03 WS-SLIN-KDPRMOD       OCCURS 100 PIC X(2).                        
011300                                                                          
011400     03 WS-DLIN-IDREFRAD      OCCURS 100 PIC S9(5) COMP-3.                
011500     03 WS-DLIN-SUNTO         OCCURS 100 PIC S9(11)V9(2) COMP-3.          
011600     03 WS-DLIN-SUVAT         OCCURS 100 PIC S9(11)V9(2) COMP-3.          
011700     03 WS-DLIN-SUBTO         OCCURS 100 PIC S9(11)V9(2) COMP-3.          
011710     03 WS-DLIN-KDVAT         OCCURS 100 PIC X(2).                        
011800     03 WS-VAT-REVAT          OCCURS 100 PIC S9(3)V9(2) COMP-3.           
011900     03 WS-VAT-BEVAT          OCCURS 100 PIC X(50).                       
012000                                                                          
012100     03 WS-IDLEGSEL                 PIC X(4).                             
012200     03 WS-DAEXDAT                  PIC X(8).                             
012300     03 WS-TIEXTID                  PIC S9(7) COMP-3.                     
012400     03 WS-KDVALISO                 PIC X(3).                             
012500     03 WS-IDLANDX3-SEND            PIC X(3).                             
012600     03 WS-IDLEVNR                  PIC X(5).                             
012700     03 WS-IDPARTNR                 PIC X(9).                             
012800     03 WS-KDFINDOC                 PIC X(4).                             
012900     03 WS-FLSOFT                   PIC X(1).                             
013000     03 WS-FLFREE                   PIC X(1).                             
013100     03 WS-FLPRIV                   PIC X(1).                             
013200     03 WS-IDBREAK-1                PIC X(8).                             
013300     03 WS-IDBREAK-2                PIC X(8).                             
013400     03 WS-PRARTNTO                 PIC S9(7)V9(2) COMP-3.                
013500     03 WS-KVLEVART                 PIC S9(7) COMP-3.                     
013600     03 WS-IDARTNR-FINANCE          PIC X(50).                            
013700     03 WS-KDVAT                    PIC X(2).                             
013800     03 WS-IDREFRAD                 PIC S9(5) COMP-3 VALUE ZERO.          
013900     03 WS-IDLOPNR                  PIC S9(5) COMP-3 VALUE ZERO.          
014000                                                                          
014100     03 WS-SUNTO                    PIC S9(11)V9(2) COMP-3.               
014200     03 WS-SUVAT                    PIC S9(11)V9(2) COMP-3.               
014300     03 WS-SUBTO                    PIC S9(11)V9(2) COMP-3.               
014400     03 WS-IDARTNR-CNTRL            PIC X(2).                             
014500     03 WS-FLPCOO                   PIC X(1).                             
014510     03 WS-IDLEVNR-ART              PIC X(5).                             
014511     03 WS-KDPRMOD                  PIC X(1).                             
014512                                                                          
014520 77  DDGS-SW                        PIC X.                                
014530     88  DDGS-DOMESTIC                  VALUE 'J'.                        
014540     88  DDGS-NOT-DOMESTIC              VALUE 'N'.                        
014600     EJECT                                                                
014700                                                                          
014710 77  IPT-SW                         PIC X.                                
014720     88  IPT-DOMESTIC                   VALUE 'J'.                        
014730     88  IPT-NOT-DOMESTIC               VALUE 'N'.                        
014740     EJECT                                                                
014750                                                                          
014800 01  DYNAMISKA-SUBPROGRAM.                                                
014900*                                                                         
015000     03  ABEND                    PIC X(8)   VALUE 'ABEND   '.            
015100                                                                          
015200 01  RKOD-ABEND-DB2               PIC S9(4)  VALUE +998 COMP SYNC.        
015300     EJECT                                                                
015301*                                                                         
015310*01  -COPY WWLANDX2                                                       
015400*                                                                         
015500*        WORK-AREAS FOR DB2-SECTIONS                                      
015600*                                                                         
015700 01  FILLER                       PIC X(16)  VALUE 'PROC-TAB   '.         
015800*01  -COPY T01PROC    -PRE PROC-                                          
015900                                                                          
016000 01  FILLER                       PIC X(16)  VALUE 'SLIN-TAB   '.         
016100*01  -COPY T01SLIN    -PRE SLIN-                                          
016200                                                                          
016300 01  FILLER                       PIC X(16)  VALUE 'INRE-TAB   '.         
016400*01  -COPY T01INRE    -PRE INRE-                                          
016500                                                                          
016600 01  FILLER                       PIC X(16)  VALUE 'CUGR-TAB   '.         
016700*01  -COPY T01CUGR    -PRE CUGR-                                          
016800                                                                          
016900 01  FILLER                       PIC X(16)  VALUE 'FCUS-TAB   '.         
017000*01  -COPY T01FCUS    -PRE FCUS-                                          
017100                                                                          
017200 01  FILLER                       PIC X(16)  VALUE 'VAT-TAB    '.         
017300*01  -COPY T01VAT     -PRE VAT-                                           
017400                                                                          
017500 01  FILLER                       PIC X(16)  VALUE 'BURE-TAB   '.         
017600*01  -COPY T01BURE    -PRE BURE-                                          
017700                                                                          
017800 01  FILLER                       PIC X(16)  VALUE 'DLIN-TAB   '.         
017900*01  -COPY T01DLIN    -PRE DLIN-                                          
018000     EJECT                                                                
018100                                                                          
018200 01  FILLER                       PIC X(16)  VALUE 'PROC-AREA'.           
018300       EXEC SQL INCLUDE T01PROC  END-EXEC.                                
018400                                                                          
018500 01  FILLER                       PIC X(16)  VALUE 'SLIN-AREA'.           
018600       EXEC SQL INCLUDE T01SLIN  END-EXEC.                                
018700                                                                          
018800 01  FILLER                       PIC X(16)  VALUE 'INRE-AREA'.           
018900       EXEC SQL INCLUDE T01INRE  END-EXEC.                                
019000                                                                          
019100 01  FILLER                       PIC X(16)  VALUE 'CUGR-AREA'.           
019200       EXEC SQL INCLUDE T01CUGR  END-EXEC.                                
019300                                                                          
019400 01  FILLER                       PIC X(16)  VALUE 'FCUS-AREA'.           
019500       EXEC SQL INCLUDE T01FCUS  END-EXEC.                                
019600                                                                          
019700 01  FILLER                       PIC X(16)  VALUE 'VAT-AREA '.           
019800       EXEC SQL INCLUDE T01VAT   END-EXEC.                                
019900                                                                          
020000 01  FILLER                       PIC X(16)  VALUE 'BURE-AREA'.           
020100       EXEC SQL INCLUDE T01BURE  END-EXEC.                                
020200                                                                          
020300 01  FILLER                       PIC X(16)  VALUE 'DLIN-AREA'.           
020400       EXEC SQL INCLUDE T01DLIN  END-EXEC.                                
020500     EJECT                                                                
020600                                                                          
020700 01  FILLER                       PIC X(16)  VALUE 'SQLCA-AREA'.          
020800       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
020900*                        **** STATUS-CODE FROM DB2                        
021000                                                                          
021100 01  FILLER                       PIC X(16)  VALUE 'SQLCODE-WS'.          
021200 01  DB2-WS.                                                              
021300   03  SQLCODE-WS                 PIC S9(3)  VALUE ZERO.                  
021400     88  ROW-FOUND                           VALUE +000.                  
021500     88  ROW-MISSING                         VALUE +100.                  
021600   03  GOOD-SQLCODES.                                                     
021700     05  GOOD-SQLCODE OCCURS 5                                            
021800         INDEXED BY SQLCODE-IX    PIC 999.                                
021900     EJECT                                                                
022000                                                                          
022100 PROCEDURE DIVISION.                                                      
022200 MAIN SECTION.                                                            
022300     PERFORM A-INIT                                                       
022400                                                                          
022500     PERFORM B-EXECUTE                                                    
022600                                                                          
022700     PERFORM Z-FINISH                                                     
022800     MOVE ZERO TO RETURN-CODE                                             
022900     GOBACK                                                               
023000     .                                                                    
023100     EJECT                                                                
023200                                                                          
023300 A-INIT SECTION.                                                          
023400     MOVE +0               TO WS-COMMIT-COUNT                             
023500     MOVE +10000           TO WS-COMMIT-FREQUENCY                         
023600     .                                                                    
023700     EJECT                                                                
023800                                                                          
023900 B-EXECUTE SECTION.                                                       
024000     PERFORM DB2-OPEN-CRS-SLIN                                            
024100     PERFORM DB2-FETCH-CRS-SLIN                                           
024200     IF SQLERRD(3) > 0                                                    
024300       MOVE 000     TO SQLCODE-WS                                         
024400     END-IF                                                               
024500     PERFORM UNTIL ROW-MISSING                                            
024600       MOVE SQLERRD(3) TO WS-MULTIFETCH                                   
024700       MOVE ZERO       TO WS-MX                                           
024800       PERFORM UNTIL WS-MX = WS-MULTIFETCH                                
024900         ADD +1        TO WS-MX                                           
025000         MOVE WS-SLIN-IDLEGSEL(WS-MX)      TO WS-IDLEGSEL                 
025100         MOVE WS-SLIN-DAEXDAT(WS-MX)       TO WS-DAEXDAT                  
025200         MOVE WS-SLIN-TIEXTID(WS-MX)       TO WS-TIEXTID                  
025300         MOVE WS-SLIN-KDVALISO(WS-MX)      TO WS-KDVALISO                 
025400         MOVE WS-SLIN-IDLANDX3-SEND(WS-MX) TO WS-IDLANDX3-SEND            
025500         MOVE WS-SLIN-IDLEVNR(WS-MX)       TO WS-IDLEVNR                  
025600         MOVE WS-SLIN-IDPARTNR(WS-MX)      TO WS-IDPARTNR                 
025700         MOVE WS-SLIN-KDFINDOC(WS-MX)      TO WS-KDFINDOC                 
025800         MOVE WS-SLIN-FLSOFT(WS-MX)        TO WS-FLSOFT                   
025900         MOVE WS-SLIN-FLFREE(WS-MX)        TO WS-FLFREE                   
026000         MOVE WS-SLIN-FLPRIV(WS-MX)        TO WS-FLPRIV                   
026100         MOVE WS-SLIN-IDBREAK-1(WS-MX)     TO WS-IDBREAK-1                
026200         MOVE WS-SLIN-IDBREAK-2(WS-MX)     TO WS-IDBREAK-2                
026300         MOVE WS-SLIN-PRARTNTO(WS-MX)      TO WS-PRARTNTO                 
026400         MOVE WS-SLIN-KVLEVART(WS-MX)      TO WS-KVLEVART                 
026500         MOVE WS-SLIN-IDARTNR-FINANCE(WS-MX) TO WS-IDARTNR-FINANCE        
026600         MOVE WS-SLIN-KDVAT(WS-MX)         TO WS-KDVAT                    
026700         MOVE WS-SLIN-IDLOPNR(WS-MX)       TO WS-IDLOPNR                  
026800         PERFORM BA-CALCULATE-AMOUNTS                                     
026900         MOVE WS-KDVAT               TO WS-DLIN-KDVAT(WS-MX)              
027000         MOVE WS-SUNTO               TO WS-DLIN-SUNTO(WS-MX)              
027100         MOVE WS-SUVAT               TO WS-DLIN-SUVAT(WS-MX)              
027200         MOVE WS-SUBTO               TO WS-DLIN-SUBTO(WS-MX)              
027300         MOVE VAT-REVAT              TO WS-VAT-REVAT(WS-MX)               
027400         MOVE VAT-BEVAT              TO WS-VAT-BEVAT(WS-MX)               
027500       END-PERFORM                                                        
027600       PERFORM DB2-INSERT-DLIN                                            
027700       IF WS-MULTIFETCH = 100                                             
027800         PERFORM DB2-FETCH-CRS-SLIN                                       
027900         IF SQLERRD(3) > 0                                                
028000           MOVE 000     TO SQLCODE-WS                                     
028100         END-IF                                                           
028200       ELSE                                                               
028300         MOVE 100     TO SQLCODE-WS                                       
028400       END-IF                                                             
028500     END-PERFORM                                                          
028600     .                                                                    
028700     EJECT                                                                
028800                                                                          
028900 BA-CALCULATE-AMOUNTS SECTION.                                            
029000     PERFORM DB2-SELECT-BUSINESSINFO                                      
029010     MOVE NEJ TO DDGS-SW                                                  
029020     MOVE NEJ TO IPT-SW                                                   
029100                                                                          
029200*    RULES FOR CALCULATION IF THE BUSINESSRELATION                        
029300*    INDICATES VAT-CODE DETERMINATION IN BILLIT                           
029400     IF BURE-FLVATCHK = JA                                                
029401**** SPECAL FIX TO HANDLE IPT TAX FOR GERMANY                             
029402*      IF  WS-IDBREAK-1 = 'SERVEXT'                                       
029403*      AND WS-SLIN-IDEXCUST-1(WS-MX) = '2278'                             
029404*        PERFORM DB2-SELECT-VATINFO-VERS2                                 
029405*        MOVE JA TO IPT-SW                                                
029406*      ELSE                                                               
029407**** HERE WE CHECK IF IT'S A DDGS FROM A EU COUNTRY                       
029410         MOVE WS-SLIN-IDDC(WS-MX) TO LANDX2-IDLANDX2                      
029420         IF LANDX2-EU-IDLANDX2                                            
029422           IF FCUS-FLDIRVAT = JA                                          
029424****   DDGS AND FLDIRVAT WE SHOULD HAVE RECIEVING COUNTRY VAT             
029425             PERFORM DB2-SELECT-VATINFO-VERS2                             
029426             MOVE JA TO DDGS-SW                                           
029427           ELSE                                                           
029429             PERFORM DB2-SELECT-VATINFO-VERS1                             
029430           END-IF                                                         
029431         ELSE                                                             
029500           PERFORM DB2-SELECT-VATINFO-VERS1                               
029510         END-IF                                                           
029520*      END-IF                                                             
029600       IF ROW-FOUND                                                       
029700         COMPUTE WS-SUNTO =    WS-PRARTNTO *                              
029800                               WS-KVLEVART                                
029900         END-COMPUTE                                                      
030000         IF WS-FLPRIV = NEJ                                               
030100           IF CUGR-FLVAT = JA                                             
030200           AND INRE-FLVAT = JA                                            
030300             COMPUTE WS-SUVAT = (WS-SUNTO *                               
030400                                 VAT-REVAT) /                             
030500                                 100                                      
030600             END-COMPUTE                                                  
030700           ELSE                                                           
030701**** DDGS DOMESTIC GETS THE RECIEVER COUNTRY VAT TO BE USED               
030710             IF DDGS-DOMESTIC                                             
030720*            OR IPT-DOMESTIC                                              
030730               COMPUTE WS-SUVAT = (WS-SUNTO *                             
030740                                   VAT-REVAT) /                           
030750                                   100                                    
030910             ELSE                                                         
030920               MOVE ZERO              TO WS-SUVAT                         
030921                                         VAT-REVAT                        
031000               IF WS-IDARTNR-FINANCE = SPACE                              
031100                 MOVE INRE-KDVAT-SERV TO WS-KDVAT                         
031200               ELSE                                                       
031201****   WE CAN HAVE SERVICES WITH PARTNUMBER                               
031210                 IF WS-KDVAT = INRE-KDVAT-SERV                            
031220                   CONTINUE                                               
031230                 ELSE                                                     
031300                   MOVE INRE-KDVAT    TO WS-KDVAT                         
031310                 END-IF                                                   
031320               END-IF                                                     
031400             END-IF                                                       
031500           END-IF                                                         
031600           COMPUTE WS-SUBTO = WS-SUNTO +                                  
031700                              WS-SUVAT                                    
031800           END-COMPUTE                                                    
031900         ELSE                                                             
032000           IF CUGR-FLVAT       = JA                                       
032100           AND INRE-FLVAT-PRIV = JA                                       
032200             COMPUTE WS-SUVAT = (WS-SUNTO *                               
032300                                 VAT-REVAT) /                             
032400                                 100                                      
032500             END-COMPUTE                                                  
032600           ELSE                                                           
032610**** DDGS DOMESTIC GETS THE RECIEVER COUNTRY VAT TO BE USED               
032620             IF DDGS-DOMESTIC                                             
032621*            OR IPT-DOMESTIC                                              
032622               COMPUTE WS-SUVAT = (WS-SUNTO *                             
032623                                   VAT-REVAT) /                           
032624                                   100                                    
032630             ELSE                                                         
032700               MOVE ZERO              TO WS-SUVAT                         
032800                                         VAT-REVAT                        
032900               IF WS-IDARTNR-FINANCE = SPACE                              
033000                 MOVE INRE-KDVAT-SERV TO WS-KDVAT                         
033100               ELSE                                                       
033101****   WE CAN HAVE SERVICES WITH PARTNUMBER                               
033110                 IF WS-KDVAT = INRE-KDVAT-SERV                            
033120                   CONTINUE                                               
033130                 ELSE                                                     
033200                   MOVE INRE-KDVAT    TO WS-KDVAT                         
033210                 END-IF                                                   
033220               END-IF                                                     
033300             END-IF                                                       
033400           END-IF                                                         
033500           COMPUTE WS-SUBTO = WS-SUNTO +                                  
033600                              WS-SUVAT                                    
033700           END-COMPUTE                                                    
033800         END-IF                                                           
033900                                                                          
034000*  VAT CODE IS MISSING                                                    
034100       ELSE                                                               
034200         COMPUTE WS-SUNTO = WS-PRARTNTO *                                 
034300                            WS-KVLEVART                                   
034400         END-COMPUTE                                                      
034500         MOVE ZERO                    TO VAT-REVAT                        
034600                                         WS-SUVAT                         
034700         MOVE WS-SUNTO                TO WS-SUBTO                         
034800                                                                          
034900         PERFORM DB2-SELECT-FCUS                                          
035000         PERFORM DB2-SELECT-INRE                                          
035100         IF WS-IDARTNR-FINANCE = SPACE                                    
035200           MOVE INRE-KDVAT-SERV       TO WS-KDVAT                         
035300         ELSE                                                             
035301**** WE CAN HAVE SERVICES WITH PARTNUMBER                                 
035310           IF WS-KDVAT = INRE-KDVAT-SERV                                  
035320             CONTINUE                                                     
035330           ELSE                                                           
035400             MOVE INRE-KDVAT            TO WS-KDVAT                       
035500           END-IF                                                         
035510         END-IF                                                           
035600         PERFORM DB2-SELECT-VATINFO-VERS2                                 
035601**** IN SOME CASES LIKE RUSSIA THEY WANT VAT ON THE SOFTWARE ORDER        
035602         IF VAT-REVAT > ZERO                                              
035610           COMPUTE WS-SUVAT = (WS-SUNTO *                                 
035620                               VAT-REVAT) /                               
035630                               100                                        
035640         ELSE                                                             
035650           MOVE ZERO                TO WS-SUVAT                           
035700         END-IF                                                           
035701         COMPUTE WS-SUBTO = WS-SUNTO +                                    
035702                            WS-SUVAT                                      
035710       END-IF                                                             
035800     ELSE                                                                 
035900                                                                          
036000*    RULES FOR CALCULATION IF THE BUSINESSRELATION DOESN'T                
036100*    INDICATE VAT-CODE DETERMINATION IN BILLIT                            
036200       PERFORM DB2-SELECT-VATINFO-VERS2                                   
036300       COMPUTE WS-SUNTO   =  WS-PRARTNTO *                                
036400                             WS-KVLEVART                                  
036500       END-COMPUTE                                                        
036600       IF VAT-REVAT > ZERO                                                
036700         COMPUTE WS-SUVAT = (WS-SUNTO *                                   
036800                             VAT-REVAT) /                                 
036900                             100                                          
037000         END-COMPUTE                                                      
037100       ELSE                                                               
037200         MOVE ZERO                TO WS-SUVAT                             
037300       END-IF                                                             
037400       COMPUTE WS-SUBTO = WS-SUNTO +                                      
037500                          WS-SUVAT                                        
037600       END-COMPUTE                                                        
037700     END-IF                                                               
037710**** NORTHERN IRELAND / TIS CUSTOMERS                                     
037720     IF (WS-SLIN-IDEXCUST-1(WS-MX) = '1378'                               
037730     AND WS-SLIN-IDEXCUST-2(WS-MX) = '22004')                             
037740     OR  WS-SLIN-IDEXCUST-1(WS-MX) = '11822004'                           
037750       IF WS-KDVAT = '90'                                                 
037760         MOVE '70' TO WS-KDVAT                                            
037770       END-IF                                                             
037780     END-IF                                                               
037800     .                                                                    
037900     EJECT                                                                
038000                                                                          
038100 Z-FINISH SECTION.                                                        
038200     PERFORM DB2-CLOSE-CRS-SLIN                                           
038300     .                                                                    
038400     EJECT                                                                
038500                                                                          
038600* --- DB2 SECTIONS  ---                                                   
038700*                                                                         
038800                                                                          
038900 DB2-OPEN-CRS-SLIN SECTION.                                               
039000     EXEC SQL                                                             
039100            DECLARE SLIN-CRS CURSOR WITH ROWSET POSITIONING FOR           
039200     SELECT   T01SLIN.IDLEGSEL                                            
039300             ,T01SLIN.DAEXDAT                                             
039400             ,T01SLIN.TIEXTID                                             
039500             ,T01SLIN.KDVALISO                                            
039600             ,T01SLIN.IDLANDX3_SEND                                       
039700             ,T01SLIN.IDLEVNR                                             
039800             ,T01SLIN.IDPARTNR                                            
039900             ,T01SLIN.KDFINDOC                                            
040000             ,T01SLIN.FLSOFT                                              
040100             ,T01SLIN.FLFREE                                              
040200             ,T01SLIN.FLPRIV                                              
040300             ,T01SLIN.IDBREAK_1                                           
040400             ,T01SLIN.IDBREAK_2                                           
040500             ,T01SLIN.IDLOPNR                                             
040600             ,T01SLIN.IDAPPEND                                            
040700             ,T01SLIN.IDLANDX3_REC                                        
040800             ,T01SLIN.IDREF                                               
040900             ,T01SLIN.IDREFRAD                                            
041000             ,T01SLIN.DAREFDAT                                            
041100             ,T01SLIN.BEVOLREF                                            
041200             ,T01SLIN.IDEXCUST_1                                          
041300             ,T01SLIN.IDEXCUST_2                                          
041400             ,T01SLIN.IDEXCUST_3                                          
041500             ,T01SLIN.IDBUNDLE                                            
041600             ,T01SLIN.IDOPTION_1                                          
041700             ,T01SLIN.IDOPTION_2                                          
041800             ,T01SLIN.IDOPTION_3                                          
041900             ,T01SLIN.IDOPTION_4                                          
042000             ,T01SLIN.IDOPTION_5                                          
042100             ,T01SLIN.IDARTNR_FINANCE                                     
042200             ,T01SLIN.BEART                                               
042300             ,T01SLIN.IDSTATNR                                            
042400             ,T01SLIN.KDVAT                                               
042500             ,T01SLIN.FLSPECPR                                            
042600             ,T01SLIN.PRARTBTO                                            
042700             ,T01SLIN.PRARTNTO                                            
042800             ,T01SLIN.REARTRAB                                            
042900             ,T01SLIN.KVBEART                                             
043000             ,T01SLIN.KVLEVART                                            
043100             ,T01SLIN.KDANMORS                                            
043200             ,T01SLIN.IDFAKREF                                            
043300             ,T01SLIN.DAFAKREF                                            
043400             ,T01SLIN.IDDC                                                
043500             ,T01SLIN.KDFRAKT                                             
043600             ,T01SLIN.BELEVVIL                                            
043700             ,T01SLIN.IDACCNT_1                                           
043800             ,T01SLIN.IDACCNT_2                                           
043900             ,T01SLIN.IDACCNT_3                                           
044000             ,T01SLIN.IDACCNT_4                                           
044100             ,T01SLIN.VKORDBTO_KOLLI                                      
044200             ,T01SLIN.VKARTNTO                                            
044300             ,T01SLIN.KDARTURS                                            
044400             ,T01SLIN.FILLER                                              
044500             ,T01SLIN.IDARTNR_CNTRL                                       
044600             ,T01SLIN.FLPCOO                                              
044610             ,T01SLIN.IDLEVNR_ART                                         
044620             ,T01SLIN.IDTRACK_1                                           
044630             ,T01SLIN.KVANT_TRACK_1                                       
044640             ,T01SLIN.IDTRACK_2                                           
044650             ,T01SLIN.KVANT_TRACK_2                                       
044660             ,T01SLIN.IDTRACK_3                                           
044670             ,T01SLIN.KVANT_TRACK_3                                       
044680             ,T01SLIN.IDTRACK_4                                           
044690             ,T01SLIN.KVANT_TRACK_4                                       
044691             ,T01SLIN.IDTRACK_5                                           
044692             ,T01SLIN.KVANT_TRACK_5                                       
044693             ,T01SLIN.KDPRMOD                                             
044700                                                                          
044800     FROM     T01PROC                                                     
044900             ,T01SLIN                                                     
045000                                                                          
045100     WHERE    T01PROC.IDSYSTEM      = 'WF02'                              
045200       AND    T01SLIN.IDLEGSEL      = T01PROC.IDLEGSEL                    
045300       AND    T01SLIN.DAEXDAT       = T01PROC.DAEXDAT                     
045400       AND    T01SLIN.TIEXTID       = T01PROC.TIEXTID                     
045500     END-EXEC                                                             
045600                                                                          
045700     EXEC SQL OPEN SLIN-CRS                                               
045800     END-EXEC                                                             
045900                                                                          
046000     MOVE 000            TO GOOD-SQLCODES                                 
046100     MOVE SQLCODE        TO SQLCODE-WS                                    
046200     PERFORM DB2-STATUS-CHECK                                             
046300     .                                                                    
046400     EJECT                                                                
046500                                                                          
046600 DB2-FETCH-CRS-SLIN SECTION.                                              
046700     EXEC SQL                                                             
046800       FETCH NEXT ROWSET FROM SLIN-CRS FOR 100 ROWS                       
046900       INTO :WS-SLIN-IDLEGSEL                                             
047000           ,:WS-SLIN-DAEXDAT                                              
047100           ,:WS-SLIN-TIEXTID                                              
047200           ,:WS-SLIN-KDVALISO                                             
047300           ,:WS-SLIN-IDLANDX3-SEND                                        
047400           ,:WS-SLIN-IDLEVNR                                              
047500           ,:WS-SLIN-IDPARTNR                                             
047600           ,:WS-SLIN-KDFINDOC                                             
047700           ,:WS-SLIN-FLSOFT                                               
047800           ,:WS-SLIN-FLFREE                                               
047900           ,:WS-SLIN-FLPRIV                                               
048000           ,:WS-SLIN-IDBREAK-1                                            
048100           ,:WS-SLIN-IDBREAK-2                                            
048200           ,:WS-SLIN-IDLOPNR                                              
048300           ,:WS-SLIN-IDAPPEND                                             
048400           ,:WS-SLIN-IDLANDX3-REC                                         
048500           ,:WS-SLIN-IDREF                                                
048600           ,:WS-DLIN-IDREFRAD                                             
048700           ,:WS-SLIN-DAREFDAT                                             
048800           ,:WS-SLIN-BEVOLREF                                             
048900           ,:WS-SLIN-IDEXCUST-1                                           
049000           ,:WS-SLIN-IDEXCUST-2                                           
049100           ,:WS-SLIN-IDEXCUST-3                                           
049200           ,:WS-SLIN-IDBUNDLE                                             
049300           ,:WS-SLIN-IDOPTION-1                                           
049400           ,:WS-SLIN-IDOPTION-2                                           
049500           ,:WS-SLIN-IDOPTION-3                                           
049600           ,:WS-SLIN-IDOPTION-4                                           
049700           ,:WS-SLIN-IDOPTION-5                                           
049800           ,:WS-SLIN-IDARTNR-FINANCE                                      
049900           ,:WS-SLIN-BEART                                                
050000           ,:WS-SLIN-IDSTATNR                                             
050100           ,:WS-SLIN-KDVAT                                                
050200           ,:WS-SLIN-FLSPECPR                                             
050300           ,:WS-SLIN-PRARTBTO                                             
050400           ,:WS-SLIN-PRARTNTO                                             
050500           ,:WS-SLIN-REARTRAB                                             
050600           ,:WS-SLIN-KVBEART                                              
050700           ,:WS-SLIN-KVLEVART                                             
050800           ,:WS-SLIN-KDANMORS                                             
050900           ,:WS-SLIN-IDFAKREF                                             
051000           ,:WS-SLIN-DAFAKREF                                             
051100           ,:WS-SLIN-IDDC                                                 
051200           ,:WS-SLIN-KDFRAKT                                              
051300           ,:WS-SLIN-BELEVVIL                                             
051400           ,:WS-SLIN-IDACCNT-1                                            
051500           ,:WS-SLIN-IDACCNT-2                                            
051600           ,:WS-SLIN-IDACCNT-3                                            
051700           ,:WS-SLIN-IDACCNT-4                                            
051800           ,:WS-SLIN-VKORDBTO-KOLLI                                       
051900           ,:WS-SLIN-VKARTNTO                                             
052000           ,:WS-SLIN-KDARTURS                                             
052100           ,:WS-SLIN-FYLLER                                               
052200           ,:WS-SLIN-IDARTNR-CNTRL                                        
052300           ,:WS-SLIN-FLPCOO                                               
052310           ,:WS-SLIN-IDLEVNR-ART                                          
052320           ,:WS-SLIN-IDTRACK-1                                            
052330           ,:WS-SLIN-KVANT-TRACK-1                                        
052340           ,:WS-SLIN-IDTRACK-2                                            
052350           ,:WS-SLIN-KVANT-TRACK-2                                        
052360           ,:WS-SLIN-IDTRACK-3                                            
052370           ,:WS-SLIN-KVANT-TRACK-3                                        
052380           ,:WS-SLIN-IDTRACK-4                                            
052390           ,:WS-SLIN-KVANT-TRACK-4                                        
052391           ,:WS-SLIN-IDTRACK-5                                            
052392           ,:WS-SLIN-KVANT-TRACK-5                                        
052393           ,:WS-SLIN-KDPRMOD                                              
052400     END-EXEC                                                             
052500                                                                          
052600     MOVE 000100         TO GOOD-SQLCODES                                 
052700     MOVE SQLCODE        TO SQLCODE-WS                                    
052800     PERFORM DB2-STATUS-CHECK                                             
052900     .                                                                    
053000     EJECT                                                                
053100                                                                          
053200 DB2-SELECT-BUSINESSINFO SECTION.                                         
053300     EXEC SQL                                                             
053400     SELECT   T01BURE.FLVATCHK                                            
053410             ,T01FCUS.FLDIRVAT                                            
053500                                                                          
053600     INTO    :BURE-FLVATCHK                                               
053610            ,:FCUS-FLDIRVAT                                               
053700                                                                          
053800     FROM     T01BURE                                                     
053900             ,T01FCUS                                                     
054000                                                                          
054100     WHERE    T01BURE.IDLEGSEL      = :WS-IDLEGSEL                        
054200       AND    T01BURE.KDFINDOC      = :WS-KDFINDOC                        
054300       AND    T01BURE.KDSTATUS      = 1                                   
054400       AND    T01BURE.DADELDAT      = '00000000'                          
054500       AND    T01FCUS.IDLEGSEL      = :WS-IDLEGSEL                        
054600       AND    T01FCUS.IDPARTNR      = :WS-IDPARTNR                        
054700       AND    T01FCUS.KDSTATUS      = 1                                   
054800       AND    T01FCUS.DADELDAT      = '00000000'                          
054900       AND    T01BURE.IDLEGSEL      = T01FCUS.IDLEGSEL                    
055000       AND    T01BURE.KDPARTTY      = T01FCUS.KDPARTTY                    
055100       AND    T01BURE.KDPARTGR      = T01FCUS.KDPARTGR                    
055200     END-EXEC                                                             
055300                                                                          
055400     MOVE 000            TO GOOD-SQLCODES                                 
055500     MOVE SQLCODE        TO SQLCODE-WS                                    
055600     PERFORM DB2-STATUS-CHECK                                             
055700     .                                                                    
055800     EJECT                                                                
055900                                                                          
056000 DB2-SELECT-VATINFO-VERS1 SECTION.                                        
056100     EXEC SQL                                                             
056200     SELECT   T01INRE.FLVAT,                                              
056300              T01INRE.FLVAT_PRIV,                                         
056400              T01INRE.KDVAT,                                              
056500              T01INRE.KDVAT_SERV,                                         
056600              T01VAT.REVAT,                                               
056700              T01VAT.BEVAT,                                               
056800              T01CUGR.FLVAT                                               
056900                                                                          
057000     INTO    :INRE-FLVAT,                                                 
057100             :INRE-FLVAT-PRIV,                                            
057200             :INRE-KDVAT,                                                 
057300             :INRE-KDVAT-SERV,                                            
057400             :VAT-REVAT,                                                  
057500             :VAT-BEVAT,                                                  
057600             :CUGR-FLVAT                                                  
057700                                                                          
057800     FROM     T01SLIN,                                                    
057900              T01INRE,                                                    
058000              T01FCUS,                                                    
058100              T01CUGR,                                                    
058200              T01VAT                                                      
058300                                                                          
058400     WHERE    T01SLIN.IDLEGSEL      = :WS-IDLEGSEL                        
058500     AND      T01SLIN.DAEXDAT       = :WS-DAEXDAT                         
058600     AND      T01SLIN.TIEXTID       = :WS-TIEXTID                         
058700     AND      T01SLIN.KDVALISO      = :WS-KDVALISO                        
058800     AND      T01SLIN.IDLANDX3_SEND = :WS-IDLANDX3-SEND                   
058900     AND      T01SLIN.IDLEVNR       = :WS-IDLEVNR                         
059000     AND      T01SLIN.IDPARTNR      = :WS-IDPARTNR                        
059100     AND      T01SLIN.KDFINDOC      = :WS-KDFINDOC                        
059200     AND      T01SLIN.FLSOFT        = :WS-FLSOFT                          
059300     AND      T01SLIN.FLFREE        = :WS-FLFREE                          
059400     AND      T01SLIN.FLPRIV        = :WS-FLPRIV                          
059500     AND      T01SLIN.IDBREAK_1     = :WS-IDBREAK-1                       
059600     AND      T01SLIN.IDBREAK_2     = :WS-IDBREAK-2                       
059700     AND      T01SLIN.IDLOPNR       = :WS-IDLOPNR                         
059800     AND      T01FCUS.IDLEGSEL      = T01SLIN.IDLEGSEL                    
059900     AND      T01FCUS.IDPARTNR      = T01SLIN.IDPARTNR                    
060000     AND      T01FCUS.KDSTATUS      = 1                                   
060100     AND      T01FCUS.DADELDAT      = '00000000'                          
060200     AND      T01INRE.IDLEGSEL      = T01SLIN.IDLEGSEL                    
060300     AND      T01INRE.IDLANDX3_SEND = T01SLIN.IDLANDX3_SEND               
060400     AND      T01INRE.IDLANDX3_REC  = T01FCUS.IDLANDX3                    
060500     AND      T01INRE.KDSTATUS      = 1                                   
060600     AND      T01INRE.DADELDAT      = '00000000'                          
060700     AND      T01CUGR.IDLEGSEL      = T01FCUS.IDLEGSEL                    
060800     AND      T01CUGR.KDPARTTY      = T01FCUS.KDPARTTY                    
060900     AND      T01CUGR.KDPARTGR      = T01FCUS.KDPARTGR                    
061000     AND      T01CUGR.KDSTATUS      = 1                                   
061100     AND      T01CUGR.DADELDAT      = '00000000'                          
061200     AND      T01VAT.IDLEGSEL       = T01SLIN.IDLEGSEL                    
061300     AND      T01VAT.IDLANDX2       = T01SLIN.IDLANDX3_SEND               
061400     AND      T01VAT.KDVAT          = T01SLIN.KDVAT                       
061500     AND      T01VAT.DADELDAT       = '00000000'                          
061600     END-EXEC                                                             
061700                                                                          
061800     MOVE 000100         TO GOOD-SQLCODES                                 
061900     MOVE SQLCODE        TO SQLCODE-WS                                    
062000     PERFORM DB2-STATUS-CHECK                                             
062100     .                                                                    
062200     EJECT                                                                
062300                                                                          
062400 DB2-SELECT-VATINFO-VERS2 SECTION.                                        
062500     EXEC SQL                                                             
062600     SELECT   DISTINCT                                                    
062700              T01VAT.REVAT,                                               
062800              T01VAT.BEVAT                                                
062900                                                                          
063000     INTO    :VAT-REVAT,                                                  
063100             :VAT-BEVAT                                                   
063200                                                                          
063300     FROM     T01VAT                                                      
063400                                                                          
063500     WHERE    T01VAT.IDLEGSEL      = :WS-IDLEGSEL                         
063600       AND    T01VAT.KDVAT         = :WS-KDVAT                            
063700       AND    T01VAT.DADELDAT      = '00000000'                           
063800     END-EXEC                                                             
063900                                                                          
064000     MOVE 000            TO GOOD-SQLCODES                                 
064100     MOVE SQLCODE        TO SQLCODE-WS                                    
064200     PERFORM DB2-STATUS-CHECK                                             
064300     .                                                                    
064400     EJECT                                                                
064500                                                                          
064600 DB2-SELECT-FCUS SECTION.                                                 
064700     EXEC SQL                                                             
064800     SELECT   T01FCUS.IDLANDX3                                            
064900                                                                          
065000     INTO    :FCUS-IDLANDX3                                               
065100                                                                          
065200     FROM     T01FCUS                                                     
065300                                                                          
065400     WHERE    T01FCUS.IDLEGSEL      = :WS-IDLEGSEL                        
065500       AND    T01FCUS.IDPARTNR      = :WS-IDPARTNR                        
065600       AND    T01FCUS.KDSTATUS      = 1                                   
065700       AND    T01FCUS.DADELDAT      = '00000000'                          
065800     END-EXEC                                                             
065900                                                                          
066000     MOVE 000            TO GOOD-SQLCODES                                 
066100     MOVE SQLCODE        TO SQLCODE-WS                                    
066200     PERFORM DB2-STATUS-CHECK                                             
066300     .                                                                    
066400     EJECT                                                                
066500                                                                          
066600 DB2-SELECT-INRE SECTION.                                                 
066700     EXEC SQL                                                             
066800     SELECT   T01INRE.KDVAT,                                              
066900              T01INRE.KDVAT_SERV                                          
067000                                                                          
067100     INTO    :INRE-KDVAT,                                                 
067200             :INRE-KDVAT-SERV                                             
067300                                                                          
067400     FROM     T01INRE                                                     
067500                                                                          
067600     WHERE    T01INRE.IDLEGSEL      = :WS-IDLEGSEL                        
067700       AND    T01INRE.IDLANDX3_SEND = :WS-IDLANDX3-SEND                   
067800       AND    T01INRE.IDLANDX3_REC  = :FCUS-IDLANDX3                      
067900       AND    T01INRE.KDSTATUS      = 1                                   
068000       AND    T01INRE.DADELDAT      = '00000000'                          
068100     END-EXEC                                                             
068200                                                                          
068300     MOVE 000            TO GOOD-SQLCODES                                 
068400     MOVE SQLCODE        TO SQLCODE-WS                                    
068500     PERFORM DB2-STATUS-CHECK                                             
068600     .                                                                    
068700     EJECT                                                                
068800                                                                          
068900 DB2-INSERT-DLIN SECTION.                                                 
069000     EXEC SQL INSERT INTO T01DLIN                                         
069100        (                                                                 
069200         IDLEGSEL                                                         
069300        ,DAEXDAT                                                          
069400        ,TIEXTID                                                          
069500        ,KDVALISO                                                         
069600        ,IDLANDX3_SEND                                                    
069700        ,IDLEVNR                                                          
069800        ,IDPARTNR                                                         
069900        ,KDFINDOC                                                         
070000        ,FLSOFT                                                           
070100        ,FLFREE                                                           
070200        ,FLPRIV                                                           
070300        ,IDBREAK_1                                                        
070400        ,IDBREAK_2                                                        
070500        ,IDLOPNR                                                          
070600        ,IDLANDX3_REC                                                     
070700        ,IDEXCUST_1                                                       
070800        ,IDEXCUST_2                                                       
070900        ,IDEXCUST_3                                                       
071000        ,IDBUNDLE                                                         
071100        ,IDREF                                                            
071200        ,IDREFRAD                                                         
071300        ,DAREFDAT                                                         
071400        ,BEVOLREF                                                         
071500        ,IDOPTION_1                                                       
071600        ,IDOPTION_2                                                       
071700        ,IDOPTION_3                                                       
071800        ,IDOPTION_4                                                       
071900        ,IDOPTION_5                                                       
072000        ,IDARTNR_FINANCE                                                  
072100        ,BEART                                                            
072200        ,IDSTATNR                                                         
072300        ,VKORDBTO_KOLLI                                                   
072400        ,VKARTNTO                                                         
072500        ,KDARTURS                                                         
072600        ,KVBEART                                                          
072700        ,KVLEVART                                                         
072800        ,PRARTBTO                                                         
072900        ,PRARTNTO                                                         
073000        ,REARTRAB                                                         
073100        ,KDVAT                                                            
073200        ,FLSPECPR                                                         
073300        ,KDANMORS                                                         
073400        ,IDFAKREF                                                         
073500        ,DAFAKREF                                                         
073600        ,IDDC                                                             
073700        ,KDFRAKT                                                          
073800        ,BELEVVIL                                                         
073900        ,IDACCNT_1                                                        
074000        ,IDACCNT_2                                                        
074100        ,IDACCNT_3                                                        
074200        ,IDACCNT_4                                                        
074300        ,FILLER                                                           
074400        ,SUNTO                                                            
074500        ,REVAT                                                            
074600        ,SUVAT_BILLIT                                                     
074700        ,SUBTO                                                            
074800        ,BEVAT                                                            
074900        ,IDAPPEND                                                         
075000        ,IDARTNR_CNTRL                                                    
075100        ,FLPCOO                                                           
075110        ,IDLEVNR_ART                                                      
075120        ,IDTRACK_1                                                        
075130        ,KVANT_TRACK_1                                                    
075140        ,IDTRACK_2                                                        
075150        ,KVANT_TRACK_2                                                    
075160        ,IDTRACK_3                                                        
075170        ,KVANT_TRACK_3                                                    
075180        ,IDTRACK_4                                                        
075190        ,KVANT_TRACK_4                                                    
075191        ,IDTRACK_5                                                        
075192        ,KVANT_TRACK_5                                                    
075193        ,KDPRMOD                                                          
075200        )                                                                 
075300       VALUES                                                             
075400        (                                                                 
075500         :WS-SLIN-IDLEGSEL                                                
075600        ,:WS-SLIN-DAEXDAT                                                 
075700        ,:WS-SLIN-TIEXTID                                                 
075800        ,:WS-SLIN-KDVALISO                                                
075900        ,:WS-SLIN-IDLANDX3-SEND                                           
076000        ,:WS-SLIN-IDLEVNR                                                 
076100        ,:WS-SLIN-IDPARTNR                                                
076200        ,:WS-SLIN-KDFINDOC                                                
076300        ,:WS-SLIN-FLSOFT                                                  
076400        ,:WS-SLIN-FLFREE                                                  
076500        ,:WS-SLIN-FLPRIV                                                  
076600        ,:WS-SLIN-IDBREAK-1                                               
076700        ,:WS-SLIN-IDBREAK-2                                               
076800        ,:WS-SLIN-IDLOPNR                                                 
076900        ,:WS-SLIN-IDLANDX3-REC                                            
077000        ,:WS-SLIN-IDEXCUST-1                                              
077100        ,:WS-SLIN-IDEXCUST-2                                              
077200        ,:WS-SLIN-IDEXCUST-3                                              
077300        ,:WS-SLIN-IDBUNDLE                                                
077400        ,:WS-SLIN-IDREF                                                   
077500        ,:WS-DLIN-IDREFRAD                                                
077600        ,:WS-SLIN-DAREFDAT                                                
077700        ,:WS-SLIN-BEVOLREF                                                
077800        ,:WS-SLIN-IDOPTION-1                                              
077900        ,:WS-SLIN-IDOPTION-2                                              
078000        ,:WS-SLIN-IDOPTION-3                                              
078100        ,:WS-SLIN-IDOPTION-4                                              
078200        ,:WS-SLIN-IDOPTION-5                                              
078300        ,:WS-SLIN-IDARTNR-FINANCE                                         
078400        ,:WS-SLIN-BEART                                                   
078500        ,:WS-SLIN-IDSTATNR                                                
078600        ,:WS-SLIN-VKORDBTO-KOLLI                                          
078700        ,:WS-SLIN-VKARTNTO                                                
078800        ,:WS-SLIN-KDARTURS                                                
078900        ,:WS-SLIN-KVBEART                                                 
079000        ,:WS-SLIN-KVLEVART                                                
079100        ,:WS-SLIN-PRARTBTO                                                
079200        ,:WS-SLIN-PRARTNTO                                                
079300        ,:WS-SLIN-REARTRAB                                                
079400        ,:WS-DLIN-KDVAT                                                   
079500        ,:WS-SLIN-FLSPECPR                                                
079600        ,:WS-SLIN-KDANMORS                                                
079700        ,:WS-SLIN-IDFAKREF                                                
079800        ,:WS-SLIN-DAFAKREF                                                
079900        ,:WS-SLIN-IDDC                                                    
080000        ,:WS-SLIN-KDFRAKT                                                 
080100        ,:WS-SLIN-BELEVVIL                                                
080200        ,:WS-SLIN-IDACCNT-1                                               
080300        ,:WS-SLIN-IDACCNT-2                                               
080400        ,:WS-SLIN-IDACCNT-3                                               
080500        ,:WS-SLIN-IDACCNT-4                                               
080600        ,:WS-SLIN-FYLLER                                                  
080700        ,:WS-DLIN-SUNTO                                                   
080800        ,:WS-VAT-REVAT                                                    
080900        ,:WS-DLIN-SUVAT                                                   
081000        ,:WS-DLIN-SUBTO                                                   
081100        ,:WS-VAT-BEVAT                                                    
081200        ,:WS-SLIN-IDAPPEND                                                
081300        ,:WS-SLIN-IDARTNR-CNTRL                                           
081400        ,:WS-SLIN-FLPCOO                                                  
081410        ,:WS-SLIN-IDLEVNR-ART                                             
081420        ,:WS-SLIN-IDTRACK-1                                               
081430        ,:WS-SLIN-KVANT-TRACK-1                                           
081440        ,:WS-SLIN-IDTRACK-2                                               
081450        ,:WS-SLIN-KVANT-TRACK-2                                           
081460        ,:WS-SLIN-IDTRACK-3                                               
081470        ,:WS-SLIN-KVANT-TRACK-3                                           
081480        ,:WS-SLIN-IDTRACK-4                                               
081490        ,:WS-SLIN-KVANT-TRACK-4                                           
081491        ,:WS-SLIN-IDTRACK-5                                               
081492        ,:WS-SLIN-KVANT-TRACK-5                                           
081493        ,:WS-SLIN-KDPRMOD                                                 
081500        ) FOR :WS-MULTIFETCH ROWS ATOMIC                                  
081600     END-EXEC                                                             
081700                                                                          
081800     MOVE 000            TO GOOD-SQLCODES                                 
081900     MOVE SQLCODE        TO SQLCODE-WS                                    
082000     IF SQLCODE-WS = -803                                                 
082100       NEXT SENTENCE                                                      
082200     ELSE                                                                 
082300       ADD +1            TO WS-COMMIT-COUNT                               
082400       PERFORM DB2-STATUS-CHECK                                           
082500     END-IF                                                               
082600     .                                                                    
082700     EJECT                                                                
082800                                                                          
082900 DB2-CLOSE-CRS-SLIN SECTION.                                              
083000     EXEC SQL                                                             
083100         CLOSE SLIN-CRS                                                   
083200     END-EXEC                                                             
083300     .                                                                    
083400     EJECT                                                                
083500                                                                          
083600 DB2-STATUS-CHECK SECTION.                                                
083700     SET SQLCODE-IX         TO 1                                          
083800     SEARCH GOOD-SQLCODE AT END                                           
083900           CALL ABEND USING RKOD-ABEND-DB2                                
084000        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
084100           CONTINUE                                                       
084200     END-SEARCH                                                           
084300     .                                                                    
