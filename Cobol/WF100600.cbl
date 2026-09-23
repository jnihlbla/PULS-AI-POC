000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000400*                                                                         
001150 ID DIVISION.                                                             
001200 PROGRAM-ID.     WF100600.                                                
001300 AUTHOR.         BO HAMMARIN.                                             
001400 DATE-WRITTEN.   DEC 2001.                                                
001500 DATE-COMPILED.                                                           
001700                                                                          
001800*   PGM                                                                   
001900*   -MATCHES TEMP. CUSTOMER-TABLE AGAINST PERMANENT CUSTOMER-TABLE        
001910*   -UPDATES CUSTOMER-TABLE WITH                                          
002000*    1 NEW CUSTOMERS                                                      
002402*    2 CHANGED CUSTOMERS                                                  
002500                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003011 FILE-CONTROL.                                                            
003012*          --- RADER SOM SKALL MED MAILSKICK TILL KUND                    
003013     SELECT WF1072                     ASSIGN TO WF1006D1.                
003014                                                                          
003015 DATA DIVISION.                                                           
003016                                                                          
003017 FILE SECTION.                                                            
003018 FD  WF1072                                                               
003019     RECORDING       V                                                    
003020     BLOCK CONTAINS  0.                                                   
003030                                                                          
003040 01  UT1-POST     PIC X(158).                                             
003050     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'WF100600'.            
004430                                                                          
004659*    ---KEYS FOR SELECTION OF ROWS IN TABLE T01FCUS                       
004668 01  WS-IDLEGSEL                 PIC X(4).                                
004670 01  WS-IDPARTNR                 PIC X(10).                               
004680 01  WS-KDSTATUS                 PIC S9(3)   COMP-3.                      
004681 01  WS-ZERO                     PIC X(8)    VALUE '00000000'.            
004682 01  WS-N                        PIC X(1)    VALUE 'N'.                   
004683 01  WS-M                        PIC X(1)    VALUE 'M'.                   
004686     EJECT                                                                
004687                                                                          
004688 77  SKRIV-WF1072-SW             PIC X       VALUE 'J'.                   
004689     88  SKRIV-WF1072-OK                     VALUE 'J'.                   
004690     88  SKRIV-WF1072-NOT-OK                 VALUE 'N'.                   
004700                                                                          
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100*                                                                         
006300     03  ABEND                    PIC X(8)   VALUE 'ABEND   '.            
006310                                                                          
006400 01  RKOD-ABEND-DB2               PIC S9(4)  VALUE +998 COMP SYNC.        
006501     EJECT                                                                
006550*                                                                         
006560 01  UT1-AREA-START              PIC X(24)   VALUE                        
006570                                 'UT1-AREA-START  '.                      
006580     SKIP2                                                                
006590 01  FILLER                      PIC X(16) VALUE 'UT1-AREA'.              
006600 01  UT1-AREA.                                                            
006700     03 UT1-IDPARTNR             PIC X(9)  VALUE SPACE.                   
006800     03 FILLER                   PIC X(1)  VALUE ';'.                     
006900     03 UT1-IDLANDX3             PIC X(3)  VALUE SPACE.                   
007000     03 FILLER                   PIC X(1)  VALUE ';'.                     
007100     03 UT1-BEBET-NAME1          PIC X(35) VALUE ZERO.                    
007200     03 FILLER                   PIC X(1)  VALUE ';'.                     
007300     03 UT1-ADBET-STREET         PIC X(35) VALUE ZERO.                    
007400     03 FILLER                   PIC X(1)  VALUE ';'.                     
007500     03 UT1-ADBET-CITY           PIC X(35) VALUE ZERO.                    
007600     03 FILLER                   PIC X(1)  VALUE ';'.                     
007900     03 UT1-BETEXT               PIC X(35) VALUE SPACE.                   
008000     03 FILLER                   PIC X(1)  VALUE ';'.                     
008100     EJECT                                                                
008200                                                                          
008210 01  DYNAMISKA-SUBPROGRAM.                                                
008300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008400                                                                          
008500*    --- PARAMETRAR TILL POSTSUM                                          
008600*                                                                         
008700*01  -COPY W0005   -PRE  POSTSUM-                                         
008800     EJECT                                                                
008900                                                                          
010602**** WORK-AREAS FOR DB2-SECTIONS                                          
010603*                                                                         
010604 01  FILLER                       PIC X(16)   VALUE 'DB2-WS1    '.        
010605*01  -COPY T01FCUS    -PRE CUST-                                          
010606                                                                          
010607 01  FILLER                       PIC X(16)   VALUE 'DB2-WS2    '.        
010608*01  -COPY T01FCUW    -PRE CUSW-                                          
010610     EJECT                                                                
010611                                                                          
010612 01  FILLER                       PIC X(16)   VALUE 'DB2-WS3    '.        
010613*01  -COPY T01COCO    -PRE COCO-                                          
010614     EJECT                                                                
010615                                                                          
010620 01  FILLER                       PIC X(16)   VALUE 'CUST-AREA'.          
010621       EXEC SQL INCLUDE T01FCUS  END-EXEC.                                
010622                                                                          
010623 01  FILLER                       PIC X(16)   VALUE 'CUSW-AREA'.          
010624       EXEC SQL INCLUDE T01FCUW  END-EXEC.                                
010625                                                                          
010626 01  FILLER                       PIC X(16)   VALUE 'COCO-AREA'.          
010627       EXEC SQL INCLUDE T01COCO  END-EXEC.                                
010628     EJECT                                                                
010629                                                                          
010634 01  FILLER                       PIC X(16)   VALUE 'SQLCA-AREA'.         
010635       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010636*                        **** STATUS-CODE FROM DB2                        
010637                                                                          
010638 01  FILLER                       PIC X(16)   VALUE 'SQLCODE-WS'.         
010639 01  DB2-WS.                                                              
010640   03  SQLCODE-WS                 PIC S9(3)   VALUE ZERO.                 
010641     88  ROW-FOUND                            VALUE +000.                 
010642     88  ROW-MISSING                          VALUE +100.                 
010643   03  GOOD-SQLCODES.                                                     
010644     05  GOOD-SQLCODE OCCURS 5                                            
010645         INDEXED BY SQLCODE-IX    PIC 999.                                
010646     EJECT                                                                
010650                                                                          
010717 PROCEDURE DIVISION.                                                      
010718                                                                          
010719 MAIN SECTION.                                                            
010720     ENTRY 'DLITCBL'.                                                     
010800                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011600     PERFORM B-EXECUTE                                                    
011900                                                                          
012400     PERFORM Z-FINISH                                                     
012500                                                                          
012600     MOVE ZERO TO RETURN-CODE                                             
012700     GOBACK                                                               
012800     .                                                                    
012900     EJECT                                                                
012910                                                                          
013000 A-INIT SECTION.                                                          
013010     OPEN OUTPUT WF1072                                                   
014100     .                                                                    
014200     EJECT                                                                
014210                                                                          
014985 B-EXECUTE SECTION.                                                       
014986     PERFORM DB2-OPEN-CRS-CHA-CUST                                        
015050     PERFORM DB2-FETCH-CRS-CHA-CUST                                       
015060     PERFORM UNTIL ROW-MISSING                                            
015061**** CONTROL OF EXISTING COUNTRY                                          
015062       PERFORM DB2-SELECT-T01COCO                                         
015064       IF ROW-FOUND                                                       
015066****   CONTROL OF BLOCK OF SAP UPDATES                                    
015067         PERFORM DB2-SELECT-T01FCUS-CURR                                  
015068         IF ROW-FOUND                                                     
015069           IF CUST-FLSAPBLK = 'J'                                         
015070             MOVE 'BLOCKED BY FINANCIAL DEPARTMENT' TO UT1-BETEXT         
015071             PERFORM S01-SKRIV-WF1072                                     
015072           ELSE                                                           
015073             PERFORM DB2-UPDATE-CUST-CURRENT                              
015074           END-IF                                                         
015075         END-IF                                                           
015076                                                                          
015077****   CONTROL OF BLOCK OF SAP UPDATES                                    
015078         PERFORM DB2-SELECT-T01FCUS-COMM                                  
015079         IF ROW-FOUND                                                     
015080           IF CUST-FLSAPBLK = 'J'                                         
015081             MOVE 'BLOCKED BY FINANCIAL DEPARTMENT' TO UT1-BETEXT         
015082             PERFORM S01-SKRIV-WF1072                                     
015083           ELSE                                                           
015084             PERFORM DB2-UPDATE-CUST-COMING                               
015085           END-IF                                                         
015086         END-IF                                                           
015087                                                                          
015088       ELSE                                                               
015089         MOVE 'COUNTRY CODE DOES NOT EXIST' TO UT1-BETEXT                 
015090         PERFORM S01-SKRIV-WF1072                                         
015091       END-IF                                                             
015092                                                                          
015093       PERFORM DB2-FETCH-CRS-CHA-CUST                                     
015094     END-PERFORM                                                          
015095     PERFORM DB2-CLOSE-CRS-CHA-CUST                                       
015096                                                                          
015097     PERFORM DB2-OPEN-CRS-NEW-CUST                                        
015098     PERFORM DB2-FETCH-CRS-NEW-CUST                                       
015099     PERFORM UNTIL ROW-MISSING                                            
015100**** CONTROL OF EXISTING COUNTRY                                          
015101       PERFORM DB2-SELECT-T01COCO                                         
015102       IF ROW-FOUND                                                       
015104         PERFORM DB2-INSERT-CUST                                          
015105       ELSE                                                               
015106         MOVE 'COUNTRY CODE DOES NOT EXIST' TO UT1-BETEXT                 
015107         PERFORM S01-SKRIV-WF1072                                         
015108       END-IF                                                             
015109                                                                          
015110       PERFORM DB2-FETCH-CRS-NEW-CUST                                     
015111     END-PERFORM                                                          
015112     PERFORM DB2-CLOSE-CRS-NEW-CUST                                       
015113     .                                                                    
015114     EJECT                                                                
015120                                                                          
015197 Z-FINISH SECTION.                                                        
015198     CLOSE WF1072                                                         
015202     .                                                                    
015203     EJECT                                                                
015204                                                                          
015205 S01-SKRIV-WF1072 SECTION.                                                
015206     MOVE CUSW-IDPARTNR     TO UT1-IDPARTNR                               
015207     MOVE CUSW-IDLANDX3     TO UT1-IDLANDX3                               
015208     MOVE CUSW-BEBET-NAME1  TO UT1-BEBET-NAME1                            
015209     MOVE CUSW-ADBET-STREET TO UT1-ADBET-STREET                           
015210     MOVE CUSW-ADBET-CITY   TO UT1-ADBET-CITY                             
015213     WRITE UT1-POST FROM UT1-AREA                                         
015214                                                                          
015215     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
015216     MOVE 'WF1072'   TO POSTSUM-FDNAMN                                    
015217     MOVE 'WF1006D1' TO POSTSUM-DDNAMN2                                   
015218     CALL POSTSUM USING POSTSUM-PARM                                      
015219     .                                                                    
015220     EJECT                                                                
015221                                                                          
015222* --- DB2 SECTIONS  ---                                                   
015223*                                                                         
015224 DB2-OPEN-CRS-NEW-CUST SECTION.                                           
015225     EXEC SQL DECLARE NEW-CRS CURSOR FOR                                  
015226     SELECT   IDLEGSEL,                                                   
015227              IDPARTNR,                                                   
015228              KDSTATUS,                                                   
015229              IDALPHA,                                                    
015230              BEBET_NAME1,                                                
015231              BEBET_NAME2,                                                
015232              BEBET_NAME3,                                                
015233              BEBET_NAME4,                                                
015234              ADBET_STREET,                                               
015235              ADBET_BOX,                                                  
015236              ADBET_CITY,                                                 
015237              ADBET_PCODE,                                                
015238              IDLANDX3,                                                   
015239              IDSPRAK,                                                    
015240              IDTFN,                                                      
015241              IDTFX,                                                      
015242              IDMAIL,                                                     
015243              IDLEVNR_AP,                                                 
015244              IDVAT,                                                      
015245              KDTRADP,                                                    
015246              KDBETALV,                                                   
015247              KDKREDSP,                                                   
015248              KDPARTTY,                                                   
015249              DAREGDAT,                                                   
015250              IDUSER                                                      
015260                                                                          
015300     FROM     T01FCUW                                                     
015400                                                                          
015600     WHERE    NOT EXISTS                                                  
015800                                                                          
015900             (SELECT *                                                    
016000              FROM   T01FCUS                                              
016100              WHERE  IDLEGSEL = T01FCUW.IDLEGSEL AND                      
016200                     IDPARTNR = T01FCUW.IDPARTNR AND                      
016300                     KDSTATUS = T01FCUW.KDSTATUS)                         
016540     END-EXEC                                                             
016600                                                                          
016700     MOVE 000            TO GOOD-SQLCODES                                 
016800     EXEC SQL OPEN NEW-CRS                                                
016810     END-EXEC                                                             
016900     MOVE SQLCODE        TO SQLCODE-WS                                    
017000     PERFORM DB2-STATUS-CHECK                                             
017100     .                                                                    
017200     EJECT                                                                
017210                                                                          
017370 DB2-OPEN-CRS-CHA-CUST SECTION.                                           
017381     EXEC SQL DECLARE CHA-CRS CURSOR FOR                                  
017393     SELECT           IDLEGSEL,                                           
017394                      IDPARTNR,                                           
017395                      KDSTATUS,                                           
017396                      IDALPHA,                                            
017397                      BEBET_NAME1,                                        
017398                      BEBET_NAME2,                                        
017399                      BEBET_NAME3,                                        
017400                      BEBET_NAME4,                                        
017401                      ADBET_STREET,                                       
017402                      ADBET_BOX,                                          
017403                      ADBET_CITY,                                         
017404                      ADBET_PCODE,                                        
017405                      IDLANDX3,                                           
017407                      IDSPRAK,                                            
017408                      IDTFN,                                              
017409                      IDTFX,                                              
017410                      IDMAIL,                                             
017411                      IDLEVNR_AP,                                         
017413                      IDVAT,                                              
017414                      KDTRADP,                                            
017415                      KDBETALV,                                           
017416                      KDKREDSP,                                           
017417                      KDPARTTY,                                           
017418                      DAUPPDAT,                                           
017419                      IDUSER                                              
017420                                                                          
017421     FROM     T01FCUW                                                     
017422                                                                          
017423     WHERE    EXISTS                                                      
017424                                                                          
017425             (SELECT *                                                    
017452              FROM   T01FCUS                                              
017453              WHERE  IDLEGSEL = T01FCUW.IDLEGSEL AND                      
017454                     IDPARTNR = T01FCUW.IDPARTNR AND                      
017455                     KDSTATUS = T01FCUW.KDSTATUS)                         
017456     FOR FETCH ONLY                                                       
017475     END-EXEC                                                             
017476                                                                          
017477     MOVE 000            TO GOOD-SQLCODES                                 
017478     EXEC SQL OPEN CHA-CRS                                                
017479     END-EXEC                                                             
017480     MOVE SQLCODE        TO SQLCODE-WS                                    
017481     PERFORM DB2-STATUS-CHECK                                             
017482     .                                                                    
017483     EJECT                                                                
017484                                                                          
017485 DB2-FETCH-CRS-NEW-CUST SECTION.                                          
017500     EXEC SQL FETCH NEW-CRS INTO                                          
017600            :CUSW-IDLEGSEL,                                               
017701            :CUSW-IDPARTNR,                                               
017702            :CUSW-KDSTATUS,                                               
017703            :CUSW-IDALPHA,                                                
017704            :CUSW-BEBET-NAME1,                                            
017705            :CUSW-BEBET-NAME2,                                            
017706            :CUSW-BEBET-NAME3,                                            
017707            :CUSW-BEBET-NAME4,                                            
017708            :CUSW-ADBET-STREET,                                           
017709            :CUSW-ADBET-BOX,                                              
017710            :CUSW-ADBET-CITY,                                             
017711            :CUSW-ADBET-PCODE,                                            
017712            :CUSW-IDLANDX3,                                               
017720            :CUSW-IDSPRAK,                                                
017721            :CUSW-IDTFN,                                                  
017730            :CUSW-IDTFX,                                                  
017800            :CUSW-IDMAIL,                                                 
017801            :CUSW-IDLEVNR-AP,                                             
017804            :CUSW-IDVAT,                                                  
017805            :CUSW-KDTRADP,                                                
017806            :CUSW-KDBETALV,                                               
017807            :CUSW-KDKREDSP,                                               
017808            :CUSW-KDPARTTY,                                               
017809            :CUSW-DAREGDAT,                                               
017810            :CUSW-IDUSER                                                  
017900     END-EXEC                                                             
017910                                                                          
017920     MOVE 000100         TO GOOD-SQLCODES                                 
018000     MOVE SQLCODE        TO SQLCODE-WS                                    
018200     PERFORM DB2-STATUS-CHECK                                             
018300     .                                                                    
018400     EJECT                                                                
019011                                                                          
019052 DB2-FETCH-CRS-CHA-CUST SECTION.                                          
019054     EXEC SQL FETCH CHA-CRS INTO                                          
019058            :CUSW-IDLEGSEL,                                               
019059            :CUSW-IDPARTNR,                                               
019060            :CUSW-KDSTATUS,                                               
019061            :CUSW-IDALPHA,                                                
019062            :CUSW-BEBET-NAME1,                                            
019063            :CUSW-BEBET-NAME2,                                            
019064            :CUSW-BEBET-NAME3,                                            
019065            :CUSW-BEBET-NAME4,                                            
019066            :CUSW-ADBET-STREET,                                           
019067            :CUSW-ADBET-BOX,                                              
019068            :CUSW-ADBET-CITY,                                             
019069            :CUSW-ADBET-PCODE,                                            
019070            :CUSW-IDLANDX3,                                               
019072            :CUSW-IDSPRAK,                                                
019073            :CUSW-IDTFN,                                                  
019074            :CUSW-IDTFX,                                                  
019075            :CUSW-IDMAIL,                                                 
019076            :CUSW-IDLEVNR-AP,                                             
019077            :CUSW-IDVAT,                                                  
019078            :CUSW-KDTRADP,                                                
019079            :CUSW-KDBETALV,                                               
019080            :CUSW-KDKREDSP,                                               
019081            :CUSW-KDPARTTY,                                               
019082            :CUSW-DAUPPDAT,                                               
019083            :CUSW-IDUSER                                                  
019084     END-EXEC                                                             
019085                                                                          
019086     MOVE 000100         TO GOOD-SQLCODES                                 
019087     MOVE SQLCODE        TO SQLCODE-WS                                    
019088     PERFORM DB2-STATUS-CHECK                                             
019089     .                                                                    
019090     EJECT                                                                
019091                                                                          
019092 DB2-INSERT-CUST SECTION.                                                 
019094     EXEC SQL INSERT INTO T01FCUS                                         
019095        (                                                                 
019096         IDLEGSEL,                                                        
019097         IDPARTNR,                                                        
019098         KDSTATUS,                                                        
019099         IDALPHA,                                                         
019100         BEBET_NAME1,                                                     
019101         BEBET_NAME2,                                                     
019102         BEBET_NAME3,                                                     
019103         BEBET_NAME4,                                                     
019104         ADBET_STREET,                                                    
019105         ADBET_BOX,                                                       
019106         ADBET_CITY,                                                      
019107         ADBET_PCODE,                                                     
019108         IDLANDX3,                                                        
019110         IDSPRAK,                                                         
019111         IDTFN,                                                           
019112         IDTFX,                                                           
019113         IDMAIL,                                                          
019114         IDLEVNR_AP,                                                      
019115         IDVAT,                                                           
019116         KDTRADP,                                                         
019117         KDBETALV,                                                        
019118         KDKREDSP,                                                        
019119         KDPARTTY,                                                        
019120         FLLOCCUR,                                                        
019121         DAREGDAT,                                                        
019122         DAUPPDAT,                                                        
019123         DADELDAT,                                                        
019124         IDUSER,                                                          
019125         KDVALTYP                                                         
019126        )                                                                 
019127       VALUES                                                             
019128        (                                                                 
019129         :CUSW-IDLEGSEL,                                                  
019130         :CUSW-IDPARTNR,                                                  
019131         :CUSW-KDSTATUS,                                                  
019132         :CUSW-IDALPHA,                                                   
019133         :CUSW-BEBET-NAME1,                                               
019134         :CUSW-BEBET-NAME2,                                               
019135         :CUSW-BEBET-NAME3,                                               
019136         :CUSW-BEBET-NAME4,                                               
019137         :CUSW-ADBET-STREET,                                              
019138         :CUSW-ADBET-BOX,                                                 
019139         :CUSW-ADBET-CITY,                                                
019140         :CUSW-ADBET-PCODE,                                               
019141         :CUSW-IDLANDX3,                                                  
019142         :CUSW-IDSPRAK,                                                   
019143         :CUSW-IDTFN,                                                     
019144         :CUSW-IDTFX,                                                     
019145         :CUSW-IDMAIL,                                                    
019146         :CUSW-IDLEVNR-AP,                                                
019147         :CUSW-IDVAT,                                                     
019148         :CUSW-KDTRADP,                                                   
019149         :CUSW-KDBETALV,                                                  
019150         :CUSW-KDKREDSP,                                                  
019151         :CUSW-KDPARTTY,                                                  
019152         :WS-N,                                                           
019153         :CUSW-DAREGDAT,                                                  
019154         :WS-ZERO,                                                        
019155         :WS-ZERO,                                                        
019156         :CUSW-IDUSER,                                                    
019157         :WS-M                                                            
019158        )                                                                 
019159     END-EXEC                                                             
019160                                                                          
019161     MOVE 000            TO GOOD-SQLCODES                                 
019162     MOVE SQLCODE        TO SQLCODE-WS                                    
019163     PERFORM DB2-STATUS-CHECK                                             
019164     .                                                                    
019165     EJECT                                                                
019166                                                                          
019167 DB2-UPDATE-CUST-CURRENT SECTION.                                         
019168     EXEC SQL UPDATE T01FCUS                                              
019169       SET                                                                
019170              IDALPHA      = :CUSW-IDALPHA,                               
019171              BEBET_NAME1  = :CUSW-BEBET-NAME1,                           
019172              BEBET_NAME2  = :CUSW-BEBET-NAME2,                           
019173              BEBET_NAME3  = :CUSW-BEBET-NAME3,                           
019174              BEBET_NAME4  = :CUSW-BEBET-NAME4,                           
019175              ADBET_STREET = :CUSW-ADBET-STREET,                          
019176              ADBET_BOX    = :CUSW-ADBET-BOX,                             
019177              ADBET_CITY   = :CUSW-ADBET-CITY,                            
019178              ADBET_PCODE  = :CUSW-ADBET-PCODE,                           
019179              IDLANDX3     = :CUSW-IDLANDX3,                              
019180              IDSPRAK      = :CUSW-IDSPRAK,                               
019181              IDTFN        = :CUSW-IDTFN,                                 
019182              IDTFX        = :CUSW-IDTFX,                                 
019183              IDMAIL       = :CUSW-IDMAIL,                                
019184              IDLEVNR_AP   = :CUSW-IDLEVNR-AP,                            
019185              IDVAT        = :CUSW-IDVAT,                                 
019186              KDTRADP      = :CUSW-KDTRADP,                               
019187              KDBETALV     = :CUSW-KDBETALV,                              
019188              KDKREDSP     = :CUSW-KDKREDSP,                              
019189              KDPARTTY     = :CUSW-KDPARTTY,                              
019190              DAUPPDAT     = :CUSW-DAUPPDAT,                              
019191              IDUSER       = :CUSW-IDUSER                                 
019192       WHERE                                                              
019193              IDLEGSEL     = :CUSW-IDLEGSEL AND                           
019194              IDPARTNR     = :CUSW-IDPARTNR AND                           
019195              KDSTATUS     = 1                                            
019196     END-EXEC                                                             
019197                                                                          
019198     MOVE 000            TO GOOD-SQLCODES                                 
019199     MOVE SQLCODE        TO SQLCODE-WS                                    
019200     PERFORM DB2-STATUS-CHECK                                             
019201     .                                                                    
019202     EJECT                                                                
019203                                                                          
019204 DB2-UPDATE-CUST-COMING SECTION.                                          
019205     EXEC SQL UPDATE T01FCUS                                              
019206       SET                                                                
019207              IDALPHA      = :CUSW-IDALPHA,                               
019208              BEBET_NAME1  = :CUSW-BEBET-NAME1,                           
019209              BEBET_NAME2  = :CUSW-BEBET-NAME2,                           
019210              BEBET_NAME3  = :CUSW-BEBET-NAME3,                           
019211              BEBET_NAME4  = :CUSW-BEBET-NAME4,                           
019212              ADBET_STREET = :CUSW-ADBET-STREET,                          
019213              ADBET_BOX    = :CUSW-ADBET-BOX,                             
019214              ADBET_CITY   = :CUSW-ADBET-CITY,                            
019215              ADBET_PCODE  = :CUSW-ADBET-PCODE,                           
019216              IDLANDX3     = :CUSW-IDLANDX3,                              
019217              IDSPRAK      = :CUSW-IDSPRAK,                               
019218              IDTFN        = :CUSW-IDTFN,                                 
019219              IDTFX        = :CUSW-IDTFX,                                 
019220              IDMAIL       = :CUSW-IDMAIL,                                
019221              IDLEVNR_AP   = :CUSW-IDLEVNR-AP,                            
019222              IDVAT        = :CUSW-IDVAT,                                 
019223              KDTRADP      = :CUSW-KDTRADP,                               
019224              KDBETALV     = :CUSW-KDBETALV,                              
019225              KDKREDSP     = :CUSW-KDKREDSP,                              
019226              KDPARTTY     = :CUSW-KDPARTTY,                              
019227              DAUPPDAT     = :CUSW-DAUPPDAT,                              
019228              IDUSER       = :CUSW-IDUSER                                 
019229       WHERE                                                              
019230              IDLEGSEL     = :CUSW-IDLEGSEL AND                           
019231              IDPARTNR     = :CUSW-IDPARTNR AND                           
019232              KDSTATUS     = 2                                            
019233     END-EXEC                                                             
019234                                                                          
019235     MOVE 000100         TO GOOD-SQLCODES                                 
019236     MOVE SQLCODE        TO SQLCODE-WS                                    
019237     PERFORM DB2-STATUS-CHECK                                             
019238     .                                                                    
019239     EJECT                                                                
019240                                                                          
019241 DB2-CLOSE-CRS-NEW-CUST SECTION.                                          
019242     EXEC SQL CLOSE NEW-CRS                                               
019243     END-EXEC                                                             
019244     .                                                                    
019245     EJECT                                                                
019246                                                                          
019247 DB2-CLOSE-CRS-CHA-CUST SECTION.                                          
019248     EXEC SQL CLOSE CHA-CRS                                               
019249     END-EXEC                                                             
019250     .                                                                    
019251     EJECT                                                                
019252                                                                          
019253*** - CHECK THAT REQUESTED COUNTRY EXISTS                                 
019254 DB2-SELECT-T01COCO SECTION.                                              
019255     MOVE 000100 TO GOOD-SQLCODES                                         
019256                                                                          
019257     EXEC SQL                                                             
019258           SELECT  BELAND                                                 
019259                                                                          
019260           INTO   :COCO-BELAND                                            
019270                                                                          
019280           FROM    T01COCO                                                
019281                                                                          
019282           WHERE   IDLANDX3 = :CUSW-IDLANDX3                              
019283     END-EXEC                                                             
019284                                                                          
019285     MOVE SQLCODE TO SQLCODE-WS                                           
019286     PERFORM DB2-STATUS-CHECK                                             
019287     .                                                                    
019288     EJECT                                                                
019289                                                                          
019290 DB2-SELECT-T01FCUS-CURR SECTION.                                         
019291     MOVE 000100 TO GOOD-SQLCODES                                         
019292                                                                          
019293     EXEC SQL                                                             
019294           SELECT  FLSAPBLK                                               
019295                                                                          
019296           INTO   :CUST-FLSAPBLK                                          
019297                                                                          
019298           FROM    T01FCUS                                                
019299                                                                          
019300           WHERE   IDLEGSEL = :CUSW-IDLEGSEL                              
019301           AND     IDPARTNR = :CUSW-IDPARTNR                              
019302           AND     KDSTATUS         = 1                                   
019303     END-EXEC                                                             
019304                                                                          
019305     MOVE SQLCODE TO SQLCODE-WS                                           
019306     PERFORM DB2-STATUS-CHECK                                             
019307     .                                                                    
019308     EJECT                                                                
019309                                                                          
019310 DB2-SELECT-T01FCUS-COMM SECTION.                                         
019311     MOVE 000100 TO GOOD-SQLCODES                                         
019312                                                                          
019313     EXEC SQL                                                             
019314           SELECT  FLSAPBLK                                               
019315                                                                          
019316           INTO   :CUST-FLSAPBLK                                          
019317                                                                          
019318           FROM    T01FCUS                                                
019319                                                                          
019320           WHERE   IDLEGSEL = :CUSW-IDLEGSEL                              
019321           AND     IDPARTNR = :CUSW-IDPARTNR                              
019322           AND     KDSTATUS         = 2                                   
019323     END-EXEC                                                             
019324                                                                          
019325     MOVE SQLCODE TO SQLCODE-WS                                           
019326     PERFORM DB2-STATUS-CHECK                                             
019327     .                                                                    
019328     EJECT                                                                
019329                                                                          
019330 DB2-STATUS-CHECK SECTION.                                                
019331     SET SQLCODE-IX         TO 1                                          
019332     SEARCH GOOD-SQLCODE AT END                                           
019333           CALL ABEND USING RKOD-ABEND-DB2                                
019340        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
019400           CONTINUE                                                       
019500     END-SEARCH                                                           
019600     .                                                                    
