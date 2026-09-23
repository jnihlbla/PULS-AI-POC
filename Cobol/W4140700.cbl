000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4140700.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   09/10/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        CREATE A REPORT OF UPLOADED SPX ORDERS AND THEIR STATUS          
001000*        THE REPORT WILL BE SENT TO THE SPX COMPANY VIA D&P               
001100*        (OUTPUT TYPE "SPX-ONORDER")                                      
001200*        INPUT: A FILE WITH EXTRACTED ORDER TRANSACTIONS HAVING           
001300*        IDSYSTEM = SPX.                                                  
001400*        RECORD TYPE 0X1 = ORDER HEAD INFO                                
001500*        RECORD TYPE 0X2 = ORDER LINES                                    
001600*        RECORD TYPE 0X3 = ORDER CONFIRMATION TRANSACTIONS                
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- SPX ORDER TRANSACTIONS                                     
002700     SELECT W414X6                     ASSIGN TO W41407D1.                
002800     SKIP2                                                                
002900*          --- SPX ORDER REPORT                                           
003000     SELECT SPX-ONORDER                ASSIGN TO W41407D2.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W414X6                                                               
003700     RECORDING       V                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  -COPY W4140X1      -L.                                               
004100*01  -COPY W4140X2      -L.                                               
004200*01  -COPY W4140X3      -L.                                               
004300     SKIP3                                                                
004400 FD  SPX-ONORDER                                                          
004500     RECORDING       V                                                    
004600     BLOCK CONTAINS  0.                                                   
004700     SKIP2                                                                
004800 01  SPX-ONORDER-LINE            PIC X(121).                              
004900     EJECT                                                                
005000 WORKING-STORAGE SECTION.                                                 
005100                                                                          
005200 77  IDPGM                       PIC X(8)    VALUE 'W4140700'.            
005300 77  YES                         PIC X       VALUE 'J'.                   
005400 77  NOO                         PIC X       VALUE 'N'.                   
005500 77  BOLD-SKIP                   PIC 9(4)    COMP  VALUE ZERO.            
005600                                                                          
005700 77  W414X6-EOF-SW               PIC X       VALUE 'N'.                   
005800     88  END-OF-W414X6                       VALUE 'J'.                   
005900                                                                          
006000 01  TODAYS-DATE-GROUP.                                                   
006100     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006200     03  FILLER                  PIC X       VALUE '-'.                   
006300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006400     03  FILLER                  PIC X       VALUE '-'.                   
006500     03  TODAYS-DATE-DAY         PIC 9(2).                                
006600 01  TODAYS-DATE REDEFINES TODAYS-DATE-GROUP                              
006700                                 PIC X(8).                                
006800                                                                          
006900 01  PREV-IDPTYP                 PIC X(3).                                
007000                                                                          
007100 01  W-TIRFS                     PIC 9(11).                               
007200 01  FILLER REDEFINES W-TIRFS.                                            
007300     03 W-TIRFS-DAT              PIC 9(7).                                
007400     03 W-TIRFS-TID              PIC 9(4).                                
007500                                                                          
007600     EJECT                                                                
007700 01  GENERAL-SUBPROGRAMS.                                                 
007800*                                                                         
007900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
008100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008200     SKIP2                                                                
008300*    --- PARAMETERS TO ABEND                                              
008400                                                                          
008500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008800                                                                          
008900 01  ERROR-TEXT.                                                          
009000     03  FILLER                  PIC X(11)   VALUE 'ERROR-TEXT'.          
009100     03  ERROR-TEXT-STR          PIC X(69)   VALUE SPACE.                 
009200     EJECT                                                                
009300*    --- PARAMETERS TO DATKORT                                            
009400*                                                                         
009500 01  PROGRAM-NAME                PIC X(6)    VALUE 'W41407'.              
009600 01  DATECARD-ID                 PIC X(6)    VALUE 'WDATUM'.              
009700                                                                          
009800*01  -COPY WDATKORT                                                       
009900     EJECT                                                                
010000*    --- PARAMETERS TO POSTSUM                                            
010100*                                                                         
010200*01  -COPY W0005   -PRE  POSTSUM-                                         
010300     EJECT                                                                
010400 01  IN-AREA-START               PIC X(24)   VALUE                        
010500                                 'IN-AREA-START  '.                       
010600 01  IN-AREA.                                                             
010700     03  IN-AREA-0.                                                       
010800       05  IN-IDPTYP             PIC X(3).                                
010900       05  FILLER                PIC X(400).                              
011000*   03  FILLER -COPY W4140X1  -PRE IN-  -RED  IN-AREA-0                   
011100*   03  FILLER -COPY W4140X2  -PRE IN-  -RED  IN-AREA-0                   
011200*   03  FILLER -COPY W4140X3  -PRE IN-  -RED  IN-AREA-0                   
011300                                                                          
011400     EJECT                                                                
011500 01  RPT-AREA-START              PIC X(24)   VALUE                        
011600                                 'RPT-AREA-START  '.                      
011700 01  RPT-HELPAREAS.                                                       
011800     03  RPT-SKIP                PIC S9(3)   COMP-3  VALUE +1.            
011900     03  RPT-MAX-LINES-PER-PAGE                                           
012000                                 PIC 9(3)    VALUE 64.                    
012100     03  RPT-LISTNR              PIC X(11)   VALUE 'SPX-ONORDER'.         
012200     03  RPT-PAGECOUNTER         PIC S9(5)   COMP-3 VALUE ZERO.           
012300     03  RPT-LINECOUNTER         PIC S9(3)   COMP-3 VALUE 999.            
012400     03  RPT-SPACE               PIC X(1)    VALUE SPACE.                 
012500                                                                          
012600     EJECT                                                                
012700 01  RPT-LINE.                                                            
012800     03  FILLER                  PIC X(121)  VALUE SPACE.                 
012900                                                                          
013000                                                                          
013100 01  RPT-HEADER1.                                                         
013200     03  FILLER                  PIC X(32)                                
013300                             VALUE ' Volvo Car Customer Service'.         
013400     03  FILLER                  PIC X(12)  VALUE 'W41407     '.          
013500     03  FILLER                  PIC X(15)  VALUE SPACE.                  
013600     03  RPT-DATE                PIC X(8).                                
013700     03  FILLER                  PIC X(5)   VALUE ' Page'.                
013800     03  RPT-PAGE                PIC Z(3)9.                               
013900                                                                          
014000 01  RPT-HEADER2.                                                         
014100     03  FILLER                  PIC X(118)                               
014200     VALUE ' The following orders have been received by PULS'.            
014300                                                                          
014400 01  RPT-HEADER-OHEAD.                                                    
014500     03  FILLER             PIC  X(10) VALUE '  District'.                
014600     03  FILLER             PIC  X(10) VALUE '    Dealer'.                
014700     03  FILLER             PIC  X(10) VALUE '  Order no'.                
014800     03  FILLER             PIC  X(10) VALUE '. Reg.date'.                
014900     03  FILLER             PIC  X(17) VALUE '  Order ref.     '.         
015000     03  FILLER             PIC  X(15) VALUE '  RFS date time'.           
015100                                                                          
015200 01  RPT-DETAIL-OHEAD.                                                    
015300     03  FILLER             PIC  X(5) VALUE SPACE.                        
015400     03  RPT-IDDISTR-OHEAD  PIC  Z(4)9.                                   
015500     03  FILLER             PIC  X(3) VALUE SPACE.                        
015600     03  RPT-IDKUNDNR-OHEAD PIC  Z(6)9.                                   
015700     03  FILLER             PIC  X(3) VALUE SPACE.                        
015800     03  RPT-IDORDNR7-OHEAD PIC  Z(6)9.                                   
015900     03  FILLER             PIC  X(4) VALUE SPACE.                        
016000     03  RPT-TIREGDAT-OHEAD PIC  9(6).                                    
016100     03  FILLER             PIC  X(2) VALUE SPACE.                        
016200     03  RPT-BEKUNDRF-OHEAD PIC  X(15).                                   
016300     03  FILLER             PIC  X(4) VALUE SPACE.                        
016400     03  RPT-TIRFS-DAT-OHEAD PIC 9(6).                                    
016500     03  FILLER             PIC  X(1) VALUE SPACE.                        
016600     03  RPT-TIRFS-TID-OHEAD PIC 9(4).                                    
016700                                                                          
016800     EJECT                                                                
016900 01  RPT-HEADER1-OLINE.                                                   
017000     03  FILLER             PIC  X(13) VALUE SPACE.                       
017100     03  FILLER             PIC  X(11) VALUE 'Order lines'.               
017200                                                                          
017300 01  RPT-HEADER2-OLINE.                                                   
017400     03  FILLER             PIC  X(10) VALUE SPACE.                       
017500     03  FILLER             PIC  X(10) VALUE '   Part no'.                
017600     03  FILLER             PIC  X(10) VALUE '. Quantity'.                
017700     03  FILLER             PIC  X(2)  VALUE SPACE.                       
017800     03  FILLER             PIC  X(10) VALUE 'Line ref. '.                
017900                                                                          
018000 01  RPT-DETAIL-OLINE.                                                    
018100     03  FILLER             PIC  X(10) VALUE SPACE.                       
018200     03  FILLER             PIC  X(2)  VALUE SPACE.                       
018300     03  RPT-IDARTNR-OLINE  PIC  Z(7)9.                                   
018400     03  FILLER             PIC  X(3)  VALUE SPACE.                       
018500     03  RPT-KVBEART-OLINE  PIC  Z(6)9.                                   
018600     03  FILLER             PIC  X(2)  VALUE SPACE.                       
018700     03  RPT-BERADREF-OLINE PIC  X(10).                                   
018800                                                                          
018900     EJECT                                                                
019000 01  RPT-HEADER1-OCONF.                                                   
019100     03  FILLER             PIC  X(13) VALUE SPACE.                       
019200     03  FILLER             PIC  X(45)                                    
019300         VALUE 'Order confirmations (see PULS screen 4282)   '.           
019400                                                                          
019500 01  RPT-HEADER2-OCONF.                                                   
019600     03  FILLER             PIC  X(10) VALUE SPACE.                       
019700     03  FILLER             PIC  X(10) VALUE '   Part no'.                
019800     03  FILLER             PIC  X(10) VALUE '. Quantity'.                
019900     03  FILLER             PIC  X(2)  VALUE SPACE.                       
020000     03  FILLER             PIC  X(10) VALUE 'Line ref. '.                
020100     03  FILLER             PIC  X(10) VALUE 'Conf code '.                
020200                                                                          
020300 01  RPT-DETAIL-OCONF.                                                    
020400     03  FILLER             PIC  X(10) VALUE SPACE.                       
020500     03  FILLER             PIC  X(2)  VALUE SPACE.                       
020600     03  RPT-IDARTNR-OCONF  PIC  Z(7)9.                                   
020700     03  FILLER             PIC  X(3)  VALUE SPACE.                       
020800     03  RPT-KVBEART-OCONF  PIC  Z(6)9.                                   
020900     03  FILLER             PIC  X(2)  VALUE SPACE.                       
021000     03  RPT-BERADREF-OCONF PIC  X(10).                                   
021100     03  FILLER             PIC  X(7)  VALUE SPACE.                       
021200     03  RPT-KDORDBEK-OCONF PIC  9(2).                                    
021300     EJECT                                                                
021400 PROCEDURE DIVISION.                                                      
021500 MAIN SECTION.                                                            
021600     SKIP2                                                                
021700                                                                          
021800     PERFORM A-INIT                                                       
021900     PERFORM S01-READ-W414X6                                              
022000     PERFORM UNTIL END-OF-W414X6                                          
022100       IF IN-IDPTYP = '0X1'                                               
022200       DISPLAY 'HEAD ' IN-0X1-BEKUNDRF                                    
022300          PERFORM B-WRITE-ORDER-HEAD                                      
022400       END-IF                                                             
022500       IF IN-IDPTYP = '0X2'                                               
022600       DISPLAY 'LINE ' IN-0X2-BERADREF                                    
022700          PERFORM C-WRITE-ORDER-LINE                                      
022800       END-IF                                                             
022900       IF IN-IDPTYP = '0X3'                                               
023000       DISPLAY 'CONF ' IN-0X3-BERADREF                                    
023100          PERFORM D-WRITE-ORDER-CONFIRMATION                              
023200       END-IF                                                             
023300       MOVE IN-IDPTYP TO PREV-IDPTYP                                      
023400                                                                          
023500       PERFORM S01-READ-W414X6                                            
023600     END-PERFORM                                                          
023700                                                                          
023800                                                                          
023900     PERFORM Z-FINIT                                                      
024000                                                                          
024100     MOVE ZERO TO RETURN-CODE                                             
024200     GOBACK                                                               
024300     .                                                                    
024400     EJECT                                                                
024500 A-INIT SECTION.                                                          
024600                                                                          
024700     OPEN INPUT  W414X6                                                   
024800     OPEN OUTPUT SPX-ONORDER                                              
024900                                                                          
025000     CALL DATKORT USING PROGRAM-NAME DATECARD-ID DATUMKORT                
025100     MOVE D-AAR     TO  TODAYS-DATE-YEAR                                  
025200     MOVE D-MAANAD  TO  TODAYS-DATE-MONTH                                 
025300     MOVE D-DAG     TO  TODAYS-DATE-DAY                                   
025400     MOVE TODAYS-DATE   TO RPT-DATE                                       
025500                                                                          
025600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
025700                                                                          
025800     MOVE SPACE TO PREV-IDPTYP                                            
025900     .                                                                    
026000                                                                          
026100     EJECT                                                                
026200 B-WRITE-ORDER-HEAD SECTION.                                              
026300                                                                          
026400     MOVE SPACE                TO RPT-LINE                                
026500     PERFORM S21-WRITE-SPX-ONORDER                                        
026600     PERFORM S21-WRITE-SPX-ONORDER                                        
026700     MOVE RPT-HEADER-OHEAD     TO RPT-LINE                                
026800     PERFORM S21-WRITE-SPX-ONORDER                                        
026900                                                                          
027000     MOVE IN-0X1-IDDISTR       TO RPT-IDDISTR-OHEAD                       
027100     MOVE IN-0X1-IDKUNDNR      TO RPT-IDKUNDNR-OHEAD                      
027200     MOVE IN-0X1-IDKUNDRF(1:7) TO RPT-IDORDNR7-OHEAD                      
027300     MOVE IN-0X1-TIREGDAT      TO RPT-TIREGDAT-OHEAD                      
027400     MOVE IN-0X1-BEKUNDRF      TO RPT-BEKUNDRF-OHEAD                      
027500     MOVE IN-0X1-TIRFS         TO W-TIRFS                                 
027600     MOVE W-TIRFS-DAT          TO RPT-TIRFS-DAT-OHEAD                     
027700     MOVE W-TIRFS-TID          TO RPT-TIRFS-TID-OHEAD                     
027800     MOVE RPT-DETAIL-OHEAD     TO RPT-LINE                                
027900                                                                          
028000     MOVE BOLD-SKIP TO RPT-SKIP                                           
028100     PERFORM S21-WRITE-SPX-ONORDER                                        
028200     .                                                                    
028300                                                                          
028400     EJECT                                                                
028500 C-WRITE-ORDER-LINE SECTION.                                              
028600                                                                          
028700     IF IN-IDPTYP NOT = PREV-IDPTYP                                       
028800       MOVE SPACE              TO RPT-LINE                                
028900       PERFORM S21-WRITE-SPX-ONORDER                                      
029000       MOVE RPT-HEADER1-OLINE  TO RPT-LINE                                
029100       PERFORM S21-WRITE-SPX-ONORDER                                      
029200       MOVE RPT-HEADER2-OLINE  TO RPT-LINE                                
029300       PERFORM S21-WRITE-SPX-ONORDER                                      
029400     END-IF                                                               
029500                                                                          
029600     MOVE IN-0X2-IDARTNR       TO RPT-IDARTNR-OLINE                       
029700     MOVE IN-0X2-KVBEART       TO RPT-KVBEART-OLINE                       
029800     MOVE IN-0X2-BERADREF      TO RPT-BERADREF-OLINE                      
029900     MOVE RPT-DETAIL-OLINE     TO RPT-LINE                                
030000                                                                          
030100     MOVE BOLD-SKIP TO RPT-SKIP                                           
030200     PERFORM S21-WRITE-SPX-ONORDER                                        
030300     .                                                                    
030400                                                                          
030500     EJECT                                                                
030600 D-WRITE-ORDER-CONFIRMATION SECTION.                                      
030700                                                                          
030800     IF IN-IDPTYP NOT = PREV-IDPTYP                                       
030900       MOVE SPACE              TO RPT-LINE                                
031000       PERFORM S21-WRITE-SPX-ONORDER                                      
031100       MOVE RPT-HEADER1-OCONF  TO RPT-LINE                                
031200       PERFORM S21-WRITE-SPX-ONORDER                                      
031300       MOVE RPT-HEADER2-OCONF  TO RPT-LINE                                
031400       PERFORM S21-WRITE-SPX-ONORDER                                      
031500     END-IF                                                               
031600                                                                          
031700     MOVE IN-0X3-IDARTNR       TO RPT-IDARTNR-OCONF                       
031800     MOVE IN-0X3-KVBEART       TO RPT-KVBEART-OCONF                       
031900     MOVE IN-0X3-BERADREF      TO RPT-BERADREF-OCONF                      
032000     MOVE IN-0X3-KDORDBEK      TO RPT-KDORDBEK-OCONF                      
032100     MOVE RPT-DETAIL-OCONF     TO RPT-LINE                                
032200                                                                          
032300     MOVE BOLD-SKIP TO RPT-SKIP                                           
032400     PERFORM S21-WRITE-SPX-ONORDER                                        
032500     .                                                                    
032600                                                                          
032700     EJECT                                                                
032800 Z-FINIT SECTION.                                                         
032900                                                                          
033000     CLOSE W414X6                                                         
033100           SPX-ONORDER                                                    
033200     SKIP2                                                                
033300     MOVE 'S' TO POSTSUM-OPKOD                                            
033400     CALL POSTSUM USING POSTSUM-PARM                                      
033500     .                                                                    
033600                                                                          
033700     EJECT                                                                
033800 S01-READ-W414X6  SECTION.                                                
033900                                                                          
034000     READ W414X6 INTO IN-AREA                                             
034100     AT END                                                               
034200        MOVE HIGH-VALUE TO IN-AREA                                        
034300        SET END-OF-W414X6 TO TRUE                                         
034400                                                                          
034500     NOT AT END                                                           
034600        MOVE 'W414X6' TO POSTSUM-FDNAMN                                   
034700        MOVE 'W41407D1' TO POSTSUM-DDNAMN2                                
034800        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
034900        CALL POSTSUM USING POSTSUM-PARM                                   
035000     END-READ                                                             
035100     .                                                                    
035200                                                                          
035300     EJECT                                                                
035400 S21-WRITE-SPX-ONORDER SECTION.                                           
035500                                                                          
035600     IF RPT-LINECOUNTER > RPT-MAX-LINES-PER-PAGE                          
035700       PERFORM S21A-WRITE-HEADERS                                         
035800     END-IF                                                               
035900                                                                          
036000     WRITE SPX-ONORDER-LINE FROM RPT-LINE AFTER 1                         
036100     IF RPT-SKIP = BOLD-SKIP                                              
036200*      -- SIMULATE BOLD FONT BY PRINTING TWICE ON SAME LINE               
036300       WRITE SPX-ONORDER-LINE FROM RPT-LINE AFTER 0                       
036400       MOVE 1 TO RPT-SKIP                                                 
036500     END-IF                                                               
036600     ADD  +1 TO RPT-LINECOUNTER                                           
036700     .                                                                    
036800                                                                          
036900     EJECT                                                                
037000 S21A-WRITE-HEADERS SECTION.                                              
037100                                                                          
037200     ADD +1 TO RPT-PAGECOUNTER                                            
037300     MOVE RPT-PAGECOUNTER TO RPT-PAGE                                     
037400     WRITE SPX-ONORDER-LINE FROM RPT-HEADER1 AFTER PAGE                   
037500     WRITE SPX-ONORDER-LINE FROM RPT-SPACE   AFTER 1                      
037600     WRITE SPX-ONORDER-LINE FROM RPT-HEADER2 AFTER 1                      
037700     WRITE SPX-ONORDER-LINE FROM RPT-SPACE   AFTER 1                      
037800     MOVE +5 TO RPT-LINECOUNTER                                           
037900     .                                                                    
038000                                                                          
038100     EJECT                                                                
038200*S99-ABEND SECTION.                                                       
038300*                                                                         
038400*    MOVE 'S' TO POSTSUM-OPKOD                                            
038500*    CALL POSTSUM USING POSTSUM-PARM                                      
038600*    MOVE RKOD-ABEND-NO-DUMP TO RKOD-ABEND                                
038700*    CALL ABEND USING RKOD-ABEND                                          
038800*    .                                                                    
