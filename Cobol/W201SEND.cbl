000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W201SEND.                                                
000400 AUTHOR.         GÖRAN KJELLSON    GUIDE                                  
000500 DATE-WRITTEN.   FEBRUARI 2006.                                           
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET SKAPAR MAIL TILL VALFRI-MAILADRESS.                   
001000*        MED ORDERINGÅNGSINFORMATION.                                     
001100*                                                                         
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500 DATA DIVISION.                                                           
001600                                                                          
001700     EJECT                                                                
001800 WORKING-STORAGE SECTION.                                                 
001900 77  IDPGM                   PIC X(8) VALUE 'W201SEND'.                   
002000 77  JA                      PIC X       VALUE 'J'.                       
002100 77  NEJ                     PIC X       VALUE 'N'.                       
002200 77  INDX                    PIC S9(4)   VALUE ZERO COMP SYNC.            
002300 77  IX1                     PIC 9(4)    VALUE ZERO.                      
002400 77  IX1-MAX                 PIC 9(4)    VALUE 25.                        
002500 77  IX2                     PIC 9(4)    VALUE ZERO.                      
002600                                                                          
002700 01  FILLER.                                                              
002800   03  COUNTER               PIC 99  VALUE ZERO.                          
002900                                                                          
003000 01  DYNAMISKA-SUBPROGRAM.                                                
003100   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
003200   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
003300                                                                          
003400     EJECT                                                                
003500*  MAILRADER                                                              
003600                                                                          
003700 01  LIST-HRAD1.                                                          
003800     03   FILLER                  PIC X(1)  VALUE SPACE.                  
003900     03   FILLER                  PIC X(12) VALUE                         
004000                                        'W201SEND-001'.                   
004100     03   FILLER                  PIC X(4)  VALUE SPACE.                  
004200     03   FILLER                  PIC X(35)  VALUE                        
004300                 'ORDERINGÅNG                        '.                   
004400     03   FILLER                  PIC X(28)  VALUE SPACE.                 
004500                                                                          
004600 01  LIST-RAD-DEL1.                                                       
004700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
004800     03   FILLER                  PIC X(15) VALUE                         
004900          'ANTAL ARTIKLAR '.                                              
005000     03   RAD-KVANTART            PIC 9(5)  VALUE ZERO.                   
005100                                                                          
005200 01  LIST-RAD-DEL2.                                                       
005300     03   FILLER                  PIC X(1)  VALUE SPACE.                  
005400     03   FILLER                  PIC X(8)  VALUE                         
005500          'ARTIKEL '.                                                     
005600     03   RAD-IDARTNR             PIC 9(8)  VALUE ZERO.                   
005700     03   FILLER                  PIC X(1)  VALUE SPACE.                  
005800     03   FILLER                  PIC X(03) VALUE                         
005900          'DC '.                                                          
006000     03   RAD-IDDC                PIC X(2)  VALUE SPACE.                  
006100     03   FILLER                  PIC X(1)  VALUE SPACE.                  
006200     03   FILLER                  PIC X(05) VALUE                         
006300          'KDOI '.                                                        
006400     03   RAD-KDOI                PIC X(2)  VALUE ZERO.                   
006500     03   FILLER                  PIC X(1)  VALUE SPACE.                  
006600     03   FILLER                  PIC X(05) VALUE                         
006700          'KVOI '.                                                        
006800     03   RAD-KDTECKEN            PIC X(1)  VALUE SPACE.                  
006900     03   RAD-KVOI                PIC 9(7)  VALUE ZERO.                   
007000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
007100     03   FILLER                  PIC X(09) VALUE                         
007200          'TIDPUNKT '.                                                    
007300     03   RAD-TIUPPDAT            PIC 9(6)  VALUE ZERO.                   
007400                                                                          
007500 01  LIST-RAD-DEL3.                                                       
007600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
007700     03   FILLER                  PIC X(15) VALUE                         
007800          'DC/FLLF/CLEAR '.                                               
007900     03   RAD-IDDC-1              PIC X(2)  VALUE SPACE.                  
008000     03   FILLER                  PIC X(01) VALUE SPACE.                  
008100     03   RAD-FLLF-1              PIC X(1)  VALUE SPACE.                  
008200     03   FILLER                  PIC X(01) VALUE SPACE.                  
008300     03   RAD-FLCLEAR-1           PIC X(1)  VALUE ZERO.                   
008400     03   FILLER                  PIC X(3)  VALUE SPACE.                  
008500     03   RAD-IDDC-2              PIC X(2)  VALUE SPACE.                  
008600     03   FILLER                  PIC X(01) VALUE SPACE.                  
008700     03   RAD-FLLF-2              PIC X(1)  VALUE SPACE.                  
008800     03   FILLER                  PIC X(01) VALUE SPACE.                  
008900     03   RAD-FLCLEAR-2           PIC X(1)  VALUE ZERO.                   
009000     03   FILLER                  PIC X(3)  VALUE SPACE.                  
009100     03   RAD-IDDC-3              PIC X(2)  VALUE SPACE.                  
009200     03   FILLER                  PIC X(01) VALUE SPACE.                  
009300     03   RAD-FLLF-3              PIC X(1)  VALUE SPACE.                  
009400     03   FILLER                  PIC X(01) VALUE SPACE.                  
009500     03   RAD-FLCLEAR-3           PIC X(1)  VALUE ZERO.                   
009600     03   FILLER                  PIC X(3)  VALUE SPACE.                  
009700     03   RAD-IDDC-4              PIC X(2)  VALUE SPACE.                  
009800     03   FILLER                  PIC X(01) VALUE SPACE.                  
009900     03   RAD-FLLF-4              PIC X(1)  VALUE SPACE.                  
010000     03   FILLER                  PIC X(01) VALUE SPACE.                  
010100     03   RAD-FLCLEAR-4           PIC X(1)  VALUE ZERO.                   
010200     03   FILLER                  PIC X(3)  VALUE SPACE.                  
010300     03   RAD-IDDC-5              PIC X(2)  VALUE SPACE.                  
010400     03   FILLER                  PIC X(01) VALUE SPACE.                  
010500     03   RAD-FLLF-5              PIC X(1)  VALUE SPACE.                  
010600     03   FILLER                  PIC X(01) VALUE SPACE.                  
010700     03   RAD-FLCLEAR-5           PIC X(1)  VALUE ZERO.                   
010800     03   FILLER                  PIC X(3)  VALUE SPACE.                  
010900     03   RAD-IDDC-6              PIC X(2)  VALUE SPACE.                  
011000     03   FILLER                  PIC X(01) VALUE SPACE.                  
011100     03   RAD-FLLF-6              PIC X(1)  VALUE SPACE.                  
011200     03   FILLER                  PIC X(01) VALUE SPACE.                  
011300     03   RAD-FLCLEAR-6           PIC X(1)  VALUE ZERO.                   
011310     03   FILLER                  PIC X(3)  VALUE SPACE.                  
011320     03   RAD-IDDC-7              PIC X(2)  VALUE SPACE.                  
011330     03   FILLER                  PIC X(01) VALUE SPACE.                  
011340     03   RAD-FLLF-7              PIC X(1)  VALUE SPACE.                  
011350     03   FILLER                  PIC X(01) VALUE SPACE.                  
011360     03   RAD-FLCLEAR-7           PIC X(1)  VALUE ZERO.                   
011400                                                                          
011500                                                                          
011600 01  LIST-TEXTRAD.                                                        
011700     03   RESTRAD-TEXT            PIC X(80)  VALUE SPACE.                 
011800                                                                          
011900 01  LIST-BLANKRAD.                                                       
012000     03   FILLER                  PIC X(80) VALUE SPACE.                  
012100                                                                          
012200 01  LIST-FELRAD.                                                         
012300     03   FILLER                  PIC X(80) VALUE 'NOLL RADER'.           
012400                                                                          
012500     EJECT                                                                
012600*    --- STATUS-KOD FRÅN IMS                                              
012700 01  STATUS-WS                   PIC XX.                                  
012800     88  STATUS-OK                           VALUE '  '.                  
012900                                                                          
013000*    --- IMS FUNKTIONSKODER                                               
013100*01  -COPY W0003                                                          
013200                                                                          
013300     EJECT                                                                
013400*01  -COPY WMSGAREA                                                       
013500                                                                          
013600     EJECT                                                                
013700*   --- PARAMETRAR TILL PROGRAM W0541X                                    
013800                                                                          
013900 01  FILLER                     PIC X(16)   VALUE 'WMSGMAIL-AREA'.        
014000*01  -COPY WMSGMAIL                                                       
014100                                                                          
014200     EJECT                                                                
014300*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
014400                                                                          
014500 01  GODK-STATUSKODER.                                                    
014600   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
014700                                                                          
014800 01  SSA1                    PIC X(32).                                   
014900                                                                          
015000     EJECT                                                                
015100 LINKAGE SECTION.                                                         
015200                                                                          
015300*01  -COPY W201SEND                                                       
015400     EJECT                                                                
015500*01  -COPY W0009      -PRE  MAIL-                                         
015600                                                                          
015700     EJECT                                                                
015800 PROCEDURE DIVISION  USING  SEND-W201SEND MAIL-PCB.                       
015900 STYR SECTION.                                                            
016000                                                                          
016100     PERFORM A-INIT                                                       
016200                                                                          
016300     PERFORM B-SKAPA-MAIL                                                 
016400                                                                          
016500     GOBACK                                                               
016600     .                                                                    
016700                                                                          
016800     EJECT                                                                
016900 A-INIT        SECTION.                                                   
017000                                                                          
017100     MOVE '2109'                TO MAIL-IDTRANS                           
017200     MOVE '1'                   TO MAIL-KDMFSFOR                          
017300     MOVE 'ORDERINGÅNG    '     TO MAIL-IDMAILTTL                         
017400                                                                          
017500     MOVE ZERO TO COUNTER                                                 
017600     INSPECT SEND-IDMAIL TALLYING COUNTER FOR ALL '@'                     
017700                                                                          
017800     .                                                                    
017900     EJECT                                                                
018000 B-SKAPA-MAIL  SECTION.                                                   
018100                                                                          
018200     PERFORM BA-REDIGERA-HUVUD                                            
018300     PERFORM BB-REDIGERA-RADER                                            
018400                                                                          
018500     MOVE INDX                TO MAIL-KVMAILLN                            
018600                                                                          
018700     IF COUNTER = 0                                                       
018800*      MOVE 'PLINDGR8@VOLVOCARS.COM'   TO MAIL-IDMAIL                     
018900*      PERFORM IMS-PURGE-TRANS0541X-MID                                   
019000                                                                          
019100       MOVE SPACE                          TO MAIL-IDMAIL                 
019200       MOVE 'GORAN.KJELLSON@VOLVOCARS.COM' TO MAIL-IDMAIL                 
019300       PERFORM IMS-PURGE-TRANS0541X-MID                                   
019400     ELSE                                                                 
019500       MOVE SEND-IDMAIL         TO MAIL-IDMAIL                            
019600       PERFORM IMS-PURGE-TRANS0541X-MID                                   
019700     END-IF                                                               
019800     .                                                                    
019900                                                                          
020000     EJECT                                                                
020100 BA-REDIGERA-HUVUD  SECTION.                                              
020200                                                                          
020300     MOVE +1                    TO INDX                                   
020400     MOVE LIST-HRAD1            TO MAIL-TEMAIL (INDX)                     
020500                                                                          
020600     ADD +1                     TO INDX                                   
020700     MOVE LIST-BLANKRAD         TO MAIL-TEMAIL (INDX)                     
020800     .                                                                    
020900                                                                          
021000     SKIP2                                                                
021100 BB-REDIGERA-RADER  SECTION.                                              
021200                                                                          
021300     MOVE SEND-KVANTART    TO RAD-KVANTART                                
021400     ADD +1                TO INDX                                        
021500     MOVE LIST-RAD-DEL1    TO MAIL-TEMAIL (INDX)                          
021600                                                                          
021700     IF SEND-KVANTART = ZERO                                              
021800        ADD +1             TO INDX                                        
021900        MOVE LIST-FELRAD   TO MAIL-TEMAIL (INDX)                          
022000     END-IF                                                               
022100                                                                          
022200     MOVE 1 TO IX1                                                        
022300     PERFORM UNTIL IX1 > SEND-KVANTART                                    
022400        MOVE SEND-IDARTNR (IX1) TO RAD-IDARTNR                            
022500        MOVE SEND-IDDC    (IX1) TO RAD-IDDC                               
022600        MOVE SEND-KDOI    (IX1) TO RAD-KDOI                               
022700        MOVE SEND-KDTECKEN(IX1) TO RAD-KDTECKEN                           
022800        MOVE SEND-KVOI    (IX1) TO RAD-KVOI                               
022900        MOVE SEND-TIUPPDAT(IX1) TO RAD-TIUPPDAT                           
023000        ADD +1                  TO INDX                                   
023100        MOVE LIST-RAD-DEL2      TO MAIL-TEMAIL (INDX)                     
023200                                                                          
023300        MOVE SEND-IDDC-CLEAR(IX1, 1) TO RAD-IDDC-1                        
023400        MOVE SEND-FLLF      (IX1, 1) TO RAD-FLLF-1                        
023500        MOVE SEND-FLCLEAR   (IX1, 1) TO RAD-FLCLEAR-1                     
023600        MOVE SEND-IDDC-CLEAR(IX1, 2) TO RAD-IDDC-2                        
023700        MOVE SEND-FLLF      (IX1, 2) TO RAD-FLLF-2                        
023800        MOVE SEND-FLCLEAR   (IX1, 2) TO RAD-FLCLEAR-2                     
023900        MOVE SEND-IDDC-CLEAR(IX1, 3) TO RAD-IDDC-3                        
024000        MOVE SEND-FLLF      (IX1, 3) TO RAD-FLLF-3                        
024100        MOVE SEND-FLCLEAR   (IX1, 3) TO RAD-FLCLEAR-3                     
024200        MOVE SEND-IDDC-CLEAR(IX1, 4) TO RAD-IDDC-4                        
024210        MOVE SEND-FLLF      (IX1, 4) TO RAD-FLLF-4                        
024220        MOVE SEND-FLCLEAR   (IX1, 4) TO RAD-FLCLEAR-4                     
024230        MOVE SEND-IDDC-CLEAR(IX1, 5) TO RAD-IDDC-5                        
024240        MOVE SEND-FLLF      (IX1, 5) TO RAD-FLLF-5                        
024250        MOVE SEND-FLCLEAR   (IX1, 5) TO RAD-FLCLEAR-5                     
024260        MOVE SEND-IDDC-CLEAR(IX1, 6) TO RAD-IDDC-6                        
024270        MOVE SEND-FLLF      (IX1, 6) TO RAD-FLLF-6                        
024280        MOVE SEND-FLCLEAR   (IX1, 6) TO RAD-FLCLEAR-6                     
024290        MOVE SEND-IDDC-CLEAR(IX1, 7) TO RAD-IDDC-7                        
024291        MOVE SEND-FLLF      (IX1, 7) TO RAD-FLLF-7                        
024292        MOVE SEND-FLCLEAR   (IX1, 7) TO RAD-FLCLEAR-7                     
024300        ADD +1                            TO INDX                         
024400        MOVE LIST-RAD-DEL3                TO MAIL-TEMAIL (INDX)           
024500        ADD 1 TO IX1                                                      
024600     END-PERFORM                                                          
024700                                                                          
024800     ADD +1                     TO INDX                                   
024900     MOVE LIST-BLANKRAD         TO MAIL-TEMAIL (INDX)                     
025000                                                                          
025100     MOVE SEND-TEXT             TO RESTRAD-TEXT                           
025200     ADD +1                     TO INDX                                   
025300     MOVE LIST-TEXTRAD          TO MAIL-TEMAIL (INDX)                     
025400                                                                          
025500     ADD +1                     TO INDX                                   
025600     MOVE LIST-BLANKRAD         TO MAIL-TEMAIL (INDX)                     
025700     .                                                                    
025800                                                                          
025900     SKIP2                                                                
026000 IMS-PURGE-TRANS0541X-MID SECTION.                                        
026100                                                                          
026200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
026300     MOVE '  '                  TO GODK-STATUSKODER                       
026400     CALL CBLTDLI USING PURG MAIL-PCB MAIL-WMSGMAIL                       
026500     MOVE MAIL-STATUS-CODE       TO STATUS-WS                             
026600     PERFORM IMS-STATUSKONTROLL                                           
026700     .                                                                    
026800                                                                          
026900     SKIP2                                                                
027000 IMS-STATUSKONTROLL SECTION.                                              
027100                                                                          
027200     SET STATUS-IX TO 1                                                   
027300     SEARCH GODK-STATUS                                                   
027400       AT END CALL FELLOG                                                 
027500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
027600     END-SEARCH                                                           
027700     .                                                                    
