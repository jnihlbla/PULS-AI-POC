000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5617500.                                                
000300 AUTHOR.         SARASWATHY S.                                            
000400 DATE-WRITTEN.   19/03/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        CREATES A REPORT FOR RECEIVED GOODS PER LPC                      
001000*        DISPLAYS TOTAL NET PRICE PER PRODUCT GROUP                       
001100*                                                                         
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- LATEST DYNAMIC BOOKING EVENTS                              
002600     SELECT W56166                     ASSIGN TO W56175D1.                
002700     SKIP2                                                                
002800*          --- RECEIVING GOODS BY LPC                                     
002900     SELECT W56175                     ASSIGN TO W56175D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W56166                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY WDR801                   -L                                    
004000     SKIP3                                                                
004100 FD  W56175                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  RECORD -COPY W56175A -PRE  UT-  -L.                                  
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                   PIC X(8)    VALUE 'W5617500'.                
004910 77  YES                     PIC X       VALUE 'J'.                       
004920 77  NOO                     PIC X       VALUE 'N'.                       
004930 77  IX-MARKUP               PIC S9(4)   BINARY.                          
004940 77  IX                      PIC S9(4)   BINARY.                          
004950 77  WS-LANDED-COST          PIC S9(10)V9(5) COMP-3.                      
004960 77  WS-MARKUP               PIC 9V9(2)  VALUE ZERO.                      
005000                                                                          
005010 77  WS-PROD-SW              PIC X       VALUE 'N'.                       
005020     88 PRODKOD-MISSING                      VALUE 'N'.                   
005030     88 PRODKOD-FOUND                        VALUE 'J'.                   
005040                                                                          
005100 77  W56166-EOF-SW           PIC X       VALUE 'N'.                       
005200     88  END-OF-W56166                   VALUE 'J'.                       
005300 77  WS-IDFAKT               PIC X(7)    VALUE SPACES.                    
005301 77  WS-TIREGDAT             PIC 9(6)    VALUE 0.                         
005310 77  WS-PRKURS-US            PIC S9(6)V9(5)  COMP-3 VALUE 0.              
005311 77  WS-KDVALISO-HUV         PIC X(3)    VALUE 'SEK'.                     
005312 77  W-DATE-AAMM             PIC 9(4)    VALUE ZERO.                      
005320 77  FELTEXT                 PIC X(80)   VALUE SPACES.                    
005400     EJECT                                                                
005500 01  GENERAL-SUBPROGRAMS.                                                 
005600*                                                                         
005700     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
005800     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
005810     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
005820     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
005830     03  W510CURR            PIC X(8)    VALUE 'W510CURR'.                
005900     SKIP2                                                                
006000*    --- PARAMETERS TO ABEND                                              
006100                                                                          
006200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006500     SKIP2                                                                
006600 01  ERROR-TEXT.                                                          
006700     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
006800     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006900     EJECT                                                                
007000*    --- PARAMETRAR TILL POSTSUM                                          
007100*                                                                         
007200*01  -COPY W0005   -PRE  POSTSUM-                                         
007300*01  -COPY WWDC99                                                         
007310*01  -COPY W510CURR                                                       
007400     EJECT                                                                
007500 01  IN-AREA-START               PIC X(24)   VALUE                        
007600                                 'IN-AREA-START  '.                       
007700     SKIP2                                                                
007800*01  AREA -COPY WDR801     -PRE IN-                                       
007900*    05   -COPY W510EKHA   -PRE IN- -RED IN-FIL-WDR801-DATA               
008000      EJECT                                                               
008100 01  UT-AREA-START               PIC X(24)   VALUE                        
008200                                 'UT-AREA-START  '.                       
008300     SKIP2                                                                
008400                                                                          
008500*01  AREA -COPY W56175A    -PRE UT-                                       
008600     EJECT                                                                
008700*01  -COPY WWMARKUP                                                       
008701     EJECT                                                                
008702*    --- KEYS TO IMS                                                      
008729 LINKAGE SECTION.                                                         
008730*01  -COPY W0008  -PRE WDG2-                                              
008731     05  FILLER                  PIC X.                                   
008732                                                                          
008733 PROCEDURE DIVISION USING WDG2-PCB.                                       
008800 MAIN SECTION.                                                            
008810     ENTRY 'DLITCBL' USING WDG2-PCB.                                      
008900     SKIP2                                                                
009000                                                                          
009100     PERFORM A-INIT                                                       
009200     PERFORM S01-READ-W56166                                              
009300     PERFORM UNTIL END-OF-W56166                                          
009400       PERFORM B-MATCH-MOVE-DATA                                          
009500       PERFORM S01-READ-W56166                                            
009600     END-PERFORM                                                          
009700                                                                          
009800                                                                          
009900     PERFORM Z-FINIT                                                      
010000                                                                          
010100     MOVE ZERO TO RETURN-CODE                                             
010200     GOBACK                                                               
010300     .                                                                    
010400     EJECT                                                                
010500 A-INIT SECTION.                                                          
010600                                                                          
010700     OPEN INPUT  W56166                                                   
010800                                                                          
010900     OPEN OUTPUT W56175                                                   
011000     SKIP2                                                                
011100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
011200     .                                                                    
011300     EJECT                                                                
011400 B-MATCH-MOVE-DATA SECTION.                                               
011410     MOVE SPACES TO UT-FREIGHT-MODE                                       
011500     MOVE ZEROS  TO UT-KDFRAKT                                            
011600     MOVE SPACES TO UT-DESCRIPTION                                        
011700     MOVE IN-EKH-IDDC-REC TO WS-IDDC                                      
011800     IF NDC-US                                                            
011900       IF IN-EKH-KDEKHHT = '102'                                          
012000         IF IN-EKH-KDEKSHT = '121' OR IN-EKH-KDEKSHT = '131'              
012800           IF IN-EKH-KDEKNIVA = 'DET'                                     
012810             MOVE IN-FIL-TIREGDAT            TO WS-TIREGDAT               
012820             MOVE WS-TIREGDAT(1:4)           TO W-DATE-AAMM               
012830             MOVE 'USD'                      TO CURR-KDVALISO-ROW         
012831             MOVE W-DATE-AAMM                TO CURR-TIAAMM               
012833             MOVE WS-KDVALISO-HUV            TO CURR-KDVALISO-HUV         
012835             MOVE 'M'                        TO CURR-KDVALTYP             
012836             CALL W510CURR USING CURR-W510CURR WDG2-PCB                   
012837             IF CURR-KDSVAR = ' '                                         
012860               MOVE CURR-PRKURS-NEW         TO WS-PRKURS-US               
012861             ELSE                                                         
012862               MOVE 1                       TO WS-PRKURS-US               
012870             END-IF                                                       
012900             MOVE IN-EKH-IDVERGL             TO WS-IDFAKT                 
013000             MOVE WS-IDFAKT                  TO UT-IDFAKT                 
013100             MOVE IN-FIL-TIREGDAT            TO UT-TIREGDAT               
013200             MOVE IN-EKH-IDDC-SEND           TO UT-IDDC-SEND              
013300             MOVE IN-EKH-IDDC-REC            TO UT-IDDC-RECV              
013400             MOVE IN-EKH-KDPSLLOC            TO UT-KDPSLLOC               
013500             COMPUTE UT-TOTAL-PRICE =                                     
013600                     (IN-EKH-PRARTNTO / WS-PRKURS-US) *                   
013700                     IN-EKH-KVANTAL                                       
013800             PERFORM BA-LANDED-COST                                       
016110             IF IN-EKH-KDFRAKT = 13                                       
016120             OR IN-EKH-KDFRAKT = 14                                       
016130             OR IN-EKH-KDFRAKT = 16                                       
016140             OR IN-EKH-KDFRAKT = 17                                       
016150             OR IN-EKH-KDFRAKT = 18                                       
016160             OR IN-EKH-KDFRAKT = 19                                       
016180               MOVE 'AIR' TO UT-FREIGHT-MODE                              
016191             ELSE                                                         
016192               IF IN-EKH-KDFRAKT = 41                                     
016193               OR IN-EKH-KDFRAKT = 42                                     
016194               OR IN-EKH-KDFRAKT = 43                                     
016195               OR IN-EKH-KDFRAKT = 44                                     
016196               OR IN-EKH-KDFRAKT = 45                                     
016197                 MOVE 'OCEAN' TO UT-FREIGHT-MODE                          
016199               END-IF                                                     
016200             END-IF                                                       
016202             PERFORM S11-WRITE-W56175                                     
016203           ELSE                                                           
016204             IF (IN-EKH-KDEKNIVA = 'EMB' OR 'FÖRS' OR 'FRAKT')            
016205               MOVE IN-EKH-SUBEL            TO UT-TOTAL-PRICE             
016206               EVALUATE IN-EKH-KDEKNIVA                                   
016207                 WHEN 'EMB'                                               
016208                   MOVE 'PACKING AND HANDLING'                            
016209                             TO UT-DESCRIPTION                            
016210                 WHEN 'FÖRS'                                              
016211                   MOVE 'INSURANCE'         TO UT-DESCRIPTION             
016212                 WHEN 'FRAKT'                                             
016213                   MOVE 'FRIEGHT'           TO UT-DESCRIPTION             
016214                   MOVE IN-EKH-KDFRAKT      TO UT-KDFRAKT                 
016215               END-EVALUATE                                               
016216               MOVE IN-EKH-IDVERGL          TO WS-IDFAKT                  
016217               MOVE WS-IDFAKT               TO UT-IDFAKT                  
016218               MOVE IN-FIL-TIREGDAT         TO UT-TIREGDAT                
016219               MOVE IN-EKH-IDDC-SEND        TO UT-IDDC-SEND               
016220               MOVE IN-EKH-IDDC-REC         TO UT-IDDC-RECV               
016221               MOVE IN-EKH-KDPSLLOC         TO UT-KDPSLLOC                
016225                                                                          
016226               PERFORM BA-LANDED-COST                                     
016227               PERFORM S11-WRITE-W56175                                   
016228             END-IF                                                       
016515           END-IF                                                         
016516         END-IF                                                           
016517       END-IF                                                             
016518     END-IF                                                               
016519     .                                                                    
016520     EJECT                                                                
016530 BA-LANDED-COST SECTION.                                                  
016540                                                                          
016550     MOVE 1                          TO WS-MARKUP                         
016560     MOVE NOO                        TO WS-PROD-SW                        
016570                                                                          
016580     PERFORM                                                              
016590       VARYING IX FROM 1 BY 1                                             
016591       UNTIL IX > MARKUP-TAB-MAX OR                                       
016592         PRODKOD-FOUND                                                    
016593         IF MARKUP-LPC (IX) = IN-EKH-KDPSLLOC                             
016595         SET PRODKOD-FOUND        TO TRUE                                 
016596           MOVE IX                  TO IX-MARKUP                          
016597         END-IF                                                           
016598     END-PERFORM                                                          
016599                                                                          
016600     IF PRODKOD-FOUND                                                     
016601        MOVE MARKUP-FAKTOR-USA (IX-MARKUP) TO WS-MARKUP                   
016602     END-IF                                                               
016609     COMPUTE WS-LANDED-COST ROUNDED = (IN-EKH-PRARTNTO *                  
016610                IN-EKH-KVANTAL / WS-PRKURS-US) * (WS-MARKUP - 1)          
016611                                                                          
016612                                                                          
016616     ON SIZE ERROR                                                        
016617        MOVE 0                  TO WS-LANDED-COST                         
016621     END-COMPUTE                                                          
016623        MOVE WS-LANDED-COST           TO UT-SULANDCO                      
016624        MOVE WS-MARKUP                TO UT-RELANDCO                      
016630     .                                                                    
016631     EJECT                                                                
016640 Z-FINIT SECTION.                                                         
016700     CLOSE W56166                                                         
016800           W56175                                                         
016900     SKIP2                                                                
017000     MOVE 'S' TO POSTSUM-OPKOD                                            
017100     CALL POSTSUM USING POSTSUM-PARM                                      
017200     .                                                                    
017300     EJECT                                                                
017400 S01-READ-W56166  SECTION.                                                
017500     READ W56166 INTO IN-AREA                                             
017600     AT END                                                               
017700        MOVE HIGH-VALUE TO IN-AREA                                        
017800        SET END-OF-W56166 TO TRUE                                         
017900                                                                          
018000     NOT AT END                                                           
018100        MOVE 'W56166' TO POSTSUM-FDNAMN                                   
018200        MOVE 'W56175D1' TO POSTSUM-DDNAMN2                                
018300        MOVE SPACES    TO POSTSUM-TRANSTYP                                
018400        CALL POSTSUM USING POSTSUM-PARM                                   
018500     END-READ                                                             
018600     .                                                                    
018700     EJECT                                                                
018800 S11-WRITE-W56175 SECTION.                                                
018900                                                                          
019000     WRITE UT-RECORD FROM UT-AREA                                         
019100                                                                          
019200     MOVE SPACES    TO POSTSUM-TRANSTYP                                   
019300     MOVE 'W56175' TO POSTSUM-FDNAMN                                      
019400     MOVE 'W56175D2' TO POSTSUM-DDNAMN2                                   
019500     CALL POSTSUM USING POSTSUM-PARM                                      
019600     .                                                                    
019700     EJECT                                                                
019800 S99-ABEND SECTION.                                                       
019900                                                                          
020000     SKIP2                                                                
020100     MOVE 'S' TO POSTSUM-OPKOD                                            
020200     CALL POSTSUM USING POSTSUM-PARM                                      
020300     CALL ABEND USING RKOD-ABEND                                          
020400     .                                                                    
