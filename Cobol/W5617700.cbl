000100 ID DIVISION.                                                             
000200 PROGRAM-ID. W5617700.                                                    
000300 AUTHOR. RAGUR SATHEESH.                                                  
000400 DATE-WRITTEN. 19/03/18.                                                  
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*    DAILY REFILL REPORT REQUESTED BY USA                                 
001000*    NEW DAILY REPORT CONTAINING INFORMATION ABOUT REFILL                 
001100*    REQUESTED BY USA.                                                    
001200                                                                          
001300                                                                          
001400*    ABENDCODES:                                                          
001500*        U0016 - . . . .                                                  
001600*        U1000 - . . . .                                                  
001700                                                                          
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- LATEST DYNAMIC BOOKING EVENTS                              
002700     SELECT W56166               ASSIGN TO W56177D1.                      
002800     SKIP2                                                                
002900*    --- DAILY REFILL REPORT FOR US                                       
003000     SELECT W56177               ASSIGN TO W56177D2.                      
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD W56166                                                                
003700     RECORDING F                                                          
003800     BLOCK CONTAINS 0.                                                    
003900                                                                          
004000*01  -COPY WDR801 -L.                                                     
004100     SKIP3                                                                
004200 FD W56177                                                                
004300     RECORDING F                                                          
004400     BLOCK CONTAINS 0.                                                    
004500                                                                          
004600*01  RECORD -COPY W56177 -PRE UT- -L.                                     
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000 77 IDPGM                  PIC X(8)    VALUE 'W5617700'.                  
005100 77 YES                    PIC X       VALUE 'J'.                         
005200 77 NOO                    PIC X       VALUE 'N'.                         
005300                                                                          
005400 77 W56166-EOF-SW          PIC X       VALUE 'N'.                         
005500     88 END-OF-W56166                  VALUE 'J'.                         
005700 77  WS-TIREGDAT           PIC 9(6)    VALUE 0.                           
005800 77  WS-PRKURS-US          PIC S9(6)V9(5)    COMP-3 VALUE 0.              
005810 77  W-DATE-AAMM           PIC 9(4)    VALUE ZERO.                        
005820 77  WS-KDVALISO-HUV       PIC X(3)    VALUE 'SEK'.                       
005900 77  FELTEXT               PIC X(80)   VALUE SPACES.                      
006000     EJECT                                                                
006100 01 TODAYS-DATE            PIC 9(6)    VALUE ZERO.                        
006200 01 FILLER REDEFINES TODAYS-DATE.                                         
006300     03 TODAYS-DATE-YEAR PIC 9(2).                                        
006400     03 TODAYS-DATE-MONTH PIC 9(2).                                       
006500     03 TODAYS-DATE-DAY PIC 9(2).                                         
006600     EJECT                                                                
006700 01 GENERAL-SUBPROGRAMS.                                                  
006800                                                                          
006900     03 ABEND              PIC X(8)    VALUE 'ABEND'.                     
007000     03 POSTSUM            PIC X(8)    VALUE 'POSTSUM'.                   
007100     03 CBLTDLI            PIC X(8)    VALUE 'CBLTDLI '.                  
007200     03 FELLOG             PIC X(8)    VALUE 'FELLOG  '.                  
007210     03 W510CURR           PIC X(8)    VALUE 'W510CURR'.                  
007300     SKIP2                                                                
007400*    --- PARAMETERS TO ABEND                                              
007500                                                                          
007600 77 RKOD-ABEND             PIC S9(4) COMP VALUE +0.                       
007700 77 RKOD-ABEND-NO-DUMP     PIC S9(4) COMP VALUE +16.                      
007800 77 RKOD-ABEND-WITH-DUMP   PIC S9(4) COMP VALUE +1000.                    
007900     SKIP2                                                                
008000 01 ERROR-TEXT.                                                           
008100     03 FILLER             PIC X(10) VALUE 'ERROR-TEXT'.                  
008200     03 ERROR-TEXT-STR     PIC X(72) VALUE SPACE.                         
008300     EJECT                                                                
008400*    --- PARAMETRAR TILL POSTSUM                                          
008500                                                                          
008600*01  -COPY W0005 -PRE POSTSUM-                                            
008700*01  -COPY WWDC99                                                         
008710*01  -COPY W510CURR                                                       
008800                                                                          
008900     EJECT                                                                
009000 01 IN-AREA-START          PIC X(24)   VALUE                              
009100                                 'IN-AREA-START  '.                       
009200     SKIP2                                                                
009300                                                                          
009400     SKIP2                                                                
009500*01  AREA -COPY WDR801     -PRE IN-                                       
009600*    05   -COPY W510EKHA   -PRE IN- -RED IN-FIL-WDR801-DATA               
009700     EJECT                                                                
009800 01 UT-AREA-START          PIC X(24)   VALUE                              
009900                                 'UT-AREA-START  '.                       
010000     SKIP2                                                                
010100                                                                          
010200*01  AREA -COPY W56177     -PRE UT-                                       
010300     EJECT                                                                
013000 LINKAGE SECTION.                                                         
013100*01 -COPY W0008 -PRE WDG2-                                                
013200     05 FILLER              PIC X.                                        
013300                                                                          
013400 PROCEDURE DIVISION USING WDG2-PCB.                                       
013500 MAIN SECTION.                                                            
013600     ENTRY 'DLITCBL' USING WDG2-PCB.                                      
013700     SKIP2                                                                
013800     PERFORM A-INIT                                                       
013900     PERFORM S01-READ-W56166                                              
014000     PERFORM UNTIL END-OF-W56166                                          
014100       PERFORM B-PROCESS-DATA                                             
014200       PERFORM S01-READ-W56166                                            
014300     END-PERFORM                                                          
014400                                                                          
014500                                                                          
014600     PERFORM Z-FINIT                                                      
014700                                                                          
014800     MOVE ZERO TO RETURN-CODE                                             
014900     GOBACK                                                               
015000     .                                                                    
015100     EJECT                                                                
015200 A-INIT SECTION.                                                          
015300                                                                          
015400     OPEN INPUT W56166                                                    
015500                                                                          
015600     OPEN OUTPUT W56177                                                   
015700     SKIP2                                                                
015800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015900     .                                                                    
016000     EJECT                                                                
016100 B-PROCESS-DATA SECTION.                                                  
016200     MOVE SPACES TO UT-FREIGHT-MODE                                       
016210     MOVE ZERO   TO UT-IDVERGL                                            
016220     MOVE SPACES TO UT-IDDC-REC                                           
016300     MOVE ZERO   TO UT-TOTAL-PRICE                                        
016310     MOVE ZERO   TO UT-TIREGDAT                                           
016400     MOVE ZERO   TO UT-PRFOERS                                            
016410     MOVE ZERO   TO UT-PRKURS                                             
016500     MOVE ZERO   TO UT-PREMBHNT                                           
016600     MOVE ZERO   TO UT-PRFRAKT                                            
016610     MOVE ZERO   TO UT-LINE-DDI                                           
016620     MOVE ZERO   TO UT-LINE-SUM                                           
016700     MOVE IN-EKH-IDDC-REC  TO WS-IDDC                                     
016800      IF NDC-US                                                           
016900       IF IN-EKH-KDEKHHT = '102'                                          
017000        IF IN-EKH-KDEKSHT = '120' OR IN-EKH-KDEKSHT = '130'               
017100          PERFORM BB-PROCESS-DET                                          
017200        END-IF                                                            
017300       END-IF                                                             
017400      END-IF                                                              
017500     .                                                                    
017600     EJECT                                                                
017700 BB-PROCESS-DET SECTION.                                                  
017900     IF IN-EKH-KDEKNIVA = 'DET'                                           
018000        MOVE IN-FIL-TIREGDAT       TO WS-TIREGDAT                         
018100        MOVE WS-TIREGDAT(1:2)      TO W-DATE-AAMM(1:2)                    
018110        MOVE WS-TIREGDAT(3:2)      TO W-DATE-AAMM(3:2)                    
018200        MOVE 'USD'                 TO CURR-KDVALISO-ROW                   
018210        MOVE W-DATE-AAMM           TO CURR-TIAAMM                         
018220        MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                   
018230        MOVE 'M'                   TO CURR-KDVALTYP                       
018300        CALL W510CURR USING CURR-W510CURR WDG2-PCB                        
018400        IF CURR-KDSVAR = ' '                                              
018500          MOVE CURR-PRKURS-NEW     TO WS-PRKURS-US                        
018510        ELSE                                                              
018520          MOVE 1                   TO WS-PRKURS-US                        
018600        END-IF                                                            
018700        MOVE IN-EKH-IDDC-REC       TO UT-IDDC-REC                         
018800        MOVE IN-FIL-TIREGDAT       TO UT-TIREGDAT                         
018900        MOVE IN-EKH-IDVERGL        TO UT-IDVERGL                          
019100        MOVE IN-EKH-PRKURS         TO UT-PRKURS                           
019200        COMPUTE UT-TOTAL-PRICE ROUNDED =                                  
019300                         (IN-EKH-PRARTNTO * IN-EKH-KVANTAL) /             
019400                          WS-PRKURS-US                                    
019500                                                                          
019600       IF IN-EKH-KDFRAKT = 13                                             
019700       OR IN-EKH-KDFRAKT = 14                                             
019800       OR IN-EKH-KDFRAKT = 16                                             
019900       OR IN-EKH-KDFRAKT = 17                                             
020000       OR IN-EKH-KDFRAKT = 18                                             
020100       OR IN-EKH-KDFRAKT = 19                                             
020200                                                                          
020300          MOVE 'AIR' TO UT-FREIGHT-MODE                                   
020400       ELSE                                                               
020500         IF IN-EKH-KDFRAKT = 41                                           
020600         OR IN-EKH-KDFRAKT = 42                                           
020700         OR IN-EKH-KDFRAKT = 43                                           
020800         OR IN-EKH-KDFRAKT = 44                                           
020900         OR IN-EKH-KDFRAKT = 45                                           
021000            MOVE 'OCEAN' TO UT-FREIGHT-MODE                               
021100         END-IF                                                           
021200       END-IF                                                             
021202                                                                          
021400        PERFORM S11-WRITE-W56177                                          
021500     ELSE                                                                 
021600        PERFORM BC-PROCESS-EMB-FORS-FRAKT                                 
021700     END-IF                                                               
021800     .                                                                    
021900     EJECT                                                                
022000 BC-PROCESS-EMB-FORS-FRAKT SECTION.                                       
022100                                                                          
022200     IF (IN-EKH-KDEKNIVA = 'EMB' )                                        
022310       MOVE IN-EKH-IDDC-REC       TO UT-IDDC-REC                          
022320       MOVE IN-FIL-TIREGDAT       TO UT-TIREGDAT                          
022330       MOVE IN-EKH-IDVERGL        TO UT-IDVERGL                           
022350       MOVE IN-EKH-PRKURS         TO UT-PRKURS                            
022360       COMPUTE UT-PREMBHNT ROUNDED =                                      
022370                         (IN-EKH-SUBEL / WS-PRKURS-US  )                  
022400       PERFORM S11-WRITE-W56177                                           
022500     END-IF                                                               
022600     IF (IN-EKH-KDEKNIVA = 'FÖRS')                                        
022610        MOVE IN-EKH-IDDC-REC       TO UT-IDDC-REC                         
022620        MOVE IN-FIL-TIREGDAT       TO UT-TIREGDAT                         
022630        MOVE IN-EKH-IDVERGL        TO UT-IDVERGL                          
022650        MOVE IN-EKH-PRKURS         TO UT-PRKURS                           
022660        COMPUTE UT-PRFOERS ROUNDED =                                      
022670                         (IN-EKH-SUBEL /  WS-PRKURS-US )                  
022690                                                                          
022800        PERFORM S11-WRITE-W56177                                          
022900     END-IF                                                               
023000     IF (IN-EKH-KDEKNIVA = 'FRAKT')                                       
023010        MOVE IN-EKH-IDDC-REC       TO UT-IDDC-REC                         
023020        MOVE IN-FIL-TIREGDAT       TO UT-TIREGDAT                         
023030        MOVE IN-EKH-IDVERGL        TO UT-IDVERGL                          
023050        MOVE IN-EKH-PRKURS         TO UT-PRKURS                           
023060        COMPUTE UT-PRFRAKT ROUNDED =                                      
023070                         (IN-EKH-SUBEL / WS-PRKURS-US)                    
023080        IF IN-EKH-KDFRAKT = 13                                            
023090        OR IN-EKH-KDFRAKT = 14                                            
023100        OR IN-EKH-KDFRAKT = 16                                            
023110        OR IN-EKH-KDFRAKT = 17                                            
023120        OR IN-EKH-KDFRAKT = 18                                            
023130        OR IN-EKH-KDFRAKT = 19                                            
023140                                                                          
023150          MOVE 'AIR' TO UT-FREIGHT-MODE                                   
023170        ELSE                                                              
023180         IF IN-EKH-KDFRAKT = 41                                           
023190         OR IN-EKH-KDFRAKT = 42                                           
023191         OR IN-EKH-KDFRAKT = 43                                           
023192         OR IN-EKH-KDFRAKT = 44                                           
023193         OR IN-EKH-KDFRAKT = 45                                           
023194            MOVE 'OCEAN' TO UT-FREIGHT-MODE                               
023196         END-IF                                                           
023197        END-IF                                                            
023198        PERFORM S11-WRITE-W56177                                          
023300     END-IF                                                               
023400     IF (IN-EKH-KDEKNIVA = 'DDI')                                         
023410        MOVE IN-EKH-IDDC-REC       TO UT-IDDC-REC                         
023420        MOVE IN-FIL-TIREGDAT       TO UT-TIREGDAT                         
023430        MOVE IN-EKH-IDVERGL        TO UT-IDVERGL                          
023450        MOVE IN-EKH-PRKURS         TO UT-PRKURS                           
023451        MOVE IN-EKH-SUBEL          TO UT-LINE-DDI                         
023800        PERFORM S11-WRITE-W56177                                          
023900     END-IF                                                               
024000     IF (IN-EKH-KDEKNIVA = 'SUM')                                         
024014        MOVE IN-EKH-IDDC-REC       TO UT-IDDC-REC                         
024020        MOVE IN-FIL-TIREGDAT       TO UT-TIREGDAT                         
024030        MOVE IN-EKH-IDVERGL        TO UT-IDVERGL                          
024050        MOVE IN-EKH-PRKURS         TO UT-PRKURS                           
024060        COMPUTE UT-LINE-SUM ROUNDED =                                     
024070                         (IN-EKH-SUBEL  / WS-PRKURS-US)                   
024400       PERFORM S11-WRITE-W56177                                           
024500     END-IF                                                               
026800     .                                                                    
026900     EJECT                                                                
027000 Z-FINIT SECTION.                                                         
027100     CLOSE W56166                                                         
027200           W56177                                                         
027300     SKIP2                                                                
027400     MOVE 'S' TO POSTSUM-OPKOD                                            
027500     CALL POSTSUM USING POSTSUM-PARM                                      
027600     .                                                                    
027700     EJECT                                                                
027800 S01-READ-W56166 SECTION.                                                 
027900     READ W56166 INTO IN-AREA                                             
028000     AT END                                                               
028100        MOVE HIGH-VALUE TO IN-AREA                                        
028200        SET END-OF-W56166 TO TRUE                                         
028300                                                                          
028400     NOT AT END                                                           
028500        MOVE 'W56166' TO POSTSUM-FDNAMN                                   
028600        MOVE 'W56177D1' TO POSTSUM-DDNAMN2                                
028700        MOVE SPACES TO POSTSUM-TRANSTYP                                   
028800        CALL POSTSUM USING POSTSUM-PARM                                   
028900     END-READ                                                             
029000     .                                                                    
029100     EJECT                                                                
029200 S11-WRITE-W56177 SECTION.                                                
029300                                                                          
029400     WRITE UT-RECORD FROM UT-AREA                                         
029500                                                                          
029600     MOVE SPACES    TO POSTSUM-TRANSTYP                                   
029700     MOVE 'W56177' TO POSTSUM-FDNAMN                                      
029800     MOVE 'W56177D2' TO POSTSUM-DDNAMN2                                   
029900     CALL POSTSUM USING POSTSUM-PARM                                      
030000     MOVE '  '      TO  UT-AREA                                           
030100     .                                                                    
030200     EJECT                                                                
