001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W6118G00.                                                
001400 AUTHOR.         UMESH JAIN.                                              
001500 DATE-WRITTEN.   09/12/21.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNCTION:                                                            
002000*        INSERT PART NUMBERS INTO TO WDR551 FOR ALL THE PART              
002100*        NUMBERS WHOES GATE(ADINPORT) HAS BE CHANGED. THIS IS TO          
002200*        UPDATE THE INFORMATION FOR THE SUPLLIERS                         
002300*                                                                         
002401*        THE PROGRAM READS     WDD9                                       
002402*        THE PROGRAM READS     WDR2 2216                                  
002403*        THE PROGRAM READS     WDK6                                       
002410*        THE PROGRAM UPDATES   WDR5 2218                                  
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- PART NUMBERS WHOES GATE HAVE BEEN CHANGED(ADINPORT)        
003310     SELECT W6118C                     ASSIGN TO W6118GD1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W6118C                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003910*01  -COPY W6118C01      -L.                                              
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W6118G00'.            
004400 01  CHKP-VAR.                                                            
004500     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004800     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004900     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005000     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005001*                                                                         
005010*    03  WDGX2216    -COPY WDGX2216 -PRE W-.                              
005020     EJECT                                                                
005030*                                          REG-INFO FÖR KONTOLL AV        
005100 77  YES                         PIC X       VALUE 'J'.                   
005200 77  NOO                         PIC X       VALUE 'N'.                   
005210*01  -COPY WWDCKONS                                                       
005300     SKIP2                                                                
005310*    -COPY WY2000W1                                                       
005320     SKIP3                                                                
005400 01  ERROR-TEXT.                                                          
005500     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
005600     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005801                                                                          
005802 77  W6118C-EOF-SW               PIC X       VALUE 'N'.                   
005810     88  END-OF-W6118C                       VALUE 'Y'.                   
006100     EJECT                                                                
006200 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006300 01  FILLER REDEFINES TODAYS-DATE.                                        
006400     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006500     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006600     03  TODAYS-DATE-DAY         PIC 9(2).                                
006700     EJECT                                                                
006800 01  GENERAL-SUBPROGRAMS.                                                 
006900*                                                                         
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007210     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007301     EJECT                                                                
007302*    --- PARAMETRAR TILL POSTSUM                                          
007303*                                                                         
007310*01  -COPY W0005   -PRE  POSTSUM-                                         
007601     EJECT                                                                
007610*01  AREA -COPY W6118C01     -PRE IN-                                     
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  KEYS-TILL-DLI.                                                       
008201     03  W-IDARTNR-X.                                                     
008202         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008203     03  W-KDSEGKEY-X.                                                    
008204         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
008205     03  W-IDLEVNR-X.                                                     
008206         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
008207     03  W-WDD905KY-X.                                                    
008208         05  W-WDD905KY          PIC X(7)    VALUE SPACE.                 
008240     03  W-WDGXKEY-X.                                                     
008250          05  W-WDGXKEY          PIC X(04)   VALUE SPACE.                 
008260          05  FILLER             PIC X(26)   Value LOW-Value.             
008270     03  W-WDGXKEY-2217-X.                                                
008280         05  FILLER              PIC X(4)    VALUE '2217'.                
008291         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
008292     03  W-WDGXKEY-PERIOD-ROT.                                            
008293         05  FILLER              PIC X(4)    VALUE '2217'.                
008294         05  FILLER              PIC X(1)    VALUE 'J'.                   
008295         05  FILLER              PIC X(25)   VALUE LOW-VALUE.             
008296     03  W-WDD901KY-X.                                                    
008297         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
008298         05  W-IDDC-D9           PIC  X(2)   VALUE SPACE.                 
008300     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FOUND                       VALUE '  '.                  
008700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008900     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
009000     88  IMS-NOT-OK                          VALUE 'XD'.                  
009100     SKIP2                                                                
009200 01  GOOD-STATUSCODES.                                                    
009300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009400     SKIP3                                                                
009500 01  SSA1                        PIC X(64).                               
009600 01  SSA2                        PIC X(64).                               
009700     EJECT                                                                
009800*    --- IMS FUNCTION CODES                                               
009900*01  -COPY W0003                                                          
010000     EJECT                                                                
010200*    ---  DLI INPUT-OUTPUT AREA                                           
010300                                                                          
010401 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
010402 01  DLI-IO-WDD901.                                                       
010403*    03  -COPY WDD901 -PRE WDD901-                                        
010404*                                                                         
010405 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
010406 01  DLI-IO-WDD902.                                                       
010407*    03  -COPY WDD902 -PRE WDD902-                                        
010408*                                                                         
010409 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
010410 01  DLI-IO-WDD905.                                                       
010411*    03  -COPY WDD905  -PRE WDD905-                                       
010412*                                                                         
010413 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR242'.                      
010414 01  DLI-IO-WDR242.                                                       
010415*    03  WDGX2216  -COPY WDGX2216                                         
010416*                                                                         
010417 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
010418 01  DLI-IO-WDK611.                                                       
010419*    03  -COPY WDK611 -PRE WDK611-                                        
010420*                                                                         
010421 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR551'.                      
010422 01  DLI-IO-WDR551.                                                       
010423*    03  -COPY  WDGX2218                                                  
010430*                                                                         
010900     EJECT                                                                
011000 LINKAGE SECTION.                                                         
011100                                                                          
011200*01  -COPY W0009   -PRE MSG-                                              
011301                                                                          
011302*01  -COPY W0008  -PRE WDD9-                                              
011303     05  FILLER                  PIC X.                                   
011304                                                                          
011305*01  -COPY W0008  -PRE WDR2-                                              
011306     05  FILLER                  PIC X.                                   
011307                                                                          
011700*01  -COPY W0008  -PRE WDK6-                                              
011701     05  FILLER                  PIC X.                                   
011702                                                                          
011703*01  -COPY W0008  -PRE WDR5-                                              
011704     05  FILLER                  PIC X.                                   
011705                                                                          
011706     EJECT                                                                
011707 PROCEDURE DIVISION  USING MSG-PCB WDD9-PCB WDR2-PCB WDK6-PCB             
011708     WDR5-PCB.                                                            
011709 MAIN SECTION.                                                            
011710     ENTRY 'DLITCBL' USING MSG-PCB WDD9-PCB WDR2-PCB WDK6-PCB             
011720     WDR5-PCB.                                                            
011800                                                                          
012100     PERFORM A-INIT                                                       
012210     PERFORM S01-READ-W6118C                                              
012300     PERFORM UNTIL END-OF-W6118C                                          
012400       IF CHKP-ANT > CHKP-MAX                                             
012500         PERFORM X-TAKE-CHECKPOINT                                        
012600       END-IF                                                             
012700       MOVE IN-IDARTNR TO W-IDARTNR                                       
012710                          W-IDARTNR-D9                                    
012800       MOVE WC-CDC-SE  TO W-IDDC-D9                                       
012810       PERFORM IMS-GU-WDD901                                              
012900       IF SEGMENT-FOUND                                                   
012910         PERFORM IMS-GNP-WDD902                                           
013000         PERFORM UNTIL SEGMENT-MISSING                                    
013001           MOVE WDD902-IDLEVNR TO W-IDLEVNR                               
013010           PERFORM IMS-GNP-WDD905                                         
013020           IF SEGMENT-FOUND                                               
013030             IF WDD905-KDAVROP = 2                                        
013031               PERFORM IMS-GU-WDK611                                      
013032               IF SEGMENT-FOUND AND WDK611-CLAG-KDHF = 0                  
013033                 MOVE SPACE        TO DLI-IO-WDR242                       
013040                 MOVE '2215'       TO W-WDGXKEY                           
013100                 PERFORM IMS-GU-WDR242                                    
013110                 IF SEGMENT-FOUND AND 2216-KDEDI NOT = 'T'                
013121                   MOVE WDGX2216   TO W-WDGX2216                          
013122                   MOVE SPACE      TO DLI-IO-WDR242                       
013123                   MOVE '2217'     TO W-WDGXKEY                           
013130                   MOVE IN-IDARTNR TO 2218-IDARTNR                        
013140                   MOVE W-IDLEVNR  TO 2218-IDLEVNR                        
013170                   MOVE WDK611-CLAG-IDANSK TO 2218-IDANSK                 
013171                   IF (W-2216-KDVECKOSL NOT = 'P')  AND                   
013172                      (2218-IDLEVNR     NOT = 'BP8HB') AND                
013173                      (2218-IDLEVNR     NOT = 'BKTVA')                    
013180                     PERFORM IMS-ISRT-WDR551                              
013181                   END-IF                                                 
013182                                                                          
013183                   IF W-2216-TISEND-PER NOT NUMERIC                       
013184                      MOVE ZERO TO W-2216-TISEND-PER                      
013185                   END-IF                                                 
013186                                                                          
013187                   MOVE W-2216-TISEND-PER   TO TMP1-YYMMDD                
013188                   MOVE W-2216-TISEND-SEN   TO TMP2-YYMMDD                
013189                   PERFORM WY2000P1                                       
013190                   IF W-2216-FLLEVPLP = YES OR                            
013191                      W-2216-FLLEVVB  = YES OR                            
013192                      (W-2216-TISEND-PER > ZERO AND                       
013193                      TMP1-YYMMDD > TMP2-YYMMDD)                          
013194                     PERFORM IMS-INSERT-WDR551-PERIOD-SEG                 
013199                   END-IF                                                 
013201                 END-IF                                                   
013203               END-IF                                                     
013204             END-IF                                                       
013205           END-IF                                                         
013300           PERFORM IMS-GNP-WDD902                                         
013400         END-PERFORM                                                      
013500       END-IF                                                             
013510       PERFORM S01-READ-W6118C                                            
013600     END-PERFORM                                                          
013610                                                                          
013700     PERFORM Z-FINIT                                                      
013800                                                                          
013900     MOVE ZERO TO RETURN-CODE                                             
014000     GOBACK                                                               
014100     .                                                                    
014200     EJECT                                                                
014300 A-INIT SECTION.                                                          
014600     PERFORM IMS-RESTART                                                  
014801                                                                          
014810     OPEN INPUT W6118C                                                    
015510     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015800     .                                                                    
016000     EJECT                                                                
016100 Z-FINIT SECTION.                                                         
016610     CLOSE W6118C                                                         
016801     SKIP2                                                                
016802     MOVE 'S' TO POSTSUM-OPKOD                                            
016810     CALL POSTSUM USING POSTSUM-PARM                                      
017000     .                                                                    
017101     EJECT                                                                
017102 S01-READ-W6118C  SECTION.                                                
017104     READ W6118C INTO IN-AREA                                             
017105     AT END                                                               
017107        SET END-OF-W6118C TO TRUE                                         
017108                                                                          
017109     NOT AT END                                                           
017110        MOVE 'W6118C' TO POSTSUM-FDNAMN                                   
017111        MOVE 'W6118GD1' TO POSTSUM-DDNAMN2                                
017112        MOVE SPACE TO POSTSUM-TRANSTYP                                    
017113        CALL POSTSUM USING POSTSUM-PARM                                   
017114                                                                          
017116     END-READ                                                             
017120     .                                                                    
017400     EJECT                                                                
017500 X-TAKE-CHECKPOINT   SECTION.                                             
017700* --- AT CHECKPOINT YOU LOSE GN-POSITION IN THE BASE                      
017800* --- SAVE DATABASE KEYS IF NECESSARY                                     
018200     PERFORM IMS-CHECKPOINT                                               
018300     MOVE ZERO TO CHKP-ANT                                                
018400* --- REREAD DATABASE IF NECESSARY                                        
018500     .                                                                    
018600     EJECT                                                                
018700* --- IMS SECTIONS  ---                                                   
018800                                                                          
018901     EJECT                                                                
018902 IMS-GU-WDK611 SECTION.                                                   
018903     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
018904          DELIMITED BY SIZE INTO SSA1                                     
018905     STRING 'WDK611  (KDSEGKEY =' W-kDSEGKEY-X ')'                        
018906          DELIMITED BY SIZE INTO SSA2                                     
018907     MOVE '  GE' TO GOOD-STATUSCODES                                      
018908     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
018909     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
018910     PERFORM IMS-STATUSCHECK                                              
018920     .                                                                    
018921     EJECT                                                                
018922 IMS-GU-WDD901 SECTION.                                                   
018924     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
018925          DELIMITED BY SIZE INTO SSA1                                     
018928     MOVE 'GE  ' TO GOOD-STATUSCODES                                      
018929     CALL CBLTDLI USING GU WDD9-PCB DLI-IO-WDD901 SSA1                    
018930     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
018931     PERFORM IMS-STATUSCHECK                                              
018932     .                                                                    
018933     SKIP3                                                                
018934 IMS-GNP-WDD902 SECTION.                                                  
018939     MOVE 'WDD902' TO SSA1                                                
018940     MOVE 'GE  ' TO GOOD-STATUSCODES                                      
018941     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD902 SSA1                   
018942     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
018943     PERFORM IMS-STATUSCHECK                                              
018944     .                                                                    
018945     SKIP3                                                                
018946 IMS-GNP-WDD905 SECTION.                                                  
018949     MOVE 'WDD905' TO SSA1                                                
018950     MOVE '  GE' TO GOOD-STATUSCODES                                      
018951     CALL CBLTDLI USING GNP WDD9-PCB DLI-IO-WDD905 SSA1                   
018952     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
018953     PERFORM IMS-STATUSCHECK                                              
018954     .                                                                    
018955     EJECT                                                                
018956 IMS-GU-WDR242 SECTION.                                                   
018957     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
018958          DELIMITED BY SIZE INTO SSA1                                     
018959     STRING 'WDR242  (IDLEVNR  =' W-IDLEVNR-X ')'                         
018960          DELIMITED BY SIZE INTO SSA2                                     
018961     MOVE '  GE' TO GOOD-STATUSCODES                                      
018962     CALL CBLTDLI USING GU  WDR2-PCB DLI-IO-WDR242 SSA1 SSA2              
018963     MOVE wdr2-STATUS-CODE TO STATUS-WS                                   
018964     PERFORM IMS-STATUSCHECK                                              
018965     .                                                                    
018966     EJECT                                                                
018985 IMS-ISRT-WDR551 SECTION.                                                 
018986     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-2217-X    ')'                 
018987          DELIMITED BY SIZE INTO SSA1                                     
018988     MOVE 'WDR551 ' TO SSA2                                               
018989     MOVE '  II' TO GOOD-STATUSCODES                                      
018990     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDR551 SSA1 SSA2             
018991     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
018992     PERFORM IMS-STATUSCHECK                                              
018993     .                                                                    
019000     EJECT                                                                
019010 IMS-INSERT-WDR551-PERIOD-SEG SECTION.                                    
019030     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-PERIOD-ROT ')'                
019040            DELIMITED BY SIZE INTO SSA1                                   
019050     MOVE 'WDR551 ' TO SSA2                                               
019060     MOVE '  II' TO GOOD-STATUSCODES                                      
019070     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-WDR551 SSA1 SSA2             
019080     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
019090     PERFORM IMS-STATUSCHECK                                              
019091     .                                                                    
019092     SKIP3                                                                
019100 IMS-RESTART SECTION.                                                     
019300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019400     MOVE '  ' TO GOOD-STATUSCODES                                        
019500     CALL CBLTDLI USING XRST MSG-PCB                                      
019600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019700                        CHKP-AREA-LENGTH CHKP-AREA                        
019800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019900     PERFORM IMS-STATUSCHECK                                              
020000     .                                                                    
020100     SKIP3                                                                
020200 IMS-CHECKPOINT SECTION.                                                  
020400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020500     MOVE '  XD' TO GOOD-STATUSCODES                                      
020600     CALL CBLTDLI USING CHKP MSG-PCB                                      
020700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020800                        CHKP-AREA-LENGTH CHKP-AREA                        
020900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021000     PERFORM IMS-STATUSCHECK                                              
021100                                                                          
021200     IF IMS-NOT-OK                                                        
021300       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
021310                                TO ERROR-TEXT-STR                         
021400       DISPLAY ERROR-TEXT                                                 
021500       CALL FELLOG                                                        
021600     END-IF                                                               
021700     .                                                                    
021800     EJECT                                                                
021900 IMS-STATUSCHECK SECTION.                                                 
022100     SET STATUS-IX TO 1                                                   
022200     SEARCH GOOD-STATUS                                                   
022300       AT END                                                             
022400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022500           DELIMITED BY SIZE INTO ERROR-TEXT                              
022600         DISPLAY ERROR-TEXT                                               
022700         CALL FELLOG                                                      
022800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
022900         CONTINUE                                                         
023000     END-SEARCH                                                           
023100     .                                                                    
023200*    -COPY WY2000P1                                                       
023300     EJECT                                                                
