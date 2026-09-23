000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W510PRMT.                                                
000300 AUTHOR.         SARASWATHY S.                                            
000400 DATE-WRITTEN.   20/03/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        CHECK NON VCCS FTG DC'S HAVE A FIXED PRICE                       
001000*                                                                         
001100*        THE PROGRAM READS     WDK7, WDB6                                 
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
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP2                                                                
002700 FILE SECTION.                                                            
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100 77  IDPGM                       PIC X(8)    VALUE 'W510PRMT'.            
003200 77  YES                         PIC X       VALUE 'J'.                   
003300 77  NOO                         PIC X       VALUE 'N'.                   
003400 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
003410 77  WS-I                        PIC 9(3)    VALUE ZERO.                  
003411 77  WS-I-MAX                    PIC 9(3)    VALUE 50.                    
003420 77  WS-IX                       PIC 9(3)    VALUE ZERO.                  
003421 77  WS-IX-MAX                   PIC 9(3)    VALUE 50.                    
003430 77  WS-IY                       PIC 9(3)    VALUE ZERO.                  
003440 77  WS-IY-MAX                   PIC 9(3)    VALUE 50.                    
003450 77  WS-CNT-CN                   PIC 9(3)    VALUE 0.                     
003460 77  WS-CNT-US                   PIC 9(3)    VALUE 0.                     
003471 77  WS-WDK7-CNT                 PIC 9(3)    VALUE 0.                     
003480 77  WS-I-CNT                    PIC 9(3)    VALUE 0.                     
003490 77  WS-IDDC-CHECK               PIC X(2)    VALUE SPACES.                
003500     EJECT                                                                
003600 77  FIXED-PRICE-SW              PIC X      VALUE 'N'.                    
003700     88  FIXED-PRICE                        VALUE 'J'.                    
003800     88  NOT-FIXED-PRICE                    VALUE 'N'.                    
003801 77  WDK724-SW                   PIC X      VALUE 'N'.                    
003802     88  WDK724-FOUND                       VALUE 'J'.                    
003803     88  WDK724-NOT-FOUND                   VALUE 'N'.                    
003804 01  WS-DCS-SAVE.                                                         
003805     03  WS-IDLEGSEL-SAVE OCCURS 50.                                      
003807        05 WS-DCS-IDLEGSEL   PIC X(4).                                    
003810 01  WS-W510PRDC.                                                         
003820     03  WS-PRDC-RESP OCCURS 50                                           
003830         INDEXED BY WS-IS.                                                
003840        05 WS-PRDC-IDDC      PIC X(2).                                    
003900 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004000 01  FILLER REDEFINES TODAYS-DATE.                                        
004100     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004200     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004300     03  TODAYS-DATE-DAY         PIC 9(2).                                
004400     EJECT                                                                
004500 01  GENERAL-SUBPROGRAMS.                                                 
004600*                                                                         
004700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004910     03  W510PRDC                PIC X(8)    VALUE 'W510PRDC'.            
005000     SKIP2                                                                
005100*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005600     SKIP2                                                                
005700 01  ERROR-TEXT.                                                          
005800     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
005900     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
006000     EJECT                                                                
006100*01  -COPY WWDC99                                                         
006110*01  -COPY W510PRDC                                                       
006200*    --- AREAS FOR IMS-SECTIONS                                           
006300*                                                                         
006400     EJECT                                                                
006500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
006600     SKIP3                                                                
006700 01  KEYS-FOR-DLI.                                                        
006800     03  W-IDARTNR-X.                                                     
006900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
006910     03  W-IDDC-X.                                                        
006920         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
006930     03  W-IDARTNR-K724-X.                                                
006940         05  W-IDARTNR-K724      PIC S9(9)   VALUE ZERO COMP-3.           
006950     03  W-IDDC-K724-X.                                                   
006960         05  W-IDDC-K724         PIC X(2)    VALUE SPACE.                 
007000     SKIP2                                                                
007100*    --- STATUS-KOD FRÅN IMS                                              
007200 01  STATUS-WS                   PIC XX.                                  
007300     88  SEGMENT-FOUND                       VALUE '  '.                  
007400     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
007500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
007510     88  SEGMENT-END-OF-DB                   VALUE 'GB'.                  
007600     SKIP2                                                                
007700 01  GOOD-STATUSCODES.                                                    
007800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007900     SKIP3                                                                
008000 01  SSA1                        PIC X(64).                               
008100 01  SSA2                        PIC X(64).                               
008110 01  SSA3                        PIC X(64).                               
008200     EJECT                                                                
008300*    --- IMS FUNCTION CODES                                               
008400*01  -COPY W0003                                                          
008500     EJECT                                                                
008600*    ---  DLI INPUT-OUTPUT AREA                                           
008700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
008800 01  DLI-IO-WDK701.                                                       
008900*    03  -COPY WDK701                                                     
009000     EJECT                                                                
009100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
009200 01  DLI-IO-WDK711.                                                       
009300*    03  -COPY WDK711                                                     
009400     EJECT                                                                
009500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK724'.                      
009600 01  DLI-IO-WDK724.                                                       
009700*    03  -COPY WDK724                                                     
009800     EJECT                                                                
009810 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
009820 01  DLI-IO-WDB601.                                                       
009830*    03  -COPY WDB601                                                     
009840     EJECT                                                                
009900 LINKAGE SECTION.                                                         
010000     SKIP2                                                                
010100 01  W510PRMT-AREA.                                                       
010200*    03  -COPY W510PRMT                                                   
010300     EJECT                                                                
010400*01  -COPY W0008  -PRE WDK7-                                              
010500     05  FILLER                  PIC X.                                   
010600     EJECT                                                                
010601*01  -COPY W0008  -PRE WDB6-                                              
010602     05  FILLER                  PIC X.                                   
010603     EJECT                                                                
010610*01  -COPY W0008  -PRE PRDC-WDB6-                                         
010620     05  FILLER                  PIC X.                                   
010630     EJECT                                                                
010700 PROCEDURE DIVISION  USING W510PRMT-AREA WDK7-PCB WDB6-PCB                
010710                           PRDC-WDB6-PCB.                                 
010800 MAIN SECTION.                                                            
010900                                                                          
011000     ACCEPT DAGENS-DATUM  FROM DATE                                       
011200                                                                          
011300     EVALUATE PRMT-KDCALL                                                 
011400                                                                          
011500       WHEN 010                                                           
011600          PERFORM A-READ-WDB601                                           
011700       WHEN OTHER                                                         
011800*** ERROR CODE ON KDCALL NOT AVAILABLE                                    
011810          CONTINUE                                                        
012000     END-EVALUATE                                                         
012100                                                                          
012200     MOVE ZERO TO RETURN-CODE                                             
012300     GOBACK                                                               
012400     .                                                                    
012500     EJECT                                                                
012600                                                                          
012610 A-READ-WDB601 SECTION.                                                   
012620     PERFORM IMS-GN-WDB601                                                
012623     PERFORM UNTIL SEGMENT-END-OF-DB                                      
012630       IF SEGMENT-FOUND                                                   
012631       AND DCS-FLMAINDC = YES                                             
012632        IF WS-I <= WS-I-MAX                                               
012635          ADD +1 TO WS-I                                                  
012636          ADD +1 TO WS-I-CNT                                              
012637          MOVE DCS-IDLEGSEL TO WS-DCS-IDLEGSEL(WS-I)                      
012638        END-IF                                                            
012639       ELSE                                                               
012640         MOVE NOO TO FIXED-PRICE-SW                                       
012641       END-IF                                                             
012642       PERFORM IMS-GN-WDB601                                              
012643     END-PERFORM                                                          
012644     MOVE 1 TO WS-I                                                       
012646     PERFORM WS-I-CNT TIMES                                               
012648       IF WS-DCS-IDLEGSEL(WS-I) = 'VCCS'                                  
012649         CONTINUE                                                         
012650       ELSE                                                               
012651         IF WS-DCS-IDLEGSEL(WS-I) = 'VCCN' OR 'VCUS'                      
012652** CALL SUBPROGRAM W510PRDC TO READ ALL DC THAT HAVE THE SAME             
012653** LEGAL SELLER IDLEGSEL                                                  
012654            IF WS-DCS-IDLEGSEL(WS-I) = 'VCUS'                             
012655               ADD +1 TO WS-CNT-US                                        
012656            END-IF                                                        
012657            IF WS-DCS-IDLEGSEL(WS-I) = 'VCCN'                             
012658               ADD +1 TO WS-CNT-CN                                        
012659            END-IF                                                        
012660            IF WS-DCS-IDLEGSEL(WS-I) = 'VCUS' AND WS-CNT-US <=1           
012661            OR WS-DCS-IDLEGSEL(WS-I) = 'VCCN' AND WS-CNT-CN <=1           
012662               MOVE 010                    TO PRDC-KDCALL                 
012663               MOVE WS-DCS-IDLEGSEL(WS-I)  TO PRDC-IDLEGSEL               
012665               CALL W510PRDC USING PRDC-W510PRDC PRDC-WDB6-PCB            
012666               IF PRDC-KDSVAR = '1'                                       
012667                 SET WS-IS TO 1                                           
012668                 MOVE 1 TO WS-IY                                          
012669                 PERFORM WS-IY-MAX TIMES                                  
012670                   IF PRDC-IDDC(WS-IY) NOT = SPACES                       
012672                     MOVE PRDC-IDDC(WS-IY) TO WS-PRDC-IDDC(WS-IS)         
012674                                              WS-IDDC                     
012675                                              WS-IDDC-CHECK               
012677                     SET WS-IS UP BY 1                                    
012678                     ADD +1 TO WS-IY                                      
012679                     PERFORM AA-VALIDATE-DC-PARTNO                        
012680                   END-IF                                                 
012682                 END-PERFORM                                              
012683*                PERFORM AA-VALIDATE-DC-PARTNO                            
012684               END-IF                                                     
012685             END-IF                                                       
012686         ELSE                                                             
012687           IF WS-WDK7-CNT = 0                                             
012688             PERFORM AB-VALIDATE-DC-PARTNO                                
012689           END-IF                                                         
012690         END-IF                                                           
012692       END-IF                                                             
012693       ADD +1 TO WS-I                                                     
012694     END-PERFORM                                                          
012695     .                                                                    
012696     EJECT                                                                
012720 AA-VALIDATE-DC-PARTNO  SECTION.                                          
012800* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
012900* VALIDATE IF CN, US HAVE THE PART                  *                     
013000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
013100     MOVE PRMT-IDARTNR        TO W-IDARTNR                                
013200     PERFORM IMS-GU-WDK701                                                
013300     IF SEGMENT-FOUND                                                     
013400       PERFORM IMS-GNP-WDK711                                             
013410*      PERFORM UNTIL SEGMENT-MISSING OR FIXED PRICE                       
013420       PERFORM UNTIL SEGMENT-MISSING                                      
013500        IF SEGMENT-FOUND                                                  
013620*        SET WS-IS TO 1                                                   
013630*        SEARCH WS-PRDC-RESP                                              
013680*          WHEN SLAG-IDDC =  WS-PRDC-IDDC(WS-IS)                          
013682         IF SLAG-IDDC = WS-IDDC-CHECK                                     
013684             MOVE SART-IDARTNR TO W-IDARTNR-K724-X                        
013685             MOVE SLAG-IDDC    TO W-IDDC-K724-X                           
013686             PERFORM AAA-GET-WDK724-PRICE                                 
013687             IF WDK724-FOUND                                              
013688               IF FIXED-PRICE                                             
013697                CONTINUE                                                  
013698               ELSE                                                       
013703                 MOVE SLAG-IDDC  TO W-IDDC                                
013704                 PERFORM IMS-GU-WDB601-DC                                 
013705                 IF SEGMENT-FOUND                                         
013706                 AND DCS-FLMAINDC = 'J'                                   
013707                   ADD +1          TO WS-IX                               
013708                   MOVE SLAG-IDDC  TO PRMT-IDDC(WS-IX)                    
013710                   MOVE '1'        TO PRMT-KDSVAR                         
013711                 END-IF                                                   
013712               END-IF                                                     
013713             ELSE                                                         
013714               IF SLAG-KVLS > 0                                           
013718                 MOVE YES TO FIXED-PRICE-SW                               
013719               ELSE                                                       
013723                 MOVE SLAG-IDDC  TO W-IDDC                                
013724                 PERFORM IMS-GU-WDB601-DC                                 
013725                 IF SEGMENT-FOUND                                         
013726                 AND DCS-FLMAINDC = 'J'                                   
013727                   ADD +1          TO WS-IX                               
013728                   MOVE SLAG-IDDC  TO PRMT-IDDC(WS-IX)                    
013730                   MOVE '1'        TO PRMT-KDSVAR                         
013731                 END-IF                                                   
013732               END-IF                                                     
013740             END-IF                                                       
013800         END-IF                                                           
015100        END-IF                                                            
015120        PERFORM IMS-GNP-WDK711                                            
015130       END-PERFORM                                                        
015410     ELSE                                                                 
015411       CONTINUE                                                           
015430     END-IF                                                               
015500     .                                                                    
015600     EJECT                                                                
015700                                                                          
015800 AAA-GET-WDK724-PRICE SECTION.                                            
015900                                                                          
015910     MOVE NOO TO FIXED-PRICE-SW                                           
015920     MOVE NOO TO WDK724-SW                                                
016000     PERFORM IMS-GNP-WDK724-FIRST                                         
016100     PERFORM UNTIL SEGMENT-MISSING OR FIXED-PRICE                         
016200       IF SEGMENT-FOUND                                                   
016210        MOVE YES TO WDK724-SW                                             
016300        IF SPRL-SUINLEV-PR > 0                                            
016400          MOVE YES TO FIXED-PRICE-SW                                      
016500        ELSE                                                              
016600          MOVE NOO TO FIXED-PRICE-SW                                      
016700        END-IF                                                            
017400      END-IF                                                              
017500      PERFORM IMS-GNP-WDK724-NEXT                                         
017600     END-PERFORM                                                          
017700     .                                                                    
017710 AB-VALIDATE-DC-PARTNO  SECTION.                                          
017720* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
017730* VALIDATE DC'S THAT HAVE ONLY ONE DC IN THE COUNTRY                      
017740* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
017750     MOVE PRMT-IDARTNR        TO W-IDARTNR                                
017760     PERFORM IMS-GU-WDK701                                                
017761     IF SEGMENT-FOUND                                                     
017762       PERFORM IMS-GNP-WDK711                                             
017770       PERFORM UNTIL SEGMENT-MISSING                                      
017792         IF SEGMENT-FOUND                                                 
017793           IF SLAG-KVLS > 0                                               
017794             CONTINUE                                                     
017795           ELSE                                                           
017800             MOVE SLAG-IDDC  TO W-IDDC                                    
017801             PERFORM IMS-GU-WDB601-DC                                     
017803             IF SEGMENT-FOUND                                             
017804             AND DCS-IDLEGSEL NOT = 'VCCS' AND 'VCCN' AND 'VCUS'          
017805             AND WS-IX <= WS-IX-MAX                                       
017807               ADD +1         TO WS-WDK7-CNT                              
017808               ADD +1         TO WS-IX                                    
017809               MOVE SLAG-IDDC TO PRMT-IDDC(WS-IX)                         
017811               MOVE '1'       TO PRMT-KDSVAR                              
017812             END-IF                                                       
017813           END-IF                                                         
017820         END-IF                                                           
017821         PERFORM IMS-GNP-WDK711                                           
017822       END-PERFORM                                                        
017890     END-IF                                                               
017891     .                                                                    
017892     EJECT                                                                
017910* --- IMS SECTIONS ---                                                    
018000     SKIP3                                                                
018010 IMS-GN-WDB601 SECTION.                                                   
018020                                                                          
018021     MOVE 'WDB601  ' TO SSA1                                              
018050     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
018060     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601 SSA1                    
018070     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
018080     PERFORM IMS-STATUSCHECK                                              
018090     .                                                                    
018091     SKIP3                                                                
018092 IMS-GU-WDB601-DC SECTION.                                                
018093                                                                          
018094     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
018095          DELIMITED BY SIZE INTO SSA1                                     
018096     MOVE '  GE'              TO GOOD-STATUSCODES                         
018097     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
018098     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
018099     PERFORM IMS-STATUSCHECK                                              
018105     .                                                                    
018106     SKIP3                                                                
018110 IMS-GU-WDK701 SECTION.                                                   
018200                                                                          
018300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
018400          DELIMITED BY SIZE INTO SSA1                                     
018500     MOVE '  GE'              TO GOOD-STATUSCODES                         
018600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
018700     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
018800     PERFORM IMS-STATUSCHECK                                              
018900     .                                                                    
019000     EJECT                                                                
019100 IMS-GNP-WDK711 SECTION.                                                  
019200     MOVE 'WDK711   '       TO SSA1                                       
019300     MOVE '  GE' TO GOOD-STATUSCODES                                      
019400     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
019500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
019600     PERFORM IMS-STATUSCHECK                                              
019700     .                                                                    
019800     EJECT                                                                
019810 IMS-GNP-WDK724-FIRST SECTION.                                            
019820     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K724-X ')'                    
019830          DELIMITED BY SIZE INTO SSA1                                     
019840     STRING 'WDK711  (IDDC     =' W-IDDC-K724-X     ')'                   
019850          DELIMITED BY SIZE INTO SSA2                                     
019860     MOVE 'WDK724  *F' TO SSA3                                            
019870     MOVE '  GE' TO GOOD-STATUSCODES                                      
019880     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1 SSA2 SSA3         
019890     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
019891     PERFORM IMS-STATUSCHECK                                              
019892     .                                                                    
019893     EJECT                                                                
019894 IMS-GNP-WDK724-NEXT SECTION.                                             
019895     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K724-X ')'                    
019896          DELIMITED BY SIZE INTO SSA1                                     
019897     STRING 'WDK711  (IDDC     =' W-IDDC-K724-X     ')'                   
019898          DELIMITED BY SIZE INTO SSA2                                     
019899     MOVE 'WDK724   ' TO SSA3                                             
019900     MOVE '  GE' TO GOOD-STATUSCODES                                      
019901     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1 SSA2 SSA3         
019902     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
019903     PERFORM IMS-STATUSCHECK                                              
019904     .                                                                    
019905     EJECT                                                                
019910*IMS-GNP-WDK724-NEXT SECTION.                                             
020000*    MOVE 'WDK724   ' TO SSA1                                             
020100*    MOVE '  GE' TO GOOD-STATUSCODES                                      
020200*    CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1                   
020300*    MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
020400*    PERFORM IMS-STATUSCHECK                                              
020500*    .                                                                    
020600*    EJECT                                                                
020700 IMS-STATUSCHECK SECTION.                                                 
020800                                                                          
020900     SET STATUS-IX TO 1                                                   
021000     SEARCH GOOD-STATUS                                                   
021100       AT END                                                             
021200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
021300           DELIMITED BY SIZE INTO ERROR-TEXT                              
021400         DISPLAY ERROR-TEXT                                               
021500         CALL FELLOG                                                      
021600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
021700         CONTINUE                                                         
021800     END-SEARCH                                                           
021900     .                                                                    
