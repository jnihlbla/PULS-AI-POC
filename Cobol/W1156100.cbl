001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W1156100.                                                
001400 AUTHOR.         ARCHANA BHAT.                                            
001500 DATE-WRITTEN.   16/03/14.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNCTION:                                                            
002000*        THIS PROGRAM UPDATES PLANNER ID FOR THE PARTS FOR ALL THE        
002100*        FUNCTION GROUP RANGES ENTERED ON SCREEN 1155.THIS PROGRAM        
002110*        IS TRIGGERED FROM 1155 SCREEN AND RECEIVES THE INPUT             
002120*        PARAMETERS FROM THE SYSIN OF JCL. THE JOB SENDS A MAIL ON        
002130*        COMPLETION OF PROGRAM WITH DETAILS OF PARTS UPDATED.             
002140*                                                                         
002200*        GCP PARTS(SUPPLIER BQ8VA) CAN BE EXCLUDED FROM THE UPDATE        
002300*        BY SELECTING GCP OPTION AS 'N' ON 1155 SCREEN.                   
002301*                                                                         
002310*        THE PROGRAM UPDATES   WDK6                                       
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003201     SKIP2                                                                
003202*          --- INFILE FROM SOP                                            
003203     SELECT INDATA                     ASSIGN TO W11561D1.                
003204     SKIP2                                                                
003205*          --- PARTS INFO                                                 
003210     SELECT W01160                     ASSIGN TO W11561D2.                
003220*          --- EXCEL FILE                                                 
003230     SELECT W11561                     ASSIGN TO W11561D3.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003801     SKIP3                                                                
003802 FD  INDATA                                                               
003803     RECORDING       F                                                    
003804     BLOCK CONTAINS  0.                                                   
003805                                                                          
003807 01  INDATA-AREA      PIC X(80).                                          
003808     SKIP3                                                                
003809 FD  W01160                                                               
003810     RECORDING       F                                                    
003811     BLOCK CONTAINS  0.                                                   
003812                                                                          
003820*01  -COPY W01160      -L.                                                
003821     SKIP3                                                                
003830 FD  W11561                                                               
003840     LABEL RECORD STANDARD                                                
003850     RECORDING V                                                          
003860     BLOCK CONTAINS 0.                                                    
003870                                                                          
003880 01  W11561-POST             PIC X(280).                                  
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W1156100'.            
004300 01  CHKP-VAR.                                                            
004400     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004500     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004600     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004700     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004800     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004900     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005000 77  YES                         PIC X       VALUE 'J'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005110 77  IX                          PIC 9       VALUE 0.                     
005120 77  MAX-IX                      PIC 9       VALUE 5.                     
005121 77  WS-LEN                      PIC 9(3)    VALUE 0.                     
005130 77  WS-PART-UPD-MESSG           PIC X(1)    VALUE 'N'.                   
005131 77  WS-PARTS-UPD                PIC 9(9)    VALUE 0.                     
005132 77  WS-PARTS-UPD-Z              PIC Z(8)9.                               
005133 77  WS-SAME-PLNR                PIC 9(9)    VALUE 0.                     
005134 77  WS-GCP-SUPPLR               PIC X(5)    VALUE 'BQ8VA'.               
005135 77  WS-UPD-GCP-ONLY             PIC X(1)    VALUE SPACES.                
005140 77  WS-IDARTNR                  PIC Z(8)9.                               
005200     SKIP2                                                                
005300 01  ERROR-TEXT.                                                          
005400     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
005500     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005701                                                                          
005702 77  SORT-CHECK-IN-SW            PIC X       VALUE 'N'.                   
005703     88  SORT-INCL                           VALUE 'J'.                   
005704     EJECT                                                                
005705 77  SORT-CHECK-EX-SW            PIC X       VALUE 'N'.                   
005706     88  SORT-EXCL                           VALUE 'J'.                   
005707     EJECT                                                                
005708 77  UPD-SW                      PIC X       VALUE 'N'.                   
005709     88  UPD-OK                              VALUE 'J'.                   
005710 77  UPD-GCP-SW                  PIC X       VALUE 'N'.                   
005711     88  UPD-GCP                             VALUE 'J'.                   
005712 77  INDATA-EOF-SW               PIC X       VALUE 'N'.                   
005713     88  END-OF-INDATA                       VALUE 'J'.                   
005714     EJECT                                                                
005715 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
005720     88  END-OF-W01160                       VALUE 'J'.                   
006000     EJECT                                                                
006100 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006200 01  FILLER REDEFINES TODAYS-DATE.                                        
006300     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006400     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006500     03  TODAYS-DATE-DAY         PIC 9(2).                                
006600     EJECT                                                                
006700 01  GENERAL-SUBPROGRAMS.                                                 
006800*                                                                         
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007201     EJECT                                                                
007202*    --- PARAMETRAR TILL POSTSUM                                          
007203*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007501     EJECT                                                                
007502 01  FILLER                      PIC X(24)   VALUE                        
007503                                             'PRM-AREA-START'.            
007504 01  PARM-AREA.                                                           
007517   03    PRM-KDPRODSL           PIC 9(2).                                 
007519   03    PRM-KDSORT-IN1         PIC X(2).                                 
007521   03    PRM-KDSORT-IN2         PIC X(2).                                 
007523   03    PRM-KDSORT-IN3         PIC X(2).                                 
007525   03    PRM-KDSORT-IN4         PIC X(2).                                 
007527   03    PRM-KDSORT-IN5         PIC X(2).                                 
007529   03    PRM-KDSORT-EX1         PIC X(2).                                 
007531   03    PRM-KDSORT-EX2         PIC X(2).                                 
007533   03    PRM-KDSORT-EX3         PIC X(2).                                 
007535   03    PRM-KDSORT-EX4         PIC X(2).                                 
007537   03    PRM-KDSORT-EX5         PIC X(2).                                 
007539   03    PRM-GCP-FLAG           PIC X(1).                                 
007540   03    PRM-IDFKNGRP-FOM       PIC 9(4).                                 
007541   03    PRM-IDFKNGRP-TOM       PIC 9(4).                                 
007543   03    PRM-IDBERED            PIC 9(2).                                 
007544*                                                                         
007555 01  IN-AREA-START              PIC X(24)   VALUE                         
007556                                             'IN-AREA-START'.             
007557     SKIP2                                                                
007558                                                                          
007560*01  AREA -COPY W01160     -PRE IN-                                       
007600*                                                                         
007700     EJECT                                                                
007710 01  UT-AREA-1.                                                           
007720     03  FILLER               PIC X(7)    VALUE 'PART NO'.                
007730     03  FILLER               PIC X       VALUE X'05'.                    
007740     03  FILLER               PIC X(9)    VALUE 'FGRP FROM'.              
007750     03  FILLER               PIC X       VALUE X'05'.                    
007760     03  FILLER               PIC X(8)    VALUE 'FGRP TOM'.               
007770     03  FILLER               PIC X       VALUE X'05'.                    
007780     03  FILLER               PIC X(11)   VALUE 'OLD PLANNER'.            
007790     03  FILLER               PIC X       VALUE X'05'.                    
007791     03  FILLER               PIC X(11)   VALUE 'NEW PLANNER'.            
007792     03  FILLER               PIC X       VALUE X'05'.                    
007793     03  FILLER               PIC X(4)    VALUE 'PGRP'.                   
007794     03  FILLER               PIC X       VALUE X'05'.                    
007795     03  FILLER               PIC X(7)    VALUE 'SORT CD'.                
007796     03  FILLER               PIC X       VALUE X'05'.                    
007798     03  FILLER               PIC X(216)  VALUE SPACE.                    
007799     EJECT                                                                
007823 01  UT-AREA-2.                                                           
007824     03  UT-IDARTNR           PIC Z(9)9   VALUE ZERO.                     
007825     03  FILLER               PIC X       VALUE X'05'.                    
007826     03  UT-IDFKNGRP-FOM      PIC 9(4)    VALUE ZERO.                     
007827     03  FILLER               PIC X       VALUE X'05'.                    
007828     03  UT-IDFKNGRP-TOM      PIC 9(4)    VALUE ZERO.                     
007829     03  FILLER               PIC X       VALUE X'05'.                    
007830     03  UT-IDBERED-OLD       PIC 9(2)    VALUE ZERO.                     
007831     03  FILLER               PIC X       VALUE X'05'.                    
007832     03  UT-IDBERED-NEW       PIC 9(2)    VALUE ZERO.                     
007833     03  FILLER               PIC X       VALUE X'05'.                    
007834     03  UT-KDPRODSL          PIC 9(2)    VALUE ZERO.                     
007835     03  FILLER               PIC X       VALUE X'05'.                    
007836     03  UT-KDSORT            PIC X(2)    VALUE SPACES.                   
007837     03  FILLER               PIC X       VALUE X'05'.                    
007839     03  FILLER               PIC X(248)  VALUE SPACES.                   
007840     EJECT                                                                
007858 01  UT-AREA-BLANK.                                                       
007859     03  FILLER               PIC X(100)  VALUE SPACE.                    
007860     EJECT                                                                
007861 01  UT-AREA-3.                                                           
007862     03  UT-AREA-3-MESSG      PIC X(100)  VALUE SPACE.                    
007863     03  FILLER               PIC X       VALUE X'05'.                    
007864     03  UT-TOT-PARTS         PIC Z(6)9   VALUE ZERO.                     
007865     EJECT                                                                
007870 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  KEYS-TILL-DLI.                                                       
008101     03  W-IDARTNR-X.                                                     
008102         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008103     03  W-KDSEGKEY-X.                                                    
008110         05  W-KDSEGKEY          PIC S9(1)   VALUE +1   COMP-3.           
008200     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FOUND                       VALUE '  '.                  
008600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008800     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
008900     88  IMS-NOT-OK                          VALUE 'XD'.                  
009000     SKIP2                                                                
009100 01  GOOD-STATUSCODES.                                                    
009200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009300     SKIP3                                                                
009400 01  SSA1                        PIC X(64).                               
009500 01  SSA2                        PIC X(64).                               
009600     EJECT                                                                
009700*    --- IMS FUNCTION CODES                                               
009800*01  -COPY W0003                                                          
009900     EJECT                                                                
010100*    ---  DLI INPUT-OUTPUT AREA                                           
010200                                                                          
010301 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
010302 01  DLI-IO-WDK601.                                                       
010303*    03  -COPY WDK601                                                     
010304     EJECT                                                                
010305 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
010306 01  DLI-IO-WDK611.                                                       
010310*    03  -COPY WDK611                                                     
010400                                                                          
010800     EJECT                                                                
010900 LINKAGE SECTION.                                                         
011000                                                                          
011100*01  -COPY W0009  -PRE MSG-                                               
011201                                                                          
011202*01  -COPY W0008  -PRE WDK6-                                              
011210     05  FILLER                  PIC X.                                   
011500     EJECT                                                                
011601 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
011602 MAIN SECTION.                                                            
011610     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
011700                                                                          
011900     SKIP2                                                                
012000     PERFORM A-INIT                                                       
012110     PERFORM S01-READ-INDATA                                              
012120     PERFORM S02-READ-W01160                                              
012130     PERFORM B-CHECK-PARM                                                 
012140                                                                          
012200     PERFORM UNTIL END-OF-W01160                                          
012300       IF CHKP-ANT > CHKP-MAX                                             
012400         PERFORM X-TAKE-CHECKPOINT                                        
012500       END-IF                                                             
012600       PERFORM C-CHECK-INDATA                                             
013210       PERFORM S02-READ-W01160                                            
013300     END-PERFORM                                                          
013500                                                                          
013600     PERFORM D-WRITE-TOTALS                                               
013610     PERFORM Z-FINIT                                                      
013700                                                                          
013900     GOBACK                                                               
014000     .                                                                    
014100     EJECT                                                                
014200 A-INIT SECTION.                                                          
014300     SKIP2                                                                
014400                                                                          
014500     PERFORM IMS-RESTART                                                  
014701                                                                          
015100     OPEN INPUT  INDATA                                                   
015110                 W01160                                                   
015111     OPEN OUTPUT W11561                                                   
015120                                                                          
015410     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015700     .                                                                    
015900     EJECT                                                                
015910 B-CHECK-PARM SECTION.                                                    
015920                                                                          
015921     MOVE 65             TO WS-LEN                                        
015930     IF PRM-KDSORT-IN1 NOT = SPACES                                       
015931        MOVE 'INCL SORT1' TO UT-AREA-1(WS-LEN:10)                         
015932        ADD 10            TO WS-LEN                                       
015933        MOVE  X'05'       TO UT-AREA-1(WS-LEN:1)                          
015934        ADD 1             TO WS-LEN                                       
015935        MOVE YES          TO SORT-CHECK-IN-SW                             
015936     END-IF                                                               
015937     IF PRM-KDSORT-IN2 NOT = SPACES                                       
015938        MOVE 'INCL SORT2' TO UT-AREA-1(WS-LEN:10)                         
015939        ADD 10            TO WS-LEN                                       
015940        MOVE  X'05'       TO UT-AREA-1(WS-LEN:1)                          
015941        ADD 1             TO WS-LEN                                       
015942        MOVE YES          TO SORT-CHECK-IN-SW                             
015948     END-IF                                                               
015949     IF PRM-KDSORT-IN3 NOT = SPACES                                       
015955        MOVE 'INCL SORT3' TO UT-AREA-1(WS-LEN:10)                         
015956        ADD 10            TO WS-LEN                                       
015957        MOVE  X'05'       TO UT-AREA-1(WS-LEN:1)                          
015958        ADD 1             TO WS-LEN                                       
015959        MOVE YES          TO SORT-CHECK-IN-SW                             
015960     END-IF                                                               
015961     IF PRM-KDSORT-IN4 NOT = SPACES                                       
015967        MOVE 'INCL SORT4' TO UT-AREA-1(WS-LEN:10)                         
015968        ADD 10            TO WS-LEN                                       
015969        MOVE  X'05'       TO UT-AREA-1(WS-LEN:1)                          
015970        ADD 1             TO WS-LEN                                       
015971        MOVE YES          TO SORT-CHECK-IN-SW                             
015972     END-IF                                                               
015973     IF PRM-KDSORT-IN5 NOT = SPACES                                       
015979        MOVE 'INCL SORT5' TO UT-AREA-1(WS-LEN:10)                         
015980        ADD 10            TO WS-LEN                                       
015981        MOVE  X'05'       TO UT-AREA-1(WS-LEN:1)                          
015982        ADD 1             TO WS-LEN                                       
015983        MOVE YES          TO SORT-CHECK-IN-SW                             
015990     END-IF                                                               
015991                                                                          
015992     IF PRM-KDSORT-EX1 NOT = SPACES                                       
015993        MOVE 'EXCL SORT1'    TO UT-AREA-1(WS-LEN:10)                      
015994        ADD 10                TO WS-LEN                                   
015995        MOVE  X'05'           TO UT-AREA-1(WS-LEN:1)                      
015996        ADD 1                 TO WS-LEN                                   
015997        MOVE YES              TO SORT-CHECK-EX-SW                         
015998     END-IF                                                               
015999     IF PRM-KDSORT-EX2 NOT = SPACES                                       
016000        MOVE 'EXCL SORT2'    TO UT-AREA-1(WS-LEN:10)                      
016001        ADD 10                TO WS-LEN                                   
016002        MOVE  X'05'           TO UT-AREA-1(WS-LEN:1)                      
016003        ADD 1                 TO WS-LEN                                   
016004        MOVE YES              TO SORT-CHECK-EX-SW                         
016005     END-IF                                                               
016006     IF PRM-KDSORT-EX3 NOT = SPACES                                       
016007        MOVE 'EXCL SORT3'    TO UT-AREA-1(WS-LEN:10)                      
016008        ADD 10                TO WS-LEN                                   
016009        MOVE  X'05'           TO UT-AREA-1(WS-LEN:1)                      
016010        ADD 1                 TO WS-LEN                                   
016011        MOVE YES              TO SORT-CHECK-EX-SW                         
016012     END-IF                                                               
016013     IF PRM-KDSORT-EX4 NOT = SPACES                                       
016014        MOVE 'EXCL SORT4'    TO UT-AREA-1(WS-LEN:10)                      
016015        ADD 10                TO WS-LEN                                   
016016        MOVE  X'05'           TO UT-AREA-1(WS-LEN:1)                      
016017        ADD 1                 TO WS-LEN                                   
016018        MOVE YES              TO SORT-CHECK-EX-SW                         
016019     END-IF                                                               
016020     IF PRM-KDSORT-EX5 NOT = SPACES                                       
016021        MOVE 'EXCL SORT5'    TO UT-AREA-1(WS-LEN:10)                      
016022        ADD 10                TO WS-LEN                                   
016023        MOVE  X'05'           TO UT-AREA-1(WS-LEN:1)                      
016024        ADD 1                 TO WS-LEN                                   
016025        MOVE YES              TO SORT-CHECK-EX-SW                         
016026     END-IF                                                               
016062                                                                          
016063     EVALUATE TRUE                                                        
016064       WHEN PRM-GCP-FLAG = YES                                            
016065          MOVE YES       TO UPD-GCP-SW                                    
016066       WHEN PRM-GCP-FLAG = NOO                                            
016067          MOVE NOO       TO UPD-GCP-SW                                    
016068       WHEN PRM-GCP-FLAG = 'E'                                            
016069          MOVE YES       TO WS-UPD-GCP-ONLY                               
016070                            UPD-GCP-SW                                    
016071     END-EVALUATE                                                         
016072                                                                          
016073     MOVE 'GCP OPTION'     TO UT-AREA-1(WS-LEN:10)                        
016074     ADD 10                TO WS-LEN                                      
016075     MOVE  X'05'           TO UT-AREA-1(WS-LEN:1)                         
016076     ADD 1                 TO WS-LEN                                      
016077                                                                          
016078     MOVE 'MESSAGE'        TO UT-AREA-1(WS-LEN:7)                         
016079     ADD 7                 TO WS-LEN                                      
016080     MOVE  X'05'           TO UT-AREA-1(WS-LEN:1)                         
016081     ADD 1                 TO WS-LEN                                      
016082     WRITE W11561-POST FROM UT-AREA-1                                     
016083     .                                                                    
016084     EJECT                                                                
016085 C-CHECK-INDATA SECTION.                                                  
016086                                                                          
016087     MOVE NOO          TO UPD-SW                                          
016088     IF IN-CLAG-KDPRODSL = PRM-KDPRODSL                                   
016089     AND (IN-CLAG-IDFKNGRP >= PRM-IDFKNGRP-FOM AND                        
016090          IN-CLAG-IDFKNGRP <= PRM-IDFKNGRP-TOM)                           
016091          IF IN-CLAG-KDSORT NOT = SPACES                                  
016092          AND (SORT-INCL OR SORT-EXCL)                                    
016093             PERFORM CA-CHECK-KDSORT                                      
016094          ELSE                                                            
016095             MOVE YES        TO UPD-SW                                    
016096          END-IF                                                          
016097          PERFORM CB-CHECK-GCP-FLAG                                       
016098     END-IF                                                               
016099     IF UPD-SW = YES                                                      
016100        MOVE IN-CLAG-IDARTNR TO W-IDARTNR                                 
016101                                WS-IDARTNR                                
016102        PERFORM IMS-GHU-WDK601                                            
016103        IF SEGMENT-FOUND                                                  
016104           PERFORM IMS-GHNP-WDK611                                        
016105           IF SEGMENT-FOUND                                               
016106              MOVE IN-CLAG-KDSORT   TO UT-KDSORT                          
016107              MOVE CLAG-IDBERED     TO UT-IDBERED-OLD                     
016108              IF CLAG-IDBERED NOT = PRM-IDBERED                           
016109                MOVE PRM-IDBERED    TO CLAG-IDBERED                       
016110                PERFORM IMS-REPL-WDK611                                   
016111                ADD +1              TO WS-PARTS-UPD                       
016112                MOVE YES            TO WS-PART-UPD-MESSG                  
016113                PERFORM CC-MOVE-UT-DATA                                   
016114              ELSE                                                        
016115                MOVE NOO            TO WS-PART-UPD-MESSG                  
016116                ADD +1              TO WS-SAME-PLNR                       
016117                PERFORM CC-MOVE-UT-DATA                                   
016118              END-IF                                                      
016119           END-IF                                                         
016120        END-IF                                                            
016121     END-IF                                                               
016122     .                                                                    
016123     EJECT                                                                
016124 CA-CHECK-KDSORT SECTION.                                                 
016125                                                                          
016126* CHECK IF THE SORT CODE ENTERED ON THE SCREEN MATCHES THE INPUT          
016127* FILE SORT CODE. IF THE INCLUDE SORT CODE MATCHES, THEN THE PART         
016128* IS TO BE UPDATED. IF THE EXCLUDE SORT CODE MATCHES, THE PART            
016129* SHOULD NOT BE UPDATED.                                                  
016130     IF SORT-INCL                                                         
016131        IF IN-CLAG-KDSORT = PRM-KDSORT-IN1                                
016132        OR IN-CLAG-KDSORT = PRM-KDSORT-IN2                                
016133        OR IN-CLAG-KDSORT = PRM-KDSORT-IN3                                
016134        OR IN-CLAG-KDSORT = PRM-KDSORT-IN4                                
016135        OR IN-CLAG-KDSORT = PRM-KDSORT-IN5                                
016136           MOVE YES        TO UPD-SW                                      
016137        END-IF                                                            
016138     ELSE                                                                 
016139        MOVE YES           TO UPD-SW                                      
016140     END-IF                                                               
016141                                                                          
016142     IF SORT-EXCL                                                         
016143        IF IN-CLAG-KDSORT = PRM-KDSORT-EX1                                
016144        OR IN-CLAG-KDSORT = PRM-KDSORT-EX2                                
016145        OR IN-CLAG-KDSORT = PRM-KDSORT-EX3                                
016146        OR IN-CLAG-KDSORT = PRM-KDSORT-EX4                                
016147        OR IN-CLAG-KDSORT = PRM-KDSORT-EX5                                
016148           MOVE NOO        TO UPD-SW                                      
016149        END-IF                                                            
016150     END-IF                                                               
016151     .                                                                    
016152     EJECT                                                                
016153 CB-CHECK-GCP-FLAG SECTION.                                               
016154                                                                          
016155* CHECK IF THE PART SUPPLIER IS A GCP SUPPLIER. GCP(GENUINE CLASSI        
016156* PARTS) ARE FOR OLD CLASSIC CARS AND HANDLED BY SEPARATE COMPANY.        
016157* THEY SHOULD BE EXCLUDED FROM THE BATCH UPDATE UNLESS SPECIFICALL        
016158* SELECTED FOR UPDATE                                                     
016159     IF IN-CLAG-IDLEVNR = WS-GCP-SUPPLR                                   
016160        IF UPD-GCP-SW = NOO                                               
016161           MOVE NOO       TO UPD-SW                                       
016162        END-IF                                                            
016163     ELSE                                                                 
016164        IF WS-UPD-GCP-ONLY = YES                                          
016165           MOVE NOO       TO UPD-SW                                       
016166        END-IF                                                            
016167     END-IF                                                               
016168     .                                                                    
016169     EJECT                                                                
016170 CC-MOVE-UT-DATA SECTION.                                                 
016171                                                                          
016172     MOVE IN-CLAG-IDARTNR   TO UT-IDARTNR                                 
016173     MOVE PRM-IDFKNGRP-FOM  TO UT-IDFKNGRP-FOM                            
016174     MOVE PRM-IDFKNGRP-TOM  TO UT-IDFKNGRP-TOM                            
016175     MOVE PRM-IDBERED       TO UT-IDBERED-NEW                             
016176     MOVE PRM-KDPRODSL      TO UT-KDPRODSL                                
016177     MOVE 34                TO WS-LEN                                     
016178     IF PRM-KDSORT-IN1 NOT = SPACES                                       
016179        MOVE PRM-KDSORT-IN1 TO UT-AREA-2(WS-LEN:2)                        
016180        ADD 2               TO WS-LEN                                     
016181        MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                        
016182        ADD 1               TO WS-LEN                                     
016183     END-IF                                                               
016184     IF PRM-KDSORT-IN2 NOT = SPACES                                       
016185        MOVE PRM-KDSORT-IN2 TO UT-AREA-2(WS-LEN:2)                        
016186        ADD 2               TO WS-LEN                                     
016187        MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                        
016188        ADD 1               TO WS-LEN                                     
016189     END-IF                                                               
016190     IF PRM-KDSORT-IN3 NOT = SPACES                                       
016191        MOVE PRM-KDSORT-IN3 TO UT-AREA-2(WS-LEN:2)                        
016192        ADD 2               TO WS-LEN                                     
016193        MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                        
016194        ADD 1               TO WS-LEN                                     
016195     END-IF                                                               
016196     IF PRM-KDSORT-IN4 NOT = SPACES                                       
016197        MOVE PRM-KDSORT-IN4 TO UT-AREA-2(WS-LEN:2)                        
016198        ADD 2               TO WS-LEN                                     
016199        MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                        
016200        ADD 1               TO WS-LEN                                     
016201     END-IF                                                               
016202     IF PRM-KDSORT-IN5 NOT = SPACES                                       
016203        MOVE PRM-KDSORT-IN5 TO UT-AREA-2(WS-LEN:2)                        
016204        ADD 2               TO WS-LEN                                     
016205        MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                        
016206        ADD 1               TO WS-LEN                                     
016207     END-IF                                                               
016208                                                                          
016209     IF PRM-KDSORT-EX1 NOT = SPACES                                       
016210        MOVE PRM-KDSORT-EX1 TO UT-AREA-2(WS-LEN:2)                        
016211        ADD 2               TO WS-LEN                                     
016212        MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                        
016213        ADD 1               TO WS-LEN                                     
016214     END-IF                                                               
016220     IF PRM-KDSORT-EX2 NOT = SPACES                                       
016230        MOVE PRM-KDSORT-EX2 TO UT-AREA-2(WS-LEN:2)                        
016231        ADD 2               TO WS-LEN                                     
016232        MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                        
016233        ADD 1               TO WS-LEN                                     
016234     END-IF                                                               
016235     IF PRM-KDSORT-EX3 NOT = SPACES                                       
016236        MOVE PRM-KDSORT-EX3 TO UT-AREA-2(WS-LEN:2)                        
016237        ADD 2               TO WS-LEN                                     
016238        MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                        
016239        ADD 1               TO WS-LEN                                     
016240     END-IF                                                               
016241     IF PRM-KDSORT-EX4 NOT = SPACES                                       
016242        MOVE PRM-KDSORT-EX4 TO UT-AREA-2(WS-LEN:2)                        
016243        ADD 2               TO WS-LEN                                     
016244        MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                        
016245        ADD 1               TO WS-LEN                                     
016246     END-IF                                                               
016247     IF PRM-KDSORT-EX5 NOT = SPACES                                       
016248        MOVE PRM-KDSORT-EX5 TO UT-AREA-2(WS-LEN:2)                        
016249        ADD 2               TO WS-LEN                                     
016250        MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                        
016251        ADD 1               TO WS-LEN                                     
016252     END-IF                                                               
016253     MOVE PRM-GCP-FLAG      TO UT-AREA-2(WS-LEN:1)                        
016254     ADD 1                  TO WS-LEN                                     
016255     MOVE  X'05'            TO UT-AREA-2(WS-LEN:1)                        
016256     ADD 1                  TO WS-LEN                                     
016257     IF WS-PART-UPD-MESSG = YES                                           
016258        MOVE SPACES         TO UT-AREA-2(WS-LEN:54)                       
016259        MOVE 'PART UPDATED' TO UT-AREA-2(WS-LEN:12)                       
016260        ADD 12              TO WS-LEN                                     
016261        MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                        
016262        ADD 1               TO WS-LEN                                     
016263     ELSE                                                                 
016266        MOVE 'ENTERED PLANNER SAME AS EXISTING PLANNER FOR THIS PA        
016267-               'RT'        TO UT-AREA-2(WS-LEN:54)                       
016268        ADD 54              TO WS-LEN                                     
016269        MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                        
016270        ADD 1               TO WS-LEN                                     
016271     END-IF                                                               
016272     WRITE W11561-POST FROM UT-AREA-2                                     
016273     .                                                                    
016274     EJECT                                                                
016275 D-WRITE-TOTALS SECTION.                                                  
016276                                                                          
016277                                                                          
016278     MOVE WS-PARTS-UPD             TO UT-TOT-PARTS                        
016279     IF WS-SAME-PLNR = 0 AND WS-PARTS-UPD = 0                             
016281        MOVE ZERO                 TO UT-IDARTNR                           
016282        MOVE PRM-IDFKNGRP-FOM     TO UT-IDFKNGRP-FOM                      
016283        MOVE PRM-IDFKNGRP-TOM     TO UT-IDFKNGRP-TOM                      
016284        MOVE ZERO                 TO UT-IDBERED-OLD                       
016285        MOVE PRM-IDBERED          TO UT-IDBERED-NEW                       
016286        MOVE PRM-KDPRODSL         TO UT-KDPRODSL                          
016288        MOVE 34                   TO WS-LEN                               
016289        IF PRM-KDSORT-IN1 NOT = SPACES                                    
016290           MOVE PRM-KDSORT-IN1 TO UT-AREA-2(WS-LEN:2)                     
016291           ADD 2               TO WS-LEN                                  
016292           MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                     
016293           ADD 1               TO WS-LEN                                  
016294        END-IF                                                            
016295        IF PRM-KDSORT-IN2 NOT = SPACES                                    
016296           MOVE PRM-KDSORT-IN2 TO UT-AREA-2(WS-LEN:2)                     
016297           ADD 2               TO WS-LEN                                  
016298           MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                     
016299           ADD 1               TO WS-LEN                                  
016300        END-IF                                                            
016301        IF PRM-KDSORT-IN3 NOT = SPACES                                    
016302           MOVE PRM-KDSORT-IN3 TO UT-AREA-2(WS-LEN:2)                     
016303           ADD 2               TO WS-LEN                                  
016304           MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                     
016305           ADD 1               TO WS-LEN                                  
016306        END-IF                                                            
016307        IF PRM-KDSORT-IN4 NOT = SPACES                                    
016308           MOVE PRM-KDSORT-IN4 TO UT-AREA-2(WS-LEN:2)                     
016309           ADD 2               TO WS-LEN                                  
016310           MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                     
016311           ADD 1               TO WS-LEN                                  
016312        END-IF                                                            
016313        IF PRM-KDSORT-IN5 NOT = SPACES                                    
016314           MOVE PRM-KDSORT-IN5 TO UT-AREA-2(WS-LEN:2)                     
016315           ADD 2               TO WS-LEN                                  
016316           MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                     
016317           ADD 1               TO WS-LEN                                  
016318        END-IF                                                            
016319                                                                          
016320        IF PRM-KDSORT-EX1 NOT = SPACES                                    
016321           MOVE PRM-KDSORT-EX1 TO UT-AREA-2(WS-LEN:2)                     
016322           ADD 2               TO WS-LEN                                  
016323           MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                     
016324           ADD 1               TO WS-LEN                                  
016325        END-IF                                                            
016326        IF PRM-KDSORT-EX2 NOT = SPACES                                    
016327           MOVE PRM-KDSORT-EX2 TO UT-AREA-2(WS-LEN:2)                     
016328           ADD 2               TO WS-LEN                                  
016329           MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                     
016330           ADD 1               TO WS-LEN                                  
016331        END-IF                                                            
016332        IF PRM-KDSORT-EX3 NOT = SPACES                                    
016333           MOVE PRM-KDSORT-EX3 TO UT-AREA-2(WS-LEN:2)                     
016334           ADD 2               TO WS-LEN                                  
016335           MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                     
016336           ADD 1               TO WS-LEN                                  
016337        END-IF                                                            
016338        IF PRM-KDSORT-EX4 NOT = SPACES                                    
016339           MOVE PRM-KDSORT-EX4 TO UT-AREA-2(WS-LEN:2)                     
016340           ADD 2               TO WS-LEN                                  
016341           MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                     
016342           ADD 1               TO WS-LEN                                  
016343        END-IF                                                            
016344        IF PRM-KDSORT-EX5 NOT = SPACES                                    
016345           MOVE PRM-KDSORT-EX5 TO UT-AREA-2(WS-LEN:2)                     
016346           ADD 2               TO WS-LEN                                  
016347           MOVE  X'05'         TO UT-AREA-2(WS-LEN:1)                     
016348           ADD 1               TO WS-LEN                                  
016349        END-IF                                                            
016350        MOVE PRM-GCP-FLAG      TO UT-AREA-2(WS-LEN:1)                     
016351        ADD 1                  TO WS-LEN                                  
016352        MOVE  X'05'            TO UT-AREA-2(WS-LEN:1)                     
016353        ADD 1                  TO WS-LEN                                  
016355        MOVE 'NO PARTS MATCH THE CRITERIA IN THE ENTERED FN GRP'          
016356                               TO UT-AREA-2(WS-LEN:49)                    
016357        ADD 49                 TO WS-LEN                                  
016358        MOVE  X'05'            TO UT-AREA-2(WS-LEN:1)                     
016359        ADD 1                  TO WS-LEN                                  
016360        WRITE W11561-POST FROM UT-AREA-2                                  
016361     END-IF                                                               
016362     WRITE W11561-POST          FROM UT-AREA-BLANK                        
016363     WRITE W11561-POST          FROM UT-AREA-BLANK                        
016364     MOVE 'TOTAL PARTS UPDATED'   TO UT-AREA-3-MESSG                      
016365     WRITE W11561-POST          FROM UT-AREA-3                            
016366     .                                                                    
016367     EJECT                                                                
016370 Z-FINIT SECTION.                                                         
016400                                                                          
016502     CLOSE INDATA                                                         
016503           W01160                                                         
016504           W11561                                                         
016701     SKIP2                                                                
016702     MOVE 'S' TO POSTSUM-OPKOD                                            
016710     CALL POSTSUM USING POSTSUM-PARM                                      
016711                                                                          
016720     IF WS-PARTS-UPD > 0                                                  
016730        MOVE ZERO TO RETURN-CODE                                          
016740     ELSE                                                                 
016750        MOVE +4   TO RETURN-CODE                                          
016760     END-IF                                                               
016900     .                                                                    
017001     EJECT                                                                
017002 S01-READ-INDATA  SECTION.                                                
017003     SKIP2                                                                
017004     READ INDATA INTO PARM-AREA                                           
017005     AT END                                                               
017006        SET END-OF-INDATA TO TRUE                                         
017024                                                                          
017025     NOT AT END                                                           
017026        MOVE 'INDATA'   TO POSTSUM-FDNAMN                                 
017027        MOVE 'W11561D1' TO POSTSUM-DDNAMN2                                
017028        MOVE 'PARM'     TO POSTSUM-TRANSTYP                               
017029        CALL POSTSUM USING POSTSUM-PARM                                   
017049     END-READ                                                             
017050     .                                                                    
017051     EJECT                                                                
017052 S02-READ-W01160  SECTION.                                                
017053     SKIP2                                                                
017054     READ W01160 INTO IN-AREA                                             
017055     AT END                                                               
017056        SET END-OF-W01160 TO TRUE                                         
017057                                                                          
017058     NOT AT END                                                           
017059        MOVE 'W01160' TO POSTSUM-FDNAMN                                   
017060        MOVE 'W11561D2' TO POSTSUM-DDNAMN2                                
017061        CALL POSTSUM USING POSTSUM-PARM                                   
017062                                                                          
017063     END-READ                                                             
017070     .                                                                    
017300     EJECT                                                                
017310 S03-WRITE-W11561 SECTION.                                                
017320                                                                          
017330     WRITE W11561-POST FROM UT-AREA-2                                     
017340     .                                                                    
017350     EJECT                                                                
017400 X-TAKE-CHECKPOINT   SECTION.                                             
017500                                                                          
018100     PERFORM IMS-CHECKPOINT                                               
018200     MOVE ZERO TO CHKP-ANT                                                
018400     .                                                                    
018500     EJECT                                                                
018600* --- IMS SECTIONS  ---                                                   
018700                                                                          
018801     EJECT                                                                
018802 IMS-GHU-WDK601 SECTION.                                                  
018803                                                                          
018806     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
018809          DELIMITED BY SIZE INTO SSA1                                     
018812     MOVE '  GE' TO GOOD-STATUSCODES                                      
018813     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK601 SSA1                   
018814     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
018815     PERFORM IMS-STATUSCHECK                                              
018816     .                                                                    
018817     SKIP3                                                                
018818 IMS-GHNP-WDK611 SECTION.                                                 
018819                                                                          
018822     MOVE   'WDK611  '  TO SSA1                                           
018824     MOVE '  GE' TO GOOD-STATUSCODES                                      
018825     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
018826     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
018827     PERFORM IMS-STATUSCHECK                                              
018828     .                                                                    
018829     SKIP3                                                                
018830 IMS-REPL-WDK611 SECTION.                                                 
018831                                                                          
018832     MOVE '  ' TO GOOD-STATUSCODES                                        
018833     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
018834     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
018835     PERFORM IMS-STATUSCHECK                                              
018836     ADD +1                TO CHKP-ANT                                    
018837     .                                                                    
018840     EJECT                                                                
019000 IMS-RESTART SECTION.                                                     
019100     SKIP2                                                                
019200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019300     MOVE '  ' TO GOOD-STATUSCODES                                        
019400     CALL CBLTDLI USING XRST MSG-PCB                                      
019500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019600                        CHKP-AREA-LENGTH CHKP-AREA                        
019700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019800     PERFORM IMS-STATUSCHECK                                              
019900     .                                                                    
020000     SKIP3                                                                
020100 IMS-CHECKPOINT SECTION.                                                  
020200     SKIP2                                                                
020300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020400     MOVE '  XD' TO GOOD-STATUSCODES                                      
020500     CALL CBLTDLI USING CHKP MSG-PCB                                      
020600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020700                        CHKP-AREA-LENGTH CHKP-AREA                        
020800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020900     PERFORM IMS-STATUSCHECK                                              
021000                                                                          
021100     IF IMS-NOT-OK                                                        
021200       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
021210                          TO ERROR-TEXT-STR                               
021300       DISPLAY ERROR-TEXT                                                 
021400       CALL FELLOG                                                        
021500     END-IF                                                               
021600     .                                                                    
021700     EJECT                                                                
021800 IMS-STATUSCHECK SECTION.                                                 
021900     SKIP2                                                                
022000     SET STATUS-IX TO 1                                                   
022100     SEARCH GOOD-STATUS                                                   
022200       AT END                                                             
022300         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
022400           DELIMITED BY SIZE INTO ERROR-TEXT                              
022500         DISPLAY ERROR-TEXT                                               
022600         CALL FELLOG                                                      
022700       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
022800         CONTINUE                                                         
022900     END-SEARCH                                                           
023000     .                                                                    
