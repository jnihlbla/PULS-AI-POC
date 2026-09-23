000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W221SEGM.                                                
000400 AUTHOR.         SURESH GUDIVADA.                                         
000500 DATE-WRITTEN.   23/11/01.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SUBPROGRAM TO DETERMINE AND RETURN                               
001000*        PROCUREMENT SEGMENTATION CODE (A.K.A. KDANSKSEG)                 
001100*        AND PROCUREMENT SEGMENT TABLE FOR SS AND EOQ                     
001101*                                                                         
001110*        KDCALL=001 RETURNS KDANSKSEG,                                    
001111*                           FLANSKSEG-CHANGED AND                         
001120*                           KDFGPRIO                                      
001131*                                                                         
001132*        KDCALL=002 RETURNS IDREFTAB,                                     
001133*                           FLTABLE-CHANGED,                              
001135*                           KVOT-RULL12 (FROM WDL7)                       
001136*                           KVVECKOR-FT (FROM WDK7)                       
001150*                   FOR NDCS                                              
001151*                                                                         
001152*       (KDCALL=003 FOR FUTURE USE                                        
001153*                   RETURNS IDREFTAB,                                     
001154*                           FLTABLE-CHANGED,                              
001155*                           KVOT-RULL12 (FROM WDL8)                       
001156*                           KVVECKOR-FT (FROM WDK6)                       
001157*                   SAME AS KDCALL=002 EXCEPT FOR CDC    )                
001160*                                                                         
001200*        PROGRAM READS     WDB6                                           
001300*                          WDL7                                           
001400*                          WDL8                                           
001500*                          WDD5                                           
001600*                                                                         
001700*        PROGRAM UPDATES   NONE                                           
001800*                                                                         
001900*                                                                         
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400                                                                          
002500 WORKING-STORAGE SECTION.                                                 
002600     SKIP3                                                                
002700 77  IDPGM                       PIC X(8)    VALUE 'W221SEGM'.            
002800 77  JA                          PIC X       VALUE 'J'.                   
002900 77  NEJ                         PIC X       VALUE 'N'.                   
003000     EJECT                                                                
003100                                                                          
003200 01  WORKING-FIELDS.                                                      
003300     03 WS-KVOT                  PIC S9(7)   VALUE ZERO COMP-3.           
003400     03 INDX                     PIC  9(2)   VALUE ZERO.                  
003500                                                                          
003600 01  SWITCHES.                                                            
003700     03 SW-SOP-2-YEARS-AGO-AT-MOST    PIC X(1)  VALUE SPACES.             
003800     03 SW-EOP-MORE-THAN-5-YEARS-AGO  PIC X(1)  VALUE SPACES.             
003900                                                                          
004000*    -COPY WWPRODSL                                                       
004100*                                                                         
004200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004300 01  FILLER REDEFINES DAGENS-DATUM.                                       
004400     03 DAGENS-AA                PIC 9(2).                                
004500     03 DAGENS-MAANAD            PIC 9(2).                                
004600     03 DAGENS-DAG               PIC 9(2).                                
004700*                                                                         
004800 01  DAGENS-DATUM-CENTURY        PIC 9(8)    VALUE ZERO.                  
004900                                                                          
005000 01  DAGENS-VECKA                PIC 9(2)    VALUE ZERO.                  
005100*                                                                         
005200                                                                          
005300**** GENERAL SUBROUTINE                                                   
005400 01  DYNAMISKA-SUBPROGRAM.                                                
005500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005700     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
005800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
005900                                                                          
006000*    ---PARAMETRAR TILL WZ20DAYS                                          
006100 01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS'.            
006200*01  -COPY WZ20DAYS                                                       
006300     EJECT                                                                
006400                                                                          
006500*    --- PARAMETRAR TILL DATKONV                                          
006600*                                                                         
006700*01  -COPY WDATAREA                                                       
006800     EJECT                                                                
006900                                                                          
007000*    -COPY WY2000W2                                                       
007100     EJECT                                                                
007200                                                                          
007300*    -COPY WY2000W3                                                       
007400     EJECT                                                                
007500 01  FELTEXT.                                                             
007600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT='.            
007700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007800     EJECT                                                                
007900*                                                                         
008000*    --- WORK AREAS TO THE IMS SECTIONS                                   
008100*                                                                         
008200 01  NYCKLAR-TILL-DLI.                                                    
008300     03  W-IDARTNR-X.                                                     
008400         05  W-IDARTNR           PIC S9(9)   COMP-3.                      
008500     03  W-IDDC-X.                                                        
008600         05  W-IDDC              PIC X(2)    VALUE ZERO.                  
008700     03  W-KDANSKSEG-X.                                                   
008800         05  W-KDANSKSEG         PIC S9(5)   COMP-3.                      
008900     03  W-TIAAAA-X.                                                      
009000         05  W-TIAAAA            PIC 9(4)   VALUE ZERO.                   
009100                                                                          
009200                                                                          
009300 01  IMS-WS.                                                              
009400     03  FILLER                  PIC X(8)    VALUE 'IMS-WS  '.            
009500                                                                          
009600*                            *** STATUSCODES FROM IMS                     
009700     03 STATUS-WS                PIC XX.                                  
009800         88  SEGMENT-FOUND                   VALUE '  '.                  
009900         88  SEGMENT-MISSING                 VALUE 'GE'.                  
010000                                                                          
010100     03  GOOD-STATUSCODES.                                                
010200         05  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.            
010300                                                                          
010400     03 ERROR-TEXT.                                                       
010500         05 FILLER               PIC X(10)   VALUE 'ERROR-TEXT'.          
010600         05 ERROR-TEXT-STR       PIC X(72)   VALUE SPACE.                 
010700                                                                          
010710 01  ALL-SSAS.                                                            
010800     03  SSA1                    PIC X(128).                              
010900     03  SSA2                    PIC X(128).                              
011000     03  SSA3                    PIC X(128).                              
011100     EJECT                                                                
011200                                                                          
011300*    --- IMS FUNCTION CODES                                               
011400*01  -COPY W0003                                                          
011500     EJECT                                                                
011600                                                                          
011700*    --- DLI INPUT-OUTPUT AREA                                            
011800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB601'.             
011900 01  DLI-IO-WDB601.                                                       
012000*    03  -COPY WDB601                                                     
012100     EJECT                                                                
012110 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB603'.             
012120 01  DLI-IO-WDB603.                                                       
012130*    03  -COPY WDB603                                                     
012140     EJECT                                                                
012200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
012300 01  DLI-IO-WDK601.                                                       
012400*    03  -COPY WDK601                                                     
012500     EJECT                                                                
012600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL711'.             
012700 01  DLI-IO-WDL711.                                                       
012800*    03  -COPY WDL711                                                     
012900     EJECT                                                                
013000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL811'.             
013100 01  DLI-IO-WDL811.                                                       
013200*    03  -COPY WDL811                                                     
013300     EJECT                                                                
013400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDD501'.             
013500 01  DLI-IO-WDD501.                                                       
013600*    03  -COPY WDD501                                                     
013700     EJECT                                                                
013710 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK722'.             
013720 01  DLI-IO-WDK722.                                                       
013730*    03  -COPY WDK722                                                     
013740     EJECT                                                                
013800 LINKAGE SECTION.                                                         
013900                                                                          
014000*01 -COPY W221SEGM                                                        
014100     EJECT                                                                
014200                                                                          
014300*01  -COPY W0008  -PRE WDB6-                                              
014400     05  FILLER                  PIC X.                                   
014500     EJECT                                                                
014600                                                                          
014700*01  -COPY W0008  -PRE WDL7-                                              
014800     05  FILLER                  PIC X.                                   
014900     EJECT                                                                
015000                                                                          
015100*01  -COPY W0008  -PRE WDL8-                                              
015200     05  FILLER                  PIC X.                                   
015300     EJECT                                                                
015400                                                                          
015500*01  -COPY W0008  -PRE WDD5-                                              
015600     05  FILLER                  PIC X.                                   
015700     EJECT                                                                
015710                                                                          
015720*01  -COPY W0008  -PRE WDK7-                                              
015730     05  FILLER                  PIC X.                                   
015740     EJECT                                                                
015800                                                                          
015900 PROCEDURE DIVISION USING SEGM-W221SEGM                                   
016000                          WDB6-PCB WDL7-PCB                               
016100                          WDL8-PCB WDD5-PCB                               
016110                          WDK7-PCB.                                       
016200                                                                          
016300     ENTRY 'DLITCBL' USING SEGM-W221SEGM                                  
016400                           WDB6-PCB WDL7-PCB                              
016410                           WDL8-PCB WDD5-PCB                              
016420                           WDK7-PCB.                                      
016600                                                                          
016700     PERFORM A-INIT                                                       
016800                                                                          
016900     IF  SEGM-IDARTNR IS NUMERIC                                          
017000     AND SEGM-IDARTNR  > ZERO                                             
017100        EVALUATE SEGM-KDCALL                                              
017200           WHEN 001                                                       
017300             PERFORM B-SET-KDANSKSEG-MAJOR                                
017400             PERFORM C-SET-STORAGE                                        
017410             PERFORM D-SET-LIFECYCLE                                      
017500             PERFORM E-SET-SPECIAL                                        
017600             PERFORM F-SET-FLANSKSEG                                      
017700           WHEN 002                                                       
017710             IF SEGM-IDDC > SPACE                                         
017720               IF  SEGM-KDANSKSEG-IN IS NUMERIC                           
017730               AND SEGM-KDANSKSEG-IN  > ZERO                              
017800                 PERFORM G-SET-IDREFTAB                                   
017810               ELSE                                                       
017820                 SET SEGM-KDSVAR-FEL TO TRUE                              
017830                 MOVE 'KDANSKSEG-IN NOT NUM' TO SEGM-TEXT                 
017850               END-IF                                                     
017860             ELSE                                                         
017870               SET SEGM-KDSVAR-FEL TO TRUE                                
017880               MOVE 'DC MUST BE GIVEN' TO SEGM-TEXT                       
017891             END-IF                                                       
017900           WHEN OTHER                                                     
018100             MOVE 'ERROR KDCALL'   TO SEGM-TEXT                           
018300        END-EVALUATE                                                      
018400     ELSE                                                                 
018500         SET SEGM-KDSVAR-FEL  TO TRUE                                     
018600         MOVE 'INVALID PART NO.'   TO SEGM-TEXT                           
018800     END-IF                                                               
018900                                                                          
019000     MOVE ZERO TO RETURN-CODE                                             
019100     GOBACK                                                               
019200     .                                                                    
019300     EJECT                                                                
019400                                                                          
019500 A-INIT SECTION.                                                          
019600                                                                          
019700     MOVE SPACES                  TO SEGM-KDSVAR                          
019800*                                                                         
019900     MOVE FUNCTION CURRENT-DATE(1:8)                                      
020000                                  TO DAGENS-DATUM-CENTURY                 
020100     MOVE FUNCTION CURRENT-DATE(1:4)                                      
020200                                  TO W-TIAAAA                             
020300                                                                          
020400     ACCEPT DAGENS-DATUM        FROM DATE                                 
020500                                                                          
020600     MOVE 'IDAG'         TO DAT-KDDATFORM                                 
020700     CALL WDATKONV USING DAT-KDDATFORM                                    
020800                         DAT-I-TIDATUM                                    
020900                         DAT-O-TIDATUM                                    
021000                         DAT-KDSVAR                                       
021100                                                                          
021200     IF DAT-KDSVAR-OK                                                     
021300        MOVE DAT-TIVV    TO DAGENS-VECKA                                  
021400     ELSE                                                                 
021500        MOVE 'ERROR FROM WDATKONV 1 IN A-INIT SECTION IN W27110'          
021600          TO FELTEXT-STR                                                  
021700        DISPLAY FELTEXT                                                   
021800        CALL FELLOG                                                       
021900     END-IF                                                               
022000                                                                          
022100     IF  SEGM-TISOP-2-YEARS-AGO   NUMERIC                                 
022200     AND SEGM-TISOP-2-YEARS-AGO > ZERO                                    
022300        CONTINUE                                                          
022400     ELSE                                                                 
022500        MOVE DAGENS-DATUM            TO DAYS-TIDATE1                      
022600        MOVE 'YYMMDD'                TO DAYS-KDDATFMT1                    
022700        MOVE -730                    TO DAYS-KVDAYS                       
022800*----------- -730 = 2 YEARS X 365 DAYS AGO                                
022900        MOVE 'YYWWD'                 TO DAYS-KDDATFMT2                    
023000        MOVE SPACE                   TO DAYS-TIDATE2                      
023100                                        DAYS-IDCALEND                     
023200                                                                          
023300        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
023400                                                                          
023500        IF DAYS-KDRC = 8                                                  
023600           STRING 'FEL SOP-ANROP TILL WZ20DAYS - SEC A-'                  
023700           DELIMITED BY SIZE INTO FELTEXT-STR                             
023800           DISPLAY FELTEXT                                                
023900           DISPLAY '     DAGENS-DATUM=' DAGENS-DATUM                      
024000           CALL FELLOG                                                    
024100        ELSE                                                              
024200           MOVE DAYS-TIDATE2(1:5)    TO SEGM-TISOP-2-YEARS-AGO            
024500        END-IF                                                            
024600     END-IF                                                               
024700                                                                          
024800     IF  SEGM-TISOP-TODAY         NUMERIC                                 
024900     AND SEGM-TISOP-TODAY       > ZERO                                    
025000        CONTINUE                                                          
025100     ELSE                                                                 
025200        MOVE DAGENS-DATUM            TO DAYS-TIDATE1                      
025300        MOVE 'YYMMDD'                TO DAYS-KDDATFMT1                    
025400        MOVE 0                       TO DAYS-KVDAYS                       
025500*----------- 0  = TODAY                                                   
025600        MOVE 'YYWWD'                 TO DAYS-KDDATFMT2                    
025700        MOVE SPACE                   TO DAYS-TIDATE2                      
025800                                        DAYS-IDCALEND                     
025900                                                                          
026000        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
026100                                                                          
026200        IF DAYS-KDRC = 8                                                  
026300           STRING 'FEL SOP-ANROP TILL WZ20DAYS - SEC A-'                  
026400           DELIMITED BY SIZE INTO FELTEXT-STR                             
026500           DISPLAY FELTEXT                                                
026600           DISPLAY '     DAGENS-DATUM=' DAGENS-DATUM                      
026700           CALL FELLOG                                                    
026800        ELSE                                                              
026900           MOVE DAYS-TIDATE2(1:5)    TO SEGM-TISOP-TODAY                  
027200        END-IF                                                            
027300     END-IF                                                               
027400                                                                          
027500     IF  SEGM-TIURPROD-5-YEARS-AGO   NUMERIC                              
027600     AND SEGM-TIURPROD-5-YEARS-AGO > ZERO                                 
027700        CONTINUE                                                          
027800     ELSE                                                                 
027900        MOVE DAGENS-DATUM            TO DAYS-TIDATE1                      
028000        MOVE 'YYMMDD'                TO DAYS-KDDATFMT1                    
028100        MOVE -1825                   TO DAYS-KVDAYS                       
028200*----------- -1825 = 5 YEARS X 365 DAYS AGO                               
028300        MOVE 'YYWW'                  TO DAYS-KDDATFMT2                    
028400*----------- DAY=1 IS ASSUMED IN WZ30DAYS                                 
028500        MOVE SPACE                   TO DAYS-TIDATE2                      
028600                                        DAYS-IDCALEND                     
028700                                                                          
028800        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
028900                                                                          
029000        IF DAYS-KDRC = 8                                                  
029100           STRING 'ERROR EOP CALL TO WZ20DAYS - SEC A-'                   
029200           DELIMITED BY SIZE INTO FELTEXT-STR                             
029300           DISPLAY FELTEXT                                                
029400           DISPLAY '     DAGENS-DATUM=' DAGENS-DATUM                      
029500           CALL FELLOG                                                    
029600        ELSE                                                              
029700           MOVE DAYS-TIDATE2(1:4)    TO SEGM-TIURPROD-5-YEARS-AGO         
030000        END-IF                                                            
030100     END-IF                                                               
030200     .                                                                    
030300     EJECT                                                                
030400                                                                          
030500 B-SET-KDANSKSEG-MAJOR SECTION.                                           
030600                                                                          
030700     PERFORM BA-PREPARE-CHECKS                                            
030800                                                                          
030900     EVALUATE TRUE                                                        
031000     WHEN KDPRODSL-TOOLS                                                  
031100*------- TOOLS                                                            
031200          MOVE 6000                   TO SEGM-KDANSKSEG                   
031300     WHEN KDPRODSL-EMB                                                    
031400*------- PACKAGING                                                        
031500          MOVE 5000                   TO SEGM-KDANSKSEG                   
031600     WHEN KDPRODSL-ACC                                                    
031700       OR KDPRODSL-WHEELS                                                 
031800*------- ACCESSORIES (OR WHEELS)                                          
031900          MOVE 4000                   TO SEGM-KDANSKSEG                   
032000     WHEN SEGM-FLBSNES  = 'Y'                                             
032100*------- KEY BUSINESS                                                     
032200          MOVE 1000                   TO SEGM-KDANSKSEG                   
032300     WHEN SEGM-KVEOP    = 15                                              
032400*------- FUNCTION CRITICAL                                                
032500          MOVE 2000                   TO SEGM-KDANSKSEG                   
032600     WHEN OTHER                                                           
032700*------- NONE OF THE ABOVE, JUST A NORMAL PART                            
032800          MOVE 3000                   TO SEGM-KDANSKSEG                   
032900     END-EVALUATE                                                         
033000     .                                                                    
033100     EJECT                                                                
033200                                                                          
033300 BA-PREPARE-CHECKS SECTION.                                               
033400                                                                          
033500     MOVE NEJ           TO SW-SOP-2-YEARS-AGO-AT-MOST                     
033600                           SW-EOP-MORE-THAN-5-YEARS-AGO                   
033800                                                                          
033900     MOVE SEGM-KDPRODSL               TO TEST-KDPRODSL                    
034000     .                                                                    
034100     EJECT                                                                
034200                                                                          
034300 C-SET-STORAGE SECTION.                                                   
034400                                                                          
034500*-- DETERMINE STORAGE OF VSOP (OR NOT)                                    
034600     EVALUATE TRUE                                                        
034800     WHEN SEGM-KDVSOP >  ZERO                                             
034810      AND SEGM-KDVSOP <  200                                              
034900          CONTINUE                                                        
035000     WHEN SEGM-KDVSOP >= 200                                              
035100      AND SEGM-KDVSOP <  300                                              
035200          ADD 100       TO  SEGM-KDANSKSEG                                
035300     WHEN OTHER                                                           
035400          ADD 200       TO  SEGM-KDANSKSEG                                
035500     END-EVALUATE                                                         
037200     .                                                                    
037300     EJECT                                                                
037400                                                                          
037410 D-SET-LIFECYCLE SECTION.                                                 
037420                                                                          
037496     PERFORM DA-PREPARE-CHECKS                                            
037497                                                                          
037498*-- LIFE CYCLE, I.E. DETERMINE IF PHASE IN, PHASE OUT OR NORMAL           
037499     EVALUATE TRUE                                                        
037500     WHEN SW-SOP-2-YEARS-AGO-AT-MOST    = JA                              
037501*------- PHASE IN                                                         
037502          CONTINUE                                                        
037503     WHEN SW-EOP-MORE-THAN-5-YEARS-AGO  = JA                              
037504*------- PHASE OUT(DECLINE)                                               
037505          ADD  20       TO SEGM-KDANSKSEG                                 
037506     WHEN OTHER                                                           
037507*------- NORMAL(PRIME)                                                    
037508          ADD  10       TO SEGM-KDANSKSEG                                 
037509     END-EVALUATE                                                         
037510     .                                                                    
037511     EJECT                                                                
037512                                                                          
037520 DA-PREPARE-CHECKS SECTION.                                               
037600                                                                          
037700*-- DETERMINE IF 2 YEARS SINCE SOP                                        
037800     IF  SEGM-TISOP  > ZERO                                               
037810     AND SEGM-TISOP NOT = 99999                                           
037900        MOVE SEGM-TISOP                 TO TMP1-YYWWD                     
038000        MOVE SEGM-TISOP-2-YEARS-AGO     TO TMP2-YYWWD                     
038100        PERFORM WY2000P2                                                  
038200        IF  TMP1-YYWWD  >= TMP2-YYWWD                                     
038300           MOVE JA   TO SW-SOP-2-YEARS-AGO-AT-MOST                        
038400        END-IF                                                            
038500     END-IF                                                               
038600                                                                          
041400*-- DETERMINE IF 5 YEARS SINCE EOP                                        
041500     IF SEGM-TIURPROD                   >  ZERO                           
041600        MOVE SEGM-TIURPROD              TO TMP1-YYWW                      
041700        MOVE SEGM-TIURPROD-5-YEARS-AGO  TO TMP2-YYWW                      
041800        PERFORM WY2000P3                                                  
041900        IF  TMP1-YYWW   <  TMP2-YYWW                                      
042000           MOVE JA   TO SW-EOP-MORE-THAN-5-YEARS-AGO                      
042100        END-IF                                                            
042200     END-IF                                                               
042900     .                                                                    
043000     EJECT                                                                
043100                                                                          
044400 E-SET-SPECIAL SECTION.                                                   
044500                                                                          
044600     PERFORM EA-PREPARE-CHECKS                                            
044700                                                                          
044800     EVALUATE TRUE                                                        
045500     WHEN SEGM-KDFGPRIO >  0                                              
045600      AND SEGM-KDFGPRIO <= 52                                             
045700*------- EXPIREDATE <= 1YEAR                                              
045800          ADD  1                      TO SEGM-KDANSKSEG                   
045900     WHEN SEGM-KDFGPRIO >  52                                             
046000      AND SEGM-KDFGPRIO <= 156                                            
046100*------- EXPIREDATE <= 3YEAR                                              
046200          ADD  3                      TO SEGM-KDANSKSEG                   
046210     WHEN SEGM-KDFARLIG =  4                                              
046220*------- DANGEROUS GOODS                                                  
046230          ADD  4                      TO SEGM-KDANSKSEG                   
046240     WHEN SEGM-KDFARLIG =  6                                              
046250*------- DANGEROUS GOODS                                                  
046260          ADD  6                      TO SEGM-KDANSKSEG                   
046300     END-EVALUATE                                                         
046400     .                                                                    
046500     EJECT                                                                
046600                                                                          
046700 EA-PREPARE-CHECKS SECTION.                                               
046800                                                                          
046900*-- DETERMINE FIFO ON SCREEN 1118                                         
046920     MOVE SEGM-IDARTNR          TO W-IDARTNR                              
047000     PERFORM IMS-GU-WDD501                                                
047100     IF SEGMENT-FOUND                                                     
047200        MOVE ART-KDFGPRIO       TO SEGM-KDFGPRIO                          
047300     ELSE                                                                 
047400        MOVE ZERO               TO SEGM-KDFGPRIO                          
047500     END-IF                                                               
047600     .                                                                    
047700     EJECT                                                                
047800                                                                          
047900 F-SET-FLANSKSEG SECTION.                                                 
048000                                                                          
048100     IF SEGM-KDANSKSEG-IN = SEGM-KDANSKSEG                                
048200        MOVE NEJ          TO SEGM-FLANSKSEG-CHANGED                       
048300     ELSE                                                                 
048400        MOVE JA           TO SEGM-FLANSKSEG-CHANGED                       
048500     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800                                                                          
049000 G-SET-IDREFTAB SECTION.                                                  
049100                                                                          
049110     MOVE SEGM-IDREFTAB-IN  TO SEGM-IDREFTAB                              
049700     MOVE JA                TO SEGM-FLTABLE-CHANGED                       
049800                                                                          
050110     PERFORM GA-PREPARE-CHECKS                                            
050111     EVALUATE TRUE                                                        
050112*---- LOW-FREQUENCY                                                       
050113      WHEN WS-KVOT          <  DCS-KVOT-RULL12HF                          
050120      AND  SEGM-KVVECKOR-FT <= DCS-KVVECKOR-FTL                           
050121           MOVE SEGT-IDREFTAB-LFL      TO SEGM-IDREFTAB                   
050122      WHEN WS-KVOT          <  DCS-KVOT-RULL12HF                          
050123      AND  SEGM-KVVECKOR-FT <= DCS-KVVECKOR-FTM                           
050124           MOVE SEGT-IDREFTAB-LFM      TO SEGM-IDREFTAB                   
050125      WHEN WS-KVOT          <  DCS-KVOT-RULL12HF                          
050126      AND  SEGM-KVVECKOR-FT <= DCS-KVVECKOR-FTH                           
050127           MOVE SEGT-IDREFTAB-LFH      TO SEGM-IDREFTAB                   
050128      WHEN WS-KVOT          <  DCS-KVOT-RULL12HF                          
050130           MOVE SEGT-IDREFTAB-LFXH     TO SEGM-IDREFTAB                   
050131                                                                          
050132*---- HIGH-FREQUENCY                                                      
050133      WHEN SEGM-KVVECKOR-FT <= DCS-KVVECKOR-FTL                           
050134           MOVE SEGT-IDREFTAB-HFL      TO SEGM-IDREFTAB                   
050135      WHEN SEGM-KVVECKOR-FT <= DCS-KVVECKOR-FTM                           
050136           MOVE SEGT-IDREFTAB-HFM      TO SEGM-IDREFTAB                   
050137      WHEN SEGM-KVVECKOR-FT <= DCS-KVVECKOR-FTH                           
050138           MOVE SEGT-IDREFTAB-HFH      TO SEGM-IDREFTAB                   
050139      WHEN OTHER                                                          
050140           MOVE SEGT-IDREFTAB-HFXH     TO SEGM-IDREFTAB                   
050141     END-EVALUATE                                                         
050142                                                                          
052900     IF SEGM-IDREFTAB-IN     = SEGM-IDREFTAB                              
053000        MOVE NEJ          TO SEGM-FLTABLE-CHANGED                         
053100     ELSE                                                                 
053200        MOVE JA           TO SEGM-FLTABLE-CHANGED                         
053300     END-IF                                                               
053500     .                                                                    
053600     EJECT                                                                
053700                                                                          
053710 GA-PREPARE-CHECKS SECTION.                                               
053720                                                                          
053723     MOVE ZERO                  TO WS-KVOT                                
053724                                   SEGM-KVVECKOR-FT                       
053725                                                                          
053726*-- GET ALL TABLES FOR THE GIVEN DC AND PROCUREMENT SEGMENT               
053727     MOVE SEGM-IDDC         TO W-IDDC                                     
053728     PERFORM IMS-GU-WDB601                                                
053729     IF SEGMENT-FOUND                                                     
053730        MOVE SEGM-KDANSKSEG-IN  TO W-KDANSKSEG                            
053731        PERFORM IMS-GNP-WDB603                                            
053732        IF SEGMENT-FOUND                                                  
053733           CONTINUE                                                       
053741        ELSE                                                              
053743           MOVE 'A'             TO SEGT-IDREFTAB-LFL                      
053744           MOVE 'A'             TO SEGT-IDREFTAB-LFM                      
053745           MOVE 'A'             TO SEGT-IDREFTAB-LFH                      
053746           MOVE 'A'             TO SEGT-IDREFTAB-LFXH                     
053747           MOVE 'A'             TO SEGT-IDREFTAB-HFL                      
053748           MOVE 'A'             TO SEGT-IDREFTAB-HFM                      
053749           MOVE 'A'             TO SEGT-IDREFTAB-HFH                      
053750           MOVE 'A'             TO SEGT-IDREFTAB-HFXH                     
053751        END-IF                                                            
053752     ELSE                                                                 
053753        MOVE 'A'             TO SEGT-IDREFTAB-LFL                         
053754        MOVE 'A'             TO SEGT-IDREFTAB-LFM                         
053755        MOVE 'A'             TO SEGT-IDREFTAB-LFH                         
053756        MOVE 'A'             TO SEGT-IDREFTAB-LFXH                        
053757        MOVE 'A'             TO SEGT-IDREFTAB-HFL                         
053758        MOVE 'A'             TO SEGT-IDREFTAB-HFM                         
053759        MOVE 'A'             TO SEGT-IDREFTAB-HFH                         
053760        MOVE 'A'             TO SEGT-IDREFTAB-HFXH                        
053761     END-IF                                                               
053762                                                                          
053763*-- GET NUMBER OF PICKS (I.E. KVOT)                                       
053764     MOVE SEGM-IDARTNR          TO W-IDARTNR                              
053765     PERFORM GAA-GET-SUM-KVOT-ORDER-HITS                                  
053770                                                                          
053780                                                                          
053790*-- GET LEADTIME                                                          
053791     PERFORM IMS-GU-WDK722                                                
053792     IF SEGMENT-FOUND                                                     
053794        MOVE XLAG-KVVECKOR-FT  TO SEGM-KVVECKOR-FT                        
053800     END-IF                                                               
053801     .                                                                    
053802     EJECT                                                                
053810                                                                          
053900 GAA-GET-SUM-KVOT-ORDER-HITS SECTION.                                     
054100                                                                          
054200     EVALUATE TRUE                                                        
054300     WHEN SEGM-KDCALL = 002                                               
054400        MOVE SEGM-IDDC                   TO W-IDDC                        
054500        PERFORM IMS-GU-WDL711                                             
054600        IF SEGMENT-FOUND                                                  
054700           MOVE +1                       TO INDX                          
054800           PERFORM UNTIL INDX            >  53                            
054900              ADD DC-KVOT-RULL(INDX)     TO WS-KVOT                       
055000              ADD DC-KVOT-REF-RULL(INDX) TO WS-KVOT                       
055100              ADD +1                     TO INDX                          
055200           END-PERFORM                                                    
055300        END-IF                                                            
055400     WHEN SEGM-KDCALL = 003                                               
055500        PERFORM IMS-GU-WDL811                                             
055600        IF SEGMENT-FOUND                                                  
055700           MOVE +1                       TO INDX                          
055800           PERFORM UNTIL INDX            >  DAGENS-VECKA - 1              
055900              ADD AAR-KVOT-PROG(INDX)    TO WS-KVOT                       
056000              ADD AAR-KVOT-REFILL(INDX)  TO WS-KVOT                       
056100              ADD +1                     TO INDX                          
056200           END-PERFORM                                                    
056300        END-IF                                                            
056400                                                                          
056500        COMPUTE W-TIAAAA  = W-TIAAAA - 1                                  
056600        PERFORM IMS-GU-WDL811                                             
056700        IF SEGMENT-FOUND                                                  
056800           MOVE DAGENS-VECKA             TO INDX                          
056900           PERFORM UNTIL INDX            >  53                            
057000              ADD AAR-KVOT-PROG(INDX)    TO WS-KVOT                       
057100              ADD AAR-KVOT-REFILL(INDX)  TO WS-KVOT                       
057200              ADD +1                     TO INDX                          
057300           END-PERFORM                                                    
057400        END-IF                                                            
057500     END-EVALUATE                                                         
057600     .                                                                    
057700     EJECT                                                                
057800                                                                          
057900 IMS-GU-WDD501    SECTION.                                                
058000                                                                          
058100     STRING 'WDD501  (IDARTNR  =' W-IDARTNR-X ')'                         
058200          DELIMITED BY SIZE INTO SSA1                                     
058300     MOVE '  GE' TO GOOD-STATUSCODES                                      
058400     CALL CBLTDLI USING GU                                                
058500                        WDD5-PCB                                          
058600                        DLI-IO-WDD501                                     
058700                        SSA1                                              
058800     MOVE WDD5-STATUS-CODE    TO STATUS-WS                                
058900     PERFORM IMS-STATUSCHECK                                              
059000     .                                                                    
059100     SKIP3                                                                
059200                                                                          
059300 IMS-GU-WDB601    SECTION.                                                
059400                                                                          
059500     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
059600          DELIMITED BY SIZE INTO SSA1                                     
059700     MOVE '  '                TO GOOD-STATUSCODES                         
059800     CALL CBLTDLI USING GU                                                
059900                        WDB6-PCB                                          
060000                        DLI-IO-WDB601                                     
060100                        SSA1                                              
060200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
060300     PERFORM IMS-STATUSCHECK                                              
060400     .                                                                    
060500     SKIP3                                                                
060600                                                                          
060610 IMS-GNP-WDB603 SECTION.                                                  
060620                                                                          
060630     STRING 'WDB603  (KDANSKSG =' W-KDANSKSEG-X ')'                       
060640          DELIMITED BY SIZE INTO SSA1                                     
060650     MOVE '  GE'            TO GOOD-STATUSCODES                           
060660     CALL CBLTDLI USING GNP                                               
060670                        WDB6-PCB                                          
060680                        DLI-IO-WDB603                                     
060690                        SSA1                                              
060691     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
060692     PERFORM IMS-STATUSCHECK                                              
060693     .                                                                    
060694     SKIP3                                                                
060695                                                                          
060700                                                                          
060800 IMS-GU-WDL711     SECTION.                                               
060900     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
061000          DELIMITED BY SIZE INTO SSA1                                     
061100     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
061200          DELIMITED BY SIZE INTO SSA2                                     
061300     MOVE '  GE' TO GOOD-STATUSCODES                                      
061400     CALL CBLTDLI USING GU                                                
061500                        WDL7-PCB                                          
061600                        DLI-IO-WDL711                                     
061700                        SSA1                                              
061800                        SSA2                                              
061900     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
062000     PERFORM IMS-STATUSCHECK                                              
062100     .                                                                    
062200     SKIP3                                                                
062300                                                                          
062400 IMS-GU-WDL811      SECTION.                                              
062500                                                                          
062600     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
062700          DELIMITED BY SIZE INTO SSA1                                     
062800     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
062900          DELIMITED BY SIZE INTO SSA2                                     
063000     MOVE '  GE' TO GOOD-STATUSCODES                                      
063100     CALL CBLTDLI USING GU                                                
063200                        WDL8-PCB                                          
063300                        DLI-IO-WDL811                                     
063400                        SSA1                                              
063500                        SSA2                                              
063600     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
063700     PERFORM IMS-STATUSCHECK                                              
063800     .                                                                    
063900     SKIP3                                                                
063901                                                                          
063910 IMS-GU-WDK722 SECTION.                                                   
063930                                                                          
063940     MOVE SPACE               TO ALL-SSAS                                 
063950     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
063960          DELIMITED BY SIZE INTO SSA1                                     
063970     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
063980          DELIMITED BY SIZE INTO SSA2                                     
063990     MOVE   'WDK722  '        TO SSA3                                     
063991     MOVE '  GE' TO GOOD-STATUSCODES                                      
063993     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
063994     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
063995     PERFORM IMS-STATUSCHECK                                              
063996     .                                                                    
063997     EJECT                                                                
064000                                                                          
064100 IMS-STATUSCHECK SECTION.                                                 
064200     SKIP2                                                                
064300     SET STATUS-IX               TO 1                                     
064400     SEARCH GOOD-STATUS                                                   
064500       AT END                                                             
064600         STRING ' INCORRECT STATUS CODE FROM IMS: ' STATUS-WS             
064700             DELIMITED BY SIZE INTO ERROR-TEXT-STR                        
064800         DISPLAY ERROR-TEXT                                               
064900         CALL FELLOG                                                      
065000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
065100         CONTINUE                                                         
065200     END-SEARCH                                                           
065300     .                                                                    
065400     EJECT                                                                
065500*    -COPY WY2000P2                                                       
065600     EJECT                                                                
065700*    -COPY WY2000P3                                                       
065800     EJECT                                                                
