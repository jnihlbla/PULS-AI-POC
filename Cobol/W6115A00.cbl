000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6115A00.                                                
000400*AUTHOR.         CONNY EGHOLT.                                            
000500*DATE-WRITTEN.   2008/03/12.                                              
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        READS THROUGH W6G1 AND CREATES AN EXTRACT FILE                   
001100*        WITH ESTIMATED NORMAL AND PRIO LEAD TIME FOR ALL IDDC            
001200*        STATED IN THE CTX W611W6G1.                                      
001300*        LAST OUT-RECORD WITH NORMAL-TOT AND PRIO-TOT LEAD TIME           
001400*        IS READ FROM W6GX6008-SEGMENT UNDER ROOT-SEG FOR DC=11           
001500*                                                                         
001510*        SUPERFLUOUS RECORDS WITH SAME 6006-KDINLUPF                      
001511*        IS WRITTEN TO THE W6115B-FILE FOR REPORT TO USERS                
001520*                                                                         
001600*        PROGRAM READS         W6G1                                       
001700*                                                                         
001800*    CHANGES:                                                             
001810*                                                                         
001900*                                                                         
002000*    ABEND CODES:                                                         
002100*        U0016 -  . . . .                                                 
002200*        U1000 -  . . . .                                                 
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*          --- FILE TO W6115500. SORTED ON IDDC                           
003300     SELECT W6115A                     ASSIGN TO W6115AD1.                
003400     SKIP2                                                                
003500*          --- DUBBLETT-FIL                                               
003600     SELECT W6115B                     ASSIGN TO W6115AD2.                
003700     SKIP2                                                                
003800*          --- SORT FILE                                                  
003900     SELECT SORTFILE                   ASSIGN TO W6115ADS.                
004000     EJECT                                                                
004100                                                                          
004200 DATA DIVISION.                                                           
004300     SKIP3                                                                
004400                                                                          
004500 FILE SECTION.                                                            
004600     SKIP2                                                                
004700                                                                          
004800 FD  W6115A                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005200*01  POST -COPY W6115A01 -PRE OUT-  -L.                                   
005300     SKIP3                                                                
005400                                                                          
005500 FD  W6115B                                                               
005600     RECORDING       F                                                    
005700     BLOCK CONTAINS  0.                                                   
005800                                                                          
005900*01  POST -COPY W6115B01 -PRE DUB-  -L.                                   
006000     SKIP2                                                                
006100                                                                          
006200 SD  SORTFILE.                                                            
006300                                                                          
006400 01  SORT-POST.                                                           
006500     03 SORT-6006-IDDC   PIC XX.                                          
006600*    03 -COPY W6GX6006  -PRE SORT- .                                      
006700                                                                          
006800     EJECT                                                                
006900                                                                          
007000 WORKING-STORAGE SECTION.                                                 
007100     SKIP2                                                                
007200                                                                          
007300*    -- CHECKED BY WY2000                                                 
007400 77  IDPGM                       PIC X(8)    VALUE 'W6115A00'.            
007700 77  IX                          PIC S9(4)   COMP-3.                      
007800 77  SAVED-KDINLUPF              PIC X(4)    VALUE SPACE.                 
007810 77  SAVED-KVTID-NORM            PIC 9(4)    VALUE ZERO.                  
007820 77  SAVED-KVTID-PRIO            PIC 9(4)    VALUE ZERO.                  
007900*                                                                         
008000 77  SORTFILE-EOF-SW             PIC X       VALUE 'N'.                   
008100     88  END-OF-SORTFILE                     VALUE 'J'.                   
008300                                                                          
008310 77  TIME-CHECK                  PIC X       VALUE 'Y'.                   
008320     88  TIMES-OK                            VALUE 'Y'.                   
008330     88  TIMES-MISMATCH                      VALUE 'N'.                   
008340     EJECT                                                                
008350                                                                          
008400 01  TODAY-DATE                  PIC 9(6)    VALUE ZERO.                  
008500 01  FILLER REDEFINES TODAY-DATE.                                         
008600     03  TODAY-DATE-YEAR         PIC 9(2).                                
008700     03  TODAY-DATE-MONTH        PIC 9(2).                                
008800     03  TODAY-DATE-DAY          PIC 9(2).                                
008900     EJECT                                                                
009000                                                                          
009001 01 SAVE-AREA.                                                            
009030     03 WS-OUT OCCURS 10.                                                 
009040        05 WS-OUT-IDDC           PIC X(2)   VALUE SPACE.                  
009041        05 WS-OUT-ADINLOMR       PIC X(4)   VALUE SPACE.                  
009050        05 WS-OUT-KDINLUPF       PIC X(4)   VALUE SPACE.                  
009060        05 WS-OUT-KVTID-NORM     PIC 9(4)   VALUE ZERO COMP-3.            
009070        05 WS-OUT-KVTID-PRIO     PIC 9(4)   VALUE ZERO COMP-3.            
009081                                                                          
009090 01  W-INDICIES.                                                          
009091     03 IX-OUT                   PIC S9(9)   VALUE ZERO COMP-3.           
009092     03 IX-WRI-OUT               PIC S9(9)   VALUE ZERO COMP-3.           
009100*      --- VALID IDDC CODES                                               
009200*01    -COPY WWDC99                                                       
009300       EJECT                                                              
009400                                                                          
009600*      -- VALID IDDC'S  ON W6G101 FOR LEAD TIME CALCULATION.              
009700*      -COPY W611W6G1.                                                    
009800       EJECT                                                              
009900                                                                          
010000 01  GENERAL-SUB-PROGRAMS.                                                
010100*                                                                         
010200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010500     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
010600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010700     SKIP2                                                                
010800*    --- PARAMETERS TO ABEND                                              
010900                                                                          
011000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011010 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
011200     SKIP2                                                                
011300 01  ERRTEXT.                                                             
011400     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
011500     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
011600     EJECT                                                                
011700                                                                          
011800*    --- PARAMETERS TO DATKORT                                            
011900*                                                                         
012000 01  PROGRAM-NAME                PIC X(6)    VALUE 'W6115A'.              
012100     SKIP2                                                                
012200 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
012300     SKIP2                                                                
012400*01  -COPY WDATKORT                                                       
012500     EJECT                                                                
012600*                                                                         
012700*    --- PARAMETERS TO POSTSUM                                            
012800*01  -COPY W0005   -PRE  POSTSUM-                                         
012900     EJECT                                                                
013000                                                                          
013100 01  OUT-AREA-START     PIC X(24)   VALUE 'OUT-AREA-START  '.             
013200     SKIP2                                                                
013300*01  AREA -COPY W6115A01  -PRE OUT-                                       
013400     EJECT                                                                
013500                                                                          
013600 01  DUB-AREA-START     PIC X(24)   VALUE 'DUB-AREA-START  '.             
013700     SKIP2                                                                
013800*01  DUB-AREA -COPY W6115B01                                              
013900     EJECT                                                                
014000                                                                          
014100 01  SORTWS-AREA-START  PIC X(24)   VALUE 'SORTWS-AREA-START  '.          
014200     SKIP2                                                                
014300 01  SORTWS-AREA.                                                         
014400     03 SORTWS-AREA-1.                                                    
014500        05 SORTWS-IDDC           PIC X(2).                                
014600*    03 AREA-2 -COPY W6GX6006    -PRE SORTWS-                             
014700                                                                          
014800 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
014900     EJECT                                                                
015000                                                                          
015100*    --- WORK AREAS TO IMS-SECTIONS                                       
015200     EJECT                                                                
015300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015400     SKIP3                                                                
015500 01  KEYS-FOR-DLI-CALLS.                                                  
015600     SKIP3                                                                
015700     03  W-W6GXKEY-6005-X.                                                
015800         05  W-6005-IDHTYP       PIC X(4)    VALUE '6005'.                
015900         05  W-6005-IDDC         PIC X(2)    VALUE SPACE.                 
016000         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
016100                                                                          
016200     03  W-W6GXKEY-6006-X.                                                
016300         05  W-6006-ADINLOMR     PIC X(4)    VALUE SPACE.                 
016400         05  FILLER              PIC X(1)    VALUE LOW-VALUE.             
016500                                                                          
016600     SKIP3                                                                
016700*    --- STATUS-CODE FROM IMS                                             
016800 01  STATUS-WS                   PIC XX.                                  
016900     88  SEGMENT-FOUND                VALUE  '  '.                        
017000     88  SEGMENT-MISSING              VALUES 'GE' 'GB'.                   
017100     SKIP2                                                                
017200 01  GOOD-STATUS-CODES.                                                   
017300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017400     SKIP3                                                                
017500 01  SSA1                        PIC X(64).                               
017600 01  SSA2                        PIC X(64).                               
017700     EJECT                                                                
017800                                                                          
017900*    --- IMS FUNCTION CODES                                               
018000*01  -COPY W0003                                                          
018100     EJECT                                                                
018200                                                                          
018300*    ---  DLI INPUT-OUTPUT AREA                                           
018400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
018500     SKIP3                                                                
018600 01  DLI-IO-AREA.                                                         
018700     SKIP3                                                                
018800     03 DLI-IO-W6G101.                                                    
018900*       05 -COPY W6GX01                                                   
019000        05 RED-W6GX01 REDEFINES ROT-W6GX01.                               
019100           07 FILLER             PIC XXXX.                                
019200           07 ROT-IDDC           PIC XX.                                  
019300     03 DLI-IO-W6G130.                                                    
019400*       05 -COPY W6GX6006                                                 
019500     03 DLI-IO-W6GX6008.                                                  
019600*       05 -COPY W6GX6008                                                 
019700     EJECT                                                                
019800 LINKAGE SECTION.                                                         
019900                                                                          
020000*01  -COPY W0008  -PRE W6G1-                                              
020100     05  FILLER                  PIC X.                                   
020200     EJECT                                                                
020300 PROCEDURE DIVISION  USING W6G1-PCB.                                      
020400     ENTRY 'DLITCBL' USING W6G1-PCB.                                      
020410                                                                          
020420     PERFORM A-INIT                                                       
020500                                                                          
020600     SORT SORTFILE ASCENDING KEY SORT-6006-IDDC                           
020700                                 SORT-6006-KDINLUPF                       
020800                                 SORT-6006-ADINLOMR                       
020900        INPUT    PROCEDURE B-SORT-INPUT                                   
021000        OUTPUT   PROCEDURE C-SORT-OUTPUT                                  
021100                                                                          
021200     IF SORT-RETURN NOT = 0                                               
021210        PERFORM S90-ABNORMAL-SORT-ENDING                                  
022000     ELSE                                                                 
022100        PERFORM Z-FINIT                                                   
022300        MOVE ZERO TO RETURN-CODE                                          
022400        GOBACK                                                            
022500     END-IF                                                               
022600     .                                                                    
022700     EJECT                                                                
022800 A-INIT SECTION.                                                          
022900                                                                          
023000     OPEN OUTPUT W6115A                                                   
023010     OPEN OUTPUT W6115B                                                   
023100     SKIP2                                                                
023200     CALL DATKORT USING PROGRAM-NAME DATUMKORT-ID DATUMKORT               
023300     MOVE D-AAR      TO TODAY-DATE-YEAR                                   
023400     MOVE D-MAANAD   TO TODAY-DATE-MONTH                                  
023500     MOVE D-DAG      TO TODAY-DATE-DAY                                    
023600     MOVE IDPGM      TO POSTSUM-PROGNAMN                                  
023610     MOVE +1 TO IX-OUT                                                    
023700     .                                                                    
023800     EJECT                                                                
023900                                                                          
024000 B-SORT-INPUT               SECTION.                                      
024100     SKIP2                                                                
024200     MOVE +1 TO IX                                                        
024300     PERFORM UNTIL IX > LT-IX-MAX                                         
024400        MOVE LT-DC (IX)  TO W-6005-IDDC                                   
024500        PERFORM IMS-GU-W6G101                                             
024600                                                                          
024700        IF SEGMENT-FOUND                                                  
024800           PERFORM IMS-GNP-W6G130                                         
025000           PERFORM UNTIL SEGMENT-MISSING                                  
025100              MOVE W-6005-IDDC   TO SORTWS-AREA-1                         
025200              MOVE 6006-W6GX6006 TO SORTWS-AREA-2                         
025300              PERFORM S31-SORT-RELEASE                                    
025400              PERFORM IMS-GNP-W6G130                                      
025500           END-PERFORM                                                    
025510*          -- GET AT LAST THE TOTAL TIME SEGMENT                          
025600           IF W-6005-IDDC = '11'                                          
025601              PERFORM IMS-GNP-W6GX6008                                    
025602              IF SEGMENT-FOUND                                            
025603                 MOVE W-6005-IDDC        TO SORTWS-IDDC                   
025604                 MOVE SPACE              TO SORTWS-6006-IDLEVNR           
025605                 MOVE SPACE              TO SORTWS-6006-FLEXCP            
025606                                                                          
025607                 MOVE 'TOT '             TO SORTWS-6006-KDINLUPF          
025608                 MOVE 6008-KVTID-NORMTOT TO SORTWS-6006-KVTID-NORM        
025609                 MOVE 6008-KVTID-PRIOTOT TO SORTWS-6006-KVTID-PRIO        
025613                 PERFORM S31-SORT-RELEASE                                 
025614                                                                          
025615                 MOVE 'CDC '             TO SORTWS-6006-KDINLUPF          
025616                 MOVE 6008-KVTID-NTCDC   TO SORTWS-6006-KVTID-NORM        
025617                 MOVE 6008-KVTID-PTCDC   TO SORTWS-6006-KVTID-PRIO        
025618                 PERFORM S31-SORT-RELEASE                                 
025619                                                                          
025620                 MOVE 'SVS '             TO SORTWS-6006-KDINLUPF          
025621                 MOVE 6008-KVTID-NTSVS   TO SORTWS-6006-KVTID-NORM        
025622                 MOVE 6008-KVTID-PTSVS   TO SORTWS-6006-KVTID-PRIO        
025623                 PERFORM S31-SORT-RELEASE                                 
025624              END-IF                                                      
025625           END-IF                                                         
025630        END-IF                                                            
025700        ADD +1 TO IX                                                      
025900     END-PERFORM                                                          
026000     .                                                                    
026100     EJECT                                                                
026200                                                                          
026300 C-SORT-OUTPUT              SECTION.                                      
026400     SKIP2                                                                
026500     PERFORM S32-SORT-RETURN                                              
026600                                                                          
026700     PERFORM UNTIL END-OF-SORTFILE                                        
026800        IF SORTWS-6006-KDINLUPF = SAVED-KDINLUPF                          
026900*          --- MULTIPLE KDINLUPF RECORDS TO DUB-W6115B                    
026910*          --- FOR LATER TREATMENT AND REPORT TO USERS                    
027010           IF ((SORTWS-6006-KVTID-NORM NOT EQUAL                          
027011                SAVED-KVTID-NORM)                                         
027021                           AND                                            
027022              (SORTWS-6006-KVTID-PRIO  NOT EQUAL                          
027023               SAVED-KVTID-PRIO))                                         
027030              SET TIMES-MISMATCH TO TRUE                                  
027040           END-IF                                                         
027100        ELSE                                                              
027110           IF TIMES-MISMATCH                                              
027111              MOVE 1 TO IX-WRI-OUT                                        
027112              PERFORM CB-CREATE-W6115B-RECORD                             
027115***** RESET FLAG                                                          
027117              SET TIMES-OK TO TRUE                                        
027120           END-IF                                                         
027130           MOVE 1 TO IX-OUT                                               
027200           PERFORM CA-CREATE-W6115A-REC                                   
027300        END-IF                                                            
027310        PERFORM S33-SAVE-SAME-STATUS-DATA                                 
027400        PERFORM S32-SORT-RETURN                                           
027500     END-PERFORM                                                          
027510**** TO PROCESS THE LAST SET OF UNPROCESSED RECORDS IF ANY                
027520     IF TIMES-MISMATCH                                                    
027530        MOVE 1 TO IX-WRI-OUT                                              
027540        PERFORM CB-CREATE-W6115B-RECORD                                   
027560        SET TIMES-OK TO TRUE                                              
027570     END-IF                                                               
027700     .                                                                    
027800     EJECT                                                                
027900                                                                          
027910 CA-CREATE-W6115A-REC            SECTION.                                 
027920     SKIP2                                                                
027930     MOVE SORTWS-IDDC              TO OUT-IDDC                            
027940     MOVE SORTWS-6006-KDINLUPF     TO OUT-KDINLUPF                        
027941                                      SAVED-KDINLUPF                      
027950     MOVE SORTWS-6006-KVTID-NORM   TO OUT-KVTID-NORM                      
027960     MOVE SORTWS-6006-KVTID-PRIO   TO OUT-KVTID-PRIO                      
027970     MOVE SORTWS-6006-IDLEVNR      TO OUT-IDLEVNR                         
027971     MOVE SORTWS-6006-FLEXCP       TO OUT-FLEXCP                          
027972     MOVE SORTWS-6006-IDAVD-DAG    TO OUT-IDAVD-DAG                       
027973     MOVE SORTWS-6006-IDGRUPP-DAG  TO OUT-IDGRUPP-DAG                     
027974     MOVE SORTWS-6006-IDAVD-NATT   TO OUT-IDAVD-NATT                      
027975     MOVE SORTWS-6006-IDGRUPP-NATT TO OUT-IDGRUPP-NATT                    
027980                                                                          
027990     PERFORM S11-WRITE-W6115A                                             
027992     .                                                                    
027993     EJECT                                                                
027994                                                                          
028000 CB-CREATE-W6115B-RECORD         SECTION.                                 
028100     SKIP2                                                                
028110     PERFORM UNTIL IX-WRI-OUT >= IX-OUT                                   
028200       MOVE WS-OUT-IDDC       (IX-WRI-OUT)  TO DUB-IDDC                   
028300       MOVE WS-OUT-ADINLOMR   (IX-WRI-OUT)  TO DUB-ADINLOMR               
028400       MOVE WS-OUT-KDINLUPF   (IX-WRI-OUT)  TO DUB-KDINLUPF               
028500       MOVE WS-OUT-KVTID-NORM (IX-WRI-OUT)  TO DUB-KVTID-NORM             
028600       MOVE WS-OUT-KVTID-PRIO (IX-WRI-OUT)  TO DUB-KVTID-PRIO             
028610       ADD 1 TO IX-WRI-OUT                                                
028700                                                                          
028800       PERFORM S12-WRITE-W6115B                                           
028810     END-PERFORM                                                          
028900     .                                                                    
029000     EJECT                                                                
029100                                                                          
031910 S11-WRITE-W6115A SECTION.                                                
031920     SKIP2                                                                
031930     WRITE OUT-POST FROM OUT-AREA                                         
031940                                                                          
031950     MOVE 'W6115A'             TO POSTSUM-FDNAMN                          
031960     MOVE 'W6115AD1'           TO POSTSUM-DDNAMN2                         
031970     CALL POSTSUM USING POSTSUM-PARM                                      
031980     .                                                                    
031990     EJECT                                                                
031991                                                                          
031992 S12-WRITE-W6115B SECTION.                                                
031993     SKIP2                                                                
031994     WRITE DUB-POST FROM DUB-AREA                                         
031995                                                                          
031996     MOVE 'W6115B'             TO POSTSUM-FDNAMN                          
031997     MOVE 'W6115AD2'           TO POSTSUM-DDNAMN2                         
031998     CALL POSTSUM USING POSTSUM-PARM                                      
031999     .                                                                    
032000     EJECT                                                                
032010                                                                          
032100 S31-SORT-RELEASE  SECTION.                                               
032200     SKIP2                                                                
032300     RELEASE SORT-POST FROM SORTWS-AREA                                   
032400     .                                                                    
032500     EJECT                                                                
032600                                                                          
032700 S32-SORT-RETURN  SECTION.                                                
032800     SKIP2                                                                
032900     RETURN SORTFILE INTO SORTWS-AREA                                     
033000     AT END                                                               
033100         SET END-OF-SORTFILE TO TRUE                                      
033200     .                                                                    
033300     EJECT                                                                
033310                                                                          
033311 S33-SAVE-SAME-STATUS-DATA  SECTION.                                      
033312     SKIP2                                                                
033314     MOVE SORTWS-IDDC            TO WS-OUT-IDDC (IX-OUT)                  
033315     MOVE SORTWS-6006-ADINLOMR   TO WS-OUT-ADINLOMR (IX-OUT)              
033316     MOVE SORTWS-6006-KDINLUPF   TO WS-OUT-KDINLUPF (IX-OUT)              
033317     MOVE SORTWS-6006-KVTID-NORM TO WS-OUT-KVTID-NORM (IX-OUT)            
033318                                    SAVED-KVTID-NORM                      
033319     MOVE SORTWS-6006-KVTID-PRIO TO WS-OUT-KVTID-PRIO (IX-OUT)            
033320                                    SAVED-KVTID-PRIO                      
033321     ADD 1                       TO IX-OUT                                
033323     .                                                                    
033324     EJECT                                                                
033325 S90-ABNORMAL-SORT-ENDING  SECTION.                                       
033326     SKIP2                                                                
033327     MOVE SORT-RETURN TO SORT-RETURN-X                                    
033328     STRING 'RETURN CODE ' SORT-RETURN-X ' FROM SORT'                     
033329         DELIMITED BY SIZE                                                
033330         INTO ERRTEXT-STR                                                 
033331     DISPLAY ERRTEXT                                                      
033332     MOVE RKOD-ABEND-NO-DUMP      TO RKOD-ABEND                           
033333                                                                          
033334     PERFORM S99-ABEND                                                    
033335     .                                                                    
033336     EJECT                                                                
033337                                                                          
033338 S99-ABEND    SECTION.                                                    
033339     SKIP2                                                                
033340     CALL ABEND USING RKOD-ABEND                                          
033341     .                                                                    
033350     EJECT                                                                
033400                                                                          
036110 Z-FINIT SECTION.                                                         
036120     CLOSE W6115A                                                         
036121     CLOSE W6115B                                                         
036130     SKIP2                                                                
036140     MOVE 'S'                  TO POSTSUM-OPKOD                           
036150     CALL POSTSUM USING POSTSUM-PARM                                      
036160     .                                                                    
036170     EJECT                                                                
036180                                                                          
036200* --- IMS SEKTIONER ---                                                   
036300     SKIP3                                                                
036400 IMS-GU-W6G101        SECTION.                                            
036500     SKIP2                                                                
036600     STRING 'W6G101  (W6GXKEY  =' W-W6GXKEY-6005-X ')'                    
036700          DELIMITED BY SIZE INTO SSA1                                     
036800     CALL CBLTDLI USING GU W6G1-PCB DLI-IO-W6G101  SSA1                   
036900     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
037000     MOVE '  GE' TO GOOD-STATUS-CODES                                     
037100     PERFORM IMS-STATUS-CONTROL                                           
037200     .                                                                    
037300     EJECT                                                                
037400 IMS-GNP-W6G130        SECTION.                                           
037500     SKIP2                                                                
037700     MOVE   'W6G130   '       TO SSA1                                     
037800     CALL CBLTDLI USING GNP W6G1-PCB DLI-IO-W6G130 SSA1                   
037900     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
038000     MOVE '  GE' TO GOOD-STATUS-CODES                                     
038100     PERFORM IMS-STATUS-CONTROL                                           
038200     .                                                                    
038300     EJECT                                                                
038400 IMS-GNP-W6GX6008      SECTION.                                           
038500     SKIP2                                                                
038800     MOVE   'W6GX6008  '      TO SSA1                                     
038900     CALL CBLTDLI USING GNP W6G1-PCB DLI-IO-W6GX6008 SSA1                 
039000     MOVE W6G1-STATUS-CODE TO STATUS-WS                                   
039100     MOVE '  GE' TO GOOD-STATUS-CODES                                     
039200     PERFORM IMS-STATUS-CONTROL                                           
039300     .                                                                    
039400     EJECT                                                                
039500 IMS-STATUS-CONTROL SECTION.                                              
039600     SKIP2                                                                
039700     SET STATUS-IX TO 1                                                   
039800     SEARCH GOOD-STATUS                                                   
039900       AT END                                                             
040000         MOVE 'FELAKTIG KOD VID IMS LÄSNING' TO ERRTEXT-STR               
040100         DISPLAY ERRTEXT                                                  
040200         CALL FELLOG                                                      
040300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
040400         CONTINUE                                                         
040500     END-SEARCH                                                           
040600     .                                                                    
