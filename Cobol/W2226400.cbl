000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2226400.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   19/02/07.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        CREATE AN OUTPUT FILE TO UPDATE PB-PLAN AND                      
001000*        VALID DATE FOR PB-PLAN.                                          
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- LAGERBANDET                                                
002500     SELECT W01160                     ASSIGN TO W22264D1.                
002600     SKIP2                                                                
002700*          --- OUTPUT FIL TO UPDATE PB-PLAN AND DATE FOR PB-PLAN          
002800     SELECT W22264                     ASSIGN TO W22264D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W01160                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W01160        -PRE  IN-  -L.                                   
003900     SKIP3                                                                
004000 FD  W22264                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  RECORD -COPY W22264 -PRE  OUT-  -L.                                  
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W2226400'.            
004900 77  YES                         PIC X       VALUE 'J'.                   
005000 77  NOO                         PIC X       VALUE 'N'.                   
005100 77  W-TIPBPLAN                  PIC 9(06).                               
005200                                                                          
005210 01  FELTEXT.                                                             
005220     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005230     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005240                                                                          
005300 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W01160                       VALUE 'J'.                   
005500     EJECT                                                                
006200 01  TODAYS-YYWW-PLUS-1V          PIC 9(4)    VALUE ZERO.                 
006300 01  TODAYS-YYWW                  PIC 9(4)    VALUE ZERO.                 
006400 01  FILLER REDEFINES TODAYS-YYWW.                                        
006500     03  TODAYS-DATE-YEAR         PIC 9(2).                               
006600     03  TODAYS-DATE-WEEK         PIC 9(2).                               
006700****************************************** PARAM. W009VADD                
006800 01  W009VADDW.                                                           
006900     03  W009VADDW-AAVV      PIC S9(5)               COMP-3.              
007000     03  W009VADDW-ANTAL     PIC S9(3)               COMP-3.              
007100     EJECT                                                                
007200 01  GENERAL-SUBPROGRAMS.                                                 
007300*                                                                         
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007600     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007700     03  W009VADD            PIC X(8)    VALUE 'W009VADD'.                
007800     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
007900                                                                          
008000 01  FILLER                  PIC X(16) VALUE 'WDATAREA        '.          
008100*    ---PARAMETRAR TILL DATKONV                                           
008200*01  -COPY WDATAREA                                                       
008300     EJECT                                                                
008400*    --- PARAMETERS TO ABEND                                              
008500                                                                          
008600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008900     SKIP2                                                                
009000 01  ERROR-TEXT.                                                          
009100     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
009200     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
009300     EJECT                                                                
009400*    --- PARAMETRAR TILL POSTSUM                                          
009500*                                                                         
009600*01  -COPY W0005   -PRE  POSTSUM-                                         
009700     EJECT                                                                
009800*    --- PARAMETRAR TILL DATKORT                                          
009900*                                                                         
010000 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W22264'.              
010100     SKIP2                                                                
010200 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
010300     SKIP2                                                                
010400*01  -COPY WDATKORT                                                       
010500     EJECT                                                                
010600 01  IN-AREA-START               PIC X(24)   VALUE                        
010700                                 'IN-AREA-START  '.                       
010800     SKIP2                                                                
010900                                                                          
011000*01  AREA -COPY W01160     -PRE IN-                                       
011100     EJECT                                                                
011200 01  OUT-AREA-START              PIC X(24)   VALUE                        
011300                                 'OUT-AREA-START  '.                      
011400     SKIP2                                                                
011500                                                                          
011600*01  AREA -COPY W22264     -PRE OUT-                                      
011700     EJECT                                                                
011800 PROCEDURE DIVISION.                                                      
011900 MAIN SECTION.                                                            
012000                                                                          
012100     PERFORM A-INIT                                                       
012200                                                                          
012300     PERFORM S01-READ-W01160                                              
012400     PERFORM UNTIL END-OF-W01160                                          
012500      IF IN-CLAG-TIPBPLAN-JUST1-FOM > ZERO                                
013100       MOVE 'AAMMDD'                      TO DAT-KDDATFORM                
013200       MOVE IN-CLAG-TIPBPLAN-JUST1-FOM    TO DAT-I-TIDATUM                
013400       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
013500                           DAT-O-TIDATUM DAT-KDSVAR                       
013700       IF DAT-KDSVAR-OK                                                   
013720        IF DAT-TIAAVV-GRP <= TODAYS-YYWW-PLUS-1V                          
013800         MOVE IN-CLAG-IDARTNR            TO OUT-IDARTNR                   
013900         MOVE IN-CLAG-KVPB-PLAN-JUST1    TO OUT-KVPB-PLAN                 
014000         MOVE 20                         TO OUT-DAPBPLAN(1:2)             
014100         MOVE IN-CLAG-TIPBPLAN-JUST1-TOM TO W-TIPBPLAN                    
014200         MOVE W-TIPBPLAN                 TO OUT-DAPBPLAN(3:6)             
014300                                                                          
014400         MOVE IN-CLAG-KVPB-PLAN-JUST2    TO OUT-KVPB-PLAN-JUST1           
014500         MOVE IN-CLAG-TIPBPLAN-JUST2-FOM TO OUT-TIPBPLAN-JUST1-FOM        
014600         MOVE IN-CLAG-TIPBPLAN-JUST2-TOM TO OUT-TIPBPLAN-JUST1-TOM        
014700                                                                          
014800         MOVE +0                         TO OUT-KVPB-PLAN-JUST2           
014900         MOVE ZERO                       TO OUT-TIPBPLAN-JUST2-FOM        
015000         MOVE ZERO                       TO OUT-TIPBPLAN-JUST2-TOM        
015100                                                                          
015200         PERFORM S11-WRITE-W22264                                         
015300        END-IF                                                            
015310       ELSE                                                               
015311        STRING ' FEL FRÅN WDATKONV I W2226400'                            
015313        DELIMITED BY SIZE INTO FELTEXT-STR                                
015314        DISPLAY FELTEXT                                                   
015315        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
015316       END-IF                                                             
015320      END-IF                                                              
035535      PERFORM S01-READ-W01160                                             
035536     END-PERFORM                                                          
035537                                                                          
035538                                                                          
035539     PERFORM Z-FINIT                                                      
035540                                                                          
035541     MOVE ZERO TO RETURN-CODE                                             
035542     GOBACK                                                               
035543     .                                                                    
035544     EJECT                                                                
035545 A-INIT SECTION.                                                          
035546                                                                          
035547     OPEN INPUT  W01160                                                   
035548                                                                          
035549     OPEN OUTPUT W22264                                                   
035550     SKIP2                                                                
035551     MOVE IDPGM          TO POSTSUM-PROGNAMN                              
035552                                                                          
035553     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
035554     MOVE D-AAR          TO TODAYS-DATE-YEAR                              
035555     MOVE D-VECKA        TO TODAYS-DATE-WEEK                              
035556                                                                          
035557     MOVE TODAYS-YYWW    TO W009VADDW-AAVV                                
035560     MOVE 1              TO W009VADDW-ANTAL                               
035561     CALL W009VADD USING W009VADDW-AAVV W009VADDW-ANTAL                   
035562     MOVE W009VADDW-AAVV TO TODAYS-YYWW-PLUS-1V                           
035563     .                                                                    
035564     EJECT                                                                
035565 Z-FINIT SECTION.                                                         
035566     CLOSE W01160                                                         
035567           W22264                                                         
035568     SKIP2                                                                
035569     MOVE 'S' TO POSTSUM-OPKOD                                            
035570     CALL POSTSUM USING POSTSUM-PARM                                      
035571     .                                                                    
035572     EJECT                                                                
035573 S01-READ-W01160  SECTION.                                                
035574     READ W01160 INTO IN-AREA                                             
035575     AT END                                                               
035576        MOVE HIGH-VALUE TO IN-AREA                                        
035577        SET END-OF-W01160 TO TRUE                                         
035578                                                                          
035579     NOT AT END                                                           
035580        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
035581        MOVE 'W01160'   TO POSTSUM-FDNAMN                                 
035582        MOVE 'W22264D1' TO POSTSUM-DDNAMN2                                
035583        CALL POSTSUM USING POSTSUM-PARM                                   
035584     END-READ                                                             
035585     .                                                                    
035586     EJECT                                                                
035587 S11-WRITE-W22264 SECTION.                                                
035588                                                                          
035589     WRITE OUT-RECORD FROM OUT-AREA                                       
035590                                                                          
035591     MOVE 'OUT'      TO POSTSUM-TRANSTYP                                  
035592     MOVE 'W22264'   TO POSTSUM-FDNAMN                                    
035593     MOVE 'W22264D2' TO POSTSUM-DDNAMN2                                   
035594     CALL POSTSUM USING POSTSUM-PARM                                      
035595     .                                                                    
035596     EJECT                                                                
035597 S99-ABEND SECTION.                                                       
035598                                                                          
035599     SKIP2                                                                
035600     MOVE 'S' TO POSTSUM-OPKOD                                            
035601     CALL POSTSUM USING POSTSUM-PARM                                      
035610     CALL ABEND USING RKOD-ABEND                                          
035700     .                                                                    
