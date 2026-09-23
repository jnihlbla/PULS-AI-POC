000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2223200.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   15/12/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        CALCULATION OF ASSETS IN CDC                                     
000900*                                                                         
001000*                                                                         
001100*    ABENDCODES:                                                          
001200*        U0016 -  . . . .                                                 
001300*        U1000 -  . . . .                                                 
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- PART FILE WDK6                                             
002400     SELECT W01160                     ASSIGN TO W22232D1.                
002500     SKIP2                                                                
002600*          --- OUTPUT FILE TO UPDATE WDK611                               
002700     SELECT W22232                     ASSIGN TO W22232D2.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP2                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W01160                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  RECORD -COPY W01160 -PRE IN-  -L.                                    
003800     SKIP3                                                                
003900 FD  W22232                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  RECORD -COPY W22232 -PRE  OUT-       -L.                             
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700 77  IDPGM                       PIC X(8)    VALUE 'W2223200'.            
004800 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
004900 77  YES                         PIC X       VALUE 'J'.                   
005000 77  NOO                         PIC X       VALUE 'N'.                   
005100                                                                          
005200 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W01160                       VALUE 'J'.                   
005400     EJECT                                                                
005500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES TODAYS-DATE.                                        
005700     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005800     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005900     03  TODAYS-DATE-DAY         PIC 9(2).                                
006000     EJECT                                                                
006100 01  GENERAL-SUBPROGRAMS.                                                 
006200*                                                                         
006300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006700     03  W222TILG                PIC X(8)    VALUE 'W222TILG'.            
006800     SKIP2                                                                
006900*    --- PARAMETERS FOR SUB PROGRAM W222TILG                              
007000 01  FILLER                      PIC X(16)   VALUE 'W222TILG'.            
007100*01  -COPY W222TILG                                                       
007200     EJECT                                                                
007300*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007400                                                                          
007500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007800     SKIP2                                                                
007900 01  ERROR-TEXT.                                                          
008000     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
008100     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL POSTSUM                                          
008400*                                                                         
008500*01  -COPY W0005   -PRE  POSTSUM-                                         
008600     EJECT                                                                
008700 01  W01160-AREA-START              PIC X(24)   VALUE                     
008800                                 'W01160-AREA-START  '.                   
008900*01  AREA -COPY W01160  -PRE IN-                                          
009000     EJECT                                                                
009100 01  OUT-AREA-START              PIC X(24)   VALUE                        
009200                                 'OUT-AREA-START  '.                      
009300     SKIP2                                                                
009400                                                                          
009500*01  AREA -COPY W22232     -PRE OUT-                                      
009600     EJECT                                                                
009700*    --- AREAS FOR IMS-SECTIONS                                           
009800*                                                                         
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010100     SKIP3                                                                
010200*    --- STATUS-KOD FRÅN IMS                                              
010300 01  STATUS-WS                   PIC XX.                                  
010400     88  SEGMENT-FOUND                       VALUE '  '.                  
010500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
010600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
010700     SKIP2                                                                
010800 01  GOOD-STATUSCODES.                                                    
010900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011000     SKIP3                                                                
011100 01  SSA1                        PIC X(64).                               
011200 01  SSA2                        PIC X(64).                               
011300     EJECT                                                                
011400*    --- IMS FUNCTION CODES                                               
011500*01  -COPY W0003                                                          
011600     EJECT                                                                
011700*    ---  DLI INPUT-OUTPUT AREA                                           
011800     EJECT                                                                
011900 LINKAGE SECTION.                                                         
012000                                                                          
012100 01  TILG-WDK7-PCB               PIC X.                                   
012200 01  TILG-WDL2-PCB               PIC X.                                   
012300 01  TILG-WDB6-PCB               PIC X.                                   
012400 01  TILG-WDD9-PCB               PIC X.                                   
012410 01  TILG-WDK6-PCB               PIC X.                                   
012420 01  TILG-WDK9-PCB               PIC X.                                   
012500     EJECT                                                                
012600 PROCEDURE DIVISION  USING TILG-WDK7-PCB TILG-WDL2-PCB                    
012700                           TILG-WDB6-PCB TILG-WDD9-PCB                    
012710                           TILG-WDK6-PCB TILG-WDK9-PCB.                   
012800 MAIN SECTION.                                                            
012900     ENTRY 'DLITCBL' USING TILG-WDK7-PCB TILG-WDL2-PCB                    
013000                           TILG-WDB6-PCB TILG-WDD9-PCB                    
013010                           TILG-WDK6-PCB TILG-WDK9-PCB.                   
013100                                                                          
013200                                                                          
013300     PERFORM A-INIT                                                       
013400                                                                          
013500     PERFORM S01-READ-W01160                                              
013600     PERFORM UNTIL END-OF-W01160                                          
013700       IF IN-CLAG-KDERS-UTG > +0                                          
013800       OR IN-CLAG-KDSORT    = 'SW'                                        
013900          CONTINUE                                                        
014000       ELSE                                                               
014100          MOVE IN-CLAG-IDARTNR      TO TILG-IDARTNR                       
014200          CALL W222TILG  USING TILG-W222TILG                              
014300                               TILG-WDK7-PCB TILG-WDL2-PCB                
014400                               TILG-WDB6-PCB TILG-WDD9-PCB                
014410                               TILG-WDK6-PCB TILG-WDK9-PCB                
014500                                                                          
014600          MOVE TILG-IDARTNR      TO OUT-IDARTNR                           
014700          MOVE TILG-KVRETUR-TOT  TO OUT-KVRETUR-TOT                       
014800          MOVE TILG-KVREFOVL-TOT TO OUT-KVREFOVL-TOT                      
014900          MOVE TILG-KVAVROP-TOT  TO OUT-KVAVROP-TOT                       
015000          MOVE TILG-KVTILLG-TOT  TO OUT-KVTILLG-TOT                       
015100                                                                          
015200          PERFORM S11-WRITE-W22232                                        
015300       END-IF                                                             
015400                                                                          
015500       PERFORM S01-READ-W01160                                            
015600                                                                          
015700     END-PERFORM                                                          
015800                                                                          
015900                                                                          
016000     PERFORM Z-FINIT                                                      
016100                                                                          
016200     MOVE ZERO TO RETURN-CODE                                             
016300     GOBACK                                                               
016400     .                                                                    
016500     EJECT                                                                
016600 A-INIT SECTION.                                                          
016700                                                                          
016800     OPEN INPUT  W01160                                                   
016900                                                                          
017000     OPEN OUTPUT W22232                                                   
017100                                                                          
017200     ACCEPT TODAYS-DATE  FROM DATE                                        
017300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017400     .                                                                    
017500     EJECT                                                                
017600 Z-FINIT SECTION.                                                         
017700     CLOSE W01160                                                         
017800           W22232                                                         
017900     SKIP2                                                                
018000     MOVE 'S' TO POSTSUM-OPKOD                                            
018100     CALL POSTSUM USING POSTSUM-PARM                                      
018200     .                                                                    
018300     EJECT                                                                
018400 S01-READ-W01160  SECTION.                                                
018500     MOVE 'S01-READ-W01160        ' TO CURRENT-SECTION                    
018600                                                                          
018700     READ W01160 INTO IN-AREA                                             
018800     AT END                                                               
018900        SET END-OF-W01160 TO TRUE                                         
019000                                                                          
019100     NOT AT END                                                           
019200        MOVE 'W01160' TO POSTSUM-FDNAMN                                   
019300        MOVE 'W22232D1' TO POSTSUM-DDNAMN2                                
019400        CALL POSTSUM USING POSTSUM-PARM                                   
019500     END-READ                                                             
019600     .                                                                    
019700     EJECT                                                                
019800                                                                          
019900 S11-WRITE-W22232 SECTION.                                                
020000                                                                          
020100     WRITE OUT-RECORD FROM OUT-AREA                                       
020200                                                                          
020300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
020400     MOVE 'OUT'      TO POSTSUM-FDNAMN                                    
020500     MOVE 'W22232D2' TO POSTSUM-DDNAMN2                                   
020600     CALL POSTSUM USING POSTSUM-PARM                                      
020700     .                                                                    
020800     EJECT                                                                
020900 S99-ABEND SECTION.                                                       
021000                                                                          
021100     SKIP2                                                                
021200     MOVE 'S' TO POSTSUM-OPKOD                                            
021300     CALL POSTSUM USING POSTSUM-PARM                                      
021400     CALL ABEND USING RKOD-ABEND                                          
021500     .                                                                    
