000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2362000.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   01/04/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*        R32 FRÅN GÅGNA VECKAN                                            
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDL2                                       
001200*                                                                         
001300*    ABENDKODER:                                                          
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
002500*          --- INLEVERANSER GÅGNA VECKAN                                  
002600     SELECT W23631                     ASSIGN TO W23620D1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W23631                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  POST -COPY W23631 -PRE  UT-  -L.                                     
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W2362000'.            
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004400 01  ARBETSAREOR.                                                         
004500     03  WS-IDARTNR              PIC S9(9)   VALUE ZERO COMP-3.           
004600     03  WS-TIAAMMDD-START       PIC S9(7)   VALUE ZERO COMP-3.           
004610     03  WS-TIAARP-START         PIC S9(4)   VALUE ZERO COMP-3.           
004700     03  WS-TIAAMMDD-SLUT        PIC S9(7)   VALUE ZERO COMP-3.           
004800     03  WS-AAVV                 PIC  9(4)   VALUE ZERO.                  
004900     03  FILLER REDEFINES WS-AAVV.                                        
005000         05  WS-AAR              PIC 9(2).                                
005100         05  WS-VECKA            PIC 9(2).                                
005200     EJECT                                                                
005300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005400 01  FILLER REDEFINES DAGENS-DATUM.                                       
005500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005800     EJECT                                                                
005900 01  DYNAMISKA-SUBPROGRAM.                                                
006000*                                                                         
006100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006400     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006600     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
006700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006800     SKIP2                                                                
006900*    --- PARAMETRAR TILL ABEND                                            
007000                                                                          
007100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007400     SKIP2                                                                
007500 01  FELTEXT.                                                             
007600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL DATKORT                                          
008000*                                                                         
008100 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W23620'.              
008200     SKIP2                                                                
008300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
008400     SKIP2                                                                
008500*01  -COPY WDATKORT                                                       
008600     EJECT                                                                
008700*    --- PARAMETRAR TILL POSTSUM                                          
008800*                                                                         
008900*01  -COPY W0005   -PRE  POSTSUM-                                         
009000     EJECT                                                                
009100*01  -COPY WORKAREA                                                       
009200     EJECT                                                                
009300*01  -COPY WDATAREA                                                       
009400     EJECT                                                                
009500 01  UT-AREA-START               PIC X(24)   VALUE                        
009600                                 'UT-AREA-START  '.                       
009700     SKIP2                                                                
009800                                                                          
009900*01  AREA -COPY W23631     -PRE UT-                                       
010000     EJECT                                                                
010100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010200*                                                                         
010300     EJECT                                                                
010400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010500     SKIP3                                                                
010600 01  NYCKLAR-TILL-DLI.                                                    
010700     03  W-IDARTNR-X.                                                     
010800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010900     03  W-IDPTYP-X.                                                      
011000         05  W-IDPTYP            PIC X(3)    VALUE SPACE.                 
011100     SKIP2                                                                
011200*    --- STATUS-KOD FRÅN IMS                                              
011300 01  STATUS-WS                   PIC XX.                                  
011400     88  SEGMENT-FINNS                       VALUE '  '.                  
011500     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
011600     SKIP2                                                                
011700 01  GODK-STATUSKODER.                                                    
011800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011900     SKIP3                                                                
012000 01  SSA1                        PIC X(64).                               
012100 01  SSA2                        PIC X(64).                               
012200     EJECT                                                                
012300*    --- IMS FUNKTIONSKODER                                               
012400*01  -COPY W0003                                                          
012500     EJECT                                                                
012600*    ---  DLI INPUT-OUTPUT AREA                                           
012700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL2'.                        
012800 01  DLI-IO-WDL2.                                                         
012900*    03  -COPY WDL221                                                     
013000 01  FILLER  REDEFINES DLI-IO-WDL2.                                       
013100*    03  -COPY WDL201                                                     
013200     EJECT                                                                
013300 LINKAGE SECTION.                                                         
013400                                                                          
013500                                                                          
013600*01  -COPY W0008  -PRE WDL2-                                              
013700     05  FILLER                  PIC X.                                   
013800     EJECT                                                                
013900 PROCEDURE DIVISION  USING WDL2-PCB.                                      
014000 MAIN SECTION.                                                            
014100     ENTRY 'DLITCBL' USING WDL2-PCB.                                      
014200                                                                          
014300*---- FLYTTA SUBPROGRAM CALL TILL RÄTT STÄLLE --                          
014400                                                                          
014500*    CALL WORKDAY USING WORKDAY                                           
014600                                                                          
014700*------------------------                                                 
014800                                                                          
014900     PERFORM A-INIT                                                       
015000                                                                          
015100     PERFORM IMS-GET-WDL2                                                 
015200     PERFORM UNTIL SEGMENT-SAKNAS                                         
015300       EVALUATE WDL2-SEG-NAME-FB                                          
015400         WHEN 'WDL201'                                                    
015500           MOVE ART-IDARTNR TO WS-IDARTNR                                 
015510         WHEN 'WDL211'                                                    
015520           CONTINUE                                                       
015600         WHEN 'WDL221'                                                    
015700           PERFORM B-KOLL-INLEV                                           
015800       END-EVALUATE                                                       
015900       PERFORM IMS-GET-WDL2                                               
016000     END-PERFORM                                                          
016100     PERFORM Z-FINIT                                                      
016200                                                                          
016300     MOVE ZERO TO RETURN-CODE                                             
016400     GOBACK                                                               
016500     .                                                                    
016600     EJECT                                                                
016700 A-INIT SECTION.                                                          
016800                                                                          
016900     OPEN OUTPUT W23631                                                   
017000                                                                          
017100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
017200     MOVE D-AAR       TO DAGENS-DATUM-AAR    WS-AAR                       
017300     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
017400     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
017500     MOVE D-VECKA     TO WS-VECKA                                         
017600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017700**** PERIODKÖRNING OBS                                                    
017800     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
017900     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
018000****                                                                      
018100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
018200                     DAT-O-TIDATUM DAT-KDSVAR                             
018300                                                                          
018400     IF DAT-KDSVAR-OK                                                     
018500       MOVE DAT-TIAARP     TO WS-TIAARP-START                             
018510       DISPLAY ' STARTDATUM TIAARP   ' WS-TIAARP-START                    
018600     ELSE                                                                 
018700       DISPLAY 'FEL I DATKONV 1 '                                         
018800       PERFORM S99-ABEND                                                  
018900     END-IF                                                               
018910**** PERIODKÖRNING OBS                                                    
018920     MOVE 'AARP'        TO DAT-KDDATFORM                                  
018930     MOVE DAT-TIAARP    TO DAT-I-TIDATUM                                  
018940****                                                                      
018950     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
018960                     DAT-O-TIDATUM DAT-KDSVAR                             
018970                                                                          
018980     IF DAT-KDSVAR-OK                                                     
018990       MOVE DAT-TIAAMMDD TO WS-TIAAMMDD-START                             
018991       DISPLAY ' STARTDATUM ÅÅMMDD ' WS-TIAAMMDD-START                    
018992     ELSE                                                                 
018993       DISPLAY 'FEL I DATKONV 1 '                                         
018994       PERFORM S99-ABEND                                                  
018995     END-IF                                                               
019000     .                                                                    
019100     EJECT                                                                
019200 B-KOLL-INLEV SECTION.                                                    
019300                                                                          
019400     IF MOT-IDPTYP = 'R32' AND MOT-KDRT = ZERO AND                        
019500        MOT-IDDC = '11'    AND MOT-KDAVVANT NOT = 2                       
019510        IF MOT-TIUPPDAT <  500000  AND                                    
019600           MOT-TIUPPDAT >= WS-TIAAMMDD-START                              
019700           MOVE WS-IDARTNR    TO UT-IDARTNR                               
019800           MOVE MOT-IDLOPNRM  TO UT-IDLOPNRM                              
019900           MOVE MOT-IDLEVNR   TO UT-IDLEVNR                               
020000           MOVE MOT-KVAVIS    TO UT-KVAVIS                                
020100           MOVE MOT-TIAVIDAT  TO UT-TIAVIDAT                              
020200           MOVE MOT-TIUPPDAT  TO UT-TIUPPDAT                              
020300           PERFORM S11-SKRIV-W23631                                       
020400        END-IF                                                            
020500     END-IF                                                               
020600     .                                                                    
020700     EJECT                                                                
020800                                                                          
020900 Z-FINIT SECTION.                                                         
021000     CLOSE W23631                                                         
021100     SKIP2                                                                
021200     MOVE 'S' TO POSTSUM-OPKOD                                            
021300     CALL POSTSUM USING POSTSUM-PARM                                      
021400     .                                                                    
021500     EJECT                                                                
021600 S11-SKRIV-W23631 SECTION.                                                
021700                                                                          
021800     WRITE UT-POST FROM UT-AREA                                           
021900                                                                          
022000     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
022100     MOVE 'W23631'   TO POSTSUM-FDNAMN                                    
022200     MOVE 'W23620D1' TO POSTSUM-DDNAMN2                                   
022300     CALL POSTSUM USING POSTSUM-PARM                                      
022400     .                                                                    
022500     EJECT                                                                
022600 S99-ABEND SECTION.                                                       
022700                                                                          
022800     SKIP2                                                                
022900     MOVE 'S' TO POSTSUM-OPKOD                                            
023000     CALL POSTSUM USING POSTSUM-PARM                                      
023100     CALL ABEND USING RKOD-ABEND                                          
023200     .                                                                    
023300     EJECT                                                                
023400* --- IMS SEKTIONER ---                                                   
023500                                                                          
023600                                                                          
023700 IMS-GET-WDL2   SECTION.                                                  
023800                                                                          
023900     CALL CBLTDLI USING GN WDL2-PCB DLI-IO-WDL2                           
024000     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
024100     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
024200     PERFORM IMS-STATUSKONTROLL                                           
024300     .                                                                    
024400     SKIP3                                                                
024500 IMS-STATUSKONTROLL SECTION.                                              
024600                                                                          
024700     SET STATUS-IX TO 1                                                   
024800     SEARCH GODK-STATUS                                                   
024900       AT END                                                             
025000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
025100           DELIMITED BY SIZE INTO FELTEXT                                 
025200         DISPLAY FELTEXT                                                  
025300         CALL FELLOG                                                      
025400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
025500         CONTINUE                                                         
025600     END-SEARCH                                                           
025700     .                                                                    
