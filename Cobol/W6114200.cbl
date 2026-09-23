000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6114200.                                                
000300 AUTHOR.         JOHAN LINDKVIST.                                         
000400 DATE-WRITTEN.   98/04/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        SB OF THE LABEL-DATABASE (ETIKETTDATABASEN) WDK3                 
000900*                                                                         
001000*        THE PROGRAM READS     WLETIA (WDK3)                              
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
002400*          --- EXTRAKT                                                    
002500     SELECT W61142                     ASSIGN TO W61142D1.                
002600*          --- MEMO SEND                                                  
002700     SELECT W611ME                     ASSIGN TO W61142D2.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP2                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W61142                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  POST -COPY W61142 -PRE  UT-  -L.                                     
003800*                                                                         
003900 FD  W611ME                                                               
004000     RECORDING       V                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300 01  MEMOSEND        PIC X(80).                                           
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700                                                                          
004800*    -- CHECKED BY WY2000                                                 
004900 77  IDPGM                       PIC X(8)    VALUE 'W6114200'.            
005000 77  YES                         PIC X       VALUE 'Y'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200                                                                          
005300 01  MEMORUBRIK.                                                          
005400     03  FILLER                  PIC X(30)   VALUE                        
005500                                 'BARCODE UPDATE'.                        
005600 01  MEMOTEXT.                                                            
005700     03  FILLER                  PIC X(30)   VALUE                        
005800                                 'ANTAL ARTIKLAR UPPDATERADE: '.          
005900     03  COUNTER-UPDATED-IX      PIC 9(6)   VALUE 0.                      
006000     EJECT                                                                
006100 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006200 01  FILLER REDEFINES TODAYS-DATE.                                        
006300     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006400     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006500     03  TODAYS-DATE-DAY         PIC 9(2).                                
006600     EJECT                                                                
006700 01  GENERAL-SUBPROGRAMS.                                                 
006800*                                                                         
006900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007300     SKIP2                                                                
007400*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007500                                                                          
007600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007900     SKIP2                                                                
008000 01  ERRTEXT.                                                             
008100     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
008200     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
008300     EJECT                                                                
008400*    --- PARAMETRAR TILL POSTSUM                                          
008500*                                                                         
008600*01  -COPY W0005   -PRE  POSTSUM-                                         
008700     EJECT                                                                
008800 01  UT-AREA-START               PIC X(24)   VALUE                        
008900                                 'UT-AREA-START  '.                       
009000     SKIP2                                                                
009100                                                                          
009200*01  AREA -COPY W61142     -PRE UT-                                       
009300     EJECT                                                                
009400*    --- AREAS FOR IMS-SECTIONS                                           
009500*                                                                         
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009800     SKIP3                                                                
009900 01  KEYS-TILL-DLI.                                                       
010000     03  W-IDARTNR-X.                                                     
010100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010200     SKIP2                                                                
010300*    --- STATUS-KOD FRÅN IMS                                              
010400 01  STATUS-WS                   PIC XX.                                  
010500     88  SEGMENT-FOUND                       VALUE '  '.                  
010600     88  SEGMENT-MISSING                     VALUE 'GB'.                  
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
011800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLETIA'.                      
011900 01 DLI-IO-AREA.                                                          
012000     03 IO-AREA     PIC X(600) VALUE SPACE.                               
012100        03  DLI-IO-WLETIA REDEFINES IO-AREA.                              
012200*           05  -COPY WDK301  -PRE ETIA-                                  
012300         03 DLI-IO-WLETIAB REDEFINES IO-AREA.                             
012400*            05 -COPY WDK311 -PRE ETIB-                                   
012500     EJECT                                                                
012600 LINKAGE SECTION.                                                         
012700                                                                          
012800                                                                          
012900*01  -COPY W0008  -PRE ETIA-                                              
013000     05  FILLER                  PIC X.                                   
013100     EJECT                                                                
013200 PROCEDURE DIVISION  USING ETIA-PCB.                                      
013300 MAIN SECTION.                                                            
013400     ENTRY 'DLITCBL' USING ETIA-PCB.                                      
013500                                                                          
013600     PERFORM A-INIT                                                       
013700                                                                          
013800     PERFORM IMS-GET-ETIA                                                 
013900     PERFORM UNTIL SEGMENT-MISSING                                        
014000       EVALUATE ETIA-SEG-NAME-FB                                          
014100         WHEN 'WDK301'                                                    
014200           MOVE ETIA-ETI-IDARTNR    TO  UT-IDARTNR                        
014300           MOVE ETIA-ETI-DAREGDAT   TO  UT-DAREGDAT                       
014400           MOVE ETIA-ETI-IDLAYOUT   TO  UT-IDLAYOUT                       
014500           MOVE ETIA-ETI-IDARTNR-ETIK TO UT-IDARTNR-ETIK                  
014600                                                                          
014700           IF ETIA-ETI-IDLAYOUT  NOT = SPACE                              
014800              COMPUTE COUNTER-UPDATED-IX = COUNTER-UPDATED-IX + 1         
014900           END-IF                                                         
015000                                                                          
015100           PERFORM S11-WRITE-W61142                                       
015200       END-EVALUATE                                                       
015300       PERFORM IMS-GET-ETIA                                               
015400     END-PERFORM                                                          
015500     PERFORM Z-FINIT                                                      
015600                                                                          
015700     MOVE ZERO TO RETURN-CODE                                             
015800     GOBACK                                                               
015900     .                                                                    
016000     EJECT                                                                
016100 A-INIT SECTION.                                                          
016200                                                                          
016300     OPEN OUTPUT W61142                                                   
016400                 W611ME                                                   
016500                                                                          
016600     ACCEPT TODAYS-DATE  FROM DATE                                        
016700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016800     .                                                                    
016900     EJECT                                                                
017000 Z-FINIT SECTION.                                                         
017100     IF COUNTER-UPDATED-IX > 0                                            
017200      WRITE MEMOSEND FROM MEMORUBRIK                                      
017300      WRITE MEMOSEND FROM MEMOTEXT                                        
017400     END-IF                                                               
017500     CLOSE W61142                                                         
017600           W611ME                                                         
017700     SKIP2                                                                
017800     MOVE 'S' TO POSTSUM-OPKOD                                            
017900     CALL POSTSUM USING POSTSUM-PARM                                      
018000     .                                                                    
018100     EJECT                                                                
018200 S11-WRITE-W61142 SECTION.                                                
018300                                                                          
018400     WRITE UT-POST FROM UT-AREA                                           
018500                                                                          
018600     MOVE 'ETIA01' TO POSTSUM-TRANSTYP                                    
018700     MOVE 'W61142' TO POSTSUM-FDNAMN                                      
018800     MOVE 'W61142D1' TO POSTSUM-DDNAMN2                                   
018900     CALL POSTSUM USING POSTSUM-PARM                                      
019000     .                                                                    
019100     EJECT                                                                
019200 S99-ABEND SECTION.                                                       
019300                                                                          
019400     SKIP2                                                                
019500     MOVE 'S' TO POSTSUM-OPKOD                                            
019600     CALL POSTSUM USING POSTSUM-PARM                                      
019700     CALL ABEND USING RKOD-ABEND                                          
019800     .                                                                    
019900     EJECT                                                                
020000* --- IMS SECTIONS  ---                                                   
020100                                                                          
020200                                                                          
020300 IMS-GET-ETIA   SECTION.                                                  
020400                                                                          
020500     CALL CBLTDLI USING GN ETIA-PCB DLI-IO-AREA                           
020600     MOVE ETIA-STATUS-CODE TO STATUS-WS                                   
020700     MOVE '  GAGKGB' TO GOOD-STATUSCODES                                  
020800     PERFORM IMS-STATUSCHECK                                              
020900     .                                                                    
021000     EJECT                                                                
021100 IMS-STATUSCHECK SECTION.                                                 
021200                                                                          
021300     SET STATUS-IX TO 1                                                   
021400     SEARCH GOOD-STATUS                                                   
021500       AT END                                                             
021600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
021700           DELIMITED BY SIZE INTO ERRTEXT                                 
021800         DISPLAY ERRTEXT                                                  
021900         CALL FELLOG                                                      
022000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
022100         CONTINUE                                                         
022200     END-SEARCH                                                           
022300     .                                                                    
