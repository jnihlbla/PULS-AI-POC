000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4832700.                                                
000300 AUTHOR.         SRINADH NADIMPALLI.                                      
000400 DATE-WRITTEN.   25/10/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        TO EXTRACT DEALER FREIGH TO SEND TO DATALAKE                     
000900*                                                                         
001000*        THE PROGRAM READS     WDB5                                       
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
002400     SELECT W48327                     ASSIGN TO W48327D1.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP2                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W48327                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  RECORD -COPY W4832701 -PRE  UT-  -L.                                 
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800 77  IDPGM                       PIC X(8)    VALUE 'W4832700'.            
003900 77  YES                         PIC X       VALUE 'J'.                   
004000 77  NOO                         PIC X       VALUE 'N'.                   
004100 77  IX                          PIC S9(3) COMP-3 VALUE 1.                
       77  TAB                         PIC X       VALUE X'05'.                 
004200     EJECT                                                                
004300 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004400 01  FILLER REDEFINES TODAYS-DATE.                                        
004500     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004600     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004700     03  TODAYS-DATE-DAY         PIC 9(2).                                
004800     EJECT                                                                
004900 01  GENERAL-SUBPROGRAMS.                                                 
005000*                                                                         
005100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005500     SKIP2                                                                
005600*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
005700                                                                          
005800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006100     SKIP2                                                                
006200 01  ERROR-TEXT.                                                          
006300     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
006400     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006500     EJECT                                                                
006600*    --- PARAMETRAR TILL POSTSUM                                          
006700*                                                                         
006800*01  -COPY W0005   -PRE  POSTSUM-                                         
006900     EJECT                                                                
007000*01  -COPY WDATAREA                                                       
007100     EJECT                                                                
007200 01  UT-AREA-START               PIC X(24)   VALUE                        
007300                                 'UT-AREA-START  '.                       
007400     SKIP2                                                                
007500                                                                          
007600*01  AREA -COPY W4832701     -PRE UT-                                     
007700                                                                          
007800*    --- STATUS-KOD FRÅN IMS                                              
007900 01  STATUS-WS                   PIC XX.                                  
008000     88  SEGMENT-FOUND                       VALUE '  '.                  
008100     88  SEGMENT-MISSING                     VALUE 'GB'.                  
008200     SKIP2                                                                
008300 01  GOOD-STATUSCODES.                                                    
008400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008500     SKIP3                                                                
008600 01  SSA1                        PIC X(64).                               
008700 01  SSA2                        PIC X(64).                               
008800     EJECT                                                                
008900*    --- IMS FUNCTION CODES                                               
009000*01  -COPY W0003                                                          
009100     EJECT                                                                
009200                                                                          
009300*    ---  DLI INPUT-OUTPUT AREA                                           
009400 01  FILLER               PIC X(16)   VALUE 'WDB501 AREA'.                
009500 01   DLI-IO-AREA-B501.                                                   
009600*     03  -COPY WDB501                                                    
009700                                                                          
009800*                                                                         
009900 LINKAGE SECTION.                                                         
010000*01  -COPY W0008  -PRE WDB5-                                              
010100     05  FILLER                  PIC X.                                   
010200*                                                                         
010300 PROCEDURE DIVISION  USING WDB5-PCB.                                      
010400 MAIN SECTION.                                                            
010500     ENTRY 'DLITCBL' USING WDB5-PCB.                                      
010600                                                                          
010700     PERFORM A-INIT                                                       
010800                                                                          
010900     PERFORM IMS-GN-WDB5                                                  
011000     PERFORM UNTIL SEGMENT-MISSING                                        
011100       EVALUATE WDB5-SEG-NAME-FB                                          
011200         WHEN 'WDB501'                                                    
011300           MOVE FK-IDDC                TO UT-IDDC                         
011400           MOVE FK-KDFRAKT             TO UT-KDFRAKT                      
011500           MOVE FK-IDDISTR             TO UT-IDDISTR                      
011600           MOVE FK-IDKUNDNR            TO UT-IDKUNDNR                     
011700           MOVE FK-BEGMRK-RAD1         TO UT-BEGMRK-RAD1                  
011800           MOVE FK-BEGMRK-RAD2         TO UT-BEGMRK-RAD2                  
011900           MOVE FK-IDTRPLOS-0          TO UT-IDTRPLOS-0                   
012000           MOVE FK-IDTRPVAR-0          TO UT-IDTRPVAR-0                   
012100           MOVE FK-IDTRPLOS-1          TO UT-IDTRPLOS-1                   
012200           MOVE FK-IDTRPVAR-1          TO UT-IDTRPVAR-1                   
012300           MOVE FK-IDTRPLOS-2          TO UT-IDTRPLOS-2                   
012400           MOVE FK-IDTRPVAR-2          TO UT-IDTRPVAR-2                   
012500           MOVE FK-IDTRPLOS-3          TO UT-IDTRPLOS-3                   
012600           MOVE FK-IDTRPVAR-3          TO UT-IDTRPVAR-3                   
012700           MOVE FK-IDTRPLOS-4          TO UT-IDTRPLOS-4                   
012800           MOVE FK-IDTRPVAR-4          TO UT-IDTRPVAR-4                   
012900           MOVE FK-KDFDKRAV            TO UT-KDFDKRAV                     
013000           MOVE FK-KDFKTYP             TO UT-KDFKTYP                      
013100           MOVE FK-KDGRANS             TO UT-KDGRANS                      
013200           MOVE FK-KDTRPKAT            TO UT-KDTRPKAT                     
013300           MOVE FK-PRLEGKST            TO UT-PRLEGKST                     
013400           MOVE FK-REFOERS             TO UT-REFOERS                      
                 PERFORM S11-WRITE-W48327                                       
013500       END-EVALUATE                                                       
013600       PERFORM IMS-GN-WDB5                                                
013700     END-PERFORM                                                          
013800     PERFORM Z-FINIT                                                      
013900                                                                          
014000     MOVE ZERO TO RETURN-CODE                                             
014100     GOBACK                                                               
014200     .                                                                    
014300     EJECT                                                                
014400 A-INIT SECTION.                                                          
014500     OPEN OUTPUT W48327                                                   
014600                                                                          
014700     ACCEPT  TODAYS-DATE FROM DATE                                        
           MOVE TAB  TO UT-TAB-1 UT-TAB-2 UT-TAB-3                              
                        UT-TAB-4 UT-TAB-5 UT-TAB-6                              
                        UT-TAB-7 UT-TAB-8 UT-TAB-9                              
                        UT-TAB-10 UT-TAB-11 UT-TAB-12                           
                        UT-TAB-13 UT-TAB-14 UT-TAB-15                           
                        UT-TAB-16 UT-TAB-17 UT-TAB-18                           
                        UT-TAB-19 UT-TAB-20 UT-TAB-21                           
014800     .                                                                    
014900     EJECT                                                                
015000 Z-FINIT SECTION.                                                         
015100     CLOSE W48327                                                         
015200     MOVE 'S' TO POSTSUM-OPKOD                                            
015300     CALL POSTSUM USING POSTSUM-PARM                                      
015400     .                                                                    
015500     EJECT                                                                
015600 S11-WRITE-W48327 SECTION.                                                
015700     WRITE UT-RECORD FROM UT-AREA                                         
015800     MOVE 'W48327'   TO POSTSUM-FDNAMN                                    
015900     MOVE 'W48327D1' TO POSTSUM-DDNAMN2                                   
016000     CALL POSTSUM USING POSTSUM-PARM                                      
016100     .                                                                    
016200     EJECT                                                                
016300 S99-ABEND SECTION.                                                       
016400     MOVE 'S' TO POSTSUM-OPKOD                                            
016500     CALL POSTSUM USING POSTSUM-PARM                                      
016600     CALL ABEND USING RKOD-ABEND                                          
016700     .                                                                    
016800     EJECT                                                                
016900* --- IMS SECTIONS  ---                                                   
017000 IMS-GN-WDB5   SECTION.                                                   
017100     CALL CBLTDLI USING GN WDB5-PCB DLI-IO-AREA-B501                      
017200     MOVE WDB5-STATUS-CODE TO STATUS-WS                                   
017300     MOVE '  GAGKGBGA' TO GOOD-STATUSCODES                                
017400     PERFORM IMS-STATUSCHECK                                              
017500     .                                                                    
017600     EJECT                                                                
017700 IMS-STATUSCHECK SECTION.                                                 
017800     SET STATUS-IX TO 1                                                   
017900     SEARCH GOOD-STATUS                                                   
018000       AT END                                                             
018100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
018200           DELIMITED BY SIZE INTO ERROR-TEXT                              
018300         DISPLAY ERROR-TEXT                                               
018400         CALL FELLOG                                                      
018500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
018600         CONTINUE                                                         
018700     END-SEARCH                                                           
018800     .                                                                    
