001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W4769000.                                                
001300 AUTHOR.         KARANDE DIGAMBAR.                                        
001400 DATE-WRITTEN.   02/09/30.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001800*        IT CREATES THE FILE FOR IDSHIPM FOR WHICH WDE101-KDKLAR          
001900*        IS = 'A'. WHENEVER THE TRANSPORT RELEASE THROUGH 4675            
002000*        SCREEN IS NOT COMPLETED, 'A' APPEARS ON 'KDKLAR'. THESE          
002100*        IDSHIPM NEEDS TO BE DELETED. HERE FILE IS CREATED FOR THE        
002200*        SAME. PROGRAM W4769100 READS THIS FILE AND DELETES THE           
002300*        IDSHIPM.                                                         
002400*                                                                         
002510*        THE PROGRAM READS     WDE1                                       
002600*                                                                         
002700*    ABENDCODES:                                                          
002800*        U0016 -  . . . .                                                 
002900*        U1000 -  . . . .                                                 
003000*                                                                         
003100                                                                          
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP2                                                                
003500 INPUT-OUTPUT SECTION.                                                    
003600                                                                          
003700 FILE-CONTROL.                                                            
003801     SKIP2                                                                
003802*          --- OUTPUT FILE FOR WDE101-KDKLAR = 'A'                        
003810     SELECT W47690                     ASSIGN TO W47690D1.                
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200     SKIP2                                                                
004300 FILE SECTION.                                                            
004401     SKIP3                                                                
004402 FD  W47690                                                               
004403     RECORDING       F                                                    
004404     BLOCK CONTAINS  0.                                                   
004405                                                                          
004410*01  POST -COPY W4769001 -PRE  UT1-  -L.                                  
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W4769000'.            
004900 77  YES                         PIC X       VALUE 'J'.                   
005000 77  NOO                         PIC X       VALUE 'N'.                   
005300     EJECT                                                                
005400 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005500 01  FILLER REDEFINES TODAYS-DATE.                                        
005600     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005700     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005800     03  TODAYS-DATE-DAY         PIC 9(2).                                
005900     EJECT                                                                
006000 01  GENERAL-SUBPROGRAMS.                                                 
006100*                                                                         
006200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006510     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006600     SKIP2                                                                
006700*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006800                                                                          
006900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007200     SKIP2                                                                
007300 01  ERRTEXT.                                                             
007400     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
007500     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
007601     EJECT                                                                
007602*    --- PARAMETRAR TILL POSTSUM                                          
007603*                                                                         
007610*01  -COPY W0005   -PRE  POSTSUM-                                         
007801     EJECT                                                                
007802 01  UT1-AREA-START              PIC X(24)   VALUE                        
007803                                 'UT1-AREA-START  '.                      
007804     SKIP2                                                                
007805                                                                          
007810*01  AREA -COPY W4769001     -PRE UT1-                                    
007900     EJECT                                                                
008000*    --- AREAS FOR IMS-SECTIONS                                           
008100*                                                                         
008200     EJECT                                                                
008300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008400     SKIP3                                                                
008500 01  KEYS-TILL-DLI.                                                       
008601     03  W-IDSHIPM-X.                                                     
008610         05  W-IDSHIPM           PIC 9(7)    VALUE ZERO.                  
008700     SKIP2                                                                
008800*    --- STATUS-KOD FRÅN IMS                                              
008900 01  STATUS-WS                   PIC XX.                                  
009000     88  SEGMENT-FOUND                       VALUE '  '.                  
009100     88  SEGMENT-MISSING                     VALUE 'GB'.                  
009200     SKIP2                                                                
009300 01  GOOD-STATUSCODES.                                                    
009400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009500     SKIP3                                                                
009600 01  SSA1                        PIC X(64).                               
009700 01  SSA2                        PIC X(64).                               
009800     EJECT                                                                
009900*    --- IMS FUNCTION CODES                                               
010000*01  -COPY W0003                                                          
010100     EJECT                                                                
010300*    ---  DLI INPUT-OUTPUT AREA                                           
010401 01  FILLER         PIC X(16) VALUE 'DLI-IO-AREA'.                        
010402 01  DLI-IO-AREA.                                                         
010410*    03  -COPY WDE101                                                     
010700     EJECT                                                                
010710                                                                          
010800 LINKAGE SECTION.                                                         
011001                                                                          
011002*01  -COPY W0008  -PRE WDE1-                                              
011010     05  FILLER                  PIC X.                                   
011100     EJECT                                                                
011200                                                                          
011201 PROCEDURE DIVISION  USING WDE1-PCB.                                      
011202 MAIN SECTION.                                                            
011210     ENTRY 'DLITCBL' USING WDE1-PCB.                                      
011300                                                                          
011600     PERFORM A-INIT                                                       
011700                                                                          
011801     PERFORM IMS-GN-WDE1                                                  
011802     PERFORM UNTIL SEGMENT-MISSING                                        
011803       EVALUATE WDE1-SEG-NAME-FB                                          
011804         WHEN 'WDE101'                                                    
011806           IF SHIP-KDKLAR = 'A'                                           
011807             MOVE SHIP-IDSHIPM  TO UT1-DEL-IDSHIPM                        
011808             PERFORM S11-WRITE-W47690                                     
011809           END-IF                                                         
011810       END-EVALUATE                                                       
011811       PERFORM IMS-GN-WDE1                                                
011820     END-PERFORM                                                          
011900     PERFORM Z-FINIT                                                      
012000                                                                          
012100     MOVE ZERO TO RETURN-CODE                                             
012200     GOBACK                                                               
012300     .                                                                    
012400     EJECT                                                                
012500 A-INIT SECTION.                                                          
012701                                                                          
012710     OPEN OUTPUT W47690                                                   
012800                                                                          
012900     ACCEPT TODAYS-DATE  FROM DATE                                        
013010     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013200     .                                                                    
013300     EJECT                                                                
013400 Z-FINIT SECTION.                                                         
013510     CLOSE W47690                                                         
013601     SKIP2                                                                
013602     MOVE 'S' TO POSTSUM-OPKOD                                            
013610     CALL POSTSUM USING POSTSUM-PARM                                      
013700     .                                                                    
013901     EJECT                                                                
013902 S11-WRITE-W47690 SECTION.                                                
013903                                                                          
013904     WRITE UT1-POST FROM UT1-AREA                                         
013905                                                                          
013908     MOVE SPACE         TO POSTSUM-TRANSTYP                               
013909     MOVE 'W47690'      TO POSTSUM-FDNAMN                                 
013910     MOVE 'W47690D1'    TO POSTSUM-DDNAMN2                                
013911     CALL POSTSUM USING POSTSUM-PARM                                      
013920     .                                                                    
014100     EJECT                                                                
014710                                                                          
014800* --- IMS SECTIONS  ---                                                   
014900                                                                          
015002 IMS-GN-WDE1   SECTION.                                                   
015003                                                                          
015004     CALL CBLTDLI USING GN WDE1-PCB DLI-IO-AREA                           
015005     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
015006     MOVE '  GAGKGB' TO GOOD-STATUSCODES                                  
015007     PERFORM IMS-STATUSCHECK                                              
015010     .                                                                    
015100     EJECT                                                                
015200 IMS-STATUSCHECK SECTION.                                                 
015300                                                                          
015400     SET STATUS-IX TO 1                                                   
015500     SEARCH GOOD-STATUS                                                   
015600       AT END                                                             
015700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
015800           DELIMITED BY SIZE INTO ERRTEXT                                 
015900         DISPLAY ERRTEXT                                                  
016000         CALL FELLOG                                                      
016100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
016200         CONTINUE                                                         
016300     END-SEARCH                                                           
016400     .                                                                    
