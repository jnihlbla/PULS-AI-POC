001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W5517800.                                                
001300 AUTHOR.         ARCHANA BHAT.                                            
001400 DATE-WRITTEN.   15/07/21.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001800*        THIS PROGRAM WRITES OUT ALL RECORDS FROM WDL222                  
001900*                                                                         
002010*        THE PROGRAM READS     WDL2                                       
002100*                                                                         
002200*    ABENDCODES:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- WDL222 DATA                                                
003310     SELECT W55178                     ASSIGN TO W55178D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W55178                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003910*01  RECORD -COPY W55178 -PRE  UT-  -L.                                   
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W5517800'.            
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004800     EJECT                                                                
004900 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005000 01  FILLER REDEFINES TODAYS-DATE.                                        
005100     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005200     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005300     03  TODAYS-DATE-DAY         PIC 9(2).                                
005400     EJECT                                                                
005500 01  GENERAL-SUBPROGRAMS.                                                 
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100     SKIP2                                                                
006200*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006300                                                                          
006400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006700     SKIP2                                                                
006800 01  ERROR-TEXT.                                                          
006900     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
007000     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007302 01  UT-AREA-START               PIC X(24)   VALUE                        
007303                                 'UT-AREA-START  '.                       
007304     SKIP2                                                                
007305                                                                          
007310*01  AREA -COPY W55178     -PRE UT-                                       
007400     EJECT                                                                
007500*    --- AREAS FOR IMS-SECTIONS                                           
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FOUND                       VALUE '  '.                  
008600     88  SEGMENT-MISSING                     VALUE 'GB'.                  
008700     SKIP2                                                                
008800 01  GOOD-STATUSCODES.                                                    
008900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009000     SKIP3                                                                
009100 01  SSA1                        PIC X(64).                               
009200 01  SSA2                        PIC X(64).                               
009300     EJECT                                                                
009400*    --- IMS FUNCTION CODES                                               
009500*01  -COPY W0003                                                          
009600     EJECT                                                                
010210*    ---  DLI INPUT-OUTPUT AREA                                           
010220 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL2'.                        
010230 01  DLI-IO-WDL2.                                                         
010240*    03  -COPY WDL201                                                     
010250 01  FILLER  REDEFINES DLI-IO-WDL2.                                       
010260*    03  -COPY WDL222                                                     
010270     EJECT                                                                
010300 LINKAGE SECTION.                                                         
010400                                                                          
010501                                                                          
010502*01  -COPY W0008  -PRE WDL2-                                              
010510     05  FILLER                  PIC X.                                   
010600     EJECT                                                                
010701 PROCEDURE DIVISION  USING WDL2-PCB.                                      
010702 MAIN SECTION.                                                            
010710     ENTRY 'DLITCBL' USING WDL2-PCB.                                      
010800                                                                          
011000                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011301     PERFORM IMS-GET-WDL2                                                 
011302     PERFORM UNTIL SEGMENT-MISSING                                        
011303       EVALUATE WDL2-SEG-NAME-FB                                          
011304         WHEN 'WDL201'                                                    
011305           MOVE ART-IDARTNR    TO UT-IDARTNR                              
011306         WHEN 'WDL222'                                                    
011308           PERFORM B-MOVE-FIELDS                                          
011310       END-EVALUATE                                                       
011311       PERFORM IMS-GET-WDL2                                               
011320     END-PERFORM                                                          
011400     PERFORM Z-FINIT                                                      
011500                                                                          
011600     MOVE ZERO TO RETURN-CODE                                             
011700     GOBACK                                                               
011800     .                                                                    
011900     EJECT                                                                
012000 A-INIT SECTION.                                                          
012201                                                                          
012210     OPEN OUTPUT W55178                                                   
012300                                                                          
012400     ACCEPT TODAYS-DATE    FROM DATE                                      
012510     MOVE IDPGM              TO POSTSUM-PROGNAMN                          
012700     .                                                                    
012800     EJECT                                                                
012810 B-MOVE-FIELDS SECTION.                                                   
012811                                                                          
012814     MOVE DIR-IDPTYP    TO UT-IDPTYP                                      
012815     MOVE DIR-IDLEVNR   TO UT-IDLEVNR                                     
012816     MOVE DIR-KVAVIS    TO UT-KVAVIS                                      
012817     MOVE DIR-TIAVSDAT  TO UT-TIAVSDAT                                    
012818                                                                          
012819     PERFORM S11-WRITE-W55178                                             
012822     .                                                                    
012830     EJECT                                                                
012900 Z-FINIT SECTION.                                                         
013010     CLOSE W55178                                                         
013101     SKIP2                                                                
013102     MOVE 'S' TO POSTSUM-OPKOD                                            
013110     CALL POSTSUM USING POSTSUM-PARM                                      
013200     .                                                                    
013401     EJECT                                                                
013402 S11-WRITE-W55178 SECTION.                                                
013403                                                                          
013404     WRITE UT-RECORD FROM UT-AREA                                         
013405                                                                          
013406     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
013407     MOVE 'W55178' TO POSTSUM-FDNAMN                                      
013408     MOVE 'W55178D1' TO POSTSUM-DDNAMN2                                   
013409     CALL POSTSUM USING POSTSUM-PARM                                      
013410     .                                                                    
013600     EJECT                                                                
013700 S99-ABEND SECTION.                                                       
013800                                                                          
013901     SKIP2                                                                
013902     MOVE 'S' TO POSTSUM-OPKOD                                            
013910     CALL POSTSUM USING POSTSUM-PARM                                      
014000     CALL ABEND USING RKOD-ABEND                                          
014100     .                                                                    
014200     EJECT                                                                
014300* --- IMS SECTIONS  ---                                                   
014400                                                                          
014501                                                                          
014502 IMS-GET-WDL2   SECTION.                                                  
014503                                                                          
014504     CALL CBLTDLI USING GN WDL2-PCB DLI-IO-WDL2                           
014505     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
014506     MOVE '  GAGKGB' TO GOOD-STATUSCODES                                  
014507     PERFORM IMS-STATUSCHECK                                              
014510     .                                                                    
014600     EJECT                                                                
014700 IMS-STATUSCHECK SECTION.                                                 
014800                                                                          
014900     SET STATUS-IX TO 1                                                   
015000     SEARCH GOOD-STATUS                                                   
015100       AT END                                                             
015200         STRING ' INVALID STATUS CODE FROM IMS:' STATUS-WS                
015300           DELIMITED BY SIZE INTO ERROR-TEXT                              
015400         DISPLAY ERROR-TEXT                                               
015500         CALL FELLOG                                                      
015600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
015700         CONTINUE                                                         
015800     END-SEARCH                                                           
015900     .                                                                    
