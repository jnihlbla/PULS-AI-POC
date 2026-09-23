001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W5710400.                                                
001300 AUTHOR.         ARCHANA BHAT.                                            
001400 DATE-WRITTEN.   11/11/11.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001800*        SB DOWNLOAD OF WDJ701                                            
001900*                                                                         
002010*        THE PROGRAM READS     WDJ7                                       
002100*                                                                         
002200*    ABENDCODES:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002800 ENVIRONMENT DIVISION.                                                    
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003302*--- SYSIN FROM JCL                                                       
003303                                                                          
003304     SELECT INDATA                       ASSIGN TO SYSIN.                 
003305*          --- INVENTORY INFO WITHOUT BUFFER INFO                         
003310     SELECT W571D1                     ASSIGN TO W57104D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  INDATA                                                               
003903     LABEL RECORD STANDARD                                                
003904     RECORDING       F                                                    
003905     BLOCK CONTAINS 0.                                                    
003906                                                                          
003907 01  INPOST             PIC X(80).                                        
003908*                                                                         
003909 FD  W571D1                                                               
003910     RECORDING       F                                                    
003911     BLOCK CONTAINS  0.                                                   
003912                                                                          
003920*01  RECORD -COPY WDJ701 -PRE  W571D1-  -L.                               
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W5710400'.            
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600 77  INDATA-EOF-SW               PIC X       VALUE 'N'.                   
004700     88  END-OF-INDATA                       VALUE 'Y'.                   
004710 77  WS-DC-FOUND-SW              PIC X       VALUE 'N'.                   
004720     88  WS-DC-FOUND                         VALUE 'Y'.                   
004730     88  WS-DC-NOT-FOUND                     VALUE 'N'.                   
004800                                                                          
005500 01  GENERAL-SUBPROGRAMS.                                                 
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006100     SKIP2                                                                
006800 01  ERROR-TEXT.                                                          
006900     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
007000     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007302 01  FILLER                      PIC X(10)   VALUE 'INAREA'.              
007303 01  INAREA.                                                              
007304     03 WS-RECV-IDDC             PIC X(2)    VALUE SPACE.                 
007305     03 FILLER                   PIC X(78)   VALUE SPACE.                 
007306 01  W571D1-AREA-START           PIC X(24)   VALUE                        
007307                                 'W571D1-AREA-START  '.                   
007308     SKIP2                                                                
007309                                                                          
007310*01  AREA -COPY WDJ701     -PRE W571D1-                                   
007400     EJECT                                                                
007500*    --- AREAS FOR IMS-SECTIONS                                           
007600*                                                                         
007700     EJECT                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FOUND                       VALUE '  '.                  
008600     88  SEGMENT-MISSING                     VALUE 'GB'.                  
008700                                                                          
008800 01  GOOD-STATUSCODES.                                                    
008900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009300                                                                          
009400*    --- IMS FUNCTION CODES                                               
009500*01  -COPY W0003                                                          
009600                                                                          
009800*    ---  DLI INPUT-OUTPUT AREA                                           
009901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ701'.                      
009902 01  DLI-IO-WDJ701.                                                       
009910*    03  -COPY WDJ701                                                     
010200                                                                          
010300 LINKAGE SECTION.                                                         
010502*01  -COPY W0008  -PRE WDJ7-                                              
010510     05  FILLER                  PIC X.                                   
010600                                                                          
010701 PROCEDURE DIVISION  USING WDJ7-PCB.                                      
010702                                                                          
010703 MAIN SECTION.                                                            
010710     ENTRY 'DLITCBL' USING WDJ7-PCB.                                      
011000                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011301     PERFORM IMS-GET-WDJ7                                                 
011302     PERFORM UNTIL SEGMENT-MISSING                                        
011303       EVALUATE WDJ7-SEG-NAME-FB                                          
011304         WHEN 'WDJ701'                                                    
011305           IF ACS-IDARTNR NOT = 999999999                                 
011306              IF ACS-IDDC = WS-RECV-IDDC                                  
011307                 MOVE ACS-WDJ701 TO W571D1-ACS-WDJ701                     
011308                 PERFORM S11-WRITE-W571D1                                 
011309                 SET WS-DC-FOUND TO TRUE                                  
011310              END-IF                                                      
011311           END-IF                                                         
011312       END-EVALUATE                                                       
011314       PERFORM IMS-GET-WDJ7                                               
011320     END-PERFORM                                                          
011330                                                                          
011340     IF WS-DC-NOT-FOUND                                                   
011341        MOVE LOW-VALUES   TO W571D1-ACS-WDJ701                            
011350        MOVE WS-RECV-IDDC TO W571D1-ACS-IDDC                              
011360        PERFORM S11-WRITE-W571D1                                          
011370     END-IF                                                               
011380                                                                          
011400     PERFORM Z-FINIT                                                      
011500                                                                          
011600     MOVE ZERO TO RETURN-CODE                                             
011700     GOBACK                                                               
011800     .                                                                    
011900                                                                          
012000 A-INIT SECTION.                                                          
012201                                                                          
012202     OPEN INPUT INDATA                                                    
012203     READ INDATA NEXT RECORD INTO INAREA                                  
012204       AT END                                                             
012205          SET END-OF-INDATA            TO TRUE                            
012206     END-READ                                                             
012207     CLOSE INDATA                                                         
012208                                                                          
012209     UNSTRING INAREA DELIMITED BY SPACE INTO WS-RECV-IDDC                 
012210                                                                          
012220     OPEN OUTPUT W571D1                                                   
012300                                                                          
012510     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012700     .                                                                    
012800     EJECT                                                                
012900 Z-FINIT SECTION.                                                         
013010     CLOSE W571D1                                                         
013101                                                                          
013102     MOVE 'S' TO POSTSUM-OPKOD                                            
013110     CALL POSTSUM USING POSTSUM-PARM                                      
013200     .                                                                    
013401     EJECT                                                                
013402 S11-WRITE-W571D1 SECTION.                                                
013403                                                                          
013404     WRITE W571D1-RECORD  FROM W571D1-AREA                                
013405                                                                          
013407     MOVE 'W571D1' TO POSTSUM-FDNAMN                                      
013408     MOVE 'W57104D1' TO POSTSUM-DDNAMN2                                   
013409     CALL POSTSUM USING POSTSUM-PARM                                      
013410     .                                                                    
014300* --- IMS SECTIONS  ---                                                   
014400                                                                          
014501                                                                          
014502 IMS-GET-WDJ7   SECTION.                                                  
014503                                                                          
014504     CALL CBLTDLI USING GN WDJ7-PCB DLI-IO-WDJ701                         
014505     MOVE WDJ7-STATUS-CODE TO STATUS-WS                                   
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
