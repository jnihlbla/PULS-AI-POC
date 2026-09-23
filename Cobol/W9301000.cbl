000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W9301000.                                                
000301 AUTHOR.         RAGUR SATHEESH.                                          
000401 DATE-WRITTEN.   17/07/13.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701*    FUNCTION:                                                            
000801*        READ THE RECORDS OLDER THAN 3 YEARS FROM 9305 AND 9308           
000901*                                                                         
001001*        THE PROGRAM READS     WDG2                                       
001101*                                                                         
001201*    ABENDCODES:                                                          
001301*        U0016 -  . . . .                                                 
001401*        U1000 -  . . . .                                                 
001501*                                                                         
001601*                                                                         
001701     SKIP3                                                                
001801 ENVIRONMENT DIVISION.                                                    
001901     SKIP2                                                                
002001 INPUT-OUTPUT SECTION.                                                    
002101                                                                          
002201 FILE-CONTROL.                                                            
002301     SELECT W93010                     ASSIGN TO W93010D1.                
002401     EJECT                                                                
002501 DATA DIVISION.                                                           
002601     SKIP2                                                                
002701 FILE SECTION.                                                            
002801     EJECT                                                                
002901     SKIP3                                                                
003001 FD  W93010                                                               
003101     RECORDING       F                                                    
003201     BLOCK CONTAINS  0.                                                   
003301                                                                          
003401*01  RECORD -COPY W93010 -PRE  UT-  -L.                                   
003501     EJECT                                                                
003601 WORKING-STORAGE SECTION.                                                 
003701                                                                          
003801 77  IDPGM                       PIC X(8)    VALUE 'W9301000'.            
003901 77  YES                         PIC X       VALUE 'J'.                   
004001 77  NOO                         PIC X       VALUE 'N'.                   
004101                                                                          
004201 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004301 01  TODAYS-MINUS-3YRS           PIC 9(6)    VALUE ZERO.                  
004401     EJECT                                                                
004501                                                                          
004601 01  GENERAL-SUBPROGRAMS.                                                 
004701*                                                                         
004801     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004901     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005001     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005101     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005201     SKIP2                                                                
005301*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
005401                                                                          
005501 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005601 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005701 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005801     SKIP2                                                                
005901 01  ERROR-TEXT.                                                          
006001     03  FILLER                  PIC X(8)    VALUE 'ERRORTXT'.            
006101     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006201     EJECT                                                                
006301*    -COPY WY2000W1                                                       
006401                                                                          
006501*    --- PARAMETRAR TILL POSTSUM                                          
006601*01  -COPY W0005   -PRE  POSTSUM-                                         
006701     EJECT                                                                
006801 01  UT-AREA-START               PIC X(24)   VALUE                        
006901                                 'UT-AREA-START  '.                       
007001     SKIP2                                                                
007101                                                                          
007201*01  AREA -COPY W93010     -PRE UT-                                       
007301     EJECT                                                                
007401*    --- AREAS FOR IMS-SECTIONS                                           
007501*                                                                         
007601     EJECT                                                                
007701 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007801     SKIP3                                                                
007901 01  KEYS-FOR-DLI.                                                        
008001     03  W-IDHTYP-X.                                                      
008101         05  W-IDHTYP            PIC X(4)     VALUE '9305'.               
008201     SKIP2                                                                
008301*    --- STATUS-KOD FRÅN IMS                                              
008401 01  STATUS-WS                   PIC XX.                                  
008501     88  SEGMENT-FOUND                       VALUE '  '.                  
008601     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008701     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008801     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
008901     SKIP2                                                                
009001 01  GOOD-STATUSCODES.                                                    
009101     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009201     SKIP3                                                                
009301 01  SSA1                        PIC X(64).                               
009401 01  SSA2                        PIC X(64).                               
009501     EJECT                                                                
009601*    --- IMS FUNCTION CODES                                               
009701*01  -COPY W0003                                                          
009801     EJECT                                                                
009901*    ---  DLI INPUT-OUTPUT AREA                                           
010001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9305'.                    
010101 01  DLI-IO-WDGX9305.                                                     
010201*    03  -COPY WDGX9305                                                   
010301     EJECT                                                                
010401 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9308'.                    
010501 01  DLI-IO-WDGX9308.                                                     
010601*    03  -COPY WDGX9308                                                   
010701     EJECT                                                                
010801 LINKAGE SECTION.                                                         
010901                                                                          
011001                                                                          
011101*01  -COPY W0008  -PRE 9305-                                              
011201     05 KFBA-IDHTYP          PIC X(4).                                    
011301     05 KFBA-KDVALISO-HUV    PIC X(3).                                    
011401     05 KFBA-KDVALTYP        PIC X(1).                                    
011501     05 FILLER               PIC X(22).                                   
011601     05 KFBA-KDVALISO        PIC X(3).                                    
011701     05 KFBA-TISTADAT-9KOMPL PIC S9(7) COMP-3.                            
011801     EJECT                                                                
011901 PROCEDURE DIVISION  USING 9305-PCB.                                      
012001 MAIN SECTION.                                                            
012101     ENTRY 'DLITCBL' USING 9305-PCB.                                      
012201                                                                          
012301                                                                          
012401     PERFORM A-INIT                                                       
012501                                                                          
012601     PERFORM IMS-GN-WDGX9305                                              
012701     PERFORM UNTIL SEGMENT-MISSING OR                                     
012801                   SEGMENT-NOMORE                                         
012901       PERFORM IMS-GNP-WDGX9308                                           
013001       PERFORM UNTIL SEGMENT-MISSING OR                                   
013101                     SEGMENT-NOMORE                                       
013201         MOVE TODAYS-MINUS-3YRS TO TMP1-YYMMDD                            
013301         MOVE 9308-TISTADAT     TO TMP2-YYMMDD                            
013401         PERFORM WY2000P1                                                 
013501         IF TMP2-YYMMDD < TMP1-YYMMDD                                     
013601           MOVE KFBA-KDVALISO-HUV    TO UT-KDVALISO-HUV                   
013701           MOVE KFBA-KDVALTYP        TO UT-KDVALTYP                       
013702           MOVE KFBA-KDVALISO        TO UT-KDVALISO                       
013801           MOVE KFBA-TISTADAT-9KOMPL TO UT-TISTADAT-9KOMPL                
013901                                                                          
014001           PERFORM S11-WRITE-W93010                                       
014101                                                                          
014201         END-IF                                                           
014301         PERFORM IMS-GNP-WDGX9308                                         
014401       END-PERFORM                                                        
014501       PERFORM IMS-GN-WDGX9305                                            
014601     END-PERFORM                                                          
014701                                                                          
014801                                                                          
014901     PERFORM Z-FINIT                                                      
015001                                                                          
015101     MOVE ZERO TO RETURN-CODE                                             
015201     GOBACK                                                               
015301     .                                                                    
015401     EJECT                                                                
015501 A-INIT SECTION.                                                          
015601                                                                          
015701     OPEN OUTPUT W93010                                                   
015801                                                                          
015901*    ACCEPT TODAYS-DATE  FROM DATE                                        
016001     ACCEPT TODAYS-DATE        FROM DATE                                  
016101     SUBTRACT 30000            FROM TODAYS-DATE                           
016201                             GIVING TODAYS-MINUS-3YRS                     
016301     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016401     .                                                                    
016501     EJECT                                                                
016601 Z-FINIT SECTION.                                                         
016701     CLOSE W93010                                                         
016801     SKIP2                                                                
016901     MOVE 'S' TO POSTSUM-OPKOD                                            
017001     CALL POSTSUM USING POSTSUM-PARM                                      
017101     .                                                                    
017201     EJECT                                                                
017301 S11-WRITE-W93010 SECTION.                                                
017401                                                                          
017501     WRITE UT-RECORD FROM UT-AREA                                         
017601                                                                          
017701     MOVE SPACES    TO POSTSUM-TRANSTYP                                   
017801     MOVE 'W93010' TO POSTSUM-FDNAMN                                      
017901     MOVE 'W93010D1' TO POSTSUM-DDNAMN2                                   
018001     CALL POSTSUM USING POSTSUM-PARM                                      
018101     .                                                                    
018201     EJECT                                                                
018301 S99-ABEND SECTION.                                                       
018401                                                                          
018501     SKIP2                                                                
018601     MOVE 'S' TO POSTSUM-OPKOD                                            
018701     CALL POSTSUM USING POSTSUM-PARM                                      
018801     CALL ABEND USING RKOD-ABEND                                          
018901     .                                                                    
019001     EJECT                                                                
019101* --- IMS SECTIONS  ---                                                   
019201                                                                          
019301     EJECT                                                                
019401 IMS-GN-WDGX9305 SECTION.                                                 
019501                                                                          
019601     STRING 'WDG201  (IDHTYP   =' W-IDHTYP-X ')'                          
019701          DELIMITED BY SIZE INTO SSA1                                     
019801     MOVE '  GB' TO GOOD-STATUSCODES                                      
019901     CALL CBLTDLI USING GN  9305-PCB DLI-IO-WDGX9305 SSA1                 
020001     MOVE 9305-STATUS-CODE TO STATUS-WS                                   
020101     PERFORM IMS-STATUSCHECK                                              
020201     .                                                                    
020301     EJECT                                                                
020401 IMS-GNP-WDGX9308 SECTION.                                                
020501                                                                          
020601     MOVE 'WDGX9308 ' TO SSA1                                             
020701     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
020801     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
020901     MOVE 9305-STATUS-CODE TO STATUS-WS                                   
021001     PERFORM IMS-STATUSCHECK                                              
021101     .                                                                    
021201     EJECT                                                                
021301 IMS-STATUSCHECK SECTION.                                                 
021401                                                                          
021501     SET STATUS-IX TO 1                                                   
021601     SEARCH GOOD-STATUS                                                   
021701       AT END                                                             
021801         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
021901           DELIMITED BY SIZE INTO ERROR-TEXT                              
022001         DISPLAY ERROR-TEXT                                               
022101         CALL FELLOG                                                      
022201       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
022301         CONTINUE                                                         
022401     END-SEARCH                                                           
022501     .                                                                    
022601*    -COPY WY2000P1                                                       
022701     EJECT                                                                
