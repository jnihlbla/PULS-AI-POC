000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W4405500.                                            
000300 AUTHOR.             LASSI OLGRENER.                                      
000400 DATE-WRITTEN.       JAN. 1995.                                           
000600     REMARKS.                                                             
000700*                                                                         
001200*    FUNKTION.                                                            
001300*             *SB*                                                        
001400*    LÄSER NER WDK7 TILL TVÅ SEKVENSFILER                                 
001500*                                                                         
001600*    UTFILER:                                                             
001610*    - W44055 (OKS)                                                       
001620*    - W44077 (RES/ROS)                                                   
001700*                                                                         
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002400     SELECT W44055           ASSIGN TO      W44055D1.                     
002410     SELECT W44077           ASSIGN TO      W44055D2.                     
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 FILE SECTION.                                                            
002800     SKIP2                                                                
002900 FD  W44055                                                               
003000     LABEL RECORD STANDARD                                                
003100     RECORDING F                                                          
003200     BLOCK CONTAINS 0.                                                    
003300*01  W44055-POST -COPY W440055    -L                                      
003400     SKIP2                                                                
003410 FD  W44077                                                               
003420     LABEL RECORD STANDARD                                                
003430     RECORDING F                                                          
003440     BLOCK CONTAINS 0.                                                    
003450*01  W44077-POST -COPY W440077    -L                                      
003500 WORKING-STORAGE SECTION.                                                 
003600     SKIP2                                                                
003601                                                                          
003610*    -- CHECKED BY WY2000                                                 
003700 77  PROGRAM-NAMN                PIC X(08)   VALUE 'W4405500'.            
003800 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003900                                                                          
004700*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
004800                                                                          
004900 01  DYNAMISKA-SUBPROGRAM.                                                
005000   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
005100   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI'.             
005200   03  FELLOG                    PIC X(8)    VALUE 'FELLOG '.             
005300     EJECT                                                                
005400*    ---- PARAMETRAR TILL POSTSUM                                         
005500                                                                          
005600*01  -COPY W0005      -PRE POSTSUM-.                                      
005800     EJECT                                                                
005900 01  FILLER                      PIC X(16)   VALUE 'UT-AREA1'.            
006000*01  AREA  -COPY W440055    -PRE UT1-                                     
006200                                                                          
006202 01  FILLER                      PIC X(16)   VALUE 'UT-AREA2'.            
006203*01  AREA  -COPY W440077    -PRE UT2-                                     
006204                                                                          
006210     EJECT                                                                
006300*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
006500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS  '.            
006600                                                                          
006700*    ---- STATUSKOD FRÅN IMS                                              
006800                                                                          
006900 01  STATUS-WS                   PIC XX.                                  
007000     88  SEGMENT-FINNS                      VALUE '  '.                   
007100     88  SEGMENT-SLUT                       VALUE 'GB'.                   
007200     SKIP3                                                                
007300 01  GODK-STATUSKODER.                                                    
007400   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
007500     SKIP3                                                                
007600 01  SSA1                        PIC X(32).                               
007700     EJECT                                                                
007800*01      -COPY W0003.                                                     
008000     EJECT                                                                
008100 01  FILLER                      PIC X(16)  VALUE                         
008200                                            'DLI-IO-AREA'.                
008300 01  DLI-IO-AREA.                                                         
008400   03  IO-AREA                   PIC X(300).                              
008500                                                                          
008600*  03  POST -COPY WDK701             -RED IO-AREA.                        
008800     EJECT                                                                
008900*  03  POST -COPY WDK711             -RED IO-AREA.                        
009100     EJECT                                                                
009200 LINKAGE SECTION.                                                         
009300     SKIP2                                                                
009400*    -COPY W0008 -PRE WDK7-.                                              
009600   05  FILLER             PIC X.                                          
009700     EJECT                                                                
009800 PROCEDURE DIVISION  USING WDK7-PCB.                                      
009900     ENTRY 'DLITCBL' USING WDK7-PCB.                                      
010000     SKIP2                                                                
010100     PERFORM A-INIT                                                       
010200                                                                          
010300     PERFORM IMS-GET-WDK7                                                 
010400     PERFORM UNTIL SEGMENT-SLUT                                           
010500       IF WDK7-SEG-NAME-FB = 'WDK701  '                                   
010600         MOVE SART-IDARTNR     TO UT1-IDARTNR                             
010700                                  UT2-ART-IDARTNR                         
011200       END-IF                                                             
011210                                                                          
011300       IF WDK7-SEG-NAME-FB = 'WDK711  '                                   
011400         MOVE SLAG-IDDC        TO UT1-IDDC                                
011410                                  UT2-ART-IDDC                            
011500         MOVE SLAG-KVOKS-DAG   TO UT1-KVOKS-DAG                           
011501         MOVE SLAG-KVOKS-BULK  TO UT1-KVOKS-BULK                          
011502         MOVE SLAG-KVROS-DAG   TO UT2-ART-KVROS-DAG                       
011503         MOVE SLAG-KVROS-BULK  TO UT2-ART-KVROS-BULK                      
011504         MOVE SLAG-KVRESS      TO UT2-ART-KVRESS                          
011510                                                                          
011600         PERFORM S01-SKRIV-UTPOST1                                        
011700         PERFORM S02-SKRIV-UTPOST2                                        
012200       END-IF                                                             
012300                                                                          
012400       PERFORM IMS-GET-WDK7                                               
013300     END-PERFORM                                                          
013400                                                                          
013500     PERFORM Z-FINIT                                                      
013600     MOVE ZERO TO RETURN-CODE                                             
013700     GOBACK                                                               
013900     .                                                                    
014000     EJECT                                                                
014100 A-INIT SECTION.                                                          
014200                                                                          
014300     OPEN OUTPUT W44055                                                   
014400                 W44077                                                   
014800     .                                                                    
014900     EJECT                                                                
015000 Z-FINIT SECTION.                                                         
015100                                                                          
015200     CLOSE W44055                                                         
015300     MOVE 'S' TO POSTSUM-OPKOD                                            
015400     CALL POSTSUM USING POSTSUM-PARM                                      
015410                                                                          
015500     CLOSE W44077                                                         
015510     MOVE 'S' TO POSTSUM-OPKOD                                            
015520     CALL POSTSUM USING POSTSUM-PARM                                      
015600     .                                                                    
015700     EJECT                                                                
015800 S01-SKRIV-UTPOST1 SECTION.                                               
015900                                                                          
017000     WRITE W44055-POST FROM UT1-AREA                                      
017001                                                                          
017002     MOVE 'K7 '      TO POSTSUM-TRANSTYP                                  
017003     MOVE 'W44055'   TO POSTSUM-FDNAMN                                    
017004     MOVE 'W44055D1' TO POSTSUM-DDNAMN2                                   
017010     CALL POSTSUM USING POSTSUM-PARM                                      
017400     .                                                                    
017500     EJECT                                                                
017510 S02-SKRIV-UTPOST2 SECTION.                                               
017520                                                                          
017530     WRITE W44077-POST FROM UT2-AREA                                      
017540                                                                          
017550     MOVE 'K7 '      TO POSTSUM-TRANSTYP                                  
017560     MOVE 'W44077'   TO POSTSUM-FDNAMN                                    
017570     MOVE 'W44055D2' TO POSTSUM-DDNAMN2                                   
017580     CALL POSTSUM USING POSTSUM-PARM                                      
017590     .                                                                    
017591     EJECT                                                                
017600*    ---- IMS SEKTIONER                                                   
017700 IMS-GET-WDK7 SECTION.                                                    
017800                                                                          
017900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
018000     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA                           
018100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
018200     PERFORM IMS-STATUSKONTROLL.                                          
018300     SKIP3                                                                
018400 IMS-STATUSKONTROLL SECTION.                                              
018500                                                                          
018600     SET STATUS-IX TO 1                                                   
018700     SEARCH GODK-STATUS                                                   
018800     AT END                                                               
018900     STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                               
019000     DELIMITED BY SIZE INTO FELTEXT                                       
019100     CALL FELLOG                                                          
019200     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
019300     END-SEARCH                                                           
019400     .                                                                    
