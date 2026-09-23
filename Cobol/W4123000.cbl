000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W4123000.                                    
000300 AUTHOR.                     GERRY CARMICHAEL.                            
000400     DATE-WRITTEN.           JUNI 1991.                                   
000500*                                                                         
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*                                                                         
001000*    LÄSER ORDERHUVUDETSREGISTRET (WDQ2) MED SB.                          
001100*    LÄSER DATABASEN OCH SKRIVER UT ORDER SOM ÄR LAGD                     
001200*    FÖREGÅENDE DAGEN PÅ EN FIL.                                          
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500 INPUT-OUTPUT SECTION.                                                    
001600*                                                                         
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900*    ---- UT-FIL W41230    OUTPUT                                         
002000                                                                          
002100     SELECT W41230           ASSIGN TO      W41230D1.                     
002200     EJECT                                                                
002300                                                                          
002400 DATA DIVISION.                                                           
002500                                                                          
002600 FILE SECTION.                                                            
002700     SKIP2                                                                
002800 FD  W41230                                                               
002900     LABEL RECORD STANDARD                                                
003000     RECORDING F                                                          
003100     BLOCK CONTAINS 0.                                                    
003200                                                                          
003300*01  UT-AREA  -COPY  W412030    -L.                                       
003400     EJECT                                                                
003500                                                                          
003600 WORKING-STORAGE SECTION.                                                 
003700     SKIP2                                                                
003701                                                                          
003710*    -- CHECKED BY WY2000                                                 
003800*    ---- GENERELLA KONSTANTER                                            
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004200                                                                          
004300*    ---- EOF-SWITCHAR                                                    
004400 77  W41230-EOF                  PIC X       VALUE 'N'.                   
004500                                                                          
004600*    ---- ARBETSFÄLT                                                      
004700 01  DAGENS-DATUM-I-DELAR.                                                
004800   03  DAGENS-DATUM-AAR          PIC 9(2)  VALUE ZERO.                    
004900   03  DAGENS-DATUM-MAANAD       PIC 9(2)  VALUE ZERO.                    
005000   03  DAGENS-DATUM-DAG          PIC 9(2)  VALUE ZERO.                    
005100                                                                          
005200 01  DAGENS-DATUM REDEFINES DAGENS-DATUM-I-DELAR     PIC 9(6).            
005300 SKIP3                                                                    
005400     EJECT                                                                
005500                                                                          
005600 01  RETURKODER.                                                          
005700   03  RKOD                      PIC S9(4) COMP SYNC VALUE ZERO.          
005800   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4) COMP SYNC VALUE +16.           
005900                                                                          
006000*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
006100                                                                          
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
006400   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
006500   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006600   03  DATKORT                   PIC X(8)    VALUE 'DATKORT '.            
006700   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
006800     SKIP3                                                                
006900                                                                          
007000*    ---- PARAMETRAR TILL DATKORT                                         
007100 01  FILLER                      PIC X(8)    VALUE 'DATKORT'.             
007200 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W41230'.              
007300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007400*01  -COPY WDATKORT                                                       
007500     EJECT                                                                
007600                                                                          
007700                                                                          
007800*    ---- PARAMETRAR TILL POSTSUM                                         
007900*01  -COPY W0005      -PRE POSTSUM-.                                      
008000     EJECT                                                                
008100*    ---- UTAREA                                                          
008200                                                                          
008300*01  FILLER                      PIC X(8)    VALUE 'UT-AREA'.             
008400                                                                          
008500*01  -COPY W412030     -PRE UT-.                                 C        
008600     EJECT                                                                
008700                                                                          
008800*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
008900                                                                          
009000 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
009100                                                                          
009200*    ---- STATUSKOD FRÅN IMS                                              
009300                                                                          
009400 01  STATUS-WS                   PIC XX.                                  
009500     88  SEGMENT-FINNS                      VALUE '  '.                   
009600     88  SEGMENT-SLUT                       VALUE 'GB'.                   
009700     SKIP3                                                                
009800 01  GODK-STATUSKODER.                                                    
009900   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
010000     SKIP3                                                                
010100 01  SSA1                        PIC X(40).                               
010200     SKIP3                                                                
010300                                                                          
010400*01      -COPY W0003.                                                     
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16)  VALUE                         
010700                                            'DLI-IO-AREA'.                
010800 01  DLI-IO-AREA.                                                         
011000*                                                                         
011100*  03  WLORQI01 -COPY WDQ201                                              
011200     EJECT                                                                
011500                                                                          
011600 LINKAGE SECTION.                                                         
011700     SKIP2                                                                
011800*    -COPY W0008 -PRE WDQ2-.                                              
011900    05  FILLER                   PIC X(1).                                
012000     EJECT                                                                
012100                                                                          
012200 PROCEDURE DIVISION  USING WDQ2-PCB.                                      
012300     ENTRY 'DLITCBL' USING WDQ2-PCB.                                      
012400 STYR SECTION.                                                            
012500     PERFORM A-INIT                                                       
012600     PERFORM IMS-GET-WDQ2                                                 
012700     PERFORM UNTIL SEGMENT-SLUT                                           
012800       EVALUATE WDQ2-SEG-NAME-FB                                          
012900         WHEN 'WDQ201  '                                                  
013000           PERFORM B-SKAPA-UTPOST                                         
013300       END-EVALUATE                                                       
013400       PERFORM IMS-GET-WDQ2                                               
013500     END-PERFORM                                                          
013600                                                                          
013700     PERFORM Z-FINIT                                                      
013800     MOVE ZERO TO RETURN-CODE                                             
013900     GOBACK                                                               
014000     .                                                                    
014100     EJECT                                                                
014200 A-INIT SECTION.                                                          
014300     SKIP2                                                                
014400     OPEN OUTPUT W41230                                                   
014500     MOVE 'W4123000'         TO POSTSUM-PROGNAMN                          
014600     MOVE 'W41230D1'         TO POSTSUM-DDNAMN2                           
014700     MOVE 'W41230  '         TO POSTSUM-FDNAMN                            
014800                                                                          
014900                                                                          
015000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
015100                                                                          
015200     MOVE D-AAR              TO DAGENS-DATUM-AAR                          
015300     MOVE D-MAANAD           TO DAGENS-DATUM-MAANAD                       
015400     MOVE D-DAG              TO DAGENS-DATUM-DAG                          
015500                                                                          
015600     .                                                                    
015700     EJECT                                                                
015800 B-SKAPA-UTPOST SECTION.                                                  
016400                                                                          
016700     IF OHUV-TIREGDAT = DAGENS-DATUM    AND                               
016800        OHUV-BEVARREF NOT = 'W480      '                                  
016810       MOVE OHUV-IDORDER              TO UT-IDORDER                       
016820       MOVE OHUV-KDORDKL              TO UT-KDORDKL                       
016900       MOVE OHUV-IDDISTR              TO UT-IDDISTR                       
017000       MOVE OHUV-IDKUNDNR             TO UT-IDKUNDNR                      
017100       MOVE OHUV-IDKUNDRF             TO UT-IDKUNDRF                      
017200       MOVE OHUV-TIREGDAT             TO UT-TIREGDAT                      
017300                                                                          
017400       WRITE UT-AREA FROM UT-W412030                                      
017500       CALL POSTSUM USING POSTSUM-PARM                                    
017600     END-IF                                                               
017700     .                                                                    
017800     EJECT                                                                
017900 Z-FINIT SECTION.                                                         
018000     SKIP2                                                                
018100     CLOSE W41230                                                         
018200     MOVE 'S' TO POSTSUM-OPKOD                                            
018300     CALL POSTSUM USING POSTSUM-PARM                                      
018400     .                                                                    
018500     EJECT                                                                
018600*    ---- IMS SEKTIONER                                                   
018700 IMS-GET-WDQ2 SECTION.                                                    
018800                                                                          
018900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
019000     CALL CBLTDLI USING GN WDQ2-PCB DLI-IO-AREA                           
019100     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
019200     PERFORM IMS-STATUSKONTROLL                                           
019300     .                                                                    
019400     SKIP3                                                                
019500 IMS-STATUSKONTROLL SECTION.                                              
019600                                                                          
019700     SET STATUS-IX TO 1                                                   
019800     SEARCH GODK-STATUS                                                   
019900       AT END CALL FELLOG                                                 
020000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
020100     END-SEARCH                                                           
020200     .                                                                    
