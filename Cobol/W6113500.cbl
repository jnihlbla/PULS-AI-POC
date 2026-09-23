001300 ID DIVISION.                                                             
001400                                                                          
001500 PROGRAM-ID.     W6113500.                                                
001600 AUTHOR.         MÅNS SAMUELSSON.                                         
001700 DATE-WRITTEN.   94/07/08.                                                
001800 DATE-COMPILED.                                                           
001900                                                                          
002000*    FUNKTION:                                                            
002100*        LISTAR WDL221 PERIOD VIS                                         
002200*                                                                         
002300*        PROGRAMMET LÄSER      WLINLE (WDL2)                              
002400*                                                                         
002500*    ABENDKODER:                                                          
002600*        U0016 -  . . . .                                                 
002700*        U1000 -  . . . .                                                 
002800*                                                                         
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200     SKIP2                                                                
003300 INPUT-OUTPUT SECTION.                                                    
003400                                                                          
003500 FILE-CONTROL.                                                            
003600     SKIP2                                                                
003700*          --- FIL MED INFO                                               
003800     SELECT W61135                     ASSIGN TO W61135D1.                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP2                                                                
004200 FILE SECTION.                                                            
004300     SKIP3                                                                
004400 FD  W61135                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  POST -COPY W6113501 -PRE  UT35-  -L.                                 
004900     EJECT                                                                
005000 WORKING-STORAGE SECTION.                                                 
005100                                                                          
005101                                                                          
005110*    -- CHECKED BY WY2000                                                 
005200 77  IDPGM                       PIC X(8)    VALUE 'W6113500'.            
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005410 01  WS-IDARTNR                  PIC S9(9)   VALUE +0.                    
005500     EJECT                                                                
005600 01  DATUM-TIAARP                PIC 9(4)    VALUE ZERO.                  
006020 01  KONV-TID                    PIC 9(16).                               
006030 01  FILLER REDEFINES KONV-TID.                                           
006040     03  KONV-SEKEL              PIC 9(2).                                
006050     03  KONV-TIAAMMDD           PIC 9(6).                                
006060     03  FILLER                  PIC 9(8).                                
006100     EJECT                                                                
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300*                                                                         
006400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006710     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006900     SKIP2                                                                
007000*    --- PARAMETRAR TILL ABEND                                            
007100                                                                          
007200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007400     SKIP2                                                                
007500 01  FELTEXT.                                                             
007600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007800     EJECT                                                                
008700*    --- PARAMETRAR TILL POSTSUM                                          
008800*                                                                         
008900*01  -COPY W0005   -PRE  POSTSUM-                                         
009000     EJECT                                                                
009010*    --- PARAMETRAR TILL DATKONV                                          
009020*                                                                         
009030*01  -COPY WDATAREA                                                       
009040     EJECT                                                                
009100 01  UT35-AREA-START             PIC X(24)   VALUE                        
009200                                 'UT35-AREA-START  '.                     
009300     SKIP2                                                                
009400                                                                          
009500*01  AREA -COPY W6113501     -PRE UT35-                                   
009600     EJECT                                                                
009700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009800*                                                                         
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010100     SKIP3                                                                
010800*    --- STATUS-KOD FRÅN IMS                                              
010900 01  STATUS-WS                   PIC XX.                                  
011000     88  SEGMENT-FINNS                       VALUE '  '.                  
011100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011200     SKIP2                                                                
011300 01  GODK-STATUSKODER.                                                    
011400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011500     SKIP3                                                                
011600 01  SSA1                        PIC X(64).                               
011700 01  SSA2                        PIC X(64).                               
011800     EJECT                                                                
011900*    --- IMS FUNKTIONSKODER                                               
012000*01  -COPY W0003                                                          
012100     EJECT                                                                
012200*    ---  DLI INPUT-OUTPUT AREA                                           
012300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012400     SKIP3                                                                
012500 01  DLI-IO-AREA.                                                         
012600     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
012700     SKIP3                                                                
012800     03  WLINLE01 REDEFINES IO-AREA.                                      
012900*        05  -COPY WDL201                                                 
013000     SKIP3                                                                
013010     03  WLINLE11 REDEFINES IO-AREA.                                      
013020*        05  -COPY WDL211                                                 
013030     SKIP3                                                                
013100     03  WLINLE21 REDEFINES IO-AREA.                                      
013200*        05  -COPY WDL221                                                 
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500                                                                          
013600     EJECT                                                                
013700*01  -COPY W0008  -PRE INLE-                                              
013800     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014000 PROCEDURE DIVISION  USING INLE-PCB.                                      
014100     ENTRY 'DLITCBL' USING INLE-PCB.                                      
014200                                                                          
014300                                                                          
014400     PERFORM A-INIT                                                       
014500     PERFORM IMS-GET-INLE                                                 
014600     PERFORM UNTIL SEGMENT-SLUT                                           
014700      EVALUATE INLE-SEG-NAME-FB                                           
014800        WHEN 'WDL201  '                                                   
014900           MOVE ART-IDARTNR TO UT35-IDARTNR                               
015000        WHEN 'WDL211  '                                                   
015100           PERFORM B-KONVERTERA-IDINLEV                                   
015110        WHEN 'WDL221  '                                                   
015120           PERFORM C-KONTROLLERA-SKRIV                                    
015200      END-EVALUATE                                                        
015300      PERFORM IMS-GET-INLE                                                
015400     END-PERFORM                                                          
015500                                                                          
015600                                                                          
015700     PERFORM Z-FINIT                                                      
015800                                                                          
015900     MOVE ZERO TO RETURN-CODE                                             
016000     GOBACK                                                               
016100     .                                                                    
016200     EJECT                                                                
016300 A-INIT SECTION.                                                          
016400                                                                          
016500     OPEN OUTPUT W61135                                                   
016600                                                                          
016610     MOVE 'IDAG'      TO DAT-KDDATFORM                                    
016620                                                                          
016700     CALL WDATKONV USING DAT-KDDATFORM                                    
016800                         DAT-I-TIDATUM                                    
016900                         DAT-O-TIDATUM                                    
017000                         DAT-KDSVAR                                       
017100                                                                          
017110     IF DAT-KDSVAR-OK                                                     
017120       MOVE DAT-TIAARP  TO DATUM-TIAARP                                   
017130     ELSE                                                                 
017140       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
017150     END-IF                                                               
017160                                                                          
017200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017300     .                                                                    
017400     EJECT                                                                
017500 B-KONVERTERA-IDINLEV SECTION.                                            
017600                                                                          
017700     COMPUTE KONV-TID = 9999999999999999 - INL-DAINLEV                    
017800                                                                          
017900     MOVE 'AAMMDD'         TO DAT-KDDATFORM                               
017910     MOVE KONV-TIAAMMDD    TO DAT-I-TIDATUM                               
018000                                                                          
018100     CALL WDATKONV USING DAT-KDDATFORM                                    
018200                         DAT-I-TIDATUM                                    
018300                         DAT-O-TIDATUM                                    
018400                         DAT-KDSVAR                                       
018410     IF DAT-KDSVAR-FEL                                                    
018411       MOVE UT35-IDARTNR     TO WS-IDARTNR                                
018420       DISPLAY 'FELAKTIG IDINLEV ARTIKEL = ' WS-IDARTNR                   
018430       DISPLAY 'KONVERTERAT DATUM        = ' DAT-I-TIDATUM                
018440*      CALL ABEND USING RKOD-ABEND-MED-DUMP                               
018450     END-IF                                                               
018500     .                                                                    
018600     EJECT                                                                
018700 C-KONTROLLERA-SKRIV SECTION.                                             
018800                                                                          
018900     IF MOT-IDPTYP = 'R32'                AND                             
019000        DAT-TIAARP     = DATUM-TIAARP                                     
019100                                                                          
019110       IF MOT-KDRT = 7 OR 8 OR 77 OR 88                                   
019120         CONTINUE                                                         
019130       ELSE                                                               
019200         MOVE MOT-IDLEVNR    TO UT35-IDLEVNR                              
019300         MOVE MOT-KVANTMOT   TO UT35-KVANTMOT                             
019400                                                                          
019500         PERFORM S11-SKRIV-W61135                                         
019610       END-IF                                                             
019700     END-IF                                                               
019710     .                                                                    
019720     EJECT                                                                
019800 Z-FINIT SECTION.                                                         
019900     CLOSE W61135                                                         
020000     SKIP2                                                                
020100     MOVE 'S' TO POSTSUM-OPKOD                                            
020200     CALL POSTSUM USING POSTSUM-PARM                                      
020300     .                                                                    
020400     EJECT                                                                
020500 S11-SKRIV-W61135 SECTION.                                                
020600                                                                          
020700     WRITE UT35-POST FROM UT35-AREA                                       
020800                                                                          
021000     MOVE 'W61135' TO POSTSUM-FDNAMN                                      
021100     MOVE 'W61135D1' TO POSTSUM-DDNAMN2                                   
021200     CALL POSTSUM USING POSTSUM-PARM                                      
021300     .                                                                    
021400     EJECT                                                                
021500 IMS-GET-INLE   SECTION.                                                  
021600                                                                          
021700     CALL CBLTDLI USING GN INLE-PCB DLI-IO-AREA                           
021800     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
021900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
022000     PERFORM IMS-STATUSKONTROLL                                           
022100     .                                                                    
022200     EJECT                                                                
022300 IMS-STATUSKONTROLL SECTION.                                              
022400                                                                          
022500     SET STATUS-IX TO 1                                                   
022600     SEARCH GODK-STATUS                                                   
022700       AT END                                                             
022800         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
022900         CALL FELLOG                                                      
023000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
023100         CONTINUE                                                         
023200     END-SEARCH                                                           
023300     .                                                                    
