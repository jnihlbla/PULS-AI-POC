000400 ID DIVISION.                                                             
000500 PROGRAM-ID.                 W2250600.                                    
000600*                                                                         
000900*AUTHOR.                     IDK, GÖTEBORG.                               
001000*DATE-WRITTEN.               SEPT 1978.                                   
001100*    SKIP2                                                                
001200*REMARKS.                                                                 
001300*    FUNKTION.                                                            
001400*        PROGRAMMET SKALL MED HJÄLP AV INDATAFILEN W22505                 
001500*        SKAPA REGISTER W22509.                                           
001600*        DÅ PERIODENS VECKONR I DATUMKORT WDATUM = 1                      
001700*        LÄGGS DUMMY PÅ REGISTERIN OCH TRANSAKTIONENS                     
001800*        VÄRDEN FLYTTAS TILL OCCURS 1,2,3.                                
001900*        DÅ PERIODENS VECKONR > 1 SKER FÖLJANDE:                          
002000*        A.  TRANSIN = REGISTERIN                                         
002100*            FLYTTA TRANSIN TILL REGUT OCCURS 2                           
002200*            ADDERA TRANSIN TILL REGUT OCCURS 3                           
002300*        B.  TRANSIN < REGISTERIN                                         
002400*            NOLLSTÄLL OCCURS 1 I REGUT                                   
002500*            FLYTTA TRANSIN TILL REGUT OCCURS 2                           
002600*            ADDERA TRANSIN TILL REGUT OCCURS 3                           
002700*        C.  TRANSIN > REGISERIN                                          
002800*            FLYTTA REGIN TILL REGUT                                      
002900                                                                          
003000*        DET KAN FÖREKOMMA FLERA POSTER PÅ VARJE ARTIKEL OCH              
003100*        ENDAST POSTER MED ORDERKLASS MINDRE ELLER LIKA MED 4             
003200*        BEHANDLAS.                                                       
003210*        ORDERKLASS 0 BEHANDLAS TILLSAMMANS MED ORDERKLASS 1.             
003220*                                                                         
003230*        SDC:                                                             
003240*        INGEN SDC FÖRÄNDRING GÖRS NU, INFILEN BESTÅR AV                  
003241*        INFILEN FRÅN TRATTEN KOMMER ENDAST ATT HA CDC UPPGIFTER.         
003242*        REGISTRET FÖRBLIR OFÖRÄNDRAT OCH HA PLATS FÖR BÅDE               
003250*        CDC OCH SDC UPPGIFTER. CDC FÄLTEN KOMMER ATT INNEHÅLLA           
003260*        CDC INFORMATION MEDAN SDC FÄLTEN KOMMER ATT VARA                 
003270*        NOLLFYLLDA.                                                      
003280*                                                                         
003300     EJECT                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500 INPUT-OUTPUT SECTION.                                                    
003600 FILE-CONTROL.                                                            
003700     SKIP2                                                                
003800*------------------------------- INFO OM AVBOKADE RADER MM     ***        
003900*                                INPUT                         ***        
004000     SELECT  W22505          ASSIGN UT-S-W22506D1.                        
004100*                                                                         
004200*------------------------------- REGISTER                      ***        
004300*                                INPUT                         ***        
004400     SELECT  W22509I         ASSIGN UT-S-W22506D2.                        
004500*                                                                         
004600*------------------------------- REGISTER                      ***        
004700*                                OUTPUT                        ***        
004800     SELECT  W22509U         ASSIGN UT-S-W22506D3.                        
004900     EJECT                                                                
005000 DATA DIVISION.                                                           
005100 FILE SECTION.                                                            
005200     SKIP2                                                                
005300 FD  W22505                                                               
005400     RECORDING F                                                          
005500     BLOCK 0                                                              
005600     .                                                                    
005700*01  -COPY W225P232C0 -L                                                  
005900     SKIP2                                                                
006000 FD  W22509I                                                              
006100     RECORDING F                                                          
006200     BLOCK 0                                                              
006300     .                                                                    
006400*01  -COPY W225P233C0 -L                                                  
006600     SKIP2                                                                
006700 FD  W22509U                                                              
006800     RECORDING F                                                          
006900     BLOCK 0                                                              
007000     .                                                                    
007100*01  POST -COPY W225P233C0 -PRE U09P233- -L                               
007300     EJECT                                                                
007400 WORKING-STORAGE SECTION.                                                 
008000     SKIP2                                                                
008001                                                                          
008010*    -- CHECKED BY WY2000                                                 
008100 01  KONSTANTER.                                                          
008200     05  JA                  PIC X       VALUE 'J'.                       
008300     05  NEJ                 PIC X       VALUE 'N'.                       
008400     SKIP2                                                                
008500 01  EOF-SWITCHAR.                                                        
008600     05  W22505-EOF          PIC X       VALUE 'N'.                       
008700     05  W22509-EOF          PIC X       VALUE 'N'.                       
008800     SKIP2                                                                
008900 01  IX.                                                                  
009000     05  IX1                 PIC S9(9)               COMP SYNC.           
009100     05  IX2                 PIC S9(9)               COMP SYNC.           
009200     SKIP2                                                                
009300 01  ID-BEGREPP.                                                          
009400     05  TRANS-ID.                                                        
009500         10  TRANS-ID-N      PIC S9(9).                                   
009600     05  REGIN-ID.                                                        
009700         10  REGIN-ID-N      PIC S9(9).                                   
009800     05  SPAR-ID.                                                         
009900         10  SPAR-ID-N       PIC S9(9).                                   
010000     SKIP2                                                                
010010 01  WS-DAGENS-AAVV          PIC 9(4).                                    
010020 01  FILLER REDEFINES WS-DAGENS-AAVV.                                     
010030     03 WS-DAGENS-AA         PIC 9(2).                                    
010040     03 WS-DAGENS-VV         PIC 9(2).                                    
010050                                                                          
010092 01  WS-FORSTA-AAVV          PIC 9(4).                                    
010093 01  FILLER REDEFINES WS-FORSTA-AAVV.                                     
010094     03 WS-FORSTA-AA         PIC 9(2).                                    
010095     03 WS-FORSTA-VV         PIC 9(2).                                    
010098     EJECT                                                                
010100 01  DYNAMISKA-SUBPROGRAM.                                                
010200     05  DATKORT             PIC X(8)    VALUE 'DATKORT '.                
010300     05  POSTSUM             PIC X(8)    VALUE 'POSTSUM '.                
010310     05  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
010400     SKIP2                                                                
010500*--------------------------------------- PARAMETRAR TILL DATKORT          
010600*                                                                         
010700 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W22506'.                  
010800 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
010900*01  -COPY WDATKORT                                                       
011100     EJECT                                                                
011110*01  -COPY WDATAREA                                                       
011120     EJECT                                                                
011200*--------------------------------------- PARAMETRAR TILL POSTSUM          
011300*                                                                         
011400*01  -COPY W0005      -PRE POSTSUM-                                       
011600     EJECT                                                                
011700*--------------------------------------- AREA FÖR W22505-POST             
011800*                                                                         
011900*01  AREA -COPY W225P232   -PRE I05P232-                                  
012100     EJECT                                                                
012200*--------------------------------------- AREA FÖR W22509-INPUTPOST        
012300*                                                                         
012400*01  AREA -COPY W225P233   -PRE I09P233-                                  
012600     EJECT                                                                
012700*--------------------------------------- AREA FÖR W22509-POST             
012800*                                        OUTPUT                           
012900*01  AREA -COPY W225P233   -PRE U09P233-                                  
013100     EJECT                                                                
013200 PROCEDURE DIVISION.                                                      
013300                                                                          
013400     PERFORM A-INITIERING                                                 
013500     PERFORM F-LAS-TRANS                                                  
013600     MOVE TRANS-ID-N TO SPAR-ID-N                                         
013700                                                                          
013800     IF  WS-DAGENS-AAVV = WS-FORSTA-AAVV                                  
013900*------------------------------------------ BEHANDLA VECKA 1              
014000       PERFORM UNTIL W22505-EOF = JA                                      
014200         MOVE 1 TO IX1                                                    
014300         PERFORM S05-NOLLA-INDEX                                          
014400         PERFORM UNTIL TRANS-ID NOT = SPAR-ID                             
014600           PERFORM C-ADDERA-INFO                                          
014700           PERFORM F-LAS-TRANS                                            
014800         END-PERFORM                                                      
014900         PERFORM B-BEHANDLA-VKA-1                                         
015000         PERFORM H-SKRIV-REGUT                                            
015100         MOVE TRANS-ID-N TO SPAR-ID-N                                     
015200       END-PERFORM                                                        
015300     ELSE                                                                 
015400       PERFORM G-LAS-REGIN                                                
015500       PERFORM UNTIL W22505-EOF = JA AND W22509-EOF = JA                  
015700         IF  TRANS-ID = REGIN-ID                                          
015800*------------------------------------------ UPPDATERA INDEX2 OCH 3        
015900           MOVE I09P233-AREA TO U09P233-AREA                              
016000           MOVE 2 TO IX1                                                  
016100           PERFORM S05-NOLLA-INDEX                                        
016200           PERFORM UNTIL TRANS-ID NOT = SPAR-ID                           
016400             PERFORM C-ADDERA-INFO                                        
016500             PERFORM F-LAS-TRANS                                          
016600           END-PERFORM                                                    
016700           PERFORM S02-BERAKNA-KVEJRO                                     
016800           PERFORM S01-ADDERA-INDEX3                                      
016900           PERFORM H-SKRIV-REGUT                                          
017000           PERFORM G-LAS-REGIN                                            
017100           MOVE TRANS-ID-N TO SPAR-ID-N                                   
017200         ELSE                                                             
017300           IF  TRANS-ID > REGIN-ID                                        
017400*----------------------------------------- TRANS SAKNAS,                  
017500*                                          INDEX2 NOLLSTÄLLS              
017600             MOVE I09P233-AREA   TO U09P233-AREA                          
017700             MOVE 2 TO IX1                                                
017800             PERFORM S05-NOLLA-INDEX                                      
017900             PERFORM H-SKRIV-REGUT                                        
018000             PERFORM G-LAS-REGIN                                          
018100           ELSE                                                           
018200             MOVE 2 TO IX1                                                
018300             PERFORM S05-NOLLA-INDEX                                      
018400             PERFORM UNTIL TRANS-ID NOT = SPAR-ID                         
018600               PERFORM C-ADDERA-INFO                                      
018700               PERFORM F-LAS-TRANS                                        
018800             END-PERFORM                                                  
018900             PERFORM D-NYUPPLAGGNING                                      
019000             PERFORM H-SKRIV-REGUT                                        
019100             MOVE TRANS-ID-N TO SPAR-ID-N                                 
019200           END-IF                                                         
019300         END-IF                                                           
019400       END-PERFORM                                                        
019500     END-IF                                                               
019600                                                                          
019700     PERFORM E-AVSLUTNING                                                 
019800     GOBACK                                                               
019900     .                                                                    
020000     EJECT                                                                
020100 A-INITIERING SECTION.                                                    
020200******************************************************************        
020300*    ÖPPNA ALLA FILER                                            *        
020400*    HÄMTA INFO FRÅN DATUMKORT                                   *        
020500******************************************************************        
020600                                                                          
020700     OPEN INPUT W22505 W22509I                                            
020800     OUTPUT W22509U                                                       
020900                                                                          
021000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
021100     MOVE PROGRAM-NAMN       TO POSTSUM-PROGNAMN                          
021120     MOVE D-AAR   TO WS-DAGENS-AA                                         
021130     MOVE D-VECKA TO WS-DAGENS-VV                                         
021140                                                                          
021160     MOVE 'AAVV  '       TO DAT-KDDATFORM                                 
021170     MOVE WS-DAGENS-AAVV TO DAT-I-TIDATUM                                 
021180     CALL WDATKONV USING DAT-KDDATFORM                                    
021190                         DAT-I-TIDATUM                                    
021191                         DAT-O-TIDATUM                                    
021192                         DAT-KDSVAR                                       
021197     MOVE 'AARP  '       TO DAT-KDDATFORM                                 
021198     MOVE DAT-TIAARP     TO DAT-I-TIDATUM                                 
021199     CALL WDATKONV USING DAT-KDDATFORM                                    
021200                         DAT-I-TIDATUM                                    
021201                         DAT-O-TIDATUM                                    
021202                         DAT-KDSVAR                                       
021203     MOVE DAT-TIAA       TO WS-FORSTA-AA                                  
021204     MOVE DAT-TIVV       TO WS-FORSTA-VV                                  
021210     .                                                                    
021300     EJECT                                                                
021400 B-BEHANDLA-VKA-1 SECTION.                                                
021500******************************************************************        
021600*    BERÄKNA KVEJRO                                              *        
021700*    FLYTTAR INFO FRÅN  INDEX1 TILL INDEX2 OCH ADDERA TILL       *        
021800*    INDEX3 I REGISTERUT                                         *        
021900******************************************************************        
022000                                                                          
022100     PERFORM S02-BERAKNA-KVEJRO                                           
022200     MOVE 1 TO IX1                                                        
022300     MOVE 2 TO IX2                                                        
022400     PERFORM S04-FLYTTA-INFO                                              
022500                                                                          
022600     MOVE 3 TO IX1                                                        
022700     PERFORM S05-NOLLA-INDEX                                              
022800     PERFORM S01-ADDERA-INDEX3                                            
022900                                                                          
023000     MOVE '233'              TO U09P233-IDPTYP                            
023100     MOVE SPAR-ID-N          TO U09P233-IDARTNR                           
023200     .                                                                    
023300     EJECT                                                                
023400 C-ADDERA-INFO SECTION.                                                   
023500******************************************************************        
023600*    ADDERAR VÄRDEN FRÅN TRANS TILL REGUT.                       *        
023700*    ENDAST POSTER MED ORDERKLASS < 5 BEHANDLAS                  *        
023800******************************************************************        
023900     SKIP2                                                                
024000     IF  I05P232-KDCLAGER = 1 OR                                          
024010         I05P232-KDCLAGER > 3                                             
024100       SKIP1                                                              
024200       IF  I05P232-KDORDKL <= 2                                           
024210        IF I05P232-KDORDKL = 0                                            
024211         IF  I05P232-KDCLAGER = 1                                         
024212         ADD I05P232-REAVBRAD TO U09P233-KVAVBRAD-CDC-0 (IX1)             
024213         ADD I05P232-REFYSAVV TO U09P233-KVFYSAVV-CDC-0 (IX1)             
024214         ADD I05P232-REINKORD TO U09P233-KVINORD-CDC-0  (IX1)             
024215         END-IF                                                           
024216         ADD I05P232-RERORAD  TO U09P233-KVRORAD-CDC-0  (IX1)             
024217                     U09P233-KVRORAD-CDC-0-VECKA        (IX1)             
024220        END-IF                                                            
024230        IF I05P232-KDORDKL = 1 OR 2                                       
024240         IF  I05P232-KDCLAGER = 1                                         
024300         ADD I05P232-REAVBRAD TO U09P233-KVAVBRAD-CDC-1-2(IX1)            
024400         ADD I05P232-REFYSAVV TO U09P233-KVFYSAVV-CDC-1-2(IX1)            
024500         ADD I05P232-REINKORD TO U09P233-KVINORD-CDC-1-2 (IX1)            
024510         END-IF                                                           
024600         ADD I05P232-RERORAD  TO U09P233-KVRORAD-CDC-1-2 (IX1)            
024700                     U09P233-KVRORAD-CDC-1-2-VECKA       (IX1)            
024710        END-IF                                                            
024800       ELSE                                                               
024900         IF  I05P232-KDORDKL NOT > 4                                      
024910           IF  I05P232-KDCLAGER = 1                                       
025000           ADD I05P232-REAVBRAD TO U09P233-KVAVBRAD-CDC-3-4 (IX1)         
025100           ADD I05P232-REFYSAVV TO U09P233-KVFYSAVV-CDC-3-4 (IX1)         
025200           ADD I05P232-REINKORD TO U09P233-KVINORD-CDC-3-4  (IX1)         
025210           END-IF                                                         
025300           ADD I05P232-RERORAD  TO U09P233-KVRORAD-CDC-3-4  (IX1)         
025400                       U09P233-KVRORAD-CDC-3-4-VECKA        (IX1)         
025508         END-IF                                                           
025509       END-IF                                                             
025510     ELSE                                                                 
025511       IF  I05P232-KDORDKL <= 2                                           
025512        IF I05P232-KDORDKL = 0                                            
025513         ADD I05P232-REAVBRAD TO U09P233-KVAVBRAD-SDC-0 (IX1)             
025514         ADD I05P232-REFYSAVV TO U09P233-KVFYSAVV-SDC-0 (IX1)             
025515         ADD I05P232-REINKORD TO U09P233-KVINORD-SDC-0  (IX1)             
025516         ADD I05P232-RERORAD  TO U09P233-KVRORAD-SDC-0  (IX1)             
025517                     U09P233-KVRORAD-SDC-0-VECKA        (IX1)             
025518        END-IF                                                            
025519        IF I05P232-KDORDKL = 1 OR 2                                       
025520         ADD I05P232-REAVBRAD TO U09P233-KVAVBRAD-SDC-1-2(IX1)            
025521         ADD I05P232-REFYSAVV TO U09P233-KVFYSAVV-SDC-1-2(IX1)            
025522         ADD I05P232-REINKORD TO U09P233-KVINORD-SDC-1-2 (IX1)            
025523         ADD I05P232-RERORAD  TO U09P233-KVRORAD-SDC-1-2 (IX1)            
025524                     U09P233-KVRORAD-SDC-1-2-VECKA       (IX1)            
025525        END-IF                                                            
025526       ELSE                                                               
025527         IF  I05P232-KDORDKL NOT > 4                                      
025528           ADD I05P232-REAVBRAD TO U09P233-KVAVBRAD-SDC-3-4 (IX1)         
025529           ADD I05P232-REFYSAVV TO U09P233-KVFYSAVV-SDC-3-4 (IX1)         
025530           ADD I05P232-REINKORD TO U09P233-KVINORD-SDC-3-4  (IX1)         
025531           ADD I05P232-RERORAD  TO U09P233-KVRORAD-SDC-3-4  (IX1)         
025532                       U09P233-KVRORAD-SDC-3-4-VECKA        (IX1)         
025540         END-IF                                                           
025600       END-IF                                                             
027300     END-IF                                                               
027400     .                                                                    
027500     EJECT                                                                
027600 D-NYUPPLAGGNING SECTION.                                                 
027700******************************************************************        
027800*    NOLLSTÄLL INDEX1 OCH FLYTTA IN VÄRDEN FRÅN TRANS TILL       *        
027900*    INDEX2 OCH INDEX3 I REGUT.                                  *        
028000******************************************************************        
028100                                                                          
028200     MOVE 1 TO IX1                                                        
028300     PERFORM S05-NOLLA-INDEX                                              
028400                                                                          
028500     MOVE 2 TO IX1                                                        
028600     PERFORM S02-BERAKNA-KVEJRO                                           
028700                                                                          
028800     MOVE 3 TO IX1                                                        
028900     PERFORM S05-NOLLA-INDEX                                              
029000     PERFORM S01-ADDERA-INDEX3                                            
029100                                                                          
029200     MOVE '233'              TO U09P233-IDPTYP                            
029300     MOVE SPAR-ID-N          TO U09P233-IDARTNR                           
029500     .                                                                    
029600     EJECT                                                                
029700 E-AVSLUTNING SECTION.                                                    
029800******************************************************************        
029900*    STÄNG ALLA FILER                                            *        
030000*    SKRIV UT POSTSUMS RÄKNEVERK                                 *        
030100******************************************************************        
030200                                                                          
030300     CLOSE W22505                                                         
030400           W22509I                                                        
030500           W22509U                                                        
030600                                                                          
030700     MOVE 'S'                TO POSTSUM-OPKOD                             
030800     CALL POSTSUM USING POSTSUM-PARM                                      
030900     .                                                                    
031000     EJECT                                                                
031100 F-LAS-TRANS SECTION.                                                     
031200******************************************************************        
031300*    LÄS W22505 OCH ÖKA UPP POSTRÄKNAREN                         *        
031400******************************************************************        
031500     SKIP2                                                                
031600     READ W22505 INTO I05P232-AREA AT END                                 
031610          MOVE JA TO W22505-EOF                                           
031620          MOVE 999999999  TO TRANS-ID                                     
032200     END-READ                                                             
032300                                                                          
032400     IF  W22505-EOF = NEJ                                                 
032500         MOVE I05P232-IDARTNR    TO TRANS-ID-N                            
032600         MOVE 'W22505'       TO POSTSUM-FDNAMN                            
032700         MOVE 'W22506D1'     TO POSTSUM-DDNAMN2                           
032800         MOVE I05P232-IDPTYP TO POSTSUM-TRANSTYP                          
032900         CALL POSTSUM USING POSTSUM-PARM                                  
033000     END-IF                                                               
033100     .                                                                    
033200     SKIP3                                                                
033300 G-LAS-REGIN SECTION.                                                     
033400******************************************************************        
033500*    LÄS W22509 OCH ÖKA UPP POSTRÄKNAREN                         *        
033600******************************************************************        
033700                                                                          
033800     READ W22509I INTO I09P233-AREA AT END                                
033900          MOVE JA TO W22509-EOF                                           
033910          MOVE 999999999  TO REGIN-ID                                     
034400     END-READ                                                             
034500                                                                          
034600     IF  W22509-EOF = NEJ                                                 
034700         MOVE I09P233-IDARTNR    TO REGIN-ID-N                            
034800         MOVE 'W22509'           TO POSTSUM-FDNAMN                        
034900         MOVE 'W22506D2'         TO POSTSUM-DDNAMN2                       
035000         MOVE I09P233-IDPTYP     TO POSTSUM-TRANSTYP                      
035100         CALL POSTSUM USING POSTSUM-PARM                                  
035200     END-IF                                                               
035300     .                                                                    
035400     EJECT                                                                
035500 H-SKRIV-REGUT SECTION.                                                   
035600******************************************************************        
035700*    SKRIV POST PÅ W22509 OCH ÖKA UPP POSTRÄKNAREN               *        
035800******************************************************************        
035900                                                                          
036000     WRITE U09P233-POST FROM U09P233-AREA                                 
036100                                                                          
036200     MOVE 'W22509'           TO POSTSUM-FDNAMN                            
036300     MOVE 'W22506D3'         TO POSTSUM-DDNAMN2                           
036400     MOVE U09P233-IDPTYP     TO POSTSUM-TRANSTYP                          
036500     SKIP1                                                                
036600     CALL POSTSUM USING POSTSUM-PARM                                      
036700     .                                                                    
036800     EJECT                                                                
036900 S01-ADDERA-INDEX3 SECTION.                                               
037000******************************************************************        
037100*    ADDERAR INDEX3 I REGUT MED INFO FRÅN TRANS                  *        
037200******************************************************************        
037300                                                                          
037400     ADD U09P233-KVAVBRAD-CDC-0  (2)                                      
037401                                    TO U09P233-KVAVBRAD-CDC-0  (3)        
037410     ADD U09P233-KVAVBRAD-CDC-1-2(2)                                      
037420                                    TO U09P233-KVAVBRAD-CDC-1-2(3)        
037500     ADD U09P233-KVAVBRAD-CDC-3-4(2)                                      
037600                                    TO U09P233-KVAVBRAD-CDC-3-4(3)        
037700     ADD U09P233-KVFYSAVV-CDC-0  (2)                                      
037701                                    TO U09P233-KVFYSAVV-CDC-0  (3)        
037710     ADD U09P233-KVFYSAVV-CDC-1-2(2)                                      
037720                                    TO U09P233-KVFYSAVV-CDC-1-2(3)        
037800     ADD U09P233-KVFYSAVV-CDC-3-4(2)                                      
037900                                    TO U09P233-KVFYSAVV-CDC-3-4(3)        
038000     ADD U09P233-KVINORD-CDC-0   (2)                                      
038001                                    TO U09P233-KVINORD-CDC-0   (3)        
038010     ADD U09P233-KVINORD-CDC-1-2 (2)                                      
038020                                    TO U09P233-KVINORD-CDC-1-2 (3)        
038100     ADD U09P233-KVINORD-CDC-3-4(2)                                       
038110                                    TO U09P233-KVINORD-CDC-3-4 (3)        
038200     ADD U09P233-KVRORAD-CDC-0  (2)                                       
038201                                    TO U09P233-KVRORAD-CDC-0   (3)        
038210     ADD U09P233-KVRORAD-CDC-1-2 (2)                                      
038220                                    TO U09P233-KVRORAD-CDC-1-2 (3)        
038300     ADD U09P233-KVRORAD-CDC-3-4(2)                                       
038310                                    TO U09P233-KVRORAD-CDC-3-4 (3)        
038400     ADD U09P233-KVRORAD-CDC-0-VECKA(2)                                   
038500                              TO U09P233-KVRORAD-CDC-0-VECKA   (3)        
038510     ADD U09P233-KVRORAD-CDC-1-2-VECKA(2)                                 
038520                              TO U09P233-KVRORAD-CDC-1-2-VECKA (3)        
038600     ADD U09P233-KVRORAD-CDC-3-4-VECKA(2)                                 
038700                              TO U09P233-KVRORAD-CDC-3-4-VECKA (3)        
038800     ADD U09P233-KVEJRO-CDC-0   (2) TO U09P233-KVEJRO-CDC-0    (3)        
038810     ADD U09P233-KVEJRO-CDC-1-2 (2) TO U09P233-KVEJRO-CDC-1-2  (3)        
038900     ADD U09P233-KVEJRO-CDC-3-4 (2) TO U09P233-KVEJRO-CDC-3-4  (3)        
039000     SKIP3                                                                
039100     ADD U09P233-KVAVBRAD-SDC-0 (2) TO U09P233-KVAVBRAD-SDC-0  (3)        
039110     ADD U09P233-KVAVBRAD-SDC-1-2(2)                                      
039120                                    TO U09P233-KVAVBRAD-SDC-1-2(3)        
039200     ADD U09P233-KVAVBRAD-SDC-3-4(2)                                      
039300                                    TO U09P233-KVAVBRAD-SDC-3-4(3)        
039400     ADD U09P233-KVFYSAVV-SDC-0 (2) TO U09P233-KVFYSAVV-SDC-0  (3)        
039410     ADD U09P233-KVFYSAVV-SDC-1-2(2)                                      
039420                                    TO U09P233-KVFYSAVV-SDC-1-2(3)        
039500     ADD U09P233-KVFYSAVV-SDC-3-4(2)                                      
039600                                    TO U09P233-KVFYSAVV-SDC-3-4(3)        
039700     ADD U09P233-KVINORD-SDC-0  (2) TO U09P233-KVINORD-SDC-0   (3)        
039710     ADD U09P233-KVINORD-SDC-1-2(2) TO U09P233-KVINORD-SDC-1-2 (3)        
039800     ADD U09P233-KVINORD-SDC-3-4(2) TO U09P233-KVINORD-SDC-3-4 (3)        
039900     ADD U09P233-KVRORAD-SDC-0  (2) TO U09P233-KVRORAD-SDC-0   (3)        
039910     ADD U09P233-KVRORAD-SDC-1-2(2) TO U09P233-KVRORAD-SDC-1-2 (3)        
040000     ADD U09P233-KVRORAD-SDC-3-4(2) TO U09P233-KVRORAD-SDC-3-4 (3)        
040100     ADD U09P233-KVRORAD-SDC-0-VECKA(2)                                   
040101                              TO U09P233-KVRORAD-SDC-0-VECKA   (3)        
040110     ADD U09P233-KVRORAD-SDC-1-2-VECKA(2)                                 
040200                              TO U09P233-KVRORAD-SDC-1-2-VECKA (3)        
040300     ADD U09P233-KVRORAD-SDC-3-4-VECKA(2)                                 
040400                              TO U09P233-KVRORAD-SDC-3-4-VECKA (3)        
040500     ADD U09P233-KVEJRO-SDC-0   (2) TO U09P233-KVEJRO-SDC-0    (3)        
040510     ADD U09P233-KVEJRO-SDC-1-2 (2) TO U09P233-KVEJRO-SDC-1-2  (3)        
040600     ADD U09P233-KVEJRO-SDC-3-4 (2) TO U09P233-KVEJRO-SDC-3-4  (3)        
040700     .                                                                    
040800     EJECT                                                                
040900 S02-BERAKNA-KVEJRO SECTION.                                              
041000******************************************************************        
041100*    BERÄKNA KVEJRO                                                       
041200******************************************************************        
041300                                                                          
041400     COMPUTE U09P233-KVEJRO-CDC-0     (IX1) =                             
041500             U09P233-KVINORD-CDC-0    (IX1)                               
041510           + U09P233-KVFYSAVV-CDC-0   (IX1)                               
041600           - U09P233-KVAVBRAD-CDC-0   (IX1)                               
041700           - U09P233-KVRORAD-CDC-0    (IX1)                               
041710                                                                          
041720     COMPUTE U09P233-KVEJRO-CDC-1-2   (IX1) =                             
041730             U09P233-KVINORD-CDC-1-2  (IX1)                               
041740           + U09P233-KVFYSAVV-CDC-1-2 (IX1)                               
041750           - U09P233-KVAVBRAD-CDC-1-2 (IX1)                               
041760           - U09P233-KVRORAD-CDC-1-2  (IX1)                               
041800                                                                          
041900     COMPUTE U09P233-KVEJRO-CDC-3-4   (IX1) =                             
042000             U09P233-KVINORD-CDC-3-4  (IX1)                               
042010           + U09P233-KVFYSAVV-CDC-3-4 (IX1)                               
042100           - U09P233-KVAVBRAD-CDC-3-4 (IX1)                               
042200           - U09P233-KVRORAD-CDC-3-4  (IX1)                               
042300                                                                          
042400     COMPUTE U09P233-KVEJRO-SDC-0     (IX1) =                             
042500             U09P233-KVINORD-SDC-0    (IX1)                               
042510           + U09P233-KVFYSAVV-SDC-0   (IX1)                               
042600           - U09P233-KVAVBRAD-SDC-0   (IX1)                               
042700           - U09P233-KVRORAD-SDC-0    (IX1)                               
042710                                                                          
042720     COMPUTE U09P233-KVEJRO-SDC-1-2   (IX1) =                             
042730             U09P233-KVINORD-SDC-1-2  (IX1)                               
042740           + U09P233-KVFYSAVV-SDC-1-2 (IX1)                               
042750           - U09P233-KVAVBRAD-SDC-1-2 (IX1)                               
042760           - U09P233-KVRORAD-SDC-1-2  (IX1)                               
042800                                                                          
042900     COMPUTE U09P233-KVEJRO-SDC-3-4   (IX1) =                             
043000             U09P233-KVINORD-SDC-3-4  (IX1)                               
043010           + U09P233-KVFYSAVV-SDC-3-4 (IX1)                               
043100           - U09P233-KVAVBRAD-SDC-3-4 (IX1)                               
043200           - U09P233-KVRORAD-SDC-3-4  (IX1)                               
043300     .                                                                    
043500     EJECT                                                                
043600 S04-FLYTTA-INFO SECTION.                                                 
043700****************************************************************          
043800*    FLYTTA INFORMATION MELLAN OLIKA INDEX                     *          
043900****************************************************************          
044000                                                                          
044100     MOVE U09P233-KVAVBRAD-CDC-0 (IX1)                                    
044200                           TO U09P233-KVAVBRAD-CDC-0   (IX2)              
044210     MOVE U09P233-KVAVBRAD-CDC-1-2 (IX1)                                  
044220                           TO U09P233-KVAVBRAD-CDC-1-2 (IX2)              
044300     MOVE U09P233-KVAVBRAD-CDC-3-4 (IX1)                                  
044400                           TO U09P233-KVAVBRAD-CDC-3-4 (IX2)              
044500     MOVE U09P233-KVFYSAVV-CDC-0   (IX1)                                  
044600                           TO U09P233-KVFYSAVV-CDC-0   (IX2)              
044610     MOVE U09P233-KVFYSAVV-CDC-1-2 (IX1)                                  
044620                           TO U09P233-KVFYSAVV-CDC-1-2 (IX2)              
044700     MOVE U09P233-KVFYSAVV-CDC-3-4 (IX1)                                  
044800                           TO U09P233-KVFYSAVV-CDC-3-4 (IX2)              
044900     MOVE U09P233-KVINORD-CDC-0    (IX1)                                  
044910                           TO U09P233-KVINORD-CDC-0    (IX2)              
044920     MOVE U09P233-KVINORD-CDC-1-2  (IX1)                                  
044930                           TO U09P233-KVINORD-CDC-1-2  (IX2)              
045000     MOVE U09P233-KVINORD-CDC-3-4  (IX1)                                  
045100                           TO U09P233-KVINORD-CDC-3-4  (IX2)              
045200     MOVE U09P233-KVRORAD-CDC-0    (IX1)                                  
045210                           TO U09P233-KVRORAD-CDC-0    (IX2)              
045220     MOVE U09P233-KVRORAD-CDC-1-2  (IX1)                                  
045230                           TO U09P233-KVRORAD-CDC-1-2  (IX2)              
045300     MOVE U09P233-KVRORAD-CDC-3-4  (IX1)                                  
045400                           TO U09P233-KVRORAD-CDC-3-4  (IX2)              
045500     MOVE U09P233-KVRORAD-CDC-0-VECKA (IX1)                               
045600                           TO U09P233-KVRORAD-CDC-0-VECKA (IX2)           
045610     MOVE U09P233-KVRORAD-CDC-1-2-VECKA (IX1)                             
045620                           TO U09P233-KVRORAD-CDC-1-2-VECKA (IX2)         
045700     MOVE U09P233-KVRORAD-CDC-3-4-VECKA (IX1)                             
045800                           TO U09P233-KVRORAD-CDC-3-4-VECKA(IX2)          
045900     MOVE U09P233-KVEJRO-CDC-0   (IX1)                                    
045910                           TO U09P233-KVEJRO-CDC-0      (IX2)             
045920     MOVE U09P233-KVEJRO-CDC-1-2 (IX1)                                    
045930                           TO U09P233-KVEJRO-CDC-1-2    (IX2)             
046000     MOVE U09P233-KVEJRO-CDC-3-4 (IX1)                                    
046100                           TO U09P233-KVEJRO-CDC-3-4    (IX2)             
046200     SKIP3                                                                
046300     MOVE U09P233-KVAVBRAD-SDC-0 (IX1)                                    
046400                           TO U09P233-KVAVBRAD-SDC-0    (IX2)             
046410     MOVE U09P233-KVAVBRAD-SDC-1-2(IX1)                                   
046420                           TO U09P233-KVAVBRAD-SDC-1-2  (IX2)             
046500     MOVE U09P233-KVAVBRAD-SDC-3-4 (IX1)                                  
046600                           TO U09P233-KVAVBRAD-SDC-3-4  (IX2)             
046700     MOVE U09P233-KVFYSAVV-SDC-0 (IX1)                                    
046800                           TO U09P233-KVFYSAVV-SDC-0    (IX2)             
046810     MOVE U09P233-KVFYSAVV-SDC-1-2(IX1)                                   
046820                           TO U09P233-KVFYSAVV-SDC-1-2  (IX2)             
046900     MOVE U09P233-KVFYSAVV-SDC-3-4 (IX1)                                  
047000                           TO U09P233-KVFYSAVV-SDC-3-4  (IX2)             
047100     MOVE U09P233-KVINORD-SDC-0 (IX1)                                     
047200                           TO U09P233-KVINORD-SDC-0     (IX2)             
047210     MOVE U09P233-KVINORD-SDC-1-2(IX1)                                    
047220                           TO U09P233-KVINORD-SDC-1-2   (IX2)             
047300     MOVE U09P233-KVINORD-SDC-3-4 (IX1)                                   
047400                           TO U09P233-KVINORD-SDC-3-4   (IX2)             
047500     MOVE U09P233-KVRORAD-SDC-0 (IX1)                                     
047510                           TO U09P233-KVRORAD-SDC-0     (IX2)             
047520     MOVE U09P233-KVRORAD-SDC-1-2(IX1)                                    
047530                           TO U09P233-KVRORAD-SDC-1-2   (IX2)             
047600     MOVE U09P233-KVRORAD-SDC-3-4 (IX1)                                   
047700                           TO U09P233-KVRORAD-SDC-3-4   (IX2)             
047800     MOVE U09P233-KVRORAD-SDC-0-VECKA (IX1)                               
047900                           TO U09P233-KVRORAD-SDC-0-VECKA (IX2)           
047910     MOVE U09P233-KVRORAD-SDC-1-2-VECKA (IX1)                             
047920                           TO U09P233-KVRORAD-SDC-1-2-VECKA (IX2)         
048000     MOVE U09P233-KVRORAD-SDC-3-4-VECKA (IX1)                             
048100                           TO U09P233-KVRORAD-SDC-3-4-VECKA (IX2)         
048200     MOVE U09P233-KVEJRO-SDC-0 (IX1)                                      
048210                           TO U09P233-KVEJRO-SDC-0      (IX2)             
048220     MOVE U09P233-KVEJRO-SDC-1-2(IX1)                                     
048230                           TO U09P233-KVEJRO-SDC-1-2    (IX2)             
048300     MOVE U09P233-KVEJRO-SDC-3-4 (IX1)                                    
048400                           TO U09P233-KVEJRO-SDC-3-4    (IX2)             
048500     .                                                                    
048600     EJECT                                                                
048700 S05-NOLLA-INDEX SECTION.                                                 
048800******************************************************************        
048900*    NOLLSTÄLL INDEX                                                      
049000******************************************************************        
049100                                                                          
049200     MOVE ZERO           TO U09P233-KVAVBRAD-CDC-0   (IX1)                
049210                            U09P233-KVAVBRAD-CDC-1-2 (IX1)                
049300                            U09P233-KVAVBRAD-CDC-3-4 (IX1)                
049400                            U09P233-KVFYSAVV-CDC-0   (IX1)                
049410                            U09P233-KVFYSAVV-CDC-1-2 (IX1)                
049500                            U09P233-KVFYSAVV-CDC-3-4 (IX1)                
049600                            U09P233-KVINORD-CDC-0    (IX1)                
049610                            U09P233-KVINORD-CDC-1-2  (IX1)                
049700                            U09P233-KVINORD-CDC-3-4  (IX1)                
049800                            U09P233-KVRORAD-CDC-0    (IX1)                
049810                            U09P233-KVRORAD-CDC-1-2  (IX1)                
049900                            U09P233-KVRORAD-CDC-3-4  (IX1)                
050000                            U09P233-KVRORAD-CDC-0-VECKA   (IX1)           
050010                            U09P233-KVRORAD-CDC-1-2-VECKA (IX1)           
050100                            U09P233-KVRORAD-CDC-3-4-VECKA (IX1)           
050200                            U09P233-KVEJRO-CDC-0     (IX1)                
050210                            U09P233-KVEJRO-CDC-1-2   (IX1)                
050300                            U09P233-KVEJRO-CDC-3-4   (IX1)                
050400                            U09P233-KVAVBRAD-SDC-0   (IX1)                
050410                            U09P233-KVAVBRAD-SDC-1-2 (IX1)                
050500                            U09P233-KVAVBRAD-SDC-3-4 (IX1)                
050600                            U09P233-KVFYSAVV-SDC-0   (IX1)                
050610                            U09P233-KVFYSAVV-SDC-1-2 (IX1)                
050700                            U09P233-KVFYSAVV-SDC-3-4 (IX1)                
050800                            U09P233-KVINORD-SDC-0    (IX1)                
050810                            U09P233-KVINORD-SDC-1-2  (IX1)                
050900                            U09P233-KVINORD-SDC-3-4  (IX1)                
051000                            U09P233-KVRORAD-SDC-0    (IX1)                
051010                            U09P233-KVRORAD-SDC-1-2  (IX1)                
051100                            U09P233-KVRORAD-SDC-3-4  (IX1)                
051200                            U09P233-KVRORAD-SDC-0-VECKA   (IX1)           
051210                            U09P233-KVRORAD-SDC-1-2-VECKA (IX1)           
051300                            U09P233-KVRORAD-SDC-3-4-VECKA (IX1)           
051400                            U09P233-KVEJRO-SDC-0     (IX1)                
051410                            U09P233-KVEJRO-SDC-1-2   (IX1)                
051500                            U09P233-KVEJRO-SDC-3-4   (IX1)                
051600     .                                                                    
