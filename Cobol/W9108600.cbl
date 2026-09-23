000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W9108600.                                                
000400 AUTHOR.         P-A HELGEGREN (KOPIA W01174)                             
000500 DATE-WRITTEN.   10/11/05.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000810*        SKAPAR FIL MED BENÄMNINGAR TILL GGC                              
000811*                       (GLOBAL GOODWILL CALCULATOR)                      
000820*                                                                         
000900*        LÄSER BENÄMNINGSBASEN WDD3 MED SB.                               
001000*        SKAPAR FIL MED SAMTLIGA BENÄMNINGAR PER ARTIKEL.       00        
001100*        W91087   BENÄMNINGAR FRÅN WDD3                                   
001300*                                                                         
001400*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001500*                                                                         
001700*        ALLA ICKE-LATIN SPRÅK  ÄR                                        
001800*        LAGRADE MED UNICODE UTF8 I BASEN.                                
001900*        DÄRFÖR MÅSTE DE KONVERTERAS MED WCNVUNCE INNAN DE KAN            
002000*        SKRIVAS UT PÅ FILEN W91087.                                      
002100*                                                                         
002200*    ABENDKODER:                                                          
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
003300     SKIP2                                                                
003400*          --- BENÄMNINGAR PER ARTIKEL                                    
003500     SELECT W91087                     ASSIGN TO W91086D1.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000     SKIP2                                                                
004100 FILE SECTION.                                                            
004200     SKIP3                                                                
004300 FD  W91087                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  POST -COPY W91087 -PRE  UTFIL-  -L.                                  
004800                                                                          
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700                                                                          
005800*    -- CHECKED BY WY2000                                                 
005900 77  IDPGM                       PIC X(8)    VALUE 'W9108600'.            
006000 77  JA                          PIC X       VALUE 'J'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200 77  X20                         PIC X       VALUE X'20'.                 
006300                                                                          
006400 77  WS-IDSKYLT                  PIC X(3).                                
006500*    --- ALLA SPRÅK ENLIGT ISO-LATIN1                                     
006600     88  GODK-IDSKYLT-latin                  VALUE 'D  '                  
006700                                                   'E  '                  
006800                                                   'F  '                  
006900                                                   'GB '                  
007000                                                   'I  '                  
007100                                                   'NL '                  
007200                                                   'P  '                  
007300                                                   'S  '                  
007400                                                   'SF '                  
007500                                                   'USA'.                 
007510     88  GODK-IDSKYLT-OVR                    VALUE 'J  '                  
007520                                                   'KOR'                  
007530                                                   'RUS'                  
007540                                                   'T  '                  
007550                                                   'RC '                  
007560                                                   'RCN'                  
007570                                                   'TR '.                 
007600                                                                          
007700 01  ARBETSAREOR.                                                         
007800     03  IX                      PIC S9(9)   VALUE ZERO COMP-3.           
007900     03  IX2                     PIC S9(9)   VALUE ZERO COMP-3.           
008000                                                                          
008100 01  FILLER                      PIC X(8)    VALUE 'SPARAREA'.            
008200 01  WS-SPAR-IDBENNR             PIC 9(7)    VALUE ZERO.                  
008300 01  WS-SPAR-GB-BEART            PIC X(25)   VALUE SPACE.                 
008400 01  BEARTEXT-UTF8               PIC X(100)  VALUE SPACE.                 
008500 77  START-POS                   PIC S9(3)   VALUE ZERO.                  
008600 77  ANT-SPACE                   PIC S9(3)   VALUE ZERO.                  
008610 77  W-FELTYP                    PIC X(15)   VALUE SPACE.                 
008700                                                                          
008800                                                                          
008900     EJECT                                                                
009000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009100 01  FILLER REDEFINES DAGENS-DATUM.                                       
009200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009500     EJECT                                                                
009600 01  DYNAMISKA-SUBPROGRAM.                                                
009700*                                                                         
009800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010110*  Konverterar från UTF-8 på basen till Char Ent.                         
010120*                                  (använder subpgm WCNVUTFU)             
010130     03  WCNVUNCE                PIC X(8)    VALUE 'WCNVUNCE'.            
010400     SKIP2                                                                
010500*    --- PARAMETRAR TILL ABEND                                            
010600                                                                          
010700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010900     SKIP2                                                                
011000 01  FELTEXT.                                                             
011100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011300     EJECT                                                                
011400*    --- PARAMETRAR TILL POSTSUM                                          
011500*                                                                         
011600*01  -COPY W0005   -PRE  POSTSUM-                                         
011700     EJECT                                                                
011800 01  W91087-AREA-START           PIC X(24)   VALUE                        
011900                                 'W91087-AREA-START  '.                   
012000     SKIP2                                                                
012100                                                                          
012200*01  AREA -COPY W91087     -PRE UT-                                       
012300     EJECT                                                                
013000     SKIP2                                                                
013100                                                                          
013200*01   -COPY WCNVAREA       -PRE CNV-                                      
013300     EJECT                                                                
013400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013500*                                                                         
013600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013700     SKIP3                                                                
013800 01  NYCKLAR-TILL-DLI.                                                    
013900     03  W-IDSKYLT-X.                                                     
014000         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
014100     03  W-IDARTNR-X.                                                     
014200         05  W-IDARTNR           PIC X(9)    VALUE SPACE.                 
014300     SKIP2                                                                
014400*    --- STATUS-KOD FRÅN IMS                                              
014500 01  STATUS-WS                   PIC XX.                                  
014600     88  SEGMENT-FINNS                       VALUE '  '.                  
014700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014800     SKIP2                                                                
014900 01  GODK-STATUSKODER.                                                    
015000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015100     SKIP3                                                                
015200 01  SSA1                        PIC X(64).                               
015300 01  SSA2                        PIC X(64).                               
015400     EJECT                                                                
015500*    --- IMS FUNKTIONSKODER                                               
015600*01  -COPY W0003                                                          
015700     EJECT                                                                
015800*    ---  DLI INPUT-OUTPUT AREA                                           
015900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016000     SKIP3                                                                
016100 01  DLI-IO-AREA.                                                         
016200     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
016300     SKIP3                                                                
016400     03  WLBENA01 REDEFINES IO-AREA.                                      
016500*        05  -COPY WDD301  -PRE WDD301-                                   
016600     SKIP3                                                                
016700     03  WLBENA11 REDEFINES IO-AREA.                                      
016800*        05  -COPY WDD311  -PRE WDD311-                                   
016900     SKIP3                                                                
017000     03  WLBENA12 REDEFINES IO-AREA.                                      
017100*        05  -COPY WDD312  -PRE WDD312-                                   
017200     EJECT                                                                
017300 LINKAGE SECTION.                                                         
017400*                                                                         
017500*01  -COPY W0008  -PRE WDD3-                                              
017600     05  FILLER                  PIC X.                                   
017700     EJECT                                                                
017800 PROCEDURE DIVISION  USING WDD3-PCB.                                      
017900     ENTRY 'DLITCBL' USING WDD3-PCB.                                      
018000                                                                          
018100     PERFORM A-INIT                                                       
018200     PERFORM IMS-GET-WDD3                                                 
018300     PERFORM UNTIL SEGMENT-SLUT                                           
018400        EVALUATE WDD3-SEG-NAME-FB                                         
018500           WHEN 'WDD301  '                                                
018600              PERFORM B-NOLLSTALL                                         
018700           WHEN 'WDD311  '                                                
018800              PERFORM C-FLYTTA-WDD311                                     
018900           WHEN 'WDD312  '                                                
019000              PERFORM D-FLYTTA-WDD312                                     
019100              PERFORM S11-SKRIV-W91087                                    
019300         END-EVALUATE                                                     
019400         PERFORM IMS-GET-WDD3                                             
019500     END-PERFORM                                                          
019600     PERFORM Z-FINIT                                                      
019700     MOVE ZERO TO RETURN-CODE                                             
019800     GOBACK                                                               
019900     .                                                                    
020000     EJECT                                                                
020100                                                                          
020200                                                                          
020300 A-INIT SECTION.                                                          
020400                                                                          
020500     OPEN OUTPUT W91087                                                   
020700                                                                          
020800     ACCEPT DAGENS-DATUM  FROM DATE                                       
020900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021000     .                                                                    
021100     EJECT                                                                
021200                                                                          
021300                                                                          
021400 B-NOLLSTALL SECTION.                                                     
021500                                                                          
021600     MOVE ZERO               TO IX                                        
021700     MOVE +1                 TO IX2                                       
021710     MOVE ZERO               TO UT-IDARTNR                                
021800     PERFORM UNTIL IX2 > 17                                               
022000        MOVE SPACE           TO UT-BEARTEXT (IX2)                         
022200                                UT-IDSKYLT  (IX2)                         
022400        ADD +1 TO IX2                                                     
022500     END-PERFORM                                                          
022600     MOVE SPACE              TO WS-SPAR-GB-BEART                          
022700     MOVE ZERO               TO IX2                                       
022800                                                                          
022900     MOVE WDD301-BEN-IDBENNR TO WS-SPAR-IDBENNR                           
023000     .                                                                    
023100     EJECT                                                                
023200                                                                          
023300                                                                          
023400 C-FLYTTA-WDD311 SECTION.                                                 
023500                                                                          
023600     MOVE WDD311-TEXT-IDSKYLT         TO WS-IDSKYLT                       
023700     IF GODK-IDSKYLT-latin                                                
023800        PERFORM  CA-IDSKYLT-IX-LATIN                                      
023900        MOVE WDD311-TEXT-IDSKYLT      TO UT-IDSKYLT(IX)                   
024000        MOVE WDD311-TEXT-BEARTEXT     TO UT-BEARTEXT(IX)                  
024100        IF WDD311-TEXT-IDSKYLT = 'GB '                                    
024200           MOVE WDD311-TEXT-BEART TO WS-SPAR-GB-BEART                     
024300        END-IF                                                            
024400     END-IF                                                               
024500                                                                          
024510     IF GODK-IDSKYLT-OVR                                                  
024520        PERFORM  CB-IDSKYLT-IX-OVR                                        
024530        MOVE WDD311-TEXT-IDSKYLT      TO UT-IDSKYLT(IX)                   
024560        MOVE WDD311-TEXT-BEARTEXT     TO CNV-TECONV-FROM                  
024570                                                                          
024571*      --- XML Unicode Character Entities ska produceras                  
024572        PERFORM S31-KONVERTERA-TEXT                                       
024573                                                                          
024574        MOVE CNV-TECONV-TO            TO UT-BEARTEXT(IX)                  
024580     END-IF                                                               
031510     .                                                                    
031600     EJECT                                                                
031700 CA-IDSKYLT-IX-LATIN SECTION.                                             
031800                                                                          
031801     IF WS-IDSKYLT = 'GB '                                                
031802        MOVE +1   TO IX                                                   
031803     END-IF                                                               
031804     IF WS-IDSKYLT = 'USA'                                                
031805        MOVE +2   TO IX                                                   
031806     END-IF                                                               
031807     IF WS-IDSKYLT = 'S  '                                                
031808        MOVE +3   TO IX                                                   
031809     END-IF                                                               
031810     IF WS-IDSKYLT = 'D  '                                                
031811        MOVE +4   TO IX                                                   
031812     END-IF                                                               
031813     IF WS-IDSKYLT = 'NL '                                                
031814        MOVE +5   TO IX                                                   
031815     END-IF                                                               
031816     IF WS-IDSKYLT = 'F  '                                                
031817        MOVE +6   TO IX                                                   
031818     END-IF                                                               
031819     IF WS-IDSKYLT = 'I  '                                                
031820        MOVE +7   TO IX                                                   
031821     END-IF                                                               
031822     IF WS-IDSKYLT = 'E  '                                                
031823        MOVE +8   TO IX                                                   
031824     END-IF                                                               
031825     IF WS-IDSKYLT = 'P  '                                                
031826        MOVE +9   TO IX                                                   
031827     END-IF                                                               
031828     IF WS-IDSKYLT = 'SF '                                                
031829        MOVE +10  TO IX                                                   
031830     END-IF                                                               
031831     .                                                                    
031832     EJECT                                                                
031833 CB-IDSKYLT-IX-OVR   SECTION.                                             
031834                                                                          
031835     IF WS-IDSKYLT = 'J  '                                                
031836        MOVE +11  TO IX                                                   
031837     END-IF                                                               
031838     IF WS-IDSKYLT = 'KOR'                                                
031839        MOVE +12  TO IX                                                   
031840     END-IF                                                               
031841     IF WS-IDSKYLT = 'RUS'                                                
031842        MOVE +13  TO IX                                                   
031843     END-IF                                                               
031844     IF WS-IDSKYLT = 'T  '                                                
031845        MOVE +14  TO IX                                                   
031846     END-IF                                                               
031847     IF WS-IDSKYLT = 'RC '                                                
031848        MOVE +15  TO IX                                                   
031849     END-IF                                                               
031850     IF WS-IDSKYLT = 'RCN'                                                
031851        MOVE +16  TO IX                                                   
031852     END-IF                                                               
031853     IF WS-IDSKYLT = 'TR '                                                
031854        MOVE +17  TO IX                                                   
031855     END-IF                                                               
031856     .                                                                    
031857     EJECT                                                                
031858 D-FLYTTA-WDD312 SECTION.                                                 
031860                                                                          
031900     MOVE WDD312-ART-IDARTNR          TO UT-IDARTNR                       
032100     .                                                                    
032200     EJECT                                                                
032300                                                                          
032400                                                                          
032500 Z-FINIT SECTION.                                                         
032600     CLOSE W91087                                                         
032800     SKIP2                                                                
032900     MOVE 'S' TO POSTSUM-OPKOD                                            
033000     CALL POSTSUM USING POSTSUM-PARM                                      
033100     .                                                                    
033200     EJECT                                                                
033300                                                                          
033400                                                                          
033500 S11-SKRIV-W91087 SECTION.                                                
033600                                                                          
033700     WRITE UTFIL-POST FROM UT-AREA                                        
033800                                                                          
033900     MOVE 'W91087'   TO POSTSUM-FDNAMN                                    
034000     MOVE 'W91086D1' TO POSTSUM-DDNAMN2                                   
034100     CALL POSTSUM USING POSTSUM-PARM                                      
034200     .                                                                    
034300     EJECT                                                                
034310 S31-KONVERTERA-TEXT SECTION.                                             
034320                                                                          
034330     IF CNV-TECONV-FROM = SPACE                                           
034340       MOVE SPACE    TO CNV-TECONV-TO                                     
034350     ELSE                                                                 
034360*      -- Ange att XML Unicode Character Entities ska produceras          
034370       MOVE  JA      TO CNV-FLTXTENT                                      
034380*      -- Ange maxlängden för konverteringen.                             
034390       MOVE 100      TO CNV-KVMAXTL                                       
034391                                                                          
034393*      -- KONVERTERING GÄLLER FRÅN UTF8 FÖR DESSA SPRÅK                   
034394       MOVE JA       TO CNV-FLUTF8                                        
034395                                                                          
034396       CALL WCNVUNCE USING CNV-WCNVAREA                                   
034427                                                                          
034428       IF CNV-KDSVAR NOT = SPACE                                          
034429         PERFORM S32-DISPLAY-KONV-FEL                                     
034430       END-IF                                                             
034431     END-IF                                                               
034432     .                                                                    
034433     EJECT                                                                
034434 S32-DISPLAY-KONV-FEL SECTION.                                            
034435     SKIP2                                                                
034436     IF CNV-KDSVAR = 'F'                                                  
034437       MOVE 'OTILLÅTET TKN:'  TO W-FELTYP                                 
034438     END-IF                                                               
034439     IF CNV-KDSVAR = 'T'                                                  
034440       MOVE 'DATA TRUNKERAT:' TO W-FELTYP                                 
034441     END-IF                                                               
034442*    DISPLAY WS-SPAR-IDBENNR     ' '                                      
034443*            WDD311-TEXT-IDSKYLT ' '                                      
034444*            W-FELTYP ' ' CNV-BEFEL                                       
034445     .                                                                    
034446     EJECT                                                                
034450                                                                          
034500                                                                          
035500* --- IMS SEKTIONER ---                                                   
035800                                                                          
035900                                                                          
036000 IMS-GET-WDD3   SECTION.                                                  
036100                                                                          
036200     CALL CBLTDLI USING GN WDD3-PCB DLI-IO-AREA                           
036300     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
036400     MOVE '  GAGKGB'       TO GODK-STATUSKODER                            
036500     PERFORM IMS-STATUSKONTROLL                                           
036600     .                                                                    
036700     EJECT                                                                
036800                                                                          
036900                                                                          
037000 IMS-STATUSKONTROLL SECTION.                                              
037100                                                                          
037200     SET STATUS-IX TO 1                                                   
037300     SEARCH GODK-STATUS                                                   
037400       AT END                                                             
037500         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
037600         DISPLAY FELTEXT                                                  
037700         CALL FELLOG                                                      
037800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
037900         CONTINUE                                                         
038000     END-SEARCH                                                           
038100     .                                                                    
