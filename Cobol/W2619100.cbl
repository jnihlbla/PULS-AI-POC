000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2619100.                                                
000300 AUTHOR.         INGER STENING.                                           
000400 DATE-WRITTEN.   21/02/21.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄSER NEDLÄST ARTIKELREGISTER                                    
001000*        OCH PLOCKAR UT ARTIKLAR MED                                      
001010*        TIURPROD > ANTAL I ÅR I TABELL WDG2/H-TYP 1143/WDGX1144          
001200*                                                                         
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002900*          --- ARTIKELINFORMATION                                         
003000     SELECT W26190                     ASSIGN TO W26191D1.                
003100     SKIP2                                                                
003200*          --- ARTIKLAR TIURPROD                                          
003300     SELECT W26191                     ASSIGN TO W26191D2.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
004500 FD  W26190                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900*01  -COPY W26190      -L.                                                
005000     SKIP3                                                                
005100 FD  W26191                                                               
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400                                                                          
005500*01  POST -COPY W26190 -PRE  UT-  -L.                                     
005600     EJECT                                                                
005700 WORKING-STORAGE SECTION.                                                 
005800                                                                          
005900*    -COPY WY2000W3                                                       
006000     SKIP3                                                                
006100 77  IDPGM                       PIC X(8)    VALUE 'W2619100'.            
006110 77  IMS-SEKTION                 PIC X(40).                               
006200 77  JA                          PIC X       VALUE 'J'.                   
006300 77  NEJ                         PIC X       VALUE 'N'.                   
006310 77  W-KDERS                     PIC S9(3)   VALUE +0 COMP-3.             
006400                                                                          
006800 77  W26190-EOF-SW               PIC X       VALUE 'N'.                   
006900     88  END-OF-W26190                       VALUE 'J'.                   
007000                                                                          
007100 77  KDPRODSL-TYP                PIC S9(3) COMP-3.                        
007200     88  PS-GRP-1                VALUE 11 14 21.                          
007300     88  PS-TILLB                VALUE 15 16 17 25 26.                    
007400     EJECT                                                                
007500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007600 01  FILLER REDEFINES DAGENS-DATUM.                                       
007700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008000                                                                          
008100 01  W-TIAAAAVV                  PIC 9(6).                                
008200 01  FILLER  REDEFINES W-TIAAAAVV.                                        
008300     03  W-TIAAAA                PIC 9(4).                                
008400     03  W-TIVV                  PIC 9(2).                                
008500 01  FILLER  REDEFINES W-TIAAAAVV.                                        
008600     03  W-TISEKEL               PIC 9(2).                                
008700     03  W-TIAAVV                PIC 9(4).                                
010100                                                                          
010110 01  W-TIAAAAVV-AARLF            PIC 9(6).                                
010120 01  FILLER  REDEFINES W-TIAAAAVV-AARLF.                                  
010130     03  W-TISEKEL-AARLF         PIC 9(2).                                
010140     03  W-TIAAVV-AARLF          PIC 9(4).                                
010150                                                                          
010200 01  ARBETSAREOR.                                                         
010610     03  W-KVAARLF               PIC 9(02)   VALUE ZERO.                  
010700                                                                          
010710 77  MAX-TAB-IX                  PIC S9(4) COMP SYNC VALUE +5000.         
010720 77  TAB-IX                      PIC S9(4) COMP SYNC.                     
010730 01  WDGX1144-TAB.                                                        
010740     03  TAB-WDGX1144 OCCURS 5000.                                        
010750         05 TAB-IDFKNGRP-FOM     PIC 9(04).                               
010760         05 TAB-IDFKNGRP-TOM     PIC 9(04).                               
010770         05 TAB-KVAARLF          PIC 9(02).                               
010780                                                                          
010790     EJECT                                                                
010800 01  DYNAMISKA-SUBPROGRAM.                                                
010900*                                                                         
011000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011210     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011220     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011300     SKIP2                                                                
011400*    --- PARAMETRAR TILL ABEND                                            
011500                                                                          
011600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011900     EJECT                                                                
012000*- - - - - - - - - - - - - -  PARAMETRAR TILL WDATKONV                    
012100                                                                          
012200*01  -COPY WDATAREA                                                       
012300     SKIP2                                                                
012400 01  FELTEXT.                                                             
012500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012700                                                                          
013200     EJECT                                                                
013300*    --- PARAMETRAR TILL POSTSUM                                          
013400*                                                                         
013500*01  -COPY W0005   -PRE  POSTSUM-                                         
013600     EJECT                                                                
013700 01  REG-AREA-START              PIC X(24)   VALUE                        
013800                                 'REG-AREA-START  '.                      
013900     SKIP2                                                                
014000                                                                          
014300 01  IN-AREA-START               PIC X(24)   VALUE                        
014400                                 'IN-AREA-START  '.                       
014500     SKIP2                                                                
014600                                                                          
014700*01  AREA -COPY W26190     -PRE IN-                                       
014800     EJECT                                                                
014900 01  UT-AREA-START               PIC X(24)   VALUE                        
015000                                 'UT-AREA-START  '.                       
015100     SKIP2                                                                
015200                                                                          
015300*01  AREA -COPY W26190     -PRE UT-                                       
015400     EJECT                                                                
015401 01  ERROR-TEXT.                                                          
015402     03  FILLER                  PIC X(08)   VALUE 'ERR-TEXT'.            
015403     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
015404     EJECT                                                                
015405                                                                          
015410*    --- AREAS FOR IMS-SECTIONS                                           
015420*                                                                         
015430     EJECT                                                                
015440 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015450     SKIP3                                                                
015460 01  KEYS-FOR-DLI.                                                        
015470     03  W-WDGXKEY-1143-X.                                                
015480         05  W-IDHTYP-1143       PIC X(4)    VALUE '1143'.                
015490         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
015491                                                                          
015492*    --- STATUS-KOD FRÅN IMS                                              
015493 01  STATUS-WS                   PIC XX.                                  
015494     88  SEGMENT-FOUND                       VALUE '  '.                  
015495     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
015496     88  SEGMENT-MISSING                     VALUE 'GE'.                  
015497     SKIP2                                                                
015498 01  GOOD-STATUSCODES.                                                    
015499     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015500     SKIP3                                                                
015501 01  SSA1                        PIC X(64).                               
015502 01  SSA2                        PIC X(64).                               
015503     EJECT                                                                
015504*    --- IMS FUNCTION CODES                                               
015505*01  -COPY W0003                                                          
015506     EJECT                                                                
015507*    ---  DLI INPUT-OUTPUT AREA                                           
015508 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
015509 01  DLI-IO-WDGX01.                                                       
015510*    03  -COPY WDGX01                                                     
015511                                                                          
015512 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX1144'.                    
015513 01  DLI-IO-WDGX1144.                                                     
015514*    03  -COPY WDGX1144                                                   
015515                                                                          
015516 LINKAGE SECTION.                                                         
015517                                                                          
015518*01  -COPY W0008   -PRE WDG2-                                             
015519     05  FILLER                  PIC X.                                   
015520     EJECT                                                                
015521 PROCEDURE DIVISION USING  WDG2-PCB.                                      
015522                                                                          
015530     ENTRY 'DLITCBL' USING WDG2-PCB.                                      
015600 MAIN SECTION.                                                            
015700     SKIP2                                                                
015800                                                                          
015900     PERFORM A-INIT                                                       
016000                                                                          
016100     PERFORM B-WDGX1144-TAB                                               
017200                                                                          
017300     PERFORM S01-LAES-W26190                                              
017400     PERFORM UNTIL END-OF-W26190                                          
017500                                                                          
017600       IF IN-TIURPROD > ZERO                                              
017700          PERFORM C-SKAPA-EV-UTPOST                                       
017800       END-IF                                                             
017900                                                                          
018000       PERFORM S01-LAES-W26190                                            
018100     END-PERFORM                                                          
018200                                                                          
018300                                                                          
018400     PERFORM Z-FINIT                                                      
018500                                                                          
018600     MOVE ZERO TO RETURN-CODE                                             
018700     GOBACK                                                               
018800     .                                                                    
018900     EJECT                                                                
019000 A-INIT SECTION.                                                          
019100                                                                          
019300     OPEN INPUT  W26190                                                   
019400                                                                          
019500     OPEN OUTPUT W26191                                                   
019600     SKIP2                                                                
019700     ACCEPT DAGENS-DATUM  FROM DATE                                       
019800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020000                                                                          
020100     MOVE 'IDAG' TO DAT-KDDATFORM                                         
020200     CALL WDATKONV USING DAT-KDDATFORM,                                   
020300                         DAT-I-TIDATUM,                                   
020400                         DAT-O-TIDATUM,                                   
020500                         DAT-KDSVAR                                       
020600                                                                          
020700     IF DAT-KDSVAR-FEL                                                    
020800        DISPLAY '****  FEL I WDATKONV  *******'                           
020900        PERFORM S99-ABEND                                                 
021000     END-IF                                                               
021100                                                                          
021200     MOVE DAT-TIAAVV-GRP TO W-TIAAVV                                      
021300     MOVE DAT-TISEKEL    TO W-TISEKEL                                     
022200     .                                                                    
022300     EJECT                                                                
022310 B-WDGX1144-TAB SECTION.                                                  
022320                                                                          
022330     MOVE +0                        TO TAB-IX                             
022340     PERFORM IMS-GU-WDGX1143                                              
022350                                                                          
022360     PERFORM IMS-GNP-WDGX1144                                             
022370     PERFORM UNTIL SEGMENT-MISSING                                        
022380        ADD +1                      TO TAB-IX                             
022390        MOVE 1144-IDFKNGRP-FOM      TO TAB-IDFKNGRP-FOM (TAB-IX)          
022391        MOVE 1144-IDFKNGRP-TOM      TO TAB-IDFKNGRP-TOM (TAB-IX)          
022392        MOVE 1144-KVAARLF           TO TAB-KVAARLF      (TAB-IX)          
022393                                                                          
022394        PERFORM IMS-GNP-WDGX1144                                          
022395                                                                          
022396        IF TAB-IX = MAX-TAB-IX                                            
022397           DISPLAY 'TABELL FÖR DB WDGX1144 > 5000 RECORDS.'               
022398           DISPLAY 'ÖKA ANTAL I TAB-WDGX1144 I WORKING STORAGE.'          
022399           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
022400        END-IF                                                            
022401                                                                          
022402     END-PERFORM                                                          
022403     MOVE TAB-IX                    TO MAX-TAB-IX                         
022404     .                                                                    
022405     EJECT                                                                
022410 C-SKAPA-EV-UTPOST SECTION.                                               
022500                                                                          
022504     MOVE IN-KDPRODSL               TO KDPRODSL-TYP                       
022505     IF PS-GRP-1 OR PS-TILLB                                              
022506        PERFORM CA-SOEK-WDGX1144-IDFKNGRP                                 
022507                                                                          
022508        MOVE IN-TIURPROD            TO TMP1-YYWW                          
022509                                                                          
022510        COMPUTE W-TIAAAAVV-AARLF = W-TIAAAAVV - (W-KVAARLF * 100)         
022513        MOVE W-TIAAVV-AARLF         TO TMP2-YYWW                          
022515                                                                          
022518        PERFORM WY2000P3                                                  
022521                                                                          
022522        IF TMP1-YYWW < TMP2-YYWW                                          
022524           PERFORM CB-SKAPA-UTFIL                                         
022525        END-IF                                                            
022526     END-IF                                                               
026400     .                                                                    
026500     EJECT                                                                
026510 CA-SOEK-WDGX1144-IDFKNGRP SECTION.                                       
026520                                                                          
026521     MOVE 15                        TO W-KVAARLF                          
026522                                                                          
026540     MOVE +1                        TO TAB-IX                             
026550     PERFORM UNTIL TAB-IX > MAX-TAB-IX                                    
026570       IF IN-IDFKNGRP >= TAB-IDFKNGRP-FOM (TAB-IX)                        
026571      AND IN-IDFKNGRP <= TAB-IDFKNGRP-TOM (TAB-IX)                        
026572          MOVE TAB-KVAARLF(TAB-IX)  TO W-KVAARLF                          
026580          MOVE 5000                 TO TAB-IX                             
026590       END-IF                                                             
026591       ADD +1                       TO TAB-IX                             
026592     END-PERFORM                                                          
026599     .                                                                    
026600     EJECT                                                                
026610 CB-SKAPA-UTFIL    SECTION.                                               
026700                                                                          
026800     IF IN-KDERS = ZERO                                                   
026900        MOVE IN-KDERS-UTG TO W-KDERS                                      
027000     END-IF                                                               
027100                                                                          
028600     IF W-KDERS = ZERO OR 09                                              
028610        MOVE IN-AREA          TO UT-AREA                                  
028700        PERFORM S11-SKRIV-W26191                                          
028800     END-IF                                                               
028900     .                                                                    
029000     EJECT                                                                
031000 Z-FINIT SECTION.                                                         
031200     CLOSE W26190                                                         
031300           W26191                                                         
031400     SKIP2                                                                
031500     MOVE 'S' TO POSTSUM-OPKOD                                            
031600     CALL POSTSUM USING POSTSUM-PARM                                      
031700     .                                                                    
031800     EJECT                                                                
033200 S01-LAES-W26190  SECTION.                                                
033300     READ W26190 INTO IN-AREA                                             
033400     AT END                                                               
033500        SET END-OF-W26190 TO TRUE                                         
033600                                                                          
033700     NOT AT END                                                           
033800        MOVE 'W26190'   TO POSTSUM-FDNAMN                                 
033900        MOVE 'W26191D2' TO POSTSUM-DDNAMN2                                
034000        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
034100        CALL POSTSUM USING POSTSUM-PARM                                   
034200     END-READ                                                             
034300     .                                                                    
034400     EJECT                                                                
034500 S11-SKRIV-W26191 SECTION.                                                
034600                                                                          
034700     WRITE UT-POST FROM UT-AREA                                           
034800                                                                          
034900     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
035000     MOVE 'W26191'   TO POSTSUM-FDNAMN                                    
035100     MOVE 'W26191D2' TO POSTSUM-DDNAMN2                                   
035200     CALL POSTSUM USING POSTSUM-PARM                                      
035300     .                                                                    
035400     EJECT                                                                
035500 S99-ABEND SECTION.                                                       
035600                                                                          
035700     SKIP2                                                                
035800     MOVE 'S' TO POSTSUM-OPKOD                                            
035900     CALL POSTSUM USING POSTSUM-PARM                                      
036000     CALL ABEND USING RKOD-ABEND                                          
036100     .                                                                    
036200     EJECT                                                                
036210 IMS-GU-WDGX1143 SECTION.                                                 
036220     MOVE 'IMS-GU-WDGX1143              ' TO IMS-SEKTION                  
036230                                                                          
036240     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-1143-X ')'                    
036250            DELIMITED BY SIZE INTO SSA1                                   
036260     MOVE '    '                TO GOOD-STATUSCODES                       
036270     CALL CBLTDLI USING GU WDG2-PCB DLI-IO-WDGX01 SSA1                    
036280     MOVE WDG2-STATUS-CODE      TO STATUS-WS                              
036290     PERFORM IMS-STATUSCHECK                                              
036291     .                                                                    
036292                                                                          
036293 IMS-GNP-WDGX1144  SECTION.                                               
036294     MOVE 'IMS-GNP-WDGX1144             ' TO IMS-SEKTION                  
036295                                                                          
036296     MOVE 'WDGX1144 '          TO SSA1                                    
036297     MOVE '  GE'               TO GOOD-STATUSCODES                        
036298     CALL CBLTDLI USING GNP WDG2-PCB DLI-IO-WDGX1144 SSA1                 
036299     MOVE WDG2-STATUS-CODE     TO STATUS-WS                               
036300     PERFORM IMS-STATUSCHECK                                              
036310     .                                                                    
036320                                                                          
036330 IMS-STATUSCHECK SECTION.                                                 
036340                                                                          
036350     SET STATUS-IX TO 1                                                   
036360     SEARCH GOOD-STATUS                                                   
036370       AT END                                                             
036380         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
036390           DELIMITED BY SIZE INTO ERROR-TEXT                              
036391         DISPLAY ERROR-TEXT                                               
036392         CALL FELLOG                                                      
036393       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
036394         CONTINUE                                                         
036395     END-SEARCH                                                           
036396     .                                                                    
036397     EJECT                                                                
036400*    -COPY WY2000P3                                                       
