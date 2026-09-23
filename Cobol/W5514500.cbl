000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5514500.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   94/05/09.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER NED VALUTAREGISTRET OCH SKAPAR EN UTFIL SOM                
001000*        INNEHÅLLER ÅR, KDVALISO, REVALUTA OCH PRKURS.                    
001100*        VALUTA FÖR AKTUELLT ÅR OCH KOMMANDE ÅR FINNS                     
001200*                                                                         
001300*        PROGRAMMET LÄSER      WDG2 (WDGX9305)                            
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- VALUTAREGISTRET                                            
002800     SELECT W55145                     ASSIGN TO W55145D1.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W55145                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700     SKIP2                                                                
003800*01  POST -COPY W55145 -PRE  UT-  -L.                                     
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200*    -- CHECKED BY WY2000                                                 
004300 77  IDPGM                       PIC X(8)    VALUE 'W5514500'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600                                                                          
004700 01  WS-AKT-AAR                  PIC S9(3)   VALUE ZERO COMP-3.           
004800 01  WS-KOM-AAR                  PIC S9(3)   VALUE ZERO COMP-3.           
004900                                                                          
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100     EJECT                                                                
005200 01  DYNAMISKA-SUBPROGRAM.                                                
005300*                                                                         
005400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005800     SKIP2                                                                
005900*    --- PARAMETRAR TILL ABEND                                            
006000                                                                          
006100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006300     SKIP2                                                                
006400 01  FELTEXT.                                                             
006500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006700     EJECT                                                                
006800*    --- PARAMETRAR TILL POSTSUM                                          
006900*                                                                         
007000*01  -COPY W0005   -PRE  POSTSUM-                                         
007100     EJECT                                                                
007200 01  UT-AREA-START               PIC X(24)   VALUE                        
007300                                 'UT-AREA-START  '.                       
007400     SKIP2                                                                
007500                                                                          
007600*01  AREA -COPY W55145     -PRE UT-                                       
007700     EJECT                                                                
007800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007900*                                                                         
008000     SKIP2                                                                
008100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008200     SKIP3                                                                
008300 01  NYCKLAR-TILL-DLI.                                                    
008400     03  W-WDGX9305-X.                                                    
008500         05  W-IDHTYP            PIC X(4)    VALUE '9305'.                
008600         05  W-KDVALISO-HUV      PIC X(3)    VALUE 'SEK'.                 
008700         05  W-KDVALTYP          PIC X(1)    VALUE 'A'.                   
008800         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
008900     03  W-KDVALISO-MIN-X.                                                
009000         05  W-KDVALISO-MIN      PIC X(3)    VALUE LOW-VALUES.            
009010     03  W-KDVALISO-MAX-X.                                                
009020         05  W-KDVALISO-MAX      PIC X(3)    VALUE HIGH-VALUES.           
009100     03  W-TISTADA9-X.                                                    
009200         05  W-TISTADAT-9KOMPL   PIC S9(7)   VALUE ZERO COMP-3.           
009300                                                                          
009400     SKIP2                                                                
009500*    --- STATUS-KOD FRÅN IMS                                              
009600 01  STATUS-WS                   PIC XX.                                  
009700     88  SEGMENT-FINNS                       VALUE '  '.                  
009800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009910     88  SEGMENT-HIGHER                      VALUE 'GA'.                  
009920     88  END-OF-DB                           VALUE 'GB'.                  
010000     SKIP2                                                                
010100 01  GODK-STATUSKODER.                                                    
010200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010300     SKIP3                                                                
010400 01  SSA1                        PIC X(64).                               
010410 01  SSA2                        PIC X(64).                               
010500     EJECT                                                                
010600*    --- IMS FUNKTIONSKODER                                               
010700*01  -COPY W0003                                                          
010800     SKIP2                                                                
010900*    ---  DLI INPUT-OUTPUT AREA                                           
011000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011100     SKIP3                                                                
012300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9305'.                    
012310 01  DLI-IO-WDGX9305.                                                     
012320*    03  -COPY WDGX9305                                                   
012330     EJECT                                                                
012340 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9306'.                    
012350 01  DLI-IO-WDGX9306.                                                     
012360*    03  -COPY WDGX9306                                                   
012392 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9308'.                    
012393 01  DLI-IO-WDGX9308.                                                     
012395*    03  -COPY WDGX9308                                                   
012400 LINKAGE SECTION.                                                         
012500                                                                          
012600     SKIP2                                                                
012700*01  -COPY W0008  -PRE WDG2-                                              
012800     05  FILLER                  PIC X.                                   
012900     EJECT                                                                
013000 PROCEDURE DIVISION  USING WDG2-PCB.                                      
013100     ENTRY 'DLITCBL' USING WDG2-PCB.                                      
013200                                                                          
013300     PERFORM A-INIT                                                       
013400                                                                          
013500**** LÄSER MED AKTUELLT ÅR                                                
013600*                                                                         
013700*    MOVE WS-AKT-AAR TO W-TIAA                                            
013800*    PERFORM IMS-GU-XXIA-XXIA01                                           
013900*    IF SEGMENT-FINNS                                                     
014000*      PERFORM B-LAES-VALUTAREG                                           
014100*    END-IF                                                               
014200*                                                                         
014300**** LÄSER MED KOMMANDE ÅR                                                
014400                                                                          
014600     PERFORM IMS-GN-WDGX9306                                              
014700     IF SEGMENT-FINNS                                                     
014800       PERFORM B-LAES-VALUTAREG                                           
014900     END-IF                                                               
015000                                                                          
015100     PERFORM Z-FINIT                                                      
015200                                                                          
015300     MOVE ZERO TO RETURN-CODE                                             
015400     GOBACK                                                               
015500     .                                                                    
015600     EJECT                                                                
015700 A-INIT SECTION.                                                          
015800                                                                          
015900     OPEN OUTPUT W55145                                                   
016000                                                                          
016100     ACCEPT DAGENS-DATUM  FROM DATE                                       
016200                                                                          
016300***** AKTUELLT AAR = INNEVARANDE ÅR                                       
016400                                                                          
016500     MOVE DAGENS-DATUM(1:2) TO WS-AKT-AAR                                 
016600                                                                          
016700***** KOMMANDE AAR = NÄSTA ÅR                                             
016800                                                                          
016900     IF WS-AKT-AAR = 99                                                   
017000       MOVE ZERO           TO WS-KOM-AAR                                  
017100     ELSE                                                                 
017200       COMPUTE WS-KOM-AAR = WS-AKT-AAR + 1                                
017300     END-IF                                                               
017400                                                                          
017500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017600     .                                                                    
017700     EJECT                                                                
017800 B-LAES-VALUTAREG SECTION.                                                
017900     SKIP2                                                                
018200     PERFORM UNTIL SEGMENT-SAKNAS                                         
018203       MOVE 9306-KDVALISO         TO UT-KDVALISO                          
018210       PERFORM IMS-GNP-WDGX9308                                           
018220       IF SEGMENT-FINNS                                                   
018300         MOVE WS-KOM-AAR         TO UT-TIAA                               
018500         MOVE 9308-PRKURS        TO UT-PRKURS                             
018600         MOVE 9308-REVALUTA-TO   TO UT-REVALUTA                           
018700         PERFORM S11-SKRIV-W55145                                         
018800       END-IF                                                             
018810       PERFORM IMS-GN-WDGX9306                                            
018900     END-PERFORM                                                          
019100     .                                                                    
019200     EJECT                                                                
019300 Z-FINIT SECTION.                                                         
019400     CLOSE W55145                                                         
019500     SKIP2                                                                
019600     MOVE 'S' TO POSTSUM-OPKOD                                            
019700     CALL POSTSUM USING POSTSUM-PARM                                      
019800     .                                                                    
019900     EJECT                                                                
020000 S11-SKRIV-W55145 SECTION.                                                
020100     SKIP2                                                                
020200     WRITE UT-POST FROM UT-AREA                                           
020300                                                                          
020400     MOVE 'VAL'     TO POSTSUM-TRANSTYP                                   
020500     MOVE 'W55145' TO POSTSUM-FDNAMN                                      
020600     MOVE 'W55145D1' TO POSTSUM-DDNAMN2                                   
020700     CALL POSTSUM USING POSTSUM-PARM                                      
020800     .                                                                    
020900     EJECT                                                                
021000* --- IMS SEKTIONER ---                                                   
021100     SKIP3                                                                
022910 IMS-GN-WDGX9306 SECTION.                                                 
022920                                                                          
022930     STRING 'WDG201  (WDGXKEY  =' W-WDGX9305-X ')'                        
022940             DELIMITED BY SIZE INTO SSA1                                  
022950     MOVE   'WDGX9306'           TO SSA2                                  
022970     MOVE '  GE'                 TO GODK-STATUSKODER                      
022980     CALL CBLTDLI             USING GN                                    
022990                                    WDG2-PCB                              
022991                                    DLI-IO-WDGX9306                       
022992                                    SSA1 SSA2                             
022993     MOVE WDG2-STATUS-CODE       TO STATUS-WS                             
022994     PERFORM IMS-STATUSKONTROLL                                           
022995     .                                                                    
022996     SKIP3                                                                
023014 IMS-GNP-WDGX9308 SECTION.                                                
023015                                                                          
023020     MOVE   'WDGX9308'           TO SSA1                                  
023021     MOVE '  GAGBGE'             TO GODK-STATUSKODER                      
023022     CALL CBLTDLI             USING GNP                                   
023023                                    WDG2-PCB                              
023024                                    DLI-IO-WDGX9308                       
023025                                    SSA1                                  
023026     MOVE WDG2-STATUS-CODE       TO STATUS-WS                             
023027     PERFORM IMS-STATUSKONTROLL                                           
023028     .                                                                    
023029     SKIP3                                                                
023030 IMS-STATUSKONTROLL SECTION.                                              
023100     SKIP2                                                                
023200     SET STATUS-IX TO 1                                                   
023300     SEARCH GODK-STATUS                                                   
023400       AT END                                                             
023500         MOVE 'XXXXXXXXXXXXXX' TO FELTEXT-STR                             
023600         DISPLAY FELTEXT                                                  
023700         CALL FELLOG                                                      
023800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
023900         CONTINUE                                                         
024000     END-SEARCH                                                           
024100     .                                                                    
