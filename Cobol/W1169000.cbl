000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1169000.                                                
000300 AUTHOR.         HÅKAN BOHLIN.                                            
000400 DATE-WRITTEN.   JANUARI 2017.                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000900*      - HANDLES DC                                                       
001000*      - MATCHAR PRISFIL INNEHÅLLANDE NYA SJÄLVKOSTPRISER TILL            
001100*        MÅNADSKURS MED PRISFIL OCH KOMPLETTERAR                          
001200*        DENNA MED DE NYA SJÄLVKOSTNADSPRISERNA,                          
001300*        ANTINGEN PRISER FÖR LOKALA LEVERANTÖRER                          
001400*        ELLER HUDLEVERANTÖRSPRISER FÖR DC 11 (LEVNR = 11).               
001500*      - BYTER URSPRUNGSKOD FRÅN ALFA TILL NUMERISKT.                     
001600*      - BYTER IDLEVNR / IDLEVNR-LOC                                      
001700*                           FRÅN ALFA TILL NUMERISKT.                     
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003300*       --- INFIL FRÅN W11689                                             
003400     SELECT W11689                     ASSIGN TO W11690D1.                
003500     SKIP2                                                                
003600*       --- PRISFIL MED ALLA PULS PRISER SÅVÄL LOKALA SOM HUVUD           
003700     SELECT W33588                     ASSIGN TO W11690D2.                
003800     SKIP2                                                                
004200*       --- SAMMA SOM IN, MED ÄNDRAD SJÄLVKOST                            
004300     SELECT W11690                     ASSIGN TO W11690D3.                
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600     SKIP2                                                                
004700 FILE SECTION.                                                            
004800     SKIP3                                                                
005600 FD  W11689                                                               
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900                                                                          
006000*01  -COPY W11610A       -L.                                              
006100     SKIP3                                                                
006200 FD  W33588                                                               
006300     RECORDING       F                                                    
006400     BLOCK CONTAINS  0.                                                   
006500                                                                          
006600*01  -COPY W33589        -L.                                              
006700     SKIP3                                                                
007400 FD  W11690                                                               
007500     RECORDING       F                                                    
007600     BLOCK CONTAINS  0.                                                   
007700                                                                          
007800*01  POST -COPY W11616A  -PRE  UT-  -L.                                   
007900     EJECT                                                                
008000 WORKING-STORAGE SECTION.                                                 
008100                                                                          
008200 77  IDPGM                       PIC X(8)      VALUE 'W1169000'.          
008300 77  JA                          PIC X         VALUE 'J'.                 
008400 77  NEJ                         PIC X         VALUE 'N'.                 
008500                                                                          
008800 77  PIX                         PIC S9(3)  VALUE ZERO COMP-3.            
008900 77  MAX-PIX                     PIC S9(3)  VALUE +99  COMP-3.            
009000 77  WS-IDMARKBO                 PIC X      VALUE SPACE.                  
009200 77  WS-PRIS-IDDC                PIC X(2)   VALUE SPACE.                  
009300                                                                          
009400 01  WS-PRARTSJK                 PIC 9(7)V9(2) VALUE ZERO.                
009410 01  WS-PRARTSTD                 PIC 9(7)V9(2) VALUE ZERO.                
009500 77  WS-FLBEST-PRIS              PIC X         VALUE 'N'.                 
009600 77  WS-KDARTURS                 PIC 9(2)      VALUE ZERO.                
009700 77  WS-IDLEVNR                  PIC 9(5)      VALUE ZERO.                
009800 77  WS-IDLEVNR-LOC              PIC 9(5)      VALUE ZERO.                
010003 77  WS-SPAR-IDDC                PIC X(2)   VALUE SPACE.                  
010103 77  TRAFF-SW                    PIC X      VALUE 'N'.                    
010203                                                                          
010204*01    -COPY WWDCLAND                                                     
010205                                                                          
010303 77  W11689-EOF-SW               PIC X         VALUE 'N'.                 
010403     88  END-OF-W11689                         VALUE 'J'.                 
010503                                                                          
010603 77  W33588-EOF-SW               PIC X         VALUE 'N'.                 
010703     88  END-OF-W33588                         VALUE 'J'.                 
010803                                                                          
011203 01  DAGENS-DATUM                PIC 9(8)      VALUE ZERO.                
011303                                                                          
011403 01  WS-AAAAMMDD.                                                         
011503     03  WS-SEKEL                       PIC 9(2).                         
011603     03  WS-AAMMDD                      PIC 9(6).                         
011703 01  WS-TIFINLV REDEFINES WS-AAAAMMDD   PIC 9(8).                         
011803     EJECT                                                                
011903 01  DYNAMISKA-SUBPROGRAM.                                                
012003*                                                                         
012103     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012203     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012303     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012403     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
012503     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012703     SKIP2                                                                
012803                                                                          
013100*01  -COPY WWDC99                                                         
013200                                                                          
013300                                                                          
013400* INFO OM KÖRTYP(M5) FRÅN CONSTANTMEDLEM VALD AV JCL'EN                   
013500* INFON KOMMER SOM FIL D1                                                 
013600                                                                          
014600 01  FELTEXT.                                                             
014700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
014800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014900     EJECT                                                                
015000*    --- PARAMETRAR TILL SUBPROGRAM W400ARTU                              
015100*01 -COPY W400ARTU                                                        
015200     EJECT                                                                
015600 01  FILLER                      PIC X(16) VALUE 'WDATAREA'.              
015700*01 -COPY WDATAREA                                                        
015800     EJECT                                                                
015900*    --- PARAMETRAR TILL POSTSUM                                          
016000*                                                                         
016100*01  -COPY W0005   -PRE  POSTSUM-                                         
016200     EJECT                                                                
016300 01  IN-AREA-START               PIC X(24)   VALUE                        
016400                                 'IN-AREA-START  '.                       
016500     SKIP2                                                                
016600                                                                          
016700*01  AREA -COPY W11610A      -PRE IN-                                     
016800     EJECT                                                                
016900 01  FILLER                      PIC X(24)   VALUE 'PRIS-AREA'.           
017000                                                                          
017100*01  AREA -COPY W33589       -PRE PRIS-                                   
017200     EJECT                                                                
017700 01  FILLER                      PIC X(24)   VALUE 'UT-AREA'.             
017800                                                                          
017900 01  UT-AREA-START               PIC X(24)   VALUE                        
018000                                 'UT-AREA-START  '.                       
018100     SKIP2                                                                
018200                                                                          
018300*01  AREA -COPY W11616A      -PRE UT-                                     
018400     EJECT                                                                
018500*    --- ARBETS-AREOR TILL  IMS-SEKTIONERNA                               
018600*                                                                         
018700 01  NYCKLAR-TILL-DLI.                                                    
020300                                                                          
020400     03  W-WDB101KY-X.                                                    
020500         05  W-IDPARTNR            PIC X(9)  VALUE SPACE.                 
020600         05  W-IDFTG               PIC 9(2)  VALUE ZERO.                  
020700                                                                          
020800     03   W-WDB1B-LOW-X.                                                  
020900         05  W-B-IDLANDX2-LOW      PIC X(2)  VALUE SPACE.                 
021000                                                                          
021100     03  W-WDB1B-HIGH-X.                                                  
021200         05  W-B-IDLANDX2-HIGH     PIC X(2)  VALUE HIGH-VALUE.            
021300                                                                          
021400     03  W-WDB1B1KY-LOW.                                                  
021500         05  W-IDLANDX2-LOW        PIC X(2)    VALUE SPACE.               
021600         05  W-IDMARKBO-LOW        PIC X(1)    VALUE 'A'.                 
021700         05  W-IDPARTNR-LOW        PIC X(9)    VALUE LOW-VALUE.           
021800         05  W-IDFTG-LOW           PIC 9(2)    VALUE ZERO.                
021900                                                                          
022000     03  W-WDB1B1KY-HIGH.                                                 
022100         05  W-IDLANDX2-HIGH       PIC X(2)    VALUE SPACE.               
022200         05  W-IDMARKBO-HIGH       PIC X(1)    VALUE 'G'.                 
022300         05  W-IDPARTNR-HIGH       PIC X(9)    VALUE HIGH-VALUE.          
022400         05  W-IDFTG-HIGH          PIC 9(2)    VALUE 99.                  
022500                                                                          
022705     03  W-IDLAND-X.                                                      
022706         05  W-IDLAND              PIC X(2)    VALUE SPACE.               
022707                                                                          
022800*    --- STATUS-KOD FRÅN IMS                                              
022900 01  STATUS-WS                   PIC XX.                                  
023000     88  SEGMENT-FINNS                       VALUE '  '.                  
023100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023300     88  BASEN-SLUT                          VALUE 'GB'.                  
023400     SKIP2                                                                
023500 01  GODK-STATUSKODER.                                                    
023600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023700     SKIP3                                                                
023800 01  SSA1                        PIC X(64).                               
023900 01  SSA2                        PIC X(64).                               
024000     EJECT                                                                
024100*    --- IMS FUNKTIONSKODER                                               
024200*01  -COPY W0003                                                          
024300     EJECT                                                                
025100 01  FILLER                  PIC X(16)   VALUE 'WDB1B1-POST'.             
025200 01  DLI-IO-WDB1B1.                                                       
025300*    03  -COPY WDB1B1                                                     
025400      EJECT                                                               
025500 01  DLI-IO-WDB101.                                                       
025600*    03  -COPY WDB101                                                     
025700      EJECT                                                               
026200 LINKAGE SECTION.                                                         
026900*01  -COPY W0008      -PRE WDB1-                                          
027000     05  FILLER                  PIC X.                                   
027100     EJECT                                                                
027200*01  -COPY W0008      -PRE  WDB1B-                                        
027300     05  FILLER                  PIC X(16).                               
027400     EJECT                                                                
027803 PROCEDURE DIVISION   USING  WDB1-PCB WDB1B-PCB.                          
028003                                                                          
028103 MAIN SECTION.                                                            
028203     ENTRY 'DLITCBL'  USING  WDB1-PCB WDB1B-PCB.                          
028403                                                                          
028503     PERFORM A-INIT                                                       
028603                                                                          
028703     PERFORM S01-LAES-W11689                                              
028803     PERFORM S02-LAES-PRISFIL                                             
028903     PERFORM UNTIL END-OF-W11689 AND END-OF-W33588                        
029203       PERFORM S07-HAMTA-IDMARKBO                                         
029207                                                                          
029504       IF IN-IDARTNR = PRIS-IDARTNR                                       
029603*** FINNS SJÄLVKOST FÖR LOKAL LEVERANTÖR LÄGGS DENNA I WS-PRARTSJK        
029703*** ANNARS LÄGGS ARTIKELNS SJÄLVKOST DÄR LIKSOM FÖR LEV = 11 (GBG)        
029803*** IN-IDLEVNR-LOC = '11' ÄR SAMMA SOM PRIS-IDLEVNR '1441'                
029903         MOVE PRIS-PRARTSJK            TO WS-PRARTSJK                     
029904         MOVE IN-PRARTSTD              TO WS-PRARTSTD                     
030003         IF IN-IDLEVNR-LOC NOT = '11'                                     
030103           MOVE +1 TO PIX                                                 
030203           MOVE NEJ TO TRAFF-SW                                           
030303           PERFORM UNTIL PIX > MAX-PIX OR TRAFF-SW = JA                   
030304             MOVE PRIS-IDDC (PIX)       TO WS-PRIS-IDDC                   
030603             IF (PRIS-IDDC    (PIX) = IN-IDDC)                            
030605             OR ((WS-PRIS-IDDC (1:1) = '4' OR '7') AND                    
030606                 (WS-PRIS-IDDC (1:1) = IN-IDDC (1:1)))                    
030805               IF PRIS-PRARTBES-PR(PIX) NOT = 0                           
030806                 IF ((WS-PRIS-IDDC (1:1) = '7') AND                       
030807                    (WS-PRIS-IDDC (1:1) = IN-IDDC (1:1)))                 
030903                   MOVE PRIS-PRARTBES-PR(PIX) TO WS-PRARTSJK              
030904                   MOVE PRIS-PRARTBES-PR(PIX) TO WS-PRARTSTD              
031003                   MOVE PRIS-IDLEVNR(PIX) TO IN-IDLEVNR-LOC               
031103                   MOVE JA TO TRAFF-SW                                    
031104                 ELSE                                                     
031105                   IF IN-IDLEVNR-LOC = PRIS-IDLEVNR(PIX)                  
031106                     MOVE PRIS-PRARTBES-PR(PIX) TO WS-PRARTSJK            
031107                     MOVE PRIS-IDLEVNR(PIX) TO IN-IDLEVNR-LOC             
031108                     MOVE JA TO TRAFF-SW                                  
031109                   END-IF                                                 
031110                 END-IF                                                   
031203               END-IF                                                     
031303             END-IF                                                       
031403             ADD +1 TO PIX                                                
031503           END-PERFORM                                                    
031504         ELSE                                                             
031508           MOVE +1 TO PIX                                                 
031509           MOVE NEJ TO TRAFF-SW                                           
031510           PERFORM UNTIL PIX > MAX-PIX OR TRAFF-SW = JA                   
031511             MOVE PRIS-IDDC (PIX) TO WS-PRIS-IDDC                         
031512             IF ((WS-PRIS-IDDC (1:1) = '7') AND                           
031513                (WS-PRIS-IDDC (1:1) = IN-IDDC (1:1)))                     
031514               IF PRIS-IDLEVNR(PIX) NOT = '1441'                          
031520                  MOVE PRIS-PRARTBES-PR(PIX) TO WS-PRARTSJK               
031530                  MOVE PRIS-PRARTBES-PR(PIX) TO WS-PRARTSTD               
031540                  MOVE PRIS-IDLEVNR(PIX)  TO IN-IDLEVNR-LOC               
031550                  MOVE JA TO TRAFF-SW                                     
031560               END-IF                                                     
031570             END-IF                                                       
031580             ADD +1 TO PIX                                                
031590           END-PERFORM                                                    
031600         END-IF                                                           
031703         PERFORM C-BYT-URSPRUNGSFORMAT                                    
031803         PERFORM S03-FLYTTA-SKRIV-UTPOST                                  
031804         MOVE IN-IDDC   TO WS-SPAR-IDDC                                   
031903         PERFORM S01-LAES-W11689                                          
032003       ELSE                                                               
032103         IF IN-IDARTNR < PRIS-IDARTNR                                     
032104           MOVE IN-IDDC TO WS-SPAR-IDDC                                   
032203           PERFORM S01-LAES-W11689                                        
032303         ELSE                                                             
032403           PERFORM S02-LAES-PRISFIL                                       
032503         END-IF                                                           
032603       END-IF                                                             
032703     END-PERFORM                                                          
032803                                                                          
032903     PERFORM Z-FINIT                                                      
033003                                                                          
033103     MOVE ZERO TO RETURN-CODE                                             
033203     GOBACK                                                               
033303     .                                                                    
033403     EJECT                                                                
033503 A-INIT SECTION.                                                          
033603                                                                          
033703     OPEN INPUT  W11689                                                   
033803                 W33588                                                   
034103     OPEN OUTPUT W11690                                                   
034203                                                                          
034303     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
034403                                                                          
035003     .                                                                    
035103     EJECT                                                                
035203 C-BYT-URSPRUNGSFORMAT SECTION.                                           
035507                                                                          
035508     INITIALIZE ARTU-W400ARTU                                             
035509     MOVE IN-KDARTURS       TO ARTU-KDARTURS                              
035510                                                                          
035603     CALL W400ARTU          USING ARTU-W400ARTU                           
035703     MOVE ARTU-KDARTURS-NUM TO WS-KDARTURS                                
035803                                                                          
035903                                                                          
036003     MOVE IN-TIFINLV        TO DAT-I-TIDATUM                              
036103     MOVE 'AAVVD'           TO DAT-KDDATFORM                              
036203     CALL WDATKONV       USING DAT-KDDATFORM                              
036303                               DAT-I-TIDATUM                              
036403                               DAT-O-TIDATUM                              
036503                               DAT-KDSVAR                                 
036603                                                                          
036703     MOVE DAT-TIAAMMDD      TO WS-AAMMDD                                  
036803     MOVE DAT-TISEKEL       TO WS-SEKEL                                   
036903     MOVE WS-TIFINLV        TO UT-DADATUM                                 
037003     .                                                                    
037103     EJECT                                                                
037203 Z-FINIT SECTION.                                                         
037303                                                                          
037603     CLOSE W11689                                                         
037703           W33588                                                         
037903           W11690                                                         
038103     SKIP2                                                                
038203     MOVE 'S'        TO POSTSUM-OPKOD                                     
038303     CALL POSTSUM USING POSTSUM-PARM                                      
038403     .                                                                    
038503     EJECT                                                                
038603 S01-LAES-W11689  SECTION.                                                
038604                                                                          
038703     READ W11689 INTO IN-AREA                                             
038803     AT END                                                               
038903        MOVE +999999999   TO IN-IDARTNR                                   
039003        SET END-OF-W11689 TO TRUE                                         
039103                                                                          
039203     NOT AT END                                                           
039303        MOVE 'INDIN '   TO POSTSUM-FDNAMN                                 
039403        MOVE 'W11690D1' TO POSTSUM-DDNAMN2                                
039503        MOVE SPACE      TO POSTSUM-TRANSTYP                               
039603        CALL POSTSUM USING POSTSUM-PARM                                   
039703     END-READ                                                             
039803     .                                                                    
039903     EJECT                                                                
040003 S02-LAES-PRISFIL  SECTION.                                               
040103                                                                          
040203     READ W33588 INTO PRIS-AREA                                           
040303     AT END                                                               
040403        MOVE +999999999   TO PRIS-IDARTNR                                 
040503        SET END-OF-W33588 TO TRUE                                         
040603     NOT AT END                                                           
040703        MOVE 'W33588'   TO POSTSUM-FDNAMN                                 
040803        MOVE 'W11690D2' TO POSTSUM-DDNAMN2                                
040903        MOVE '  '       TO POSTSUM-TRANSTYP                               
041003        CALL POSTSUM USING POSTSUM-PARM                                   
041103     END-READ                                                             
041203     .                                                                    
041303     EJECT                                                                
041403 S03-FLYTTA-SKRIV-UTPOST SECTION.                                         
041503                                                                          
041603     MOVE IN-IDARTNR         TO UT-IDARTNR                                
041604     MOVE W-IDLAND           TO UT-IDLANDX2                               
041703     MOVE IN-IDFKNGRP        TO UT-IDFKNGRP                               
041803     MOVE IN-KDSRA           TO UT-KDSRA                                  
041903     MOVE IN-KVQPACK-0       TO UT-KVQPACK-0                              
042003     MOVE WS-KDARTURS        TO UT-KDARTURS-NUM                           
042103     MOVE IN-KDPRODSL        TO UT-KDPRODSL                               
042203     MOVE IN-VLARTNTO        TO UT-VLARTNTO                               
042303     MOVE IN-VKART           TO UT-VKART                                  
042403     MOVE IN-KDVSOP          TO UT-KDVSOP                                 
042503     MOVE IN-IDSTATNR        TO UT-IDSTATNR                               
042603     MOVE IN-KDSORT          TO UT-KDSORT                                 
042703     MOVE IN-KDERS           TO UT-KDERS                                  
042803     MOVE IN-KDBPSR          TO UT-KDBPSR                                 
042903     MOVE IN-KDBBCL          TO UT-KDBBCL                                 
043104     MOVE IN-IDLEVNR         TO UT-IDLEVNR-DUBLETT                        
043203     MOVE IN-KDAGE           TO UT-KDAGE                                  
043303     MOVE IN-IDPROJ          TO UT-IDPROJ                                 
043403     MOVE WS-PRARTSJK        TO UT-PRARTSJK                               
043503     MOVE WS-PRARTSTD        TO UT-PRARTSTD                               
043703     MOVE IN-BEART-L1        TO UT-BEART-L1                               
043803     MOVE IN-BEART-L2        TO UT-BEART-L2                               
043903     MOVE IN-KDPSLLOC        TO UT-KDPSLLOC                               
044003     MOVE IN-IDLEVNR-LOC     TO UT-IDLEVNR-LOC                            
044103                                UT-IDLEVNR-LOC-DUBLETT                    
044203     MOVE IN-KDSTANAUTG      TO UT-KDSTANAUTG                             
044303     MOVE IN-FLOVRLAG        TO UT-FLOVRLAG                               
044403     MOVE IN-FLSOFTWARE      TO UT-FLSOFTWARE                             
044503                                                                          
044603     MOVE '00000'            TO UT-IDLEVNR                                
044703     MOVE '00000'            TO UT-IDLEVNR-LOC                            
044803     MOVE IN-KDTIPPR         TO UT-KDTIPPR                                
044903     MOVE IN-IDKAT(1)        TO UT-IDKAT(1)                               
045003     MOVE IN-IDKAT(2)        TO UT-IDKAT(2)                               
045103     MOVE IN-IDKAT(3)        TO UT-IDKAT(3)                               
045203     MOVE IN-BELEVART        TO UT-BELEVART                               
045303     MOVE IN-FLGEMFMC        TO UT-FLGEMFMC                               
045403     MOVE IN-IDPROJUP        TO UT-IDPROJUP                               
045503     MOVE IN-TIURPROD        TO UT-TIURPROD                               
045504     MOVE IN-BEARTEXT        TO UT-BEARTEXT                               
045603                                                                          
045703     WRITE UT-POST FROM UT-AREA                                           
045803                                                                          
045903     MOVE 'INDUT '   TO POSTSUM-FDNAMN                                    
046003     MOVE 'W11690D3' TO POSTSUM-DDNAMN2                                   
046103     CALL POSTSUM USING POSTSUM-PARM                                      
046203     .                                                                    
050800 S07-HAMTA-IDMARKBO   SECTION.                                            
050820                                                                          
050830     PERFORM S9-SEARCH-IDLAND                                             
050840                                                                          
051000     IF IN-IDDC NOT = WS-SPAR-IDDC                                        
051405       IF W-IDLAND NOT = W-B-IDLANDX2-LOW                                 
051803         MOVE W-IDLAND           TO W-B-IDLANDX2-LOW                      
051903                                    W-B-IDLANDX2-HIGH                     
052003                                    W-IDLANDX2-LOW                        
052103                                    W-IDLANDX2-HIGH                       
052104                                                                          
052106         MOVE IN-IDDC            TO WS-IDDC                               
052108         IF IN-IDDC(1:1) = '4' OR '7'                                     
052111            MOVE 'E'             TO W-IDMARKBO-LOW                        
052112         ELSE                                                             
052113            MOVE 'A'             TO W-IDMARKBO-LOW                        
052120         END-IF                                                           
052130                                                                          
052203         PERFORM IMS-GU-WDB1B1                                            
052303         IF SEGMENT-FINNS                                                 
052305           IF SEQB-IDPARTNR NOT = W-IDPARTNR                              
052403             MOVE SEQB-IDPARTNR    TO W-IDPARTNR                          
052503             MOVE SEQB-IDFTG       TO W-IDFTG                             
052506                                                                          
052603             PERFORM IMS-GU-WDB101                                        
052703             IF SEGMENT-FINNS                                             
052803               MOVE BET-IDMARKBO   TO WS-IDMARKBO                         
052903             END-IF                                                       
052904           END-IF                                                         
053003         END-IF                                                           
053004       END-IF                                                             
053203     END-IF                                                               
053303     .                                                                    
053403     EJECT                                                                
054704 S9-SEARCH-IDLAND SECTION.                                                
054713                                                                          
054715     SEARCH ALL DC-LAND                                                   
054716       AT END                                                             
054717         MOVE SPACE          TO W-IDLAND                                  
054718       WHEN DCLAND-IDDC (DCLAND-IX) = IN-IDDC                             
054719         MOVE DCLAND-IDLANDX2(DCLAND-IX) TO W-IDLAND                      
054722     END-SEARCH                                                           
054730     .                                                                    
054740     EJECT                                                                
054750                                                                          
054803*************** I M S *******************                                 
054903                                                                          
057503 IMS-GU-WDB101 SECTION.                                                   
057603     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
057703          DELIMITED BY SIZE INTO SSA1                                     
057803     MOVE '  GE' TO GODK-STATUSKODER                                      
057903     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
058003     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
058103     PERFORM IMS-STATUSKONTROLL                                           
058203     .                                                                    
058303     SKIP3                                                                
058403 IMS-GU-WDB1B1 SECTION.                                                   
058503     STRING 'WDB1B1  (WDB1B1KY=>' W-WDB1B1KY-LOW                          
058603                    '&WDB1B1KY=<' W-WDB1B1KY-HIGH ')'                     
058703          DELIMITED BY SIZE INTO SSA1                                     
058803     MOVE '    ' TO GODK-STATUSKODER                                      
058903     CALL CBLTDLI USING GU WDB1B-PCB DLI-IO-WDB1B1 SSA1                   
059003     MOVE WDB1B-STATUS-CODE TO STATUS-WS                                  
059103     PERFORM IMS-STATUSKONTROLL                                           
059203     .                                                                    
059303     SKIP3                                                                
060503 IMS-STATUSKONTROLL SECTION.                                              
060603                                                                          
060703     SET STATUS-IX TO 1                                                   
060803     SEARCH GODK-STATUS                                                   
060903       AT END CALL FELLOG                                                 
061003       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
061103     END-SEARCH                                                           
062000     .                                                                    
070000     EJECT                                                                
