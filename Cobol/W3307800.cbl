000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W3307800.                                    
000300 AUTHOR.                     STEFAN KIHLBERG                              
000400 DATE-WRITTEN.               JAN-90.                                      
000500     SKIP2                                                                
000600     REMARKS.                                                             
000700                                                                          
000800*    FUNKTION.                                                            
000900                                                                          
001000*    ARTIKELSTATISTIK.                                                    
001100*    SUMMERAR RESULTAT AV URVAL FRÅN BILD 3202 OCH 3203.                  
001200*    URVALKOMMER FRÅN FILEN W33035, DENNA STANDARDSORTERAS                
001300*    SORTERAS STIGANDE PÅ URVAL I PROCEDUREN. RESULTAT AV URVAL           
001400*    KOMMER FRÅN FILEN W33077.  URVALEN SUMMERAS PÅ ARTIKELNUMMER         
001500*    OCH MARKNAD/KONCERN/DISTRIKT OCH SKRIVS EFTER RESP URVAL PÅ          
001600*    FILEN W33079.                                                        
001700                                                                          
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100                                                                          
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*    --- INFILER:                                                         
002700     SELECT SORTFIL            ASSIGN TO W33078DS.                        
002800     SELECT W33035S            ASSIGN TO W33078D1.                        
002900     SELECT W33077             ASSIGN TO W33078D2.                        
003000*    --- UTFILER:                                                         
003100     SELECT W33079             ASSIGN TO W33078D3.                        
003200     EJECT                                                                
003300                                                                          
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800                                                                          
003900 SD  SORTFIL                                                              
004000     RECORDING      V                                                     
004100     SKIP2                                                                
004200 01  SORTERAD-POST.                                                       
004300   03  SORT-IDUSER                 PIC X(8).                              
004400   03  SORT-DAREGDAT               PIC X(8).                              
004500   03  SORT-TIREGTID               PIC X(7).                              
004600   03  SORT-FOERSTA                PIC X(9).                              
004700   03  SORT-ANDRA                  PIC X(9).                              
004800   03  SORT-TREDJE                 PIC X(9).                              
004900   03  CTEXT.                                                             
005000     05  FILLER                    PIC X(123).                            
005100                                                                          
005200*03  POST -COPY W33077  -L -PRE SRT77- -RED CTEXT.                        
005300*++INCLUDE W33077                                                         
005400     EJECT                                                                
005500                                                                          
005600 FD  W33035S                                                              
005700     LABEL RECORD   STANDARD                                              
005800     RECORDING      V                                                     
005900     BLOCK CONTAINS 0.                                                    
006000     SKIP2                                                                
006100*01  -COPY W3303503  -L.                                                  
006200*++INCLUDE W3303503                                                       
006300     SKIP2                                                                
006400*01  -COPY W3303502  -L.                                                  
006500*++INCLUDE W3303502                                                       
006600     EJECT                                                                
006700                                                                          
006800 FD  W33077                                                               
006900     LABEL RECORD   STANDARD                                              
007000     RECORDING      F                                                     
007100     BLOCK CONTAINS 0.                                                    
007200     SKIP2                                                                
007300*01  POST -COPY W33077  -L -PRE I77-.                                     
007400*++INCLUDE W33077                                                         
007500     EJECT                                                                
007600                                                                          
007700 FD  W33079                                                               
007800     LABEL RECORD   STANDARD                                              
007900     RECORDING      V                                                     
008000     BLOCK CONTAINS 0.                                                    
008100     SKIP2                                                                
008200                                                                          
008300*01  POST -COPY W3303503  -L -PRE U379-.                                  
008400*++INCLUDE W3303503                                                       
008500     SKIP2                                                                
008600                                                                          
008700*01  POST -COPY W3303502  -L -PRE U279-.                                  
008800*++INCLUDE W3303502                                                       
008900     SKIP2                                                                
009000                                                                          
009100*01  POST -COPY W33079  -L -PRE U79-.                                     
009200*++INCLUDE W33079                                                         
009300     SKIP2                                                                
009400                                                                          
009500 01  FILLER                  PIC X(16)     VALUE 'W-S SECTION '.          
009600                                                                          
009700 WORKING-STORAGE SECTION.                                                 
009710                                                                          
009720*  --  CHECKED BY WY2000                                                  
009800     SKIP2                                                                
009900                                                                          
010000 77  PROGRAM-NAMN            PIC X(8)       VALUE 'W3307800'.             
010100 77  JA                      PIC X          VALUE 'J'.                    
010200 77  NEJ                     PIC X          VALUE 'N'.                    
010300 77  FELKOD                  PIC S9(4)      VALUE +16 COMP SYNC.          
010400                                                                          
010500 77  WS-BRYT-VECKA           PIC  9(6)      VALUE ZERO.                   
010600 77  WS-BRYT-IDDISTR         PIC S9(5)      VALUE ZERO COMP-3.            
010700 77  WS-BRYT-IDKONCNR        PIC S9(3)      VALUE ZERO COMP-3.            
010800 77  WS-BRYT-KDMARK-BUDG     PIC S9(3)      VALUE ZERO COMP-3.            
010900 77  WS-BRYT-IDARTNR         PIC S9(9)      VALUE ZERO COMP-3.            
011000                                                                          
011100 77  WS-SULEVANT-VV          PIC S9(9)      VALUE ZERO COMP-3.            
011200 77  WS-SUARTFSG-VV          PIC S9(9)V9(3) VALUE ZERO COMP-3.            
011300 77  WS-RETOTBV-VV           PIC S9(9)V9(2) VALUE ZERO COMP-3.            
011400 77  WS-SUARTSJK-VV          PIC S9(9)V9(3) VALUE ZERO COMP-3.            
011500 77  WS-SULEVANT-FVV         PIC S9(9)      VALUE ZERO COMP-3.            
011600 77  WS-SUARTFSG-FVV         PIC S9(9)V9(3) VALUE ZERO COMP-3.            
011700 77  WS-RETOTBV-FVV          PIC S9(9)V9(2) VALUE ZERO COMP-3.            
011800 77  WS-SUARTSJK-FVV         PIC S9(9)V9(3) VALUE ZERO COMP-3.            
011900 77  W-SUARTSJK-VV           PIC S9(9)V9(3) VALUE ZERO COMP-3.            
012000 77  W-SUARTSJK-FVV          PIC S9(9)V9(3) VALUE ZERO COMP-3.            
012100                                                                          
012200 77  MARK-IX                 PIC S9(1)      VALUE 0.                      
012300 77  KONC-IX                 PIC S9(1)      VALUE 0.                      
012400 77  DIST-IX                 PIC S9(1)      VALUE 0.                      
012500 77  MAX-MARK-IX             PIC S9(1)      VALUE 5.                      
012600 77  MAX-KONC-IX             PIC S9(1)      VALUE 9.                      
012700 77  MAX-DIST-IX             PIC S9(1)      VALUE 5.                      
012800                                                                          
012900                                                                          
013000 01  FILLER                  PIC X(16)     VALUE 'OMR-FLAGGA'.            
013100                                                                          
013200 01  OMR-FLAGGA              PIC 9(01).                                   
013300    88  DISTRIKT-NIVA                      VALUE 1.                       
013400    88  KONCERN-NIVA                       VALUE 2.                       
013500    88  MARKNAD-NIVA                       VALUE 3.                       
013600    88  UTAN-NIVA                          VALUE 4.                       
013700                                                                          
013800 01  FILLER                  PIC X(16)     VALUE 'SWITCHAR'.              
013900                                                                          
014000 01    SWITCHAR.                                                          
014100                                                                          
014200    03  W33035-EOF           PIC X(1)       VALUE 'N'.                    
014300    03  W33077-EOF           PIC X(1)       VALUE 'N'.                    
014400    03  SORT-EOF             PIC X(1)       VALUE 'N'.                    
014500                                                                          
014600                                                                          
014700                                                                          
014800 01  FILLER                  PIC X(16)     VALUE 'DYN SUB-PGM'.           
014900                                                                          
015000 01  DYNAMISKA-SUBPROGRAM.                                                
015100*                                                                         
015200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
015300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
015400     SKIP2                                                                
015500*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
015600*                                                                         
015700 01  FILLER                       PIC X(16)  VALUE 'POSTSUM'.             
015800*01  -COPY W0005 -PRE  POSTSUM-                                           
015900*++INCLUDE W0005                                                          
016000     EJECT                                                                
016100*                                                                         
016200 01  FILLER                       PIC X(16)  VALUE 'WS-SORT-A'.           
016300                                                                          
016400 01  WS-SORTERAD-AREA.                                                    
016500   03  WS-SORT-IDUSER              PIC X(8).                              
016600   03  WS-SORT-DAREGDAT            PIC X(8).                              
016700   03  WS-SORT-TIREGTID            PIC X(7).                              
016800   03  WS-SORT-FOERSTA             PIC X(9).                              
016900   03  WS-SORT-ANDRA               PIC X(9).                              
017000   03  WS-SORT-TREDJE              PIC X(9).                              
017100   03  WS-CTEXT.                                                          
017200     05  FILLER                      PIC X(123).                          
017300                                                                          
017400*03  AREA -COPY W33077  -PRE SORTWS- -RED WS-CTEXT.                       
017500*++INCLUDE W33077                                                         
017600     EJECT                                                                
017700                                                                          
017800 01  FILLER                  PIC X(16)     VALUE 'WS-79     '.            
017900                                                                          
018000*01        -COPY W33079  -PRE WS79-.                                      
018100*++INCLUDE W33079                                                         
018200    EJECT                                                                 
018300                                                                          
018400 01  FILLER                       PIC X(16)  VALUE 'WI-35-FILEN'.         
018500                                                                          
018600 01  WI35-AREA                   PIC X(2619).                             
018700                                                                          
018800*01  AREA  -COPY W3303503  -PRE WIG35- -RED WI35-AREA.                    
018900*++INCLUDE W3303503                                                       
019000    EJECT                                                                 
019100                                                                          
019200*01  AREA  -COPY W3303503  -PRE WI335- -RED WI35-AREA.                    
019300*++INCLUDE W3303503                                                       
019400    EJECT                                                                 
019500                                                                          
019600*01  AREA  -COPY W3303502  -PRE WI235- -RED WI35-AREA.                    
019700*++INCLUDE W3303502                                                       
019800    EJECT                                                                 
019900                                                                          
020000 01  FILLER                       PIC X(16)  VALUE 'I77-FILEN'.           
020100                                                                          
020200*01  AREA  -COPY W33077  -PRE WI77-.                                      
020300*++INCLUDE W33077                                                         
020400    EJECT                                                                 
020500                                                                          
020600                                                                          
020700                                                                          
020800 01  FILLER                       PIC X(16)  VALUE 'UTFIL'.               
020900                                                                          
021000 01  WU79-AREA                   PIC X(2619).                             
021100                                                                          
021200*01  AREA  -COPY W3303503  -PRE WU379-  REDIFINES WU79-AREA.              
021300*++INCLUDE W3303503                                                       
021400    EJECT                                                                 
021500                                                                          
021600*01  AREA  -COPY W3303502  -PRE WU279- REDIFINES WU79-AREA.               
021700*++INCLUDE W3303502                                                       
021800    EJECT                                                                 
021900                                                                          
022000*01  AREA  -COPY W33079  -PRE WU079- REDIFINES WU79-AREA.                 
022100*++INCLUDE W33079                                                         
022200     EJECT                                                                
022300                                                                          
022400 PROCEDURE DIVISION.                                                      
022500    SKIP2                                                                 
022600 STYR SECTION.                                                            
022700     PERFORM A-INIT                                                       
022800     PERFORM S01-LAS-77-FIL                                               
022900     PERFORM S02-LAS-35-FIL                                               
023000     PERFORM UNTIL W33035-EOF = JA                                        
023100        IF WI77-IDUSER         = WIG35-IDUSER AND                         
023200           WI77-DAREGDAT       = WIG35-DAREGDAT AND                       
023300           WI77-TIREGTID       = WIG35-TIREGTID                           
023400           PERFORM B-KONTROLLERA-NIVA                                     
023500           MOVE WIG35-DAFSGVV-FOM TO WS-BRYT-VECKA                        
023600           PERFORM C1-SKRIVA-MATCHADE-URVAL                               
023700           SORT SORTFIL                                                   
023800              ASCENDING KEY SORT-IDUSER                                   
023900                            SORT-DAREGDAT                                 
024000                            SORT-TIREGTID                                 
024100                            SORT-FOERSTA                                  
024200                            SORT-ANDRA                                    
024300                            SORT-TREDJE                                   
024400              INPUT PROCEDURE D-SORTERA-RESULTAT-PER-URVAL                
024500              OUTPUT PROCEDURE E-SUMMERA-SKRIVA-RESULTAT                  
024600           MOVE NEJ TO SORT-EOF                                           
024700           IF SORT-RETURN = ZERO                                          
024800              CONTINUE                                                    
024900           ELSE                                                           
025000              CALL ABEND USING FELKOD                                     
025100           END-IF                                                         
025200           PERFORM S02-LAS-35-FIL                                         
025300        ELSE                                                              
025400           PERFORM C2-SKRIVA-EJ-MATCHADE-URVAL                            
025500           PERFORM S02-LAS-35-FIL                                         
025600        END-IF                                                            
025700     END-PERFORM                                                          
025800     PERFORM Z-FINIT                                                      
025900     MOVE ZERO TO RETURN-CODE                                             
026000     GOBACK                                                               
026100     .                                                                    
026200     EJECT                                                                
026300 A-INIT SECTION.                                                          
026400     SKIP2                                                                
026500     OPEN INPUT  W33035S                                                  
026600                 W33077                                                   
026700     OPEN OUTPUT W33079                                                   
026800     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
026900     .                                                                    
027000     EJECT                                                                
027100                                                                          
027200 B-KONTROLLERA-NIVA SECTION.                                              
027300                                                                          
027400***  UTVÄRDERAR OM URVALET ÄR PÅ DISTRIKT, KONCERN ELLER                  
027500***  MARKNAD.                                                             
027600                                                                          
027700     EVALUATE WIG35-IDGTYP                                                
027800        WHEN 1                                                            
027900           MOVE 1              TO OMR-FLAGGA                              
028000        WHEN 2                                                            
028100           MOVE 2              TO OMR-FLAGGA                              
028200        WHEN 3                                                            
028300           MOVE 3              TO OMR-FLAGGA                              
028400        WHEN 4                                                            
028500           MOVE 4              TO OMR-FLAGGA                              
028600     END-EVALUATE.                                                        
028700                                                                          
028800                                                                          
028900 C1-SKRIVA-MATCHADE-URVAL SECTION.                                        
029000                                                                          
029100***  SKRIVER URVALSPOSTER PÅ 79-FILEN                                     
029200                                                                          
029300     IF WIG35-IDPTYP = 'VA2'                                              
029400        EVALUATE  WIG35-IDTRANS                                           
029500           WHEN 3202                                                      
029600              MOVE WI235-AREA  TO WU279-AREA                              
029700              PERFORM S04-SKRIV-U279                                      
029800           WHEN 3203                                                      
029900              MOVE WI335-AREA  TO WU379-AREA                              
030000              PERFORM S05-SKRIV-U379                                      
030100        END-EVALUATE                                                      
030200     END-IF                                                               
030300     .                                                                    
030400                                                                          
030500                                                                          
030600 C2-SKRIVA-EJ-MATCHADE-URVAL SECTION.                                     
030700                                                                          
030800***  SKRIVER URVALSPOSTER PÅ 79-FILEN                                     
030900                                                                          
031000     IF WIG35-IDPTYP = 'VA2'                                              
031100        EVALUATE  WIG35-IDTRANS                                           
031200           WHEN 3202                                                      
031300              MOVE WI235-AREA  TO WU279-AREA                              
031400              MOVE ZERO        TO WU279-IDGTYP                            
031500              PERFORM S04-SKRIV-U279                                      
031600           WHEN 3203                                                      
031700              MOVE WI335-AREA  TO WU379-AREA                              
031800              MOVE ZERO        TO WU379-IDGTYP                            
031900              PERFORM S05-SKRIV-U379                                      
032000        END-EVALUATE                                                      
032100     END-IF                                                               
032200     .                                                                    
032300                                                                          
032400 D-SORTERA-RESULTAT-PER-URVAL SECTION.                                    
032500                                                                          
032600                                                                          
032700     EVALUATE OMR-FLAGGA                                                  
032800                                                                          
032900        WHEN 1                                                            
033000                                                                          
033100***     LÄSER 77-FILEN OCH SKICKAR RESULTATPOSTER TILL COBOL-             
033200***     SORTERING. SORTERING SKER PÅ URVAL, ARTIKEL,                      
033300***     >>>DISTRIKT<<< OCH VECKA                                          
033400                                                                          
033500           PERFORM UNTIL                                                  
033600           (WI77-IDUSER  NOT  =  WIG35-IDUSER)  OR                        
033700           (WI77-DAREGDAT NOT = WIG35-DAREGDAT) OR                        
033800           (WI77-TIREGTID NOT = WIG35-TIREGTID) OR                        
033900           (W33077-EOF = JA)                                              
034000                 MOVE WI77-IDUSER   TO WS-SORT-IDUSER                     
034100                 MOVE WI77-DAREGDAT TO WS-SORT-DAREGDAT                   
034200                 MOVE WI77-TIREGTID TO WS-SORT-TIREGTID                   
034300                 MOVE WI77-IDARTNR  TO WS-SORT-FOERSTA                    
034400                 MOVE WI77-IDDISTR  TO WS-SORT-ANDRA                      
034500                 MOVE WI77-DAFSGVV  TO WS-SORT-TREDJE                     
034600                 MOVE WI77-AREA     TO SORTWS-AREA                        
034700                 RELEASE SORTERAD-POST FROM WS-SORTERAD-AREA              
034800                 PERFORM S01-LAS-77-FIL                                   
034900           END-PERFORM                                                    
035000                                                                          
035100        WHEN 2                                                            
035200                                                                          
035300***     LÄSER 77-FILEN OCH SKICKAR RESULTATPOSTER TILL COBOL-             
035400***     SORTERING. SORTERING SKER PÅ URVAL, ARTIKEL,                      
035500***     >>>KONCERN<<< OCH VECKA                                           
035600                                                                          
035700           PERFORM UNTIL                                                  
035800           (WI77-IDUSER  NOT  = WIG35-IDUSER)   OR                        
035900           (WI77-DAREGDAT NOT = WIG35-DAREGDAT) OR                        
036000           (WI77-TIREGTID NOT = WIG35-TIREGTID) OR                        
036100           (W33077-EOF = JA)                                              
036200              MOVE WI77-IDUSER      TO WS-SORT-IDUSER                     
036300              MOVE WI77-DAREGDAT    TO WS-SORT-DAREGDAT                   
036400              MOVE WI77-TIREGTID    TO WS-SORT-TIREGTID                   
036500              MOVE WI77-IDARTNR     TO WS-SORT-FOERSTA                    
036600              MOVE WI77-IDKONCNR    TO WS-SORT-ANDRA                      
036700              MOVE WI77-DAFSGVV     TO WS-SORT-TREDJE                     
036800              MOVE WI77-AREA        TO SORTWS-AREA                        
036900              RELEASE SORTERAD-POST FROM WS-SORTERAD-AREA                 
037000              PERFORM S01-LAS-77-FIL                                      
037100           END-PERFORM                                                    
037200                                                                          
037300        WHEN 3                                                            
037400                                                                          
037500***     LÄSER 77-FILEN OCH SKICKAR RESULTATPOSTER TILL COBOL-             
037600***     SORTERING. SORTERING SKER PÅ URVAL, ARTIKEL,                      
037700***     >>>MARKNAD<<< OCH VECKA                                           
037800                                                                          
037900           PERFORM UNTIL                                                  
038000           (WI77-IDUSER  NOT  = WIG35-IDUSER)   OR                        
038100           (WI77-DAREGDAT NOT = WIG35-DAREGDAT) OR                        
038200           (WI77-TIREGTID NOT = WIG35-TIREGTID) OR                        
038300           (W33077-EOF = JA)                                              
038400              MOVE WI77-IDUSER      TO WS-SORT-IDUSER                     
038500              MOVE WI77-DAREGDAT    TO WS-SORT-DAREGDAT                   
038600              MOVE WI77-TIREGTID    TO WS-SORT-TIREGTID                   
038700              MOVE WI77-IDARTNR     TO WS-SORT-FOERSTA                    
038800              MOVE WI77-KDMARK-BUDG TO WS-SORT-ANDRA                      
038900              MOVE WI77-DAFSGVV     TO WS-SORT-TREDJE                     
039000              MOVE WI77-AREA        TO SORTWS-AREA                        
039100              RELEASE SORTERAD-POST FROM WS-SORTERAD-AREA                 
039200              PERFORM S01-LAS-77-FIL                                      
039300           END-PERFORM                                                    
039400                                                                          
039500        WHEN 4                                                            
039600                                                                          
039700***     LÄSER 77-FILEN OCH SKICKAR RESULTATPOSTER TILL COBOL-             
039800***     SORTERING. SORTERING SKER PÅ URVAL, ARTIKEL,                      
039900***     OCH VECKA. OBS! URVAL UTAN SÖKNING PÅ DISTRIKT -                  
040000***     KONCERN - MARKNAD.                                                
040100                                                                          
040200*       PÅ DISTRIKT/MARKNAD/KONCERN TILL SORTERING.                       
040300                                                                          
040400           PERFORM UNTIL                                                  
040500           (WI77-IDUSER  NOT  = WIG35-IDUSER)   OR                        
040600           (WI77-DAREGDAT NOT = WIG35-DAREGDAT) OR                        
040700           (WI77-TIREGTID NOT = WIG35-TIREGTID) OR                        
040800           (W33077-EOF = JA)                                              
040900              MOVE WI77-IDUSER      TO WS-SORT-IDUSER                     
041000              MOVE WI77-DAREGDAT    TO WS-SORT-DAREGDAT                   
041100              MOVE WI77-TIREGTID    TO WS-SORT-TIREGTID                   
041200              MOVE WI77-IDARTNR     TO WS-SORT-FOERSTA                    
041300              MOVE WI77-DAFSGVV     TO WS-SORT-ANDRA                      
041400              MOVE ZERO             TO SORT-TREDJE                        
041500              MOVE WI77-AREA        TO SORTWS-AREA                        
041600              RELEASE SORTERAD-POST FROM WS-SORTERAD-AREA                 
041700              PERFORM S01-LAS-77-FIL                                      
041800           END-PERFORM                                                    
041900                                                                          
042000       END-EVALUATE                                                       
042100                                                                          
042200     .                                                                    
042300                                                                          
042400 E-SUMMERA-SKRIVA-RESULTAT SECTION.                                       
042500                                                                          
042600     EVALUATE OMR-FLAGGA                                                  
042700                                                                          
042800        WHEN 1                                                            
042900                                                                          
043000*       HÄMTAR POSTER MED AKTUELLT URVAL FRÅN COBOL-SORTERING,            
043100*       SAMLAR UPPGIFTER PER ARTIKELNUMMER,SORTERAT PÅ DISTRIKT           
043200*       GÖR BERÄKNINGAR PÅ DESSA OCH SKRIVER PÅ 79-FILEN.                 
043300                                                                          
043400           PERFORM S03-SORT-RETURN                                        
043500           MOVE SORTWS-001-GRUPP    TO WS79-001-GRUPP                     
043600           MOVE SORTWS-IDARTNR      TO WS-BRYT-IDARTNR                    
043700           MOVE SORTWS-IDDISTR      TO WS-BRYT-IDDISTR                    
043800           PERFORM UNTIL SORT-EOF = JA                                    
043900              PERFORM UNTIL SORT-EOF = JA  OR                             
044000                    WS-BRYT-IDARTNR NOT = SORTWS-IDARTNR OR               
044100                    WS-BRYT-IDDISTR NOT = SORTWS-IDDISTR                  
044200                 MOVE SORTWS-003-GRUPP   TO WS79-003-GRUPP                
044300                 IF SORTWS-DAFSGVV < WS-BRYT-VECKA                        
044400                    PERFORM EB-SAMLA-FVV                                  
044500                 ELSE                                                     
044600                    PERFORM EC-SAMLA-VV                                   
044700                 END-IF                                                   
044800                 PERFORM S03-SORT-RETURN                                  
044900              END-PERFORM                                                 
045000              IF WS-SUARTFSG-FVV NOT = ALL ZERO                           
045100                 PERFORM ED-SJK-TG-FVV                                    
045200              ELSE                                                        
045300                 MOVE ALL ZERO      TO WS-RETOTBV-FVV                     
045400              END-IF                                                      
045500              IF WS-SUARTFSG-VV NOT = ALL ZERO                            
045600                 PERFORM EE-SJK-TG-VV                                     
045700              ELSE                                                        
045800                 MOVE ALL ZERO      TO WS-RETOTBV-VV                      
045900              END-IF                                                      
046000              PERFORM EF-FLYTTA-TILL-UTSKRIFT-WU079                       
046100              PERFORM S06-SKRIV-U079                                      
046200              PERFORM EA-NOLLSTALL-SUMFAELT                               
046300              MOVE SORTWS-IDDISTR   TO WS-BRYT-IDDISTR                    
046400              MOVE SORTWS-IDARTNR   TO WS-BRYT-IDARTNR                    
046500           END-PERFORM                                                    
046600                                                                          
046700        WHEN 2                                                            
046800                                                                          
046900                                                                          
047000*       HÄMTAR POSTER MED AKTUELLT URVAL FRÅN COBOL-SORTERING,            
047100*       SAMLAR UPPGIFTER PER ARTIKELNUMMER,SORTERAT PÅ KONCERN            
047200*       GÖR BERÄKNINGAR PÅ DESSA OCH SKRIVER PÅ 79-FILEN.                 
047300                                                                          
047400           PERFORM S03-SORT-RETURN                                        
047500           MOVE SORTWS-001-GRUPP    TO WS79-001-GRUPP                     
047600           MOVE SORTWS-IDARTNR      TO WS-BRYT-IDARTNR                    
047700           MOVE SORTWS-IDKONCNR     TO WS-BRYT-IDKONCNR                   
047800           PERFORM UNTIL SORT-EOF = JA                                    
047900              PERFORM UNTIL SORT-EOF = JA OR                              
048000                    WS-BRYT-IDARTNR NOT = SORTWS-IDARTNR OR               
048100                    WS-BRYT-IDKONCNR NOT = SORTWS-IDKONCNR                
048200                 MOVE SORTWS-003-GRUPP TO WS79-003-GRUPP                  
048300                 IF SORTWS-DAFSGVV < WS-BRYT-VECKA                        
048400                    PERFORM EB-SAMLA-FVV                                  
048500                 ELSE                                                     
048600                    PERFORM EC-SAMLA-VV                                   
048700                 END-IF                                                   
048800                 PERFORM S03-SORT-RETURN                                  
048900              END-PERFORM                                                 
049000              IF WS-SUARTFSG-FVV NOT = ALL ZERO                           
049100                 PERFORM ED-SJK-TG-FVV                                    
049200              ELSE                                                        
049300                 MOVE ALL ZERO TO WS-RETOTBV-FVV                          
049400              END-IF                                                      
049500              IF WS-SUARTFSG-VV NOT = ALL ZERO                            
049600                 PERFORM EE-SJK-TG-VV                                     
049700              ELSE                                                        
049800                 MOVE ALL ZERO TO WS-RETOTBV-VV                           
049900              END-IF                                                      
050000              PERFORM EF-FLYTTA-TILL-UTSKRIFT-WU079                       
050100              PERFORM S06-SKRIV-U079                                      
050200              PERFORM EA-NOLLSTALL-SUMFAELT                               
050300              MOVE SORTWS-IDKONCNR TO WS-BRYT-IDKONCNR                    
050400              MOVE SORTWS-IDARTNR TO WS-BRYT-IDARTNR                      
050500           END-PERFORM                                                    
050600                                                                          
050700        WHEN 3                                                            
050800                                                                          
050900*       HÄMTAR POSTER MED AKTUELLT URVAL FRÅN COBOL-SORTERING,            
051000*       SAMLAR UPPGIFTER PER ARTIKELNUMMER,SORTERAT PÅ MARKNAD            
051100*       GÖR BERÄKNINGAR PÅ DESSA OCH SKRIVER PÅ 79-FILEN.                 
051200                                                                          
051300                                                                          
051400           PERFORM S03-SORT-RETURN                                        
051500           MOVE SORTWS-001-GRUPP    TO WS79-001-GRUPP                     
051600           MOVE SORTWS-IDARTNR      TO WS-BRYT-IDARTNR                    
051700           MOVE SORTWS-KDMARK-BUDG  TO WS-BRYT-KDMARK-BUDG                
051800           PERFORM UNTIL SORT-EOF = JA                                    
051900              PERFORM UNTIL SORT-EOF = JA                                 
052000                    OR WS-BRYT-IDARTNR NOT = SORTWS-IDARTNR               
052100                    OR WS-BRYT-KDMARK-BUDG NOT =                          
052200                    SORTWS-KDMARK-BUDG                                    
052300                 MOVE SORTWS-003-GRUPP TO WS79-003-GRUPP                  
052400                 IF SORTWS-DAFSGVV < WS-BRYT-VECKA                        
052500                    PERFORM EB-SAMLA-FVV                                  
052600                 ELSE                                                     
052700                    PERFORM EC-SAMLA-VV                                   
052800                 END-IF                                                   
052900                 PERFORM S03-SORT-RETURN                                  
053000              END-PERFORM                                                 
053100              IF WS-SUARTFSG-FVV NOT = ALL ZERO                           
053200                 PERFORM ED-SJK-TG-FVV                                    
053300              ELSE                                                        
053400                 MOVE ALL ZERO      TO WS-RETOTBV-FVV                     
053500              END-IF                                                      
053600              IF WS-SUARTFSG-VV NOT = ALL ZERO                            
053700                 PERFORM EE-SJK-TG-VV                                     
053800              ELSE                                                        
053900                 MOVE ALL ZERO      TO WS-RETOTBV-VV                      
054000              END-IF                                                      
054100              PERFORM EF-FLYTTA-TILL-UTSKRIFT-WU079                       
054200              PERFORM S06-SKRIV-U079                                      
054300              PERFORM EA-NOLLSTALL-SUMFAELT                               
054400              MOVE SORTWS-KDMARK-BUDG TO WS-BRYT-KDMARK-BUDG              
054500              MOVE SORTWS-IDARTNR TO WS-BRYT-IDARTNR                      
054600           END-PERFORM                                                    
054700                                                                          
054800                                                                          
054900        WHEN 4                                                            
055000                                                                          
055100*       HÄMTAR POSTER MED AKTUELLT URVAL FRÅN COBOL-SORTERING,            
055200*       SAMLAR UPPGIFTER PER ARTIKELNUMMER,UTAN SORTERING PÅ D/K/M        
055300*       GÖR BERÄKNINGAR PÅ DESSA OCH SKRIVER PÅ 79-FILEN.                 
055400                                                                          
055500           PERFORM S03-SORT-RETURN                                        
055600           MOVE SORTWS-001-GRUPP    TO WS79-001-GRUPP                     
055700           MOVE SORTWS-IDARTNR      TO WS-BRYT-IDARTNR                    
055800           PERFORM UNTIL SORT-EOF = JA                                    
055900              PERFORM UNTIL WS-BRYT-IDARTNR NOT = SORTWS-IDARTNR          
056000                    OR SORT-EOF = JA                                      
056100                 MOVE SORTWS-003-GRUPP TO WS79-003-GRUPP                  
056200                 IF SORTWS-DAFSGVV NOT < WS-BRYT-VECKA                    
056300                    PERFORM EB-SAMLA-FVV                                  
056400                 ELSE                                                     
056500                    PERFORM EC-SAMLA-VV                                   
056600                 END-IF                                                   
056700                 PERFORM S03-SORT-RETURN                                  
056800              END-PERFORM                                                 
056900              IF WS-SUARTFSG-FVV NOT = ALL ZERO                           
057000                 PERFORM ED-SJK-TG-FVV                                    
057100              ELSE                                                        
057200                 MOVE ALL ZERO      TO WS-RETOTBV-FVV                     
057300              END-IF                                                      
057400              IF WS-SUARTFSG-VV NOT = ALL ZERO                            
057500                 PERFORM EE-SJK-TG-VV                                     
057600              ELSE                                                        
057700                 MOVE ALL ZERO      TO WS-RETOTBV-VV                      
057800              END-IF                                                      
057900              PERFORM EF-FLYTTA-TILL-UTSKRIFT-WU079                       
058000              PERFORM S06-SKRIV-U079                                      
058100              PERFORM EA-NOLLSTALL-SUMFAELT                               
058200              MOVE SORTWS-IDARTNR TO WS-BRYT-IDARTNR                      
058300           END-PERFORM                                                    
058400     END-EVALUATE                                                         
058500           .                                                              
058600                                                                          
058700                                                                          
058800  EA-NOLLSTALL-SUMFAELT SECTION.                                          
058900                                                                          
059000*** TÖMMA SUMMERINGSFÄLT                                                  
059100                                                                          
059200     INITIALIZE WS-SUARTSJK-FVV WS-SULEVANT-FVV                           
059300     INITIALIZE WS-SUARTFSG-FVV WS-RETOTBV-FVV                            
059400     INITIALIZE WS-SUARTSJK-VV  WS-SULEVANT-VV                            
059500     INITIALIZE WS-SUARTFSG-VV  WS-RETOTBV-VV.                            
059600                                                                          
059700                                                                          
059800  EB-SAMLA-FVV SECTION.                                                   
059900                                                                          
060000***BERÄKNAR SJÄLVKOSTNAD PER VECKOUPPGIFT FÖR ARTIKEL,                    
060100***SAMLAR VECKOUPPGIFTER PER ARTIKEL ANGÅEDE SJÄLVKOSTNAD,                
060200***FÖRSÄLJNINGSVÄRDE OCH FÖRSÅLT ANTAL (FÖREGÅENDE RULLANDE ÅR).          
060300                                                                          
060400     INITIALIZE W-SUARTSJK-FVV                                            
060500     COMPUTE W-SUARTSJK-FVV ROUNDED =                                     
060600     SORTWS-SULEVANT * SORTWS-PRARTSJK                                    
060700                                                                          
060800     COMPUTE WS-SUARTSJK-FVV =                                            
060900     WS-SUARTSJK-FVV + W-SUARTSJK-FVV                                     
061000     COMPUTE WS-SUARTFSG-FVV =                                            
061100     WS-SUARTFSG-FVV + SORTWS-SUARTFSG                                    
061200     COMPUTE WS-SULEVANT-FVV =                                            
061300     WS-SULEVANT-FVV + SORTWS-SULEVANT.                                   
061400                                                                          
061500                                                                          
061600 EC-SAMLA-VV SECTION.                                                     
061700                                                                          
061800***BERÄKNAR SJÄLVKOSTNAD PER VECKOUPPGIFT FÖR ARTIKEL,                    
061900***SAMLAR VECKOUPPGIFTER PER ARTIKEL ANGÅEDE SJÄLVKOSTNAD,                
062000***FÖRSÄLJNINGSVÄRDE OCH FÖRSÅLT ANTAL (SENASTE RULLANDE ÅR).             
062100                                                                          
062200     INITIALIZE W-SUARTSJK-VV                                             
062300     COMPUTE W-SUARTSJK-VV ROUNDED =                                      
062400     SORTWS-SULEVANT * SORTWS-PRARTSJK                                    
062500                                                                          
062600     COMPUTE WS-SUARTSJK-VV =                                             
062700     WS-SUARTSJK-VV + W-SUARTSJK-VV                                       
062800     COMPUTE WS-SUARTFSG-VV =                                             
062900     WS-SUARTFSG-VV + SORTWS-SUARTFSG                                     
063000     COMPUTE WS-SULEVANT-VV =                                             
063100     WS-SULEVANT-VV + SORTWS-SULEVANT.                                    
063200                                                                          
063300                                                                          
063400 ED-SJK-TG-FVV SECTION.                                                   
063500                                                                          
063600***  BERÄKNAR TÄCKNINGSBIDRAG FÖR SÖKT ARTIKEL OCH SÖKTA                  
063700***  VECKOR. OM TÄCKNINGSBIDRAGET UNDERSTIGER -99.9% ELLER                
063800***  ÖVERSTIGER 99.9%, ANGES TÄCKNINGSBIDRAGET SOM -99.9%                 
063900***  RESP 99.9%. (FÖREGÅENDE RULLANDE ÅR)                                 
064000                                                                          
064100     COMPUTE WS-RETOTBV-FVV ROUNDED =                                     
064200     100 * (( WS-SUARTFSG-FVV -                                           
064300     WS-SUARTSJK-FVV ) / WS-SUARTFSG-FVV).                                
064400                                                                          
064500     IF WS-RETOTBV-FVV < - 99.9                                           
064600        MOVE -99.90 TO WS-RETOTBV-FVV                                     
064700     ELSE                                                                 
064800        IF WS-RETOTBV-FVV > 99.9                                          
064900           MOVE 99.90 TO WS-RETOTBV-FVV                                   
065000        END-IF                                                            
065100     END-IF.                                                              
065200                                                                          
065300 EE-SJK-TG-VV SECTION.                                                    
065400                                                                          
065500***  BERÄKNAR TÄCKNINGSBIDRAG FÖR SÖKT ARTIKEL OCH SÖKTA                  
065600***  VECKOR. OM TÄCKNINGSBIDRAGET UNDERSTIGER -99.9% ELLER                
065700***  ÖVERSTIGER 99.9%, ANGES TÄCKNINGSBIDRAGET SOM -99.9%                 
065800***  RESP 99.9%. (SENASTE RULLANDE ÅR)                                    
065900                                                                          
066000     COMPUTE WS-RETOTBV-VV ROUNDED =                                      
066100     100 * (( WS-SUARTFSG-VV -                                            
066200     WS-SUARTSJK-VV )  / WS-SUARTFSG-VV).                                 
066300                                                                          
066400     IF WS-RETOTBV-VV < - 99.9                                            
066500        MOVE -99.90 TO WS-RETOTBV-VV                                      
066600     ELSE                                                                 
066700        IF WS-RETOTBV-VV > 99.9                                           
066800           MOVE 99.90 TO WS-RETOTBV-VV                                    
066900        END-IF                                                            
067000     END-IF.                                                              
067100                                                                          
067200 EF-FLYTTA-TILL-UTSKRIFT-WU079 SECTION.                                   
067300                                                                          
067400     MOVE WS79-001-GRUPP       TO WU079-001-GRUPP                         
067500                                                                          
067600     MOVE WS79-003-GRUPP       TO WU079-003-GRUPP                         
067700                                                                          
067800     MOVE WS-SULEVANT-FVV      TO WU079-SULEVANT-FVV                      
067900     MOVE WS-SUARTFSG-FVV      TO WU079-SUARTFSG-FVV                      
068000     MOVE WS-RETOTBV-FVV       TO WU079-RETOTBV-FVV                       
068100     MOVE WS-SUARTSJK-FVV      TO WU079-SUARTSJK-FVV                      
068200     MOVE WS-SULEVANT-VV       TO WU079-SULEVANT-VV                       
068300     MOVE WS-SUARTFSG-VV       TO WU079-SUARTFSG-VV                       
068400     MOVE WS-RETOTBV-VV        TO WU079-RETOTBV-VV                        
068500     MOVE WS-SUARTSJK-VV       TO WU079-SUARTSJK-VV                       
068600     .                                                                    
068700                                                                          
068800 S01-LAS-77-FIL   SECTION.                                                
068900     SKIP2                                                                
069000     READ W33077 INTO WI77-AREA                                           
069100     AT END                                                               
069200       MOVE JA                 TO W33077-EOF                              
069300     END-READ                                                             
069400                                                                          
069500     IF W33077-EOF = NEJ                                                  
069600       MOVE '    '             TO POSTSUM-TRANSTYP                        
069700       MOVE 'W33077'           TO POSTSUM-FDNAMN                          
069800       MOVE 'W33078D2'         TO POSTSUM-DDNAMN2                         
069900       CALL POSTSUM            USING POSTSUM-PARM POSTSUM-PARM2           
070000     END-IF                                                               
070100     .                                                                    
070200     EJECT                                                                
070300                                                                          
070400 S02-LAS-35-FIL SECTION.                                                  
070500     SKIP2                                                                
070600     READ W33035S INTO WI35-AREA                                          
070700     AT END                                                               
070800       MOVE JA                 TO W33035-EOF                              
070900     END-READ                                                             
071000                                                                          
071100     IF W33035-EOF = NEJ                                                  
071200       MOVE '    '             TO POSTSUM-TRANSTYP                        
071300       MOVE 'W33035'           TO POSTSUM-FDNAMN                          
071400       MOVE 'W33078D1'         TO POSTSUM-DDNAMN2                         
071500       CALL POSTSUM            USING POSTSUM-PARM POSTSUM-PARM2           
071600     END-IF                                                               
071700     .                                                                    
071800     EJECT                                                                
071900                                                                          
072000 S03-SORT-RETURN SECTION.                                                 
072100     SKIP2                                                                
072200     RETURN SORTFIL INTO WS-SORTERAD-AREA                                 
072300     AT END                                                               
072400       MOVE JA                 TO SORT-EOF                                
072500     END-RETURN                                                           
072600                                                                          
072700     IF SORT-EOF = NEJ                                                    
072800       MOVE 'SORT'             TO POSTSUM-TRANSTYP                        
072900       MOVE '      '           TO POSTSUM-FDNAMN                          
073000       MOVE '        '         TO POSTSUM-DDNAMN2                         
073100       CALL POSTSUM            USING POSTSUM-PARM POSTSUM-PARM2           
073200     END-IF                                                               
073300     .                                                                    
073400     EJECT                                                                
073500                                                                          
073600 S04-SKRIV-U279 SECTION.                                                  
073700     SKIP1                                                                
073800     WRITE U279-POST           FROM WU279-AREA                            
073900     MOVE '279'                TO POSTSUM-TRANSTYP                        
074000     MOVE 'W33078D3'           TO POSTSUM-DDNAMN2                         
074100     MOVE 'W33079'             TO POSTSUM-FDNAMN                          
074200     CALL POSTSUM              USING POSTSUM-PARM POSTSUM-PARM2           
074300     .                                                                    
074400 S05-SKRIV-U379 SECTION.                                                  
074500     SKIP1                                                                
074600     WRITE U379-POST           FROM WU379-AREA                            
074700     MOVE '379'                TO POSTSUM-TRANSTYP                        
074800     MOVE 'W33078D3'           TO POSTSUM-DDNAMN2                         
074900     MOVE 'W33079'             TO POSTSUM-FDNAMN                          
075000     CALL POSTSUM              USING POSTSUM-PARM POSTSUM-PARM2           
075100     .                                                                    
075200 S06-SKRIV-U079 SECTION.                                                  
075300     SKIP1                                                                
075400     WRITE U79-POST            FROM WU079-AREA                            
075500     MOVE '079'                TO POSTSUM-TRANSTYP                        
075600     MOVE 'W33078D3'           TO POSTSUM-DDNAMN2                         
075700     MOVE 'W33079'             TO POSTSUM-FDNAMN                          
075800     CALL POSTSUM              USING POSTSUM-PARM POSTSUM-PARM2           
075900     .                                                                    
076000 Z-FINIT SECTION.                                                         
076100     SKIP2                                                                
076200     CLOSE W33035S                                                        
076300           W33077                                                         
076400           W33079                                                         
076500     MOVE 'S'                  TO POSTSUM-OPKOD                           
076600     CALL POSTSUM              USING POSTSUM-PARM POSTSUM-PARM2           
076700     .                                                                    
