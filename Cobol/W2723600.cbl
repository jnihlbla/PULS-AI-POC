000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2723600.                                                
000400*AUTHOR.         JOHAN NIHLBLAD.                                          
000500*DATE-WRITTEN.   NOV-15.                                                  
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        SKAPAR EN FIL FÖR ATT I EFTERFÖLJANDE PGM                        
001100*        ARV AV DATA VID ERSÄTTNINGAR                                     
001300*                                                                         
001400*        SKAPAR EN FIL FÖR UPPDATERING (W27236)                           
001500*                                                                         
001800*    ABENDCODES:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- FIL MED SAMTLIGA ARTIKLAR FÖR UPPFÖLJNING                  
003100     SELECT W27236                     ASSIGN TO W27236D1.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700     SKIP3                                                                
003800 FD  W27236                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  POST -COPY W27236 -PRE UT-     -L.                                   
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500     SKIP2                                                                
004600*    -COPY WY2000W1                                                       
004700     SKIP3                                                                
004800 77  IDPGM                       PIC X(8)    VALUE 'W2723600'.            
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100 77  DEFINITIV                   PIC S9      VALUE +1  COMP-3.            
005200 77  BEHANDLA-SW                 PIC X       VALUE 'J'.                   
005300     88  BEHANDLA                            VALUE 'J'.                   
005400     88  BEHANDLA-EJ                         VALUE 'N'.                   
005510 77  TILLK-SW                    PIC X       VALUE 'N'.                   
005520     88  TILLK                               VALUE 'J'.                   
005530     88  EJ-TILLK                            VALUE 'N'.                   
005540                                                                          
005600                                                                          
005700 01  ARBETSFALT.                                                          
005800     03 IX                       PIC 9(2)    VALUE ZERO.                  
005900     03 IX-2                     PIC 9(2)    VALUE ZERO.                  
006000     03 WS-FORSTA-DAT            PIC 9(6)    VALUE ZERO.                  
006100     03 WS-ANDRA-DAT             PIC 9(6)    VALUE ZERO.                  
006200     03 WS-SLUT-DAT              PIC 9(6)    VALUE ZERO.                  
006300     03  WS-PRIS                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
006400     03  WS-PRIS-K6              PIC S9(7)V9(2) VALUE ZERO COMP-3.        
007100     03 WS-KVPB-SEP              PIC S9(9)V9(2)                           
007200                                             COMP-3  VALUE ZERO.          
007210     03 WS-IDDC-REF              PIC X(2)    VALUE SPACE.                 
007220     03 WS-DAGAR-LEDT            PIC 9(3)    VALUE ZERO.                  
007230     03 WS-IDPERSON-BUY          PIC 9(3)    VALUE ZERO.                  
007240     03 WS-KDREFSTA              PIC X(1)    VALUE SPACE.                 
007250     03 WS-FLREFILL              PIC X(1)    VALUE SPACE.                 
007251     03 WS-FLFLYG                PIC X(1)    VALUE SPACE.                 
007260     03 WS-FLWILSON              PIC X(1)    VALUE SPACE.                 
007270     03 WS-KDERS                 PIC 9(2)    VALUE ZERO.                  
007280     03 WS-IDARTNR               PIC 9(9)    VALUE ZERO.                  
007300                                                                          
007400 01  ERRTEXT.                                                             
007500     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
007600     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
007700                                                                          
007800 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
007900 01  FILLER REDEFINES TODAYS-DATE.                                        
008000     03  TODAYS-DATE-YEAR        PIC 9(2).                                
008100     03  TODAYS-DATE-MONTH       PIC 9(2).                                
008200     03  TODAYS-DATE-DAY         PIC 9(2).                                
008300*                                                                         
008400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008500 01  FILLER REDEFINES DAGENS-DATUM.                                       
008600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008900*                                                                         
009000 01  DAGENS-DATUM2               PIC 9(5)    VALUE ZERO.                  
009100 01  FILLER REDEFINES DAGENS-DATUM2.                                      
009303     03  DAGENS-DATUM2-AA         PIC 9(2).                               
009403     03  DAGENS-DATUM2-VV         PIC 9(2).                               
009503     03  DAGENS-DATUM2-D          PIC 9(1).                               
009600*                                                                         
009700     EJECT                                                                
009800*      --- VALID IDDC CODES                                               
009900*                                                                         
010000*01    -COPY WWDC99                                                       
010100       EJECT                                                              
010200*01    -COPY WWDCKONS                                                     
010300       EJECT                                                              
010400 01  GENERAL-SUBPROGRAM.                                                  
010500*                                                                         
010600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
011100     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011110     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
011200     EJECT                                                                
011300*    --- PARAMETRAR TILL POSTSUM                                          
011400*                                                                         
011500*01  -COPY W0005   -PRE  POSTSUM-                                         
011600     EJECT                                                                
011610*    --- PARAMETERS FOR WZ20DAYS SUBPROGRAM                               
011620*01  -COPY WZ20DAYS                                                       
011630     EJECT                                                                
011700*    --- PARAMETRAR TILL DATKORT                                          
011800*                                                                         
011900 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27236'.              
012000     SKIP2                                                                
012100 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
012200     SKIP2                                                                
012300*01  -COPY WDATKORT                                                       
012400     EJECT                                                                
012500*    --- PARAMETRAR TILL WDATKONV                                         
012600*                                                                         
012700*01  -COPY WDATAREA                                                       
012800     EJECT                                                                
013300 01  UT-AREA-START               PIC X(24)   VALUE                        
013400                                             'UT-AREA-START'.             
013500     SKIP2                                                                
013600                                                                          
013700*01  AREA -COPY W27236     -PRE UT-                                       
013800*                                                                         
013900     EJECT                                                                
014000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014100     SKIP3                                                                
014200 01  KEYS-TILL-DLI.                                                       
014300     03  W-IDARTNR-X.                                                     
014400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014500     03  W-IDDC-X.                                                        
014600         05  W-IDDC              PIC X(02)   VALUE SPACE.                 
014610     03  W-IDDC-REF-X.                                                    
014620         05  W-IDDC-REF          PIC X(02)   VALUE SPACE.                 
014700     03  W-IDDC-B616-X.                                                   
014800         05  W-IDDC-B616         PIC X(02)   VALUE SPACE.                 
015000     03  W-IDLAND-X.                                                      
015100         05  W-IDLAND            PIC X(02)   VALUE SPACE.                 
015200     03  W-IDLEVNR-X.                                                     
015300         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
015400     03  W-KDSEGKEY-X.                                                    
015500         05  W-KDSEGKEY          PIC X(01)   VALUE '1'.                   
015600     SKIP2                                                                
015700*    --- STATUS-KOD FRÅN IMS                                              
015800 01  STATUS-WS                   PIC XX.                                  
015900     88  SEGMENT-FOUND                       VALUE '  '.                  
016000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
016100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
016200     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
016300     88  IMS-NOT-OK                          VALUE 'XD'.                  
016400     SKIP2                                                                
016500 01  GOOD-STATUSCODES.                                                    
016600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016700     SKIP3                                                                
016800 01  SSA1                        PIC X(64).                               
016900 01  SSA2                        PIC X(64).                               
017000     EJECT                                                                
017100*    --- IMS FUNCTION CODES                                               
017200*01  -COPY W0003                                                          
017300     EJECT                                                                
017400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
017500 01  DLI-IO-WDK601.                                                       
017600*    03  -COPY WDK601                                                     
017700     EJECT                                                                
017800                                                                          
018000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
019000 01  DLI-IO-WDK611.                                                       
019100*    03  -COPY WDK611                                                     
019200     EJECT                                                                
019300                                                                          
019400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK629'.                      
019500 01  DLI-IO-WDK629.                                                       
019600*    03  -COPY WDK629                                                     
019700     EJECT                                                                
019800                                                                          
019903 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601-2'.                    
020003 01  DLI-IO-WDK601-2.                                                     
020103*    03  -COPY WDK601  -PRE TILL-                                         
020203     EJECT                                                                
020303                                                                          
020403 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611-2'.                    
020503 01  DLI-IO-WDK611-2.                                                     
020603*    03  -COPY WDK611  -PRE TILL-                                         
020703     EJECT                                                                
020803                                                                          
020903 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK629-2'.                    
021003 01  DLI-IO-WDK629-2.                                                     
021103*    03  -COPY WDK629  -PRE TILL-                                         
021203     EJECT                                                                
021303                                                                          
021403 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD701'.                      
021503 01  DLI-IO-WDD701.                                                       
021603*    03  -COPY WDD701                                                     
021703     EJECT                                                                
021803                                                                          
021903 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD702'.                      
022003 01  DLI-IO-WDD702.                                                       
022103*    03  -COPY WDD702                                                     
022203     EJECT                                                                
022303                                                                          
022403 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDD704'.                      
022503 01  DLI-IO-WDD704.                                                       
022603*    03  -COPY WDD704                                                     
022703     EJECT                                                                
022803                                                                          
022903 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
023003 01   DLI-IO-AREA-B601.                                                   
023103*     03  -COPY WDB601                                                    
023203     EJECT                                                                
023303 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
023403 01   DLI-IO-AREA-B616.                                                   
023503*     03  -COPY WDB616                                                    
023603     EJECT                                                                
023703                                                                          
023803 LINKAGE SECTION.                                                         
023903                                                                          
024003*01  -COPY W0009   -PRE MSG-                                              
024103     EJECT                                                                
024203*01  -COPY W0008  -PRE WDK6-                                              
024303     05  FILLER                  PIC X.                                   
024403     EJECT                                                                
024503*01  -COPY W0008  -PRE WDD7-                                              
024603     05  FILLER                  PIC X.                                   
024703     EJECT                                                                
024803*01  -COPY W0008  -PRE WDK62-                                             
024903     05  FILLER                  PIC X.                                   
025003     EJECT                                                                
025103*01  -COPY W0008      -PRE WDB6-                                          
025203     05  FILLER                  PIC X.                                   
025303     EJECT                                                                
025403 PROCEDURE DIVISION  USING                                                
025503                           MSG-PCB  WDK6-PCB WDD7-PCB                     
025603                           WDK62-PCB WDB6-PCB.                            
025703     ENTRY 'DLITCBL' USING                                                
025803                           MSG-PCB  WDK6-PCB WDD7-PCB                     
025903                           WDK62-PCB WDB6-PCB.                            
026003                                                                          
026103     PERFORM A-INIT                                                       
026203     PERFORM IMS-GN-WDK629                                                
026303     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                      
026403       IF CREF-FLPB-FLYTT = 'N'                                           
026503         PERFORM IMS-GNP-WDK611                                           
026603         PERFORM IMS-GNP-WDK601                                           
026703         MOVE ART-IDARTNR    TO W-IDARTNR                                 
026704                                WS-IDARTNR                                
026705         MOVE CLAG-KDERS     TO WS-KDERS                                  
026803         IF (WS-KDERS (2:1) = 1)                                          
026903         OR (WS-KDERS (2:1) = 2)                                          
027003         OR (WS-KDERS (2:1) = 3)                                          
027103         OR (WS-KDERS (2:1) = 4)                                          
027203         OR (WS-KDERS (2:1) = 5)                                          
027303         OR (WS-KDERS (2:1) = 6)                                          
027403         OR (WS-KDERS (2:1) = 7)                                          
028003           PERFORM C-KOLLA-DAT-FLYTT                                      
034500         END-IF                                                           
034600       END-IF                                                             
034700       PERFORM IMS-GN-WDK629                                              
034800     END-PERFORM                                                          
034900                                                                          
035000     PERFORM Z-FINIT                                                      
035100                                                                          
036000     MOVE ZERO TO RETURN-CODE                                             
036100     GOBACK                                                               
036200     .                                                                    
036300     EJECT                                                                
036400 A-INIT SECTION.                                                          
036500                                                                          
036600     OPEN OUTPUT W27236                                                   
036700                                                                          
036800     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
036900     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
037003                         DAGENS-DATUM2-AA                                 
037100     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
037300     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
037403     MOVE D-VECKA     TO DAGENS-DATUM2-VV                                 
037503     MOVE D-DAGNR     TO DAGENS-DATUM2-D                                  
037700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
037800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
037900                                                                          
038000     ACCEPT TODAYS-DATE FROM DATE                                         
038100     .                                                                    
039000     EJECT                                                                
044700 C-KOLLA-DAT-FLYTT SECTION.                                               
044800                                                                          
044900     MOVE JA     TO BEHANDLA-SW                                           
045000     MOVE ZERO   TO WS-FORSTA-DAT                                         
045100                    WS-ANDRA-DAT                                          
045203                    WS-KVPB-SEP                                           
045303                    WS-IDPERSON-BUY                                       
045403     MOVE SPACE  TO WS-KDREFSTA                                           
045503                    WS-FLREFILL                                           
045603                    WS-FLWILSON                                           
045703                                                                          
046200     PERFORM CA-HAMTA-FORSTA-DAT                                          
046300                                                                          
046400     IF BEHANDLA                                                          
046500       PERFORM CB-DAT-MINUS-4V                                            
047000     END-IF                                                               
047100                                                                          
047200     IF BEHANDLA                                                          
047300       IF WS-KDERS (2:1) = 3                                              
047500       OR WS-KDERS (2:1) = 6                                              
047600         PERFORM CC-DAT-MINUS-LEDT                                        
047700       ELSE                                                               
047800         MOVE WS-ANDRA-DAT TO WS-SLUT-DAT                                 
047900       END-IF                                                             
048000     END-IF                                                               
048100                                                                          
048200     IF BEHANDLA                                                          
048300       IF WS-SLUT-DAT <= DAGENS-DATUM                                     
048400         PERFORM CD-BEHANDLA-ART                                          
048500       END-IF                                                             
048600     END-IF                                                               
048700                                                                          
048800     .                                                                    
048900     EJECT                                                                
049000                                                                          
049100 CA-HAMTA-FORSTA-DAT SECTION.                                             
049200                                                                          
049300     IF WS-KDERS (2:1) = 1                                                
049400     OR WS-KDERS (2:1) = 2                                                
049500     OR WS-KDERS (2:1) = 5                                                
049600     OR WS-KDERS (2:1) = 7                                                
049700       IF CLAG-TISTOREF = 0                                               
049800         MOVE NEJ TO BEHANDLA-SW                                          
049900       ELSE                                                               
050000         IF CLAG-TISTOREF > 50000                                         
050100***********FEJK DATUM FÖR DATUM FÖRE 2000 TALET                           
050200           MOVE 030101        TO WS-FORSTA-DAT                            
050300         ELSE                                                             
050310           MOVE 'AAVVD' TO DAT-KDDATFORM                                  
050320           MOVE CLAG-TISTOREF    TO DAT-I-TIDATUM                         
050330                                                                          
050340           CALL WDATKONV USING DAT-KDDATFORM                              
050350                         DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR           
050360                                                                          
050370           IF DAT-KDSVAR-OK                                               
050380              MOVE DAT-TIAAMMDD TO WS-FORSTA-DAT                          
050390           ELSE                                                           
050391              MOVE NEJ TO BEHANDLA-SW                                     
050392           END-IF                                                         
050500         END-IF                                                           
050600       END-IF                                                             
050700     END-IF                                                               
050800     IF WS-KDERS(2:1) = 3                                                 
050900     OR WS-KDERS(2:1) = 4                                                 
051000     OR WS-KDERS(2:1) = 6                                                 
051100       PERFORM IMS-GU-WDD704                                              
051200       IF SEGMENT-FOUND                                                   
051300         IF TIERSDAT-PREL-C1 = 0                                          
051400           MOVE NEJ TO BEHANDLA-SW                                        
051500         ELSE                                                             
051600           IF TIERSDAT-PREL-C1 > 50000                                    
051700***********FEJK DATUM FÖR DATUM FÖRE 2000 TALET                           
051800             MOVE 03011   TO WS-FORSTA-DAT                                
051900           ELSE                                                           
052000             MOVE 'AAVVD' TO DAT-KDDATFORM                                
052100             MOVE TIERSDAT-PREL-C1 TO DAT-I-TIDATUM                       
052200                                                                          
052300             CALL WDATKONV USING DAT-KDDATFORM                            
052400                           DAT-I-TIDATUM DAT-O-TIDATUM DAT-KDSVAR         
052500                                                                          
052600             IF DAT-KDSVAR-OK                                             
052700                MOVE DAT-TIAAMMDD TO WS-FORSTA-DAT                        
052800             ELSE                                                         
052900                MOVE NEJ TO BEHANDLA-SW                                   
053000             END-IF                                                       
053100           END-IF                                                         
053200         END-IF                                                           
053300       END-IF                                                             
053400     END-IF                                                               
053500     .                                                                    
053600     EJECT                                                                
053700                                                                          
053800 CB-DAT-MINUS-4V SECTION.                                                 
053900                                                                          
054000     MOVE SPACE                   TO DAYS-TIDATE1                         
054100     MOVE 'YYMMDD'                TO DAYS-KDDATFMT1                       
054200     MOVE 'YYMMDD'                TO DAYS-KDDATFMT2                       
054300*****MINUS 4 VECKOR = 28 DAGAR                                            
054400     MOVE 28                      TO DAYS-KVDAYS                          
054500     MOVE WS-FORSTA-DAT           TO DAYS-TIDATE2                         
054600                                     DAYS-IDCALEND                        
054700     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
054800*                                                                         
054900     IF DAYS-KDRC = 8                                                     
055000       MOVE NEJ TO BEHANDLA-SW                                            
055100     ELSE                                                                 
055200       MOVE DAYS-TIDATE1(1:6) TO WS-ANDRA-DAT                             
055300     END-IF                                                               
055400     .                                                                    
055500     EJECT                                                                
055600                                                                          
055700 CC-DAT-MINUS-LEDT SECTION.                                               
055800                                                                          
055900     MOVE '11'           TO W-IDDC                                        
056000     MOVE CLAG-IDDC-REF  TO W-IDDC-REF                                    
056100     PERFORM IMS-GU-WDB616                                                
056200     IF SEGMENT-FOUND                                                     
056300       IF CREF-FLFLYG = 'J'                                               
056400         MOVE REF-KVDLTID-AIRREQ  TO WS-DAGAR-LEDT                        
056500       ELSE                                                               
056600         MOVE REF-KVDLTID-TOT     TO WS-DAGAR-LEDT                        
056700       END-IF                                                             
056800     ELSE                                                                 
056900       MOVE NEJ TO BEHANDLA-SW                                            
057000     END-IF                                                               
057100     MOVE SPACE                   TO DAYS-TIDATE1                         
057200     MOVE 'YYMMDD'                TO DAYS-KDDATFMT1                       
057300     MOVE 'YYMMDD'                TO DAYS-KDDATFMT2                       
057500     MOVE WS-DAGAR-LEDT           TO DAYS-KVDAYS                          
057600     MOVE WS-ANDRA-DAT            TO DAYS-TIDATE2                         
057700                                     DAYS-IDCALEND                        
057800     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
057900*                                                                         
058000     IF DAYS-KDRC = 8                                                     
058100       MOVE NEJ TO BEHANDLA-SW                                            
058200     ELSE                                                                 
058300       MOVE DAYS-TIDATE1(1:6) TO WS-SLUT-DAT                              
058400     END-IF                                                               
058500     .                                                                    
058600     EJECT                                                                
058700                                                                          
058801 CD-BEHANDLA-ART SECTION.                                                 
058901                                                                          
059001     MOVE CLAG-KVPB-SEP     TO WS-KVPB-SEP                                
059101     MOVE CREF-FLWILSON     TO WS-FLWILSON                                
059201     MOVE CREF-IDPERSON-BUY TO WS-IDPERSON-BUY                            
059301     MOVE CREF-FLREFILL     TO WS-FLREFILL                                
059402     MOVE CREF-KDREFSTA     TO WS-KDREFSTA                                
059403     MOVE CREF-FLFLYG       TO WS-FLFLYG                                  
059502                                                                          
059602     MOVE NEJ TO  TILLK-SW                                                
059702     PERFORM IMS-GU-WDD701                                                
059802     IF SEGMENT-FOUND                                                     
059902       PERFORM IMS-GNP-WDD702                                             
060002       PERFORM UNTIL SEGMENT-MISSING                                      
060104         IF FLTEXT = 'N'                                                  
060204           MOVE IDARTNR-TILLK TO W-IDARTNR                                
060304           PERFORM IMS-GU-WDK601-2                                        
060404           IF SEGMENT-FOUND                                               
060504             IF TILL-ART-KDERS-UTG = 0                                    
060604               PERFORM IMS-GNP-WDK629-2                                   
060704               IF SEGMENT-FOUND                                           
060804                 MOVE JA              TO TILLK-SW                         
060904                 MOVE JA              TO UT-FLTILLK                       
061004                 MOVE WS-KVPB-SEP     TO UT-KVPB-SEP                      
061104                 MOVE WS-FLWILSON     TO UT-FLWILSON                      
061204                 MOVE WS-IDPERSON-BUY TO UT-IDPERSON-BUY                  
061304                 MOVE WS-FLREFILL     TO UT-FLREFILL                      
061404                 MOVE WS-KDREFSTA     TO UT-KDREFSTA                      
061405                 MOVE WS-FLFLYG       TO UT-FLFLYG                        
061504                 MOVE DAGENS-DATUM2   TO UT-TIPBDAT                       
061604                 MOVE TILL-ART-IDARTNR TO UT-IDARTNR                      
061704                 MOVE SPACE           TO UT-FLPB-FLYTT                    
061804                 MOVE ZERO            TO UT-TIREFSTO                      
061904                 MOVE SPACE           TO UT-FLREFBEO                      
061905                 MOVE DAGENS-DATUM    TO UT-DAGENS-DATUM                  
062004                 PERFORM S11-SKRIV-W27236                                 
062104               END-IF                                                     
062204             END-IF                                                       
062304           END-IF                                                         
062404         END-IF                                                           
062405         PERFORM IMS-GNP-WDD702                                           
062504       END-PERFORM                                                        
062604     END-IF                                                               
062804     PERFORM CDA-BEHANDLA-ERSATT                                          
063004     .                                                                    
063104     EJECT                                                                
063204                                                                          
063304 CDA-BEHANDLA-ERSATT SECTION.                                             
063404                                                                          
063504     MOVE NEJ                         TO UT-FLTILLK                       
063604     MOVE ZERO                        TO UT-KVPB-SEP                      
063704     MOVE SPACE                       TO UT-FLWILSON                      
063804     MOVE ZERO                        TO UT-IDPERSON-BUY                  
063904     MOVE SPACE                       TO UT-FLREFILL                      
064004     MOVE SPACE                       TO UT-KDREFSTA                      
064005     MOVE SPACE                       TO UT-FLFLYG                        
064104     MOVE ZERO                        TO UT-TIPBDAT                       
064105     MOVE JA                          TO UT-FLPB-FLYTT                    
064204     MOVE ART-IDARTNR                 TO UT-IDARTNR                       
064205     IF TILLK                                                             
064404       MOVE ZERO                      TO UT-TIREFSTO                      
064405     ELSE                                                                 
064407       MOVE CREF-TIREFSTO             TO UT-TIREFSTO                      
064408     END-IF                                                               
064504     MOVE 'S'                         TO UT-FLREFBEO                      
064505     MOVE DAGENS-DATUM                TO UT-DAGENS-DATUM                  
064604     PERFORM S11-SKRIV-W27236                                             
064704     .                                                                    
064804     EJECT                                                                
064904 Z-FINIT SECTION.                                                         
065004                                                                          
065104     CLOSE W27236                                                         
065204     SKIP2                                                                
065304     MOVE 'S' TO POSTSUM-OPKOD                                            
065404     CALL POSTSUM USING POSTSUM-PARM                                      
065504     .                                                                    
065604     EJECT                                                                
065704 S11-SKRIV-W27236 SECTION.                                                
065804     SKIP2                                                                
065904     WRITE UT-POST FROM UT-AREA                                           
066004                                                                          
066104     MOVE UT-IDARTNR TO POSTSUM-TRANSTYP                                  
066204     MOVE 'W27236 '  TO POSTSUM-FDNAMN                                    
066304     MOVE 'W27236D1' TO POSTSUM-DDNAMN2                                   
066404     CALL POSTSUM    USING POSTSUM-PARM                                   
066504     .                                                                    
066604     EJECT                                                                
066704* --- IMS SECTIONS  ---                                                   
066804     SKIP3                                                                
066904     EJECT                                                                
067004 IMS-GN-WDK629 SECTION.                                                   
067104     MOVE 'WDK629 '        TO SSA1                                        
067204     MOVE '  GEGB'           TO GOOD-STATUSCODES                          
067304     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-WDK629 SSA1                    
067404     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
067504     PERFORM IMS-STATUSCHECK                                              
067604     SKIP3                                                                
067704     .                                                                    
067804     EJECT                                                                
067904 IMS-GNP-WDK611 SECTION.                                                  
068004     MOVE 'WDK611 '        TO SSA1                                        
068104     MOVE '  '             TO GOOD-STATUSCODES                            
068204     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
068304     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
068404     PERFORM IMS-STATUSCHECK                                              
068504     SKIP3                                                                
068604     .                                                                    
068704     EJECT                                                                
068804 IMS-GNP-WDK601 SECTION.                                                  
068904     MOVE 'WDK601 '        TO SSA1                                        
069004     MOVE '  '             TO GOOD-STATUSCODES                            
069104     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK601 SSA1                   
069204     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
069304     PERFORM IMS-STATUSCHECK                                              
069404     SKIP3                                                                
069504     .                                                                    
069604     EJECT                                                                
069704 IMS-GU-WDK601-2 SECTION.                                                 
069804                                                                          
069904     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
070004          DELIMITED BY SIZE INTO SSA1                                     
070104     MOVE '  GE' TO GOOD-STATUSCODES                                      
070204     CALL CBLTDLI USING GU WDK62-PCB DLI-IO-WDK601-2 SSA1                 
070304     MOVE WDK62-STATUS-CODE TO STATUS-WS                                  
070404     PERFORM IMS-STATUSCHECK                                              
070504     .                                                                    
070604     EJECT                                                                
070704                                                                          
071603 IMS-GNP-WDK629-2 SECTION.                                                
071703     MOVE 'WDK629 '        TO SSA1                                        
071803     MOVE '  GE'           TO GOOD-STATUSCODES                            
071903     CALL CBLTDLI USING GNP WDK62-PCB DLI-IO-WDK629-2 SSA1                
072003     MOVE WDK62-STATUS-CODE TO STATUS-WS                                  
072103     PERFORM IMS-STATUSCHECK                                              
072203     SKIP3                                                                
072303     .                                                                    
072403     EJECT                                                                
072504 IMS-GU-WDD701 SECTION.                                                   
072604                                                                          
072704     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
072804          DELIMITED BY SIZE INTO SSA1                                     
072904     MOVE '  GE' TO GOOD-STATUSCODES                                      
073004     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD701 SSA1                    
073104     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
073204     PERFORM IMS-STATUSCHECK                                              
073304     .                                                                    
073404     EJECT                                                                
073504                                                                          
073704 IMS-GNP-WDD702 SECTION.                                                  
073804                                                                          
073904     MOVE 'WDD702   ' TO SSA1                                             
074004     MOVE '  GE' TO GOOD-STATUSCODES                                      
074104     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD702 SSA1                   
074204     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
074304     PERFORM IMS-STATUSCHECK                                              
074404     .                                                                    
074504     EJECT                                                                
074603 IMS-GU-WDD704 SECTION.                                                   
074703                                                                          
074803     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
074903          DELIMITED BY SIZE INTO SSA1                                     
075003     MOVE 'WDD704   ' TO SSA2                                             
075103     MOVE '  ' TO GOOD-STATUSCODES                                        
075203     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD704 SSA1 SSA2               
075303     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
075403     PERFORM IMS-STATUSCHECK                                              
075503     .                                                                    
075603     EJECT                                                                
075703 IMS-GU-WDB616    SECTION.                                                
075803     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
075903          DELIMITED BY SIZE INTO SSA1                                     
076003     STRING 'WDB616  (IDDCREF  =' W-IDDC-REF-X ')'                        
076103          DELIMITED BY SIZE INTO SSA2                                     
076203     MOVE '  ' TO GOOD-STATUSCODES                                        
076303     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
076403     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
076503     PERFORM IMS-STATUSCHECK                                              
076603     .                                                                    
076703     EJECT                                                                
076803                                                                          
076903 IMS-STATUSCHECK SECTION.                                                 
077003     SKIP2                                                                
077103     SET STATUS-IX TO 1                                                   
077203     SEARCH GOOD-STATUS                                                   
077303       AT END                                                             
077403         MOVE 'WRONG CODE' TO ERRTEXT-STR                                 
077503         DISPLAY ERRTEXT                                                  
077603         CALL FELLOG                                                      
077703       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
077803         CONTINUE                                                         
077903     END-SEARCH                                                           
078003     .                                                                    
079000     EJECT                                                                
080000*    -COPY WY2000P1                                                       
