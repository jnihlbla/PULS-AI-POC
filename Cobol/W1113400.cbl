000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1113400.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   95/10/30.                                                
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*        LÄSER FIL FÖR UPPDATERING AV STATNUMMER WDK6                     
001000*        - SKAPAR TRANS W1T111X TILL DISPATCHEN                           
001100*                                                                         
001200*    INDATA .                                                             
001300*                                                                         
001400*        FIL FRÅN LOGENT W11.W111B1.W11138                                
001500*                                                                         
001600*    UTDATA .                                                             
001700*                                                                         
001800*        W1I11101 + WMSGKOM                                               
001810*        UTFIL W11135  MED IDARTNR IDLEVNR KDARTURS                       
001900                                                                          
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600                                                                          
002700*                         STATNUMMER FRÅN VOLVO TRANSPORT                 
002800     SELECT W11138                     ASSIGN TO W11134D1.                
002801*                                                                         
002810*                         UTFIL MED KDARTURS                              
002820     SELECT W11135                     ASSIGN TO W11134D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP2                                                                
003400 FD  W11138                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700     SKIP2                                                                
003800*01  -COPY  ARTSTAUR      -L.                                             
003810     SKIP2                                                                
003820 FD  W11135                                                               
003830     RECORDING       F                                                    
003840     BLOCK CONTAINS  0.                                                   
003850     SKIP2                                                                
003860*01  POST  -COPY  W11135 -PRE UT-   -L.                                   
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W1113400'.            
004400 77  W-CHKP-MAX                  PIC S9(3)   VALUE +200  COMP-3.          
004500 77  W-CHKP-RAEKNARE             PIC S9(3)   VALUE +0    COMP-3.          
004600 77  W-MSG-IO-AREA-LENGTH        PIC S9(9)   VALUE +32  COMP SYNC.        
004700 77  W-MSG-IO-AREA               PIC X(32)   VALUE SPACE.                 
004800 77  W-CHKP-AREA-1-LENGTH        PIC S9(9)   VALUE +32  COMP SYNC.        
004900 77  W-CHKP-AREA-1               PIC X(32)   VALUE SPACE.                 
004901 77  JA                          PIC X       VALUE 'J'.                   
004902 77  NEJ                         PIC X       VALUE 'N'.                   
004903 77  INPUT-RAETT                 PIC X       VALUE SPACE.                 
004910 77  WS-TIUPPDAT                 PIC S9(7) COMP-3 VALUE ZERO.             
004920 77  WS-TIUPPTID                 PIC S9(9) COMP-3 VALUE ZERO.             
005000                                                                          
005100 77  W11138-EOF-SW               PIC X       VALUE 'N'.                   
005200     88  END-OF-W11138                       VALUE 'J'.                   
005300                                                                          
005400 01  WS-IDARTNR                  PIC X(9)  VALUE SPACE.                   
005401 01  IDARTNR-WS REDEFINES WS-IDARTNR PIC 9(9).                            
005402                                                                          
005440 01  WS-IDSTATNR                 PIC X(14) VALUE SPACE.                   
005441 01  FILLER REDEFINES WS-IDSTATNR.                                        
005442     03  WS-IDSTATNR1-8          PIC X(8).                                
005443     03  WS-IDSTATNR9-14         PIC X(6).                                
005444 01  WS-IDSTATNR1-9              PIC X(9) VALUE SPACE.                    
005445                                                                          
005446 01  WS-LEVID                    PIC X(9).                                
005447*01  FILLER REDEFINES WS-LEVID.                                           
005448*    03  WS-IDLEVNR-X            PIC X(5).                                
005449*    03  FILLER                  PIC X(4).                                
005450*01  WS-IDLEVNR-NUM              PIC X(5).                                
005451*01  FILLER      REDEFINES  WS-IDLEVNR-NUM.                               
005452 01  WS-IDLEVNR                  PIC X(5).                                
005453*01  IX-ALF                      PIC S9(3) COMP-3.                        
005454*01  IX-NUM                      PIC S9(3) COMP-3.                        
005460                                                                          
005468 01  FELTEXT.                                                             
005469     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005470     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005480     EJECT                                                                
005490 01  DYNAMISKA-SUBPROGRAM.                                                
005500*                                                                         
005600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005710     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
005800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005900     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
006000     EJECT                                                                
006100*    --- PARAMETRAR TILL POSTSUM                                          
006200*                                                                         
006300*01  -COPY W009CIA                                                        
006400     EJECT                                                                
006610*01  -COPY W0005 -PRE  POSTSUM-                                           
006620     EJECT                                                                
006630 01  IN-AREA-START               PIC X(24)   VALUE                        
006640                                             'IN-AREA-START'.             
006700     SKIP2                                                                
006900*                                                                         
006910 01  IN-AREA.                                                             
006911*01  -COPY ARTSTAUR -PRE IN-                                              
006912     EJECT                                                                
006913 01  UT-AREA-START               PIC X(24)   VALUE                        
006914                                             'UT-AREA-START'.             
006915     SKIP2                                                                
006916*                                                                         
006918*01  AREA   -COPY W11135   -PRE UT-                                       
007000     EJECT                                                                
007010 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007020                                                                          
007021 01  NYCKLAR-TILL-DLI.                                                    
007022     03  W-IDARTNR-X.                                                     
007023         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
007024     SKIP3                                                                
007025*    --- STATUS-KOD FRÅN IMS                                              
007026 01  STATUS-WS                   PIC XX.                                  
007027     88  SEGMENT-FINNS                       VALUE '  '.                  
007028     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
007029     88  SEGMENT-SLUT                        VALUE 'GB'.                  
007030     88  IMS-EJ-OK                           VALUE 'XD'.                  
007040                                                                          
007050 01  GODK-STATUSKODER.                                                    
007060     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007070                                                                          
007080 01  SSA1                        PIC X(64).                               
007090     EJECT                                                                
007100*    --- IMS FUNKTIONSKODER                                               
007200*01  -COPY W0003                                                          
007300     EJECT                                                                
007400 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA'.          
007500 01  DLI-IO-AREA.                                                         
007600     03  IO-AREA                 PIC X(200).                              
007700     03  WLARTC01 REDEFINES IO-AREA.                                      
007710*        05    -COPY WDK601                                               
007720     EJECT                                                                
007730*    ---  MSG INPUT-OUTPUT AREA                                           
007740 01  FILLER                      PIC X(16)   VALUE 'MSG-IO-AREA'.         
007750                                                                          
007760*01  -COPY WMSGAREA                                                       
007770     EJECT                                                                
007780     05  FILLER REDEFINES MSG-MID-OUT.                                    
007790        07  -COPY W1I11101                                                
007800     EJECT                                                                
007900*    ---  AREA FÖR W006KOM SUBMODUL                                       
008000 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
008100                                                                          
008200 01  KOM-IO-AREA.                                                         
008300*    03  -COPY WMSGKOM                                                    
008400     EJECT                                                                
008500 LINKAGE SECTION.                                                         
008600                                                                          
008700*01  -COPY W0009 -PRE MSG-                                                
008800     EJECT                                                                
008900*01  -COPY W0009 -PRE ALT-                                                
009000     EJECT                                                                
009100*01  -COPY W0009 -PRE KOMA-                                               
009200     EJECT                                                                
009300*01  -COPY W0008 -PRE ARTC-                                               
009400     05  FILLER      PIC X.                                               
009500     EJECT                                                                
009600 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB KOMA-PCB ARTC-PCB.             
009700     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB KOMA-PCB ARTC-PCB.             
009800                                                                          
009900     PERFORM A-INIT                                                       
010000     PERFORM S01-LAES-W11138                                              
010100                                                                          
010200     PERFORM UNTIL END-OF-W11138                                          
010210        IF IN-STATNR NOT = SPACE                                          
010300           PERFORM B-KOLLA-INDATA                                         
010400           IF INPUT-RAETT = NEJ                                           
010500              CONTINUE                                                    
010510           ELSE                                                           
010600              IF WS-IDSTATNR1-8 = SPACE                                   
010700                CONTINUE                                                  
010800              ELSE                                                        
010900                MOVE IDARTNR-WS TO W-IDARTNR                              
011000                PERFORM IMS-GET-ARTC01                                    
011100                IF SEGMENT-FINNS                                          
011110                   IF ART-KDERS-UTG = ZERO                                
011120                      PERFORM C-SKAPA-TRANS                               
011130                      PERFORM D-SKICKA-TRANS                              
011131                      CONTINUE                                            
011140                   END-IF                                                 
011150                END-IF                                                    
011160              END-IF                                                      
011161           END-IF                                                         
011170        END-IF                                                            
011171        IF IN-ARTIKELURSPRUNG  NOT = SPACE                                
011172           PERFORM E-URSPRUNG-UTFIL                                       
011185        END-IF                                                            
011186        PERFORM S01-LAES-W11138                                           
011190     END-PERFORM                                                          
011200                                                                          
011300     PERFORM Z-FINIT                                                      
011400                                                                          
011500     MOVE ZERO TO RETURN-CODE                                             
011600     GOBACK                                                               
011700     .                                                                    
011800     EJECT                                                                
011900 A-INIT SECTION.                                                          
012000                                                                          
012100     OPEN INPUT W11138                                                    
012110         OUTPUT W11135                                                    
012200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012300     MOVE ZERO TO W-CHKP-RAEKNARE                                         
012400     PERFORM IMS-RESTART                                                  
012500     PERFORM AA-SKAPA-HEADER                                              
012600     .                                                                    
012700     EJECT                                                                
012800 AA-SKAPA-HEADER SECTION.                                                 
012900                                                                          
013000     MOVE +54                   TO MSG-KOM-KVLL                           
013100     MOVE LOW-VALUE             TO MSG-KOM-KDZ1                           
013200                                   MSG-KOM-KDZ2                           
013300     MOVE SPACE                 TO MSG-KOM-KDTRANS                        
013400     MOVE 'W1I11101'            TO MSG-KOM-IDCPYTXT                       
013500     MOVE 'STATNR2'             TO MSG-KOM-IDSNDNOD                       
013600     MOVE IDPGM                 TO MSG-KOM-IDSNDJOB                       
013700                                                                          
013800     ACCEPT WS-TIUPPDAT FROM DATE                                         
013900     ACCEPT WS-TIUPPTID FROM TIME                                         
014000     MOVE WS-TIUPPDAT           TO MSG-KOM-TIREGDAT                       
014100     MOVE WS-TIUPPTID           TO MSG-KOM-TIKLOCK                        
014300     MOVE SPACE                 TO MSG-KOM-IDMFSMED                       
014400                                   MSG-KOM-KDSVAR                         
014500     .                                                                    
014600     EJECT                                                                
014700 B-KOLLA-INDATA SECTION.                                                  
014800                                                                          
014801     MOVE JA TO INPUT-RAETT                                               
014802                                                                          
014810     MOVE 'VO' TO CIA-IDARTPRE-IN                                         
014900     MOVE IN-ARTIKELNR TO CIA-IDARTBET-IN                                 
014902     CALL W009CIA USING CIA-W009CIA                                       
014903     IF CIA-KDSVAR = 'F'                                                  
014904        MOVE NEJ TO INPUT-RAETT                                           
014905     ELSE                                                                 
014907        MOVE CIA-IDARTNR        TO WS-IDARTNR                             
014912     END-IF                                                               
014913                                                                          
014914     MOVE 'VO' TO CIA-IDARTPRE-IN                                         
014915     MOVE IN-STATNR TO WS-IDSTATNR                                        
014916     MOVE WS-IDSTATNR1-8 TO CIA-IDARTBET-IN                               
014917     CALL W009CIA USING CIA-W009CIA                                       
014918     IF CIA-KDSVAR = 'F'                                                  
014919        MOVE NEJ TO INPUT-RAETT                                           
014920     ELSE                                                                 
014921        MOVE CIA-IDARTNR TO WS-IDSTATNR1-9                                
014930     END-IF                                                               
015210     .                                                                    
015220     EJECT                                                                
015321 C-SKAPA-TRANS SECTION.                                                   
015322                                                                          
015323     MOVE +80                   TO MSG-KVLL                               
015324     MOVE LOW-VALUE             TO MSG-KDZ1                               
015325                                   MSG-KDZ2                               
015326     MOVE 'W1T111X'             TO MSG-KDTRANS-1                          
015327     MOVE '1111'                TO MSG-IDTRANS-1                          
015328     MOVE '1'                   TO MSG-KDMFSFOR-1                         
015329                                                                          
015330     MOVE '+++++++++'           TO MID-IDARTNR-IN                         
015331     MOVE '+++++++++'           TO MID-IDSTATNR(1)                        
015332                                   MID-IDSTATNR(2)                        
015333                                   MID-IDSTATNR(4)                        
015334                                   MID-IDSTATNR(5)                        
015335     MOVE WS-IDARTNR            TO MID-IDARTNR-UT                         
015337     MOVE WS-IDSTATNR1-9        TO MID-IDSTATNR(3)                        
015341     .                                                                    
015342     EJECT                                                                
015343 D-SKICKA-TRANS SECTION.                                                  
015344                                                                          
015345     CALL W006KOM USING MSG-PCB                                           
015346                        ALT-PCB                                           
015347                        KOMA-PCB                                          
015348                        MSG-KOM-WMSGKOM                                   
015349                        MSG-IO-AREA                                       
015350                                                                          
015351     ADD +1 TO W-CHKP-RAEKNARE                                            
015352                                                                          
015353     IF W-CHKP-RAEKNARE > W-CHKP-MAX                                      
015354        PERFORM IMS-CHECKPOINT                                            
015355        MOVE +0 TO W-CHKP-RAEKNARE                                        
015356        ADD +1 TO MSG-KOM-TIKLOCK                                         
015357     END-IF                                                               
015358     .                                                                    
015359     EJECT                                                                
015360 E-URSPRUNG-UTFIL SECTION.                                                
015361                                                                          
015362     MOVE 'VO' TO CIA-IDARTPRE-IN                                         
015363     MOVE IN-ARTIKELNR TO CIA-IDARTBET-IN                                 
015364     CALL W009CIA USING CIA-W009CIA                                       
015365     IF CIA-KDSVAR = 'F'                                                  
015366        MOVE NEJ TO INPUT-RAETT                                           
015367        DISPLAY 'FELPOST-CIA ' IN-POST                                    
015368     ELSE                                                                 
015369        MOVE CIA-IDARTNR        TO WS-IDARTNR                             
015370        MOVE IDARTNR-WS         TO UT-IDARTNR                             
015371                                                                          
015372        MOVE IN-ARTIKELURSPRUNG TO UT-KDARTURS                            
015373                                                                          
015374        MOVE IN-LEVID           TO WS-IDLEVNR                             
015375****---------------------------------------------                         
015376*       *** DENNA KOD ÖVERFLÖDIG DÅ IDLEVNR BLIR ALFANUMERISKT            
015377*       *** IN-LEVID ÄR ALFANUM-9, TRUNKERAS VID MOVE TILL 5 BYTES        
015378*                                                                         
015379*       MOVE IN-LEVID           TO WS-LEVID                               
015380*       MOVE SPACE              TO WS-IDLEVNR                             
015381*       MOVE +5                 TO IX-NUM IX-ALF                          
015382*       PERFORM UNTIL IX-ALF = ZERO OR IX-NUM = ZERO                      
015383*          IF WS-IDLEVNR-X (IX-ALF:1) NUMERIC                             
015384*             MOVE WS-IDLEVNR-X (IX-ALF:1) TO                             
015385*                  WS-IDLEVNR-NUM (IX-NUM:1)                              
015386*             SUBTRACT 1 FROM IX-NUM                                      
015387*          END-IF                                                         
015388*          SUBTRACT 1 FROM IX-ALF                                         
015389*       END-PERFORM                                                       
015390****---------------------------------------------                         
015391        MOVE WS-IDLEVNR         TO UT-IDLEVNR                             
015392                                                                          
015393        PERFORM S02-SKRIV-W11135                                          
015394     END-IF                                                               
015395     .                                                                    
015396     EJECT                                                                
015397 Z-FINIT SECTION.                                                         
015398                                                                          
015399     CLOSE W11138                                                         
015400           W11135                                                         
015401                                                                          
015402     MOVE 'S' TO POSTSUM-OPKOD                                            
015403     CALL POSTSUM USING POSTSUM-PARM                                      
015404     .                                                                    
015405     EJECT                                                                
015410 S01-LAES-W11138  SECTION.                                                
015500                                                                          
015600     READ W11138 INTO IN-AREA                                             
015700     AT END                                                               
015800        SET END-OF-W11138 TO TRUE                                         
015900                                                                          
015901     NOT AT END                                                           
015902        MOVE 'W11138'       TO POSTSUM-FDNAMN                             
015903        MOVE 'W11134D1'     TO POSTSUM-DDNAMN2                            
015904        MOVE 'IN'           TO POSTSUM-TRANSTYP                           
015905        CALL POSTSUM USING POSTSUM-PARM                                   
015906     END-READ                                                             
015907     .                                                                    
015908     EJECT                                                                
015909 S02-SKRIV-W11135  SECTION.                                               
015910                                                                          
015911     WRITE UT-POST FROM UT-AREA                                           
015914                                                                          
015916        MOVE 'W11135'       TO POSTSUM-FDNAMN                             
015917        MOVE 'W11134D2'     TO POSTSUM-DDNAMN2                            
015918        MOVE 'UT'           TO POSTSUM-TRANSTYP                           
015919        CALL POSTSUM USING POSTSUM-PARM                                   
015921     .                                                                    
015922     EJECT                                                                
015923* IMS SECTIONER                                                           
015924                                                                          
015925 IMS-GET-ARTC01 SECTION.                                                  
015926     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
015927          DELIMITED BY SIZE INTO SSA1                                     
015928     MOVE '  GE' TO GODK-STATUSKODER                                      
015929     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
015930     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
015931     PERFORM IMS-STATUSKONTROLL                                           
015932     .                                                                    
015933     SKIP3                                                                
015934 IMS-RESTART SECTION.                                                     
015935                                                                          
015936     MOVE SPACE TO W-MSG-IO-AREA                                          
015940     MOVE '  ' TO GODK-STATUSKODER                                        
015950     CALL CBLTDLI USING XRST MSG-PCB                                      
015960                             W-MSG-IO-AREA-LENGTH                         
015970                             W-MSG-IO-AREA                                
015980                             W-CHKP-AREA-1-LENGTH                         
015990                             W-CHKP-AREA-1                                
016000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
016100     PERFORM IMS-STATUSKONTROLL                                           
016200     .                                                                    
016300     EJECT                                                                
016400 IMS-CHECKPOINT SECTION.                                                  
016500     SKIP2                                                                
016600     MOVE IDPGM TO W-MSG-IO-AREA                                          
016700     MOVE '  XD' TO GODK-STATUSKODER                                      
016800     CALL CBLTDLI USING CHKP MSG-PCB                                      
016900                             W-MSG-IO-AREA-LENGTH                         
017000                             W-MSG-IO-AREA                                
017100                             W-CHKP-AREA-1-LENGTH                         
017200                             W-CHKP-AREA-1                                
017300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
017400     PERFORM IMS-STATUSKONTROLL                                           
017500     IF IMS-EJ-OK                                                         
017600       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
017700       CALL FELLOG                                                        
017800     END-IF                                                               
017900     .                                                                    
018000     SKIP3                                                                
018100 IMS-STATUSKONTROLL SECTION.                                              
018200                                                                          
018300     SET STATUS-IX TO 1                                                   
018400     SEARCH GODK-STATUS                                                   
018500       AT END                                                             
018600         MOVE 'IMS RETURKOD : ' TO FELTEXT-STR                            
018700         DISPLAY FELTEXT STATUS-WS                                        
018800         CALL FELLOG                                                      
018900       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
019000         CONTINUE                                                         
019100     END-SEARCH                                                           
019200     .                                                                    
