000101 ID DIVISION.                                                             
000201                                                                          
000301 PROGRAM-ID.     W2231000.                                                
000401 AUTHOR.         EVA LUNDELL.                                             
000501 DATE-WRITTEN.   96/06/07.                                                
000601 DATE-COMPILED.                                                           
000701                                                                          
000801                                                                          
000901*    FUNKTION:                                                            
001001*        PROGRAMMET LÄSER INFIL MED URSPRUNGSLAND FÖR ARTIKLAR            
001101*        OCH UPPDATERAR ARTIKELBASEN (WDK6) OM ARTIKELNS UR-              
001201*        SPRUNG HAR ÄNDRATS.                                              
001301*                                                                         
001401*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
001501*                                                                         
001601*     +  STARTAR EV DISPATCH FÖR UPPDATERING AV URSPRUNG PÅ               
001701*        INLEVERANSREGISTRET.(W6D1)                                       
001801*                                                                         
001901*                                                                         
002001*    SUBPROGRAM.                                                          
002101*        W006KOM  (DISPATCH)                                              
002201*    ABENDKODER:                                                          
002301*        U0016 -  . . . .                                                 
002401*        U1000 -  . . . .                                                 
002501*                                                                         
002601                                                                          
002701     SKIP3                                                                
002801 ENVIRONMENT DIVISION.                                                    
002901     SKIP2                                                                
003001 INPUT-OUTPUT SECTION.                                                    
003101                                                                          
003201 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003401*          --- INFIL MED ARTIKLAR OCH URSPRUNG                            
003501     SELECT W11135                     ASSIGN TO W22310D1.                
003601*          --- MEMO-FIL MED LARM OM FELAKTIGT URSPRUNG ELLER              
003701*              TVIVELAKTIG LEVERANTÖR                                     
003801     SELECT W22311                     ASSIGN TO W22310D2.                
003901     EJECT                                                                
004001 DATA DIVISION.                                                           
004101     SKIP3                                                                
004201 FILE SECTION.                                                            
004301     SKIP3                                                                
004401 FD  W11135                                                               
004501     RECORDING       F                                                    
004601     BLOCK CONTAINS  0.                                                   
004701                                                                          
004801*01  -COPY W11135        -L.                                              
004901     SKIP3                                                                
005001                                                                          
006001 FD  W22311                                                               
006101     RECORDING       V                                                    
006201     BLOCK CONTAINS  0.                                                   
006301                                                                          
006401 01  MEMO-POST            PIC X(84).                                      
006501     EJECT                                                                
006601 WORKING-STORAGE SECTION.                                                 
006701     SKIP2                                                                
006801                                                                          
006901*    -- CHECKED BY WY2000                                                 
007001 77  IDPGM                       PIC X(8)    VALUE 'W2231000'.            
007101 01  CHKP-VAR.                                                            
007201 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
007301 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
007401 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
007501 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
007601 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
007701 03  CHKP-MAX                    PIC S9(3)   VALUE +50.                   
007801 77  JA                          PIC X       VALUE 'J'.                   
007901 77  NEJ                         PIC X       VALUE 'N'.                   
008001 77  INDX                        PIC S9(3)   COMP-3 VALUE ZERO.           
008101 77  SPARA-INDX                  PIC S9(3)   COMP-3 VALUE ZERO.           
008201 77  ANTAL-MEMO-RADER            PIC S9(3)   COMP-3 VALUE ZERO.           
008301 77  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
008401 77  LNG-P-TO-P-PREFIX           PIC S9(4)   COMP SYNC  VALUE +17.        
008501 77  DADATTID                    PIC 9(14).                               
008601 77  TODAYS-DATE                 PIC S9(8)   VALUE ZERO.                  
008701     SKIP2                                                                
008801 01  FELTEXT.                                                             
008901     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009001     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009101*      --- VALID IDDC CODES                                               
009201*                                                                         
009301*01    -COPY WWDCKONS                                                     
009401       EJECT                                                              
009501                                                                          
009601 01  MEMO-RADER.                                                          
009701     03  RAD-1                   PIC X(80)   VALUE                        
009801    ' SYSTEMGENERERAT MEMO ANGÅENDE URSRUNGSMÄRKNING PÅ ARTIKLAR'.        
009901     03  RAD-2-FEL               PIC X(80)   VALUE                        
010001    ' FELAKTIG URSPRUNGSKOD: '.                                           
010101     03  RAD-2-TVIVEL            PIC X(80)   VALUE                        
010201    ' TVIVELAKTIG LEVERANTÖR: '.                                          
010301     03  RAD-3.                                                           
010401       05  FILLER                PIC X(7)    VALUE ' ARTNR '.             
010501       05  RAD-3-IDARTNR         PIC 9(9).                                
010601       05  FILLER                PIC X(7)    VALUE ' LEVNR '.             
010701       05  RAD-3-IDLEVNR         PIC X(5).                                
010801       05  FILLER                PIC X(10)   VALUE ' URSPRUNG '.          
010901       05  RAD-3-KDARTURS        PIC X(2).                                
011001       05  FILLER                PIC X(8)    VALUE ' SYSTEM '.            
011101       05  RAD-3-IDSYSTEM        PIC X(4).                                
011201                                                                          
011301 01  MEMO-RAD                    PIC X(80).                               
011401                                                                          
011501 77  W11135-EOF-SW               PIC X       VALUE 'N'.                   
011601     88  END-OF-W11135                       VALUE 'J'.                   
011701                                                                          
011801 77  URSPRUNG-FINNS-SW           PIC X       VALUE 'N'.                   
011901     88  URSPRUNG-FINNS                      VALUE 'J'.                   
012001     EJECT                                                                
012101 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
012201 01  FILLER REDEFINES DAGENS-DATUM.                                       
013001     03  DAGENS-DATUM-AAR        PIC 9(2).                                
013101     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
013201     03  DAGENS-DATUM-DAG        PIC 9(2).                                
013301                                                                          
013401     EJECT                                                                
013501*01  -COPY WWLEV03                                                        
013601     EJECT                                                                
013701 01  DYNAMISKA-SUBPROGRAM.                                                
013801*                                                                         
013901     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014001     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
014101     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
014201     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
014301     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
014401     EJECT                                                                
014501*    --- PARAMETRAR TILL POSTSUM                                          
014601*                                                                         
014701*01  -COPY W0005   -PRE  POSTSUM-                                         
014801     EJECT                                                                
014901*    --- PARAMETRAR TILL W400ARTU                                         
015001*                                                                         
015101*01  -COPY W400ARTU                                                       
015201     EJECT                                                                
015301                                                                          
015401 01  FILLER                   PIC X(16)   VALUE 'KOM-IO-AREA'.            
015501     SKIP3                                                                
015601 01  FILLER                   PIC X(16)  VALUE 'KOM-MSG-IO-AREA '.        
015701 01  KOM-MSG-IO-AREA.                                                     
015801*03  -COPY WMSGKOM                                                        
015901     EJECT                                                                
016001                                                                          
016101 01  P-TO-P-SW.                                                           
016201     02     P-TO-P-KVLL             PIC S9(4) COMP SYNC.                  
016301     02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.            
016401     02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.            
016501     02     P-TO-P-KDTRANS          PIC X(8).                             
016601     02     P-TO-P-IDTRANS          PIC X(4).                             
016701     02     P-TO-P-KDMFSFOR         PIC X(1).                             
016801     02     P-TO-P-DATA             PIC X(1000).                          
016901     EJECT                                                                
017001                                                                          
017101 01      FILLER                  PIC X(24)   VALUE                        
017201                                 'MOD619B-MID-W6I19B01'.                  
017301     SKIP2                                                                
017401     -COPY W6I19B01 -PRE MOD619B-                                         
017501     EJECT                                                                
017601 01  SPAR-AREA-START             PIC X(24)   VALUE                        
017701                                             'SPAR-AREA-START'.           
017801     SKIP2                                                                
017901                                                                          
018001*01  AREA -COPY W11135       -PRE SPAR-                                   
018101     EJECT                                                                
018201 01  IN-AREA-START               PIC X(24)   VALUE                        
018301                                             'IN-AREA-START'.             
018401     SKIP2                                                                
018501                                                                          
018601*01  AREA -COPY W11135       -PRE IN-                                     
018701     EJECT                                                                
018801 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018901     SKIP3                                                                
019001*    --- FÖR TEST AV BYTESNR                                              
020001*                                                                         
020101 01  FILLER                      PIC  X(16)  VALUE 'BYTES-TEST'.          
020201 01  TEST-IDARTNR                PIC  9(9)   COMP-3.                      
020301*01  FILLER  -COPY WWBYT03     -RED TEST-IDARTNR.                         
020401*                                                                         
020501 01  NYCKLAR-TILL-DLI.                                                    
020601     03  W-IDARTNR-X.                                                     
020701         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
020801     03  W-KDSEGKEY-X.                                                    
020901         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
021001     SKIP2                                                                
021101*    --- STATUS-KOD FRÅN IMS                                              
021201 01  STATUS-WS                   PIC XX.                                  
021301     88  SEGMENT-FINNS                       VALUE '  '.                  
021401     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021501     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021601     88  SEGMENT-SLUT                        VALUE 'GB'.                  
021701     88  IMS-EJ-OK                           VALUE 'XD'.                  
021801     SKIP2                                                                
021901 01  GODK-STATUSKODER.                                                    
022001     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022101     SKIP3                                                                
022201 01  SSA1                        PIC X(64).                               
022301 01  SSA2                        PIC X(64).                               
022401     EJECT                                                                
022501*    --- IMS FUNKTIONSKODER                                               
022601*01  -COPY W0003                                                          
022701     EJECT                                                                
022801*    ---  DLI INPUT-OUTPUT AREA                                           
022901 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
023001     SKIP3                                                                
023101 01  DLI-IO-AREA1.                                                        
023201     03  IO-AREA1                PIC X(150)  VALUE SPACE.                 
023301     SKIP3                                                                
023401     03  WLARTC01 REDEFINES IO-AREA1.                                     
023501*        05  -COPY WDK601                                                 
023601     EJECT                                                                
023701 01  DLI-IO-AREA2.                                                        
023801     03  IO-AREA2                PIC X(900)  VALUE SPACE.                 
023901     SKIP3                                                                
024001     03  WLARTC11 REDEFINES IO-AREA2.                                     
024101*        05  -COPY WDK611                                                 
024201     EJECT                                                                
024301 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDT501'.            
024401 01  DLI-IO-WDT501.                                                       
024501*    03  -COPY WDT501   -PRE WDT501-                                      
024601     EJECT                                                                
024701 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT511'.                      
024801 01  DLI-IO-WDT511.                                                       
024901*    03  -COPY WDT511                                                     
025001     EJECT                                                                
025100 LINKAGE SECTION.                                                         
025200                                                                          
025300*01  -COPY W0009   -PRE MSG-                                              
025400     EJECT                                                                
025500*01  -COPY W0009   -PRE DISP-                                             
025600     EJECT                                                                
025700*01  -COPY W0008  -PRE ARTC-                                              
025800     05  FILLER                  PIC X.                                   
025900     EJECT                                                                
026000*    PCB'ER FÖR SUBPGM                                                    
026100                                                                          
026200 01 KOM-KOMA-PCB         PIC X.                                           
026300     EJECT                                                                
026401*01  -COPY W0008  -PRE WDT5-                                              
026501      05 FILLER                  PIC X.                                   
026600                                                                          
026700*                                                                         
026801 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB ARTC-PCB KOM-KOMA-PCB         
026901                           WDT5-PCB.                                      
027000 MAIN SECTION.                                                            
027101     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB ARTC-PCB KOM-KOMA-PCB         
027201                           WDT5-PCB.                                      
027300                                                                          
027400     SKIP2                                                                
027500* *** FÖR VARJE INPOST PÅ W11135-FILEN                                    
027600*     OM URSPRUNGET EJ FÖRÄNDRAT : INGEN ÅTGÄRD                           
027700*     OM URSPRUNGET FÖRÄNDRAT :                                           
027800*         KOLLA MED VÅR ÖVERSÄTTNINGSTABELL. OM URSPRUNGSKODEN            
027900*         INTE FINNS DÄR; FELMEMO SAMT INGEN VIDARE BEHANDLING AV         
028000*         IN-POSTEN.                                                      
028100*         OM URSPRUNGSKODEN FINNS I ÖVERSÄTTNINGSTABELLEN OCH OM          
028201*         DET GÄLLER HUVUDLEVERANTÖREN SÅ BYTS KOD ÄVEN I BASEN.          
028301**************************************************************            
028401                                                                          
028501     PERFORM A-INIT                                                       
028601     PERFORM S01-LAES-W11135                                              
028700     PERFORM UNTIL END-OF-W11135                                          
028800       IF CHKP-ANT > CHKP-MAX                                             
028900         PERFORM X-TAG-CHECKPOINT                                         
029000       END-IF                                                             
029100                                                                          
029200       IF IN-AREA = SPAR-AREA                                             
029300* FIX                                                                     
029400         MOVE IN-AREA  TO SPAR-AREA                                       
029500* END-FIX                                                                 
029600         PERFORM S01-LAES-W11135                                          
029700       ELSE                                                               
029801         PERFORM S04-KONTR-BEHANDLA-URSPRUNG                              
029901*FIX                                                                      
030001         MOVE IN-AREA TO SPAR-AREA                                        
030101*END-FIX                                                                  
031001         PERFORM S01-LAES-W11135                                          
032001       END-IF                                                             
033001     END-PERFORM                                                          
034001                                                                          
035001     PERFORM Z-FINIT                                                      
036001                                                                          
036101     MOVE ZERO TO RETURN-CODE                                             
036201     GOBACK                                                               
036301     .                                                                    
036401     EJECT                                                                
036501 A-INIT SECTION.                                                          
036601     SKIP2                                                                
036701                                                                          
036801     PERFORM IMS-RESTART                                                  
036901                                                                          
037001     ACCEPT DAGENS-DATUM FROM DATE                                        
037101     ACCEPT DAGENS-TID   FROM TIME                                        
037201                                                                          
037301     OPEN INPUT W11135                                                    
037401                                                                          
037501     OPEN OUTPUT W22311                                                   
037601                                                                          
037701     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
037801     .                                                                    
037901     EJECT                                                                
038001 Z-FINIT SECTION.                                                         
039001                                                                          
039101                                                                          
039201     CLOSE W11135                                                         
039301           W22311                                                         
039401     SKIP2                                                                
039501     MOVE 'S' TO POSTSUM-OPKOD                                            
039601     CALL POSTSUM USING POSTSUM-PARM                                      
039701     .                                                                    
039801     EJECT                                                                
039901 S01-LAES-W11135  SECTION.                                                
040001     SKIP2                                                                
040101     READ W11135 INTO IN-AREA                                             
040201     AT END                                                               
040301        SET END-OF-W11135 TO TRUE                                         
040401                                                                          
040501     NOT AT END                                                           
040601        MOVE 'W11135' TO POSTSUM-FDNAMN                                   
040701        MOVE 'W22310D1' TO POSTSUM-DDNAMN2                                
040801        CALL POSTSUM USING POSTSUM-PARM                                   
040901                                                                          
041001     END-READ                                                             
042001     .                                                                    
043001     EJECT                                                                
044001 S04-KONTR-BEHANDLA-URSPRUNG SECTION.                                     
045001     SKIP2                                                                
045101     PERFORM S05-SOEK-I-KDARTURS-TAB                                      
045201     IF URSPRUNG-FINNS                                                    
045301         PERFORM S07-KOLLA-UPPDAT-WLARTC                                  
045401     ELSE                                                                 
045501         IF ANTAL-MEMO-RADER = +0                                         
045601             MOVE RAD-1  TO MEMO-RAD                                      
045701             PERFORM S12-SKRIV-LARM-MEMO-RAD                              
045801             ADD +1 TO ANTAL-MEMO-RADER                                   
045901         END-IF                                                           
046001         MOVE RAD-2-FEL TO MEMO-RAD                                       
046101         PERFORM S12-SKRIV-LARM-MEMO-RAD                                  
046201         PERFORM S09-REDIGERA-FELMEMO-RAD-3                               
046301         PERFORM S12-SKRIV-LARM-MEMO-RAD                                  
046401     END-IF                                                               
046501     .                                                                    
046601     EJECT                                                                
046701 S05-SOEK-I-KDARTURS-TAB SECTION.                                         
046801     SKIP2                                                                
046901     MOVE NEJ TO URSPRUNG-FINNS-SW                                        
047001     MOVE IN-KDARTURS   TO ARTU-KDARTURS                                  
047101     MOVE SPACE         TO ARTU-IDDC                                      
047201     MOVE ZERO          TO ARTU-IDDISTR                                   
047301     CALL W400ARTU USING ARTU-W400ARTU                                    
047401     IF ARTU-KDARTURS-NUM > ZERO                                          
047501        MOVE JA         TO URSPRUNG-FINNS-SW                              
047601     END-IF                                                               
047701     .                                                                    
047801     EJECT                                                                
047901 S07-KOLLA-UPPDAT-WLARTC SECTION.                                         
048001     SKIP2                                                                
049001     MOVE IN-IDARTNR   TO W-IDARTNR                                       
050001                          TEST-IDARTNR                                    
050101     PERFORM IMS-GET-ARTC-WLARTC01                                        
050201     IF SEGMENT-FINNS                                                     
050301         IF ART-IDLEVNR = IN-IDLEVNR                                      
050401              PERFORM IMS-GET-CLAG-WLARTC11                               
050501              IF SEGMENT-FINNS                                            
050601                  IF IN-KDARTURS = CLAG-KDARTURS                          
050701                     CONTINUE                                             
050801                  ELSE                                                    
051000                     MOVE IN-KDARTURS  TO CLAG-KDARTURS                   
051010                                          GLO-KDARTURS                    
051100                     PERFORM IMS-REPL-CLAG-WLARTC11                       
051200                     IF GLO-KDARTURS > ' '                                
051201                       PERFORM S13-UPDATE-WDT5                            
051202                     END-IF                                               
051300                     PERFORM S10-STARTA-DISPATCHEN                        
051400                     ADD +1 TO CHKP-ANT                                   
051500                   END-IF                                                 
051700              END-IF                                                      
051800         ELSE                                                             
051900           IF BYT03-OBJEKT                                                
052000              MOVE TEST-IDARTNR   TO W-IDARTNR                            
052100              PERFORM IMS-GET-CLAG-WLARTC11                               
052200              IF SEGMENT-FINNS                                            
052300                  IF IN-KDARTURS = CLAG-KDARTURS                          
052400                     CONTINUE                                             
052500                  ELSE                                                    
052700                     MOVE IN-KDARTURS  TO CLAG-KDARTURS                   
052710                                          GLO-KDARTURS                    
052800                     PERFORM IMS-REPL-CLAG-WLARTC11                       
052900                     IF GLO-KDARTURS > ' '                                
052901                       PERFORM S13-UPDATE-WDT5                            
052902                     END-IF                                               
053000                     PERFORM S10-STARTA-DISPATCHEN                        
053100                     ADD +1 TO CHKP-ANT                                   
053300                  END-IF                                                  
053400              END-IF                                                      
053500           END-IF                                                         
053600         END-IF                                                           
053700     END-IF                                                               
053800     .                                                                    
053901     EJECT                                                                
054001 S09-REDIGERA-FELMEMO-RAD-3 SECTION.                                      
054101     SKIP2                                                                
054201     MOVE IN-IDARTNR  TO RAD-3-IDARTNR                                    
055001     MOVE IN-IDLEVNR  TO RAD-3-IDLEVNR                                    
055101     MOVE IN-KDARTURS TO RAD-3-KDARTURS                                   
055201     MOVE 'VTAB'      TO RAD-3-IDSYSTEM                                   
055301     MOVE RAD-3       TO MEMO-RAD                                         
055401     .                                                                    
055501     EJECT                                                                
055601 S10-STARTA-DISPATCHEN SECTION.                                           
055701     SKIP2                                                                
055801     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
055901     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
056001     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
056101     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
056201     MOVE SPACE                TO MSG-KOM-KDTRANS                         
056301     MOVE 'W6I19B01'           TO MSG-KOM-IDCPYTXT                        
056401     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
056501     MOVE 'W2231000'           TO MSG-KOM-IDSNDJOB                        
056601     MOVE DAGENS-DATUM         TO MSG-KOM-TIREGDAT                        
056701     MOVE DAGENS-TID           TO MSG-KOM-TIKLOCK                         
056801     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
056901                                                                          
057001     MOVE ALL '+'              TO MOD619B-MID-W6I19B01                    
057101     MOVE IN-IDARTNR           TO MOD619B-MID-IDARTNR                     
057201     MOVE WC-CDC-SE            TO MOD619B-MID-IDDC                        
057301     MOVE IN-KDARTURS          TO MOD619B-MID-KDARTURS                    
057401     COMPUTE P-TO-P-KVLL       =  LNG-P-TO-P-PREFIX + 87                  
057501     MOVE 'W6T19BX '           TO P-TO-P-KDTRANS                          
057601     MOVE '2231'               TO P-TO-P-IDTRANS                          
057701     MOVE '1'                  TO P-TO-P-KDMFSFOR                         
057801     MOVE MOD619B-MID-W6I19B01 TO P-TO-P-DATA                             
057901                                                                          
058001     CALL W006KOM USING MSG-PCB                                           
058101                        DISP-PCB                                          
058201                        KOM-KOMA-PCB                                      
058301                        MSG-KOM-WMSGKOM                                   
058401                        P-TO-P-SW                                         
058501     .                                                                    
058601     EJECT                                                                
058701 S12-SKRIV-LARM-MEMO-RAD SECTION.                                         
058801     SKIP2                                                                
058901     WRITE MEMO-POST  FROM MEMO-RAD                                       
059001                                                                          
060001     MOVE 'W22311'   TO POSTSUM-FDNAMN                                    
060101     MOVE 'W22310D2' TO POSTSUM-DDNAMN2                                   
060201     CALL POSTSUM USING POSTSUM-PARM                                      
060301     .                                                                    
060401     EJECT                                                                
060501 S13-UPDATE-WDT5 SECTION.                                                 
060601                                                                          
060701     PERFORM IMS-GU-WDT501                                                
060702     IF SEGMENT-FINNS                                                     
060703      PERFORM IMS-GHNP-WDT511                                             
060704      MOVE ART-IDLEVNR TO GLO-IDLEVNR                                     
060705      PERFORM IMS-REPL-WDT511                                             
060706     END-IF                                                               
060801     IF SEGMENT-SAKNAS                                                    
060901        MOVE W-IDARTNR TO WDT501-ARTU-IDARTNR                             
061001        PERFORM IMS-ISRT-WDT501                                           
061101     END-IF                                                               
061201                                                                          
061301     MOVE 'NE25'                TO GLO-IDUSER                             
061302     MOVE SPACES                TO GLO-IDLEVNR                            
061401     MOVE FUNCTION CURRENT-DATE(1:14) TO DADATTID                         
061501     MOVE FUNCTION CURRENT-DATE(1:8)  TO TODAYS-DATE                      
061601     COMPUTE GLO-DADATTID-9KOMPL =                                        
061701             99999999999999 - DADATTID                                    
061801                                                                          
061901     PERFORM IMS-ISRT-WDT511                                              
062001     .                                                                    
062101     EJECT                                                                
062201                                                                          
062301 X-TAG-CHECKPOINT   SECTION.                                              
062401                                                                          
062501* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
062601* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
062701     PERFORM IMS-CHECKPOINT                                               
062801     MOVE ZERO TO CHKP-ANT                                                
062901* --- LÄS OM DATABAS OM DET BEHÖVS                                        
063001     .                                                                    
063101     EJECT                                                                
063201* --- IMS SEKTIONER ---                                                   
063301     SKIP3                                                                
063401     EJECT                                                                
063501 IMS-GET-ARTC-WLARTC01 SECTION.                                           
063601                                                                          
063701     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
063801          DELIMITED BY SIZE INTO SSA1                                     
063901     MOVE '  GE' TO GODK-STATUSKODER                                      
064001     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA1 SSA1                    
064101     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
064201     PERFORM IMS-STATUSKONTROLL                                           
064301     .                                                                    
064401     EJECT                                                                
064501 IMS-GET-CLAG-WLARTC11 SECTION.                                           
064601                                                                          
064701     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
064801          DELIMITED BY SIZE INTO SSA1                                     
064901     MOVE '  GE' TO GODK-STATUSKODER                                      
065001     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA2 SSA1                   
065101     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
065201     PERFORM IMS-STATUSKONTROLL                                           
065301     .                                                                    
065401     SKIP3                                                                
065501 IMS-REPL-CLAG-WLARTC11 SECTION.                                          
065601                                                                          
065701     MOVE '  ' TO GODK-STATUSKODER                                        
065801     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA2                        
065901     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
066001     PERFORM IMS-STATUSKONTROLL                                           
066101     .                                                                    
066201     EJECT                                                                
066301 IMS-GU-WDT501 SECTION.                                                   
066401     STRING 'WDT501  (IDARTNR = ' W-IDARTNR-X ')'                         
066501          DELIMITED BY SIZE INTO SSA1                                     
066601     MOVE '  GE' TO GODK-STATUSKODER                                      
066701     CALL CBLTDLI USING GU  WDT5-PCB DLI-IO-WDT501 SSA1                   
066801     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
066901     PERFORM IMS-STATUSKONTROLL                                           
067001     .                                                                    
067101     EJECT                                                                
067201 IMS-ISRT-WDT501 SECTION.                                                 
067301     MOVE 'WDT501 ' TO SSA1                                               
067401     MOVE '  II' TO GODK-STATUSKODER                                      
067501     CALL CBLTDLI USING ISRT WDT5-PCB DLI-IO-WDT501 SSA1                  
067601     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
067701     PERFORM IMS-STATUSKONTROLL                                           
067801     .                                                                    
067802 IMS-GHNP-WDT511 SECTION.                                                 
067803     MOVE   'WDT511  *F' TO SSA1                                          
067804     MOVE '  ' TO GODK-STATUSKODER                                        
067805     CALL CBLTDLI USING GHNP  WDT5-PCB DLI-IO-WDT511 SSA1                 
067806     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
067807     PERFORM IMS-STATUSKONTROLL                                           
067808     .                                                                    
067809     EJECT                                                                
067810 IMS-REPL-WDT511 SECTION.                                                 
067820     MOVE '  ' TO GODK-STATUSKODER                                        
067830     CALL CBLTDLI USING REPL  WDT5-PCB DLI-IO-WDT511                      
067840     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
067850     PERFORM IMS-STATUSKONTROLL                                           
067860     .                                                                    
067870     EJECT                                                                
067901 IMS-ISRT-WDT511 SECTION.                                                 
068001     STRING 'WDT501  (IDARTNR  =' W-IDARTNR-X ')'                         
068101            DELIMITED BY SIZE INTO SSA1                                   
068201     MOVE 'WDT511 ' TO SSA2                                               
068301     MOVE '  II' TO GODK-STATUSKODER                                      
068401     CALL CBLTDLI USING ISRT WDT5-PCB DLI-IO-WDT511 SSA1 SSA2             
068501     MOVE WDT5-STATUS-CODE TO STATUS-WS                                   
068601     PERFORM IMS-STATUSKONTROLL                                           
068701     .                                                                    
068801 IMS-RESTART SECTION.                                                     
068901     SKIP2                                                                
069001     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
069101     MOVE '  ' TO GODK-STATUSKODER                                        
069201     CALL CBLTDLI USING XRST MSG-PCB                                      
069301                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
069401                        CHKP-AREA-LENGTH CHKP-AREA                        
069501     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
069601     PERFORM IMS-STATUSKONTROLL                                           
069701     .                                                                    
069801     EJECT                                                                
069901 IMS-CHECKPOINT SECTION.                                                  
070001     SKIP2                                                                
070101     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
070201     MOVE '  XD' TO GODK-STATUSKODER                                      
070301     CALL CBLTDLI USING CHKP MSG-PCB                                      
070401                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
070501                        CHKP-AREA-LENGTH CHKP-AREA                        
070601     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
070701     PERFORM IMS-STATUSKONTROLL                                           
070801                                                                          
070901     IF IMS-EJ-OK                                                         
071001       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
071101       DISPLAY FELTEXT                                                    
071201       CALL FELLOG                                                        
071301     END-IF                                                               
071401     .                                                                    
071501     EJECT                                                                
071601 IMS-STATUSKONTROLL SECTION.                                              
071701     SKIP2                                                                
071801     SET STATUS-IX TO 1                                                   
071901     SEARCH GODK-STATUS                                                   
072001       AT END                                                             
072101         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
072201           DELIMITED BY SIZE INTO FELTEXT                                 
072301         DISPLAY FELTEXT                                                  
072401         CALL FELLOG                                                      
072501       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
072601         CONTINUE                                                         
073001     END-SEARCH                                                           
080001     .                                                                    
