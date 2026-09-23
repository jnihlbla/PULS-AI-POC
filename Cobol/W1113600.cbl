000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1113600.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500 DATE-WRITTEN.   96/10/15.                                                
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000810*        FIX-PGM                                                          
000900*        LÄSER FIL FÖR REGISTRERING AV ERSÄTTNING                         
001000*        - SKAPAR TRANS W1T113X TILL DISPATCHEN                           
001100*                                                                         
001200*    INDATA .                                                             
001300*                                                                         
001400*        FIL FRÅN JANNE (CLASSIC ARTIKLAR)                                
001500*                                                                         
001600*    UTDATA .                                                             
001700*                                                                         
001800*        W1I11301 + WMSGKOM                                               
001900                                                                          
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600                                                                          
002700*                                              ARTIKELFIL                 
002800     SELECT INFIL                      ASSIGN TO W11136D1.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP2                                                                
003400 FD  INFIL                                                                
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700     SKIP2                                                                
003800 01  INPOST                      PIC X(8).                                
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004101                                                                          
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                       PIC X(8)    VALUE 'W1113600'.            
004300 77  W-CHKP-MAX                  PIC S9(3)   VALUE +200  COMP-3.          
004400 77  W-CHKP-RAEKNARE             PIC S9(3)   VALUE +0    COMP-3.          
004500 77  W-MSG-IO-AREA-LENGTH        PIC S9(9)   VALUE +32  COMP SYNC.        
004600 77  W-MSG-IO-AREA               PIC X(32)   VALUE SPACE.                 
004700 77  W-CHKP-AREA-1-LENGTH        PIC S9(9)   VALUE +32  COMP SYNC.        
004800 77  W-CHKP-AREA-1               PIC X(32)   VALUE SPACE.                 
004900 77  JA                          PIC X       VALUE 'J'.                   
004901 77  NEJ                         PIC X       VALUE 'N'.                   
004902 77  INPUT-RAETT                 PIC X       VALUE SPACE.                 
004903 77  WS-TIUPPDAT                 PIC S9(7) COMP-3 VALUE ZERO.             
004904 77  WS-TIUPPTID                 PIC S9(9) COMP-3 VALUE ZERO.             
004905                                                                          
004906 77  INFIL-SW                    PIC X       VALUE 'N'.                   
004907     88  END-OF-INFIL                        VALUE 'J'.                   
004908                                                                          
004909 01  WS-IDARTNR                  PIC X(9)  VALUE SPACE.                   
004910 01  IDARTNR-WS REDEFINES WS-IDARTNR PIC 9(9).                            
004980                                                                          
004990 01  FELTEXT.                                                             
005000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005200     EJECT                                                                
005300 01  DYNAMISKA-SUBPROGRAM.                                                
005400*                                                                         
005500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005700     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
005800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005900     03  W006KOM                 PIC X(8)    VALUE 'W006KOM'.             
006000     EJECT                                                                
006100*    --- PARAMETRAR TILL POSTSUM                                          
006200*                                                                         
006500*01  -COPY W0005 -PRE  POSTSUM-                                           
006600     EJECT                                                                
006601*01  -COPY W009CIA                                                        
006602     EJECT                                                                
006610 01  IN-AREA-START               PIC X(24)   VALUE                        
006620                                             'IN-AREA-START'.             
006632 01  IN-AREA.                                                             
006633     03  IN-IDARTNR-OLD          PIC 9(8).                                
006635     EJECT                                                                
006690*                                                                         
006700 01  NYCKLAR-TILL-DLI.                                                    
006800     03  W-IDARTNR-X.                                                     
006900         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
007000     SKIP3                                                                
007010*    --- STATUS-KOD FRÅN IMS                                              
007020 01  STATUS-WS                   PIC XX.                                  
007021     88  SEGMENT-FINNS                       VALUE '  '.                  
007022     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
007023     88  SEGMENT-SLUT                        VALUE 'GB'.                  
007024     88  IMS-EJ-OK                           VALUE 'XD'.                  
007025                                                                          
007026 01  GODK-STATUSKODER.                                                    
007027     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007028                                                                          
007029 01  SSA1                        PIC X(64).                               
007030     EJECT                                                                
007040*    --- IMS FUNKTIONSKODER                                               
007050*01  -COPY W0003                                                          
007060     EJECT                                                                
007070 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA'.          
007080 01  DLI-IO-AREA.                                                         
007090     03  IO-AREA                 PIC X(900).                              
007100     03  WLARTC01 REDEFINES IO-AREA.                                      
007200*        05    -COPY WDK601                                               
007220     03  WLARTC11 REDEFINES IO-AREA.                                      
007230*        05    -COPY WDK611                                               
007300     EJECT                                                                
007400*    ---  MSG INPUT-OUTPUT AREA                                           
007500 01  FILLER                      PIC X(16)   VALUE 'MSG-IO-AREA'.         
007600                                                                          
007700*01  -COPY WMSGAREA                                                       
007710     EJECT                                                                
007720     05  FILLER REDEFINES MSG-MID-OUT.                                    
007730        07  -COPY W1I11301                                                
007740     EJECT                                                                
007750*    ---  AREA FÖR W006KOM SUBMODUL                                       
007760 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
007770                                                                          
007780 01  KOM-IO-AREA.                                                         
007790*    03  -COPY WMSGKOM                                                    
007800     EJECT                                                                
007900 LINKAGE SECTION.                                                         
008000                                                                          
008100*01  -COPY W0009 -PRE MSG-                                                
008200     EJECT                                                                
008300*01  -COPY W0009 -PRE ALT-                                                
008400     EJECT                                                                
008500*01  -COPY W0009 -PRE KOMA-                                               
008600     EJECT                                                                
008700*01  -COPY W0008 -PRE ARTC-                                               
008800     05  FILLER      PIC X.                                               
008801     EJECT                                                                
008810*01  -COPY W0008 -PRE ERSA-                                               
008820     05  FILLER      PIC X.                                               
008900     EJECT                                                                
009000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB KOMA-PCB ARTC-PCB.             
009100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB KOMA-PCB ARTC-PCB.             
009200                                                                          
009300     PERFORM A-INIT                                                       
009400     PERFORM S01-LAES-INFIL                                               
009500                                                                          
009600     PERFORM UNTIL END-OF-INFIL                                           
009800        PERFORM B-KOLLA-INDATA                                            
009900        IF INPUT-RAETT = NEJ                                              
010000           DISPLAY 'INDATA-FEL' IN-IDARTNR-OLD                            
010100        ELSE                                                              
010500           MOVE IDARTNR-WS TO W-IDARTNR                                   
010600           PERFORM IMS-GET-ARTC01                                         
010700           IF SEGMENT-FINNS                                               
010800              IF ART-KDERS-UTG = ZERO                                     
010810                 PERFORM IMS-GET-ARTC11                                   
010900                 IF CLAG-KDERS = 04 OR 24                                 
010920                       PERFORM E-SKAPA-RIVNINGS-TRANS                     
010922                       PERFORM D-SKICKA-TRANS                             
010926                       PERFORM F-CHECK-POINT                              
010980                 ELSE                                                     
010990                    DISPLAY 'FEL EK' IDARTNR-WS                           
011000                 END-IF                                                   
011030              ELSE                                                        
011050                 DISPLAY 'FEL ARTIKEL ' IDARTNR-WS                        
011100              END-IF                                                      
011101           ELSE                                                           
011103              DISPLAY 'SAKNAS ARTREG ' IDARTNR-WS                         
011110           END-IF                                                         
011120        END-IF                                                            
011150        PERFORM S01-LAES-INFIL                                            
011160     END-PERFORM                                                          
011170                                                                          
011180     PERFORM Z-FINIT                                                      
011190                                                                          
011200     MOVE ZERO TO RETURN-CODE                                             
011300     GOBACK                                                               
011400     .                                                                    
011500     EJECT                                                                
011600 A-INIT SECTION.                                                          
011700                                                                          
011800     OPEN INPUT INFIL                                                     
011900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012000     MOVE ZERO TO W-CHKP-RAEKNARE                                         
012100     PERFORM IMS-RESTART                                                  
012200     PERFORM AA-SKAPA-HEADER                                              
012300     .                                                                    
012400     EJECT                                                                
012500 AA-SKAPA-HEADER SECTION.                                                 
012600                                                                          
012700     MOVE +54                   TO MSG-KOM-KVLL                           
012800     MOVE LOW-VALUE             TO MSG-KOM-KDZ1                           
012900                                   MSG-KOM-KDZ2                           
013000     MOVE SPACE                 TO MSG-KOM-KDTRANS                        
013100     MOVE 'W1I11301'            TO MSG-KOM-IDCPYTXT                       
013200     MOVE 'ERSREG'              TO MSG-KOM-IDSNDNOD                       
013300     MOVE IDPGM                 TO MSG-KOM-IDSNDJOB                       
013400                                                                          
013500     ACCEPT WS-TIUPPDAT FROM DATE                                         
013600     ACCEPT WS-TIUPPTID FROM TIME                                         
013700     MOVE WS-TIUPPDAT           TO MSG-KOM-TIREGDAT                       
013800     MOVE WS-TIUPPTID           TO MSG-KOM-TIKLOCK                        
013900     MOVE SPACE                 TO MSG-KOM-IDMFSMED                       
014000                                   MSG-KOM-KDSVAR                         
014100     .                                                                    
014200     EJECT                                                                
014300 B-KOLLA-INDATA SECTION.                                                  
014400                                                                          
014700     MOVE 'VO' TO CIA-IDARTPRE-IN                                         
014800     MOVE IN-IDARTNR-OLD TO CIA-IDARTBET-IN                               
014900     CALL W009CIA USING CIA-W009CIA                                       
014901     IF CIA-KDSVAR = 'F'                                                  
014902        MOVE NEJ TO INPUT-RAETT                                           
014903     ELSE                                                                 
014904        MOVE CIA-IDARTNR TO WS-IDARTNR                                    
014905        MOVE JA TO INPUT-RAETT                                            
014906     END-IF                                                               
014916     .                                                                    
014917     EJECT                                                                
015341 D-SKICKA-TRANS SECTION.                                                  
015342                                                                          
015343     CALL W006KOM USING MSG-PCB                                           
015344                        ALT-PCB                                           
015345                        KOMA-PCB                                          
015346                        MSG-KOM-WMSGKOM                                   
015347                        MSG-IO-AREA                                       
015348                                                                          
015356     .                                                                    
015357     EJECT                                                                
015358 E-SKAPA-RIVNINGS-TRANS SECTION.                                          
015359                                                                          
015360     MOVE +452                     TO MSG-KVLL                            
015361     ADD  +17                      TO MSG-KVLL                            
015362                                                                          
015363     MOVE LOW-VALUE                TO MSG-KDZ1                            
015364                                      MSG-KDZ2                            
015365     MOVE 'W1T113X'                TO MSG-KDTRANS-1                       
015366     MOVE '1113'                   TO MSG-IDTRANS-1                       
015367     MOVE '1'                      TO MSG-KDMFSFOR-1                      
015368                                                                          
015369     MOVE ALL '+'                  TO MID-W1I11301                        
015370     MOVE ZERO                     TO MID-IDKORTNR-SPAR1                  
015371                                      MID-IDKORTNR-SPAR2                  
015372                                      MID-IDKORTNR-SPAR3                  
015373     MOVE WS-IDARTNR               TO MID-IDARTNR-UT                      
015375     MOVE '00'                     TO MID-KDERS                           
015394                                                                          
015395     MOVE 'J'                      TO MID-FLKLAR                          
015397**************                                                            
015398     DISPLAY 'RIVNING' WS-IDARTNR                                         
015399**************                                                            
015400**** DISPLAY MID-W1I11301                                                 
015401     .                                                                    
015402     EJECT                                                                
015403 F-CHECK-POINT SECTION.                                                   
015404                                                                          
015405     ADD +1 TO W-CHKP-RAEKNARE                                            
015406                                                                          
015407     IF W-CHKP-RAEKNARE > W-CHKP-MAX                                      
015408        PERFORM IMS-CHECKPOINT                                            
015409        MOVE +0 TO W-CHKP-RAEKNARE                                        
015410        ADD +1 TO MSG-KOM-TIKLOCK                                         
015411     END-IF                                                               
015412     .                                                                    
015413     EJECT                                                                
015414 Z-FINIT SECTION.                                                         
015415                                                                          
015416     CLOSE INFIL                                                          
015417                                                                          
015418     MOVE 'S' TO POSTSUM-OPKOD                                            
015419     CALL POSTSUM USING POSTSUM-PARM                                      
015420     .                                                                    
015421     EJECT                                                                
015422 S01-LAES-INFIL  SECTION.                                                 
015423                                                                          
015424     READ INFIL INTO IN-AREA                                              
015425     AT END                                                               
015426        SET END-OF-INFIL TO TRUE                                          
015427                                                                          
015428     NOT AT END                                                           
015430        MOVE 'INFIL '       TO POSTSUM-FDNAMN                             
015500        MOVE 'W11136D1'     TO POSTSUM-DDNAMN2                            
015600        MOVE 'IN'           TO POSTSUM-TRANSTYP                           
015700        CALL POSTSUM USING POSTSUM-PARM                                   
015800     END-READ                                                             
015900     .                                                                    
015901     EJECT                                                                
015902* IMS SECTIONER                                                           
015903                                                                          
015904 IMS-GET-ARTC01 SECTION.                                                  
015905     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
015906          DELIMITED BY SIZE INTO SSA1                                     
015907     MOVE '  GE' TO GODK-STATUSKODER                                      
015908     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
015909     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
015910     PERFORM IMS-STATUSKONTROLL                                           
015920     .                                                                    
015921     SKIP3                                                                
015922 IMS-GET-ARTC11 SECTION.                                                  
015925     MOVE 'WLARTC11 ' TO SSA1                                             
015926     MOVE '  GE' TO GODK-STATUSKODER                                      
015927     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
015928     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
015929     PERFORM IMS-STATUSKONTROLL                                           
015930     .                                                                    
015931     SKIP3                                                                
015932 IMS-RESTART SECTION.                                                     
015934     MOVE SPACE TO W-MSG-IO-AREA                                          
015935     MOVE '  ' TO GODK-STATUSKODER                                        
015936     CALL CBLTDLI USING XRST MSG-PCB                                      
015937                             W-MSG-IO-AREA-LENGTH                         
015938                             W-MSG-IO-AREA                                
015939                             W-CHKP-AREA-1-LENGTH                         
015940                             W-CHKP-AREA-1                                
015941     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
015950     PERFORM IMS-STATUSKONTROLL                                           
015960     .                                                                    
015970     EJECT                                                                
015980 IMS-CHECKPOINT SECTION.                                                  
016000     MOVE IDPGM TO W-MSG-IO-AREA                                          
016100     MOVE '  XD' TO GODK-STATUSKODER                                      
016200     CALL CBLTDLI USING CHKP MSG-PCB                                      
016300                             W-MSG-IO-AREA-LENGTH                         
016400                             W-MSG-IO-AREA                                
016500                             W-CHKP-AREA-1-LENGTH                         
016600                             W-CHKP-AREA-1                                
016700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
016800     PERFORM IMS-STATUSKONTROLL                                           
016900     IF IMS-EJ-OK                                                         
017000       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
017100       CALL FELLOG                                                        
017200     END-IF                                                               
017300     .                                                                    
017400     SKIP3                                                                
017500 IMS-STATUSKONTROLL SECTION.                                              
017700     SET STATUS-IX TO 1                                                   
017800     SEARCH GODK-STATUS                                                   
017900       AT END                                                             
018000         MOVE 'IMS RETURKOD : ' TO FELTEXT-STR                            
018100         DISPLAY FELTEXT STATUS-WS                                        
018200         CALL FELLOG                                                      
018300       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
018400         CONTINUE                                                         
018500     END-SEARCH                                                           
018600     .                                                                    
