001100 ID DIVISION.                                                             
001200     SKIP2                                                                
001300 PROGRAM-ID.     W2180200.                                                
001400*AUTHOR.         STEFAN KIHLBERG.                                         
001500*DATE-WRITTEN.   92/03/24.                                                
001600                                                                          
001700*    REMARKS.                                                             
001800*                                                                         
001900*    FUNKTION:                                                            
002000*        LÄSER FIL MED ARTIKLAR FRÅN BATCH DÄR TIDISPIN SKALL             
002100*        OMRÄKNAS. SKICKAR ARTIKLAR TILL SUBPGM W218DISP FÖR              
002200*        BERÄKNING OCH UPPDATERING  PÅ WDK611.                            
002300*                                                                         
002400*        SVARSKODER FRÅN SUBPROGRAMMET:                                   
002500*                                                                         
002510*        DISP-KDSVAR = 1    ARTIKEL SAKNAS                                
002511*        DISP-KDSVAR = 2    KDERS-UTG > 0                                 
002512*        DISP-KDSVAR = 3    WDK611 UPPDATERAD                             
002515*                                                                         
002516*        DISP-KDSVAR = 7    1 ST TRANS TILL HÄNDELSE 2241                 
002520*                                                                         
002530*                                                                         
002560*                                                                         
002600*    ABENDKODER:                                                          
002700*        U0016 -  . . . .                                                 
002800*        U1000 -  . . . .                                                 
002900*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003701     SKIP2                                                                
003702*          --- ARTIKLAR FÖR TIDISPIN BERÄKNING                            
003710     SELECT W21802                     ASSIGN TO W21802D1.                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004301     SKIP3                                                                
004302 FD  W21802                                                               
004303     RECORDING       F                                                    
004304     BLOCK CONTAINS  0.                                                   
004305     SKIP2                                                                
004310*01  -COPY W21802      -L.                                                
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600     SKIP2                                                                
004601                                                                          
004610*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(8)    VALUE 'W2180200'.            
004800 01  CHKP-VAR.                                                            
004900 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005000 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005100 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005200 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005300 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005400 03  CHKP-MAX                    PIC S9(3)   VALUE 100.                   
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005610                                                                          
005632 77  WS-ART-SAKNAS-ANT           PIC 9(9)   VALUE ZERO.                   
005633 77  WS-UTG-STORRE-ANT           PIC 9(9)   VALUE ZERO.                   
005634 77  WS-C1-UPPDAT-ANT            PIC 9(9)   VALUE ZERO.                   
005636 77  WS-TRANS-TILL-2241-ANT      PIC 9(9)   VALUE ZERO.                   
005637                                                                          
005700     SKIP2                                                                
005800 01  FELTEXT.                                                             
005900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006201                                                                          
006202 01  ARBETSAREOR.                                                         
006203     03 WS-IDARTNR               PIC S9(9)   VALUE ZERO COMP-3.           
006206                                                                          
006207 77  W21802-EOF-SW               PIC X       VALUE 'N'.                   
006210     88  END-OF-W21802                       VALUE 'J'.                   
006500     EJECT                                                                
006600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006700 01  FILLER REDEFINES DAGENS-DATUM.                                       
006800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007100     EJECT                                                                
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300*                                                                         
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007601     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007610     03  W218DISP                PIC X(8)    VALUE 'W218DISP'.            
007701     EJECT                                                                
007702*    --- PARAMETRAR TILL POSTSUM                                          
007703*                                                                         
007710*01  -COPY W0005   -PRE  POSTSUM-                                         
007901     EJECT                                                                
007902*  LÄNKAREA TILL SUBPROGRAM W218DISP                                      
007910*01  -COPY W218DISP                                                       
008001     EJECT                                                                
008002 01  IN-AREA-START               PIC X(24)   VALUE                        
008003                                             'IN-AREA-START'.             
008004     SKIP2                                                                
008005                                                                          
008010*01  AREA -COPY W21802     -PRE IN-                                       
008100*                                                                         
008200     EJECT                                                                
008300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008400     SKIP3                                                                
008800*    --- STATUS-KOD FRÅN IMS                                              
008900 01  STATUS-WS                   PIC XX.                                  
009000     88  SEGMENT-FINNS                       VALUE '  '.                  
009100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009400     88  IMS-EJ-OK                           VALUE 'XD'.                  
009500     SKIP2                                                                
009600 01  GODK-STATUSKODER.                                                    
009700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009800     SKIP3                                                                
009900 01  SSA1                        PIC X(64).                               
010000 01  SSA2                        PIC X(64).                               
010100     EJECT                                                                
010200*    --- IMS FUNKTIONSKODER                                               
010300*01  -COPY W0003                                                          
010400     EJECT                                                                
012100 LINKAGE SECTION.                                                         
012200                                                                          
012210*01 -COPY W0009           -PRE MSG-                                       
012220     EJECT                                                                
012300**** PCB FÖR SUBPROGRAM W218DISP                                          
012401                                                                          
012402 01  ARTC-PCB                    PIC X.                                   
012403 01  ARTS-PCB                    PIC X.                                   
012404 01  OIGA-PCB                    PIC X.                                   
012410 01  INLB-PCB                    PIC X.                                   
012420 01  XXCT-PCB                    PIC X.                                   
012700     EJECT                                                                
012801 PROCEDURE DIVISION  USING MSG-PCB                                        
012802                           ARTC-PCB ARTS-PCB                              
012803                           OIGA-PCB INLB-PCB                              
012804                           XXCT-PCB.                                      
012810     ENTRY 'DLITCBL' USING MSG-PCB                                        
012820                           ARTC-PCB ARTS-PCB                              
012840                           OIGA-PCB INLB-PCB                              
012850                           XXCT-PCB.                                      
012900                                                                          
013020                                                                          
013100     SKIP2                                                                
013200     PERFORM A-INIT                                                       
013300     MOVE +1 TO CHKP-ANT                                                  
013310     PERFORM S01-LAES-W21802                                              
013590     PERFORM UNTIL END-OF-W21802                                          
013594        MOVE IN-IDARTNR TO DISP-IDARTNR                                   
013597        MOVE +0         TO DISP-KDSVAR                                    
013598        MOVE IDPGM      TO DISP-IDPGM                                     
013599        CALL W218DISP USING DISP-W218DISP                                 
013600                            ARTC-PCB                                      
013601                            ARTS-PCB                                      
013602                            OIGA-PCB                                      
013606                            INLB-PCB                                      
013607                            XXCT-PCB                                      
013608        ADD +1 TO CHKP-ANT                                                
013610        PERFORM Y-RAKNA-UPPDAT                                            
013613        IF CHKP-ANT >= CHKP-MAX                                           
013614           PERFORM X-TAG-CHECKPOINT                                       
013616        END-IF                                                            
013620        PERFORM S01-LAES-W21802                                           
013630     END-PERFORM                                                          
014800     PERFORM Z-FINIT                                                      
014810     DISPLAY 'ARTIKEL-SAKNAS     = '    WS-ART-SAKNAS-ANT                 
014811     DISPLAY 'KDERS-UTG > 0      = '    WS-UTG-STORRE-ANT                 
014840     DISPLAY 'UPPDAT C1          = '    WS-C1-UPPDAT-ANT                  
014842     DISPLAY 'TRANSAR TILL 2241  = '    WS-TRANS-TILL-2241-ANT            
014900                                                                          
015000     MOVE ZERO TO RETURN-CODE                                             
015100     GOBACK                                                               
015200     .                                                                    
015300     EJECT                                                                
015400 A-INIT SECTION.                                                          
015500     SKIP2                                                                
015600                                                                          
015700     PERFORM IMS-RESTART                                                  
015910     OPEN INPUT W21802                                                    
016200                                                                          
016300     ACCEPT DAGENS-DATUM       FROM DATE                                  
016600                                                                          
016710     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017000     .                                                                    
017200     EJECT                                                                
017201                                                                          
017230                                                                          
017250                                                                          
017260 Y-RAKNA-UPPDAT SECTION.                                                  
017261                                                                          
017270     IF DISP-KDSVAR = 1                                                   
017280        ADD +1 TO WS-ART-SAKNAS-ANT                                       
017290     END-IF                                                               
017291     IF DISP-KDSVAR = 2                                                   
017292        ADD +1 TO WS-UTG-STORRE-ANT                                       
017293     END-IF                                                               
017297     IF DISP-KDSVAR = 3                                                   
017298        ADD +1 TO WS-C1-UPPDAT-ANT                                        
017299     END-IF                                                               
017307     IF DISP-KDSVAR = 7                                                   
017308        ADD +1 TO WS-TRANS-TILL-2241-ANT                                  
017310     END-IF                                                               
017314     .                                                                    
017315     EJECT                                                                
017316                                                                          
017320 Z-FINIT SECTION.                                                         
017400                                                                          
017801                                                                          
017810     CLOSE W21802                                                         
018001     SKIP2                                                                
018002     MOVE 'S' TO POSTSUM-OPKOD                                            
018010     CALL POSTSUM USING POSTSUM-PARM                                      
018200     .                                                                    
018301     EJECT                                                                
018302 S01-LAES-W21802  SECTION.                                                
018303     SKIP2                                                                
018304     READ W21802 INTO IN-AREA                                             
018305     AT END                                                               
018307        SET END-OF-W21802 TO TRUE                                         
018308                                                                          
018309     NOT AT END                                                           
018310        MOVE IDPGM      TO POSTSUM-PROGNAMN                               
018311        MOVE 'W21802'  TO POSTSUM-FDNAMN                                  
018312        MOVE 'W21802D1' TO POSTSUM-DDNAMN2                                
018313        MOVE SPACE      TO POSTSUM-TRANSTYP                               
018314        CALL POSTSUM USING POSTSUM-PARM                                   
018317     END-READ                                                             
018320     .                                                                    
018600     EJECT                                                                
018700 X-TAG-CHECKPOINT   SECTION.                                              
018800                                                                          
019300     PERFORM IMS-CHECKPOINT                                               
019400     MOVE +1 TO CHKP-ANT                                                  
019500     .                                                                    
019600     EJECT                                                                
019700* --- IMS SEKTIONER ---                                                   
019800     SKIP3                                                                
020000     EJECT                                                                
020100 IMS-RESTART SECTION.                                                     
020200     SKIP2                                                                
020300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020400     MOVE '  ' TO GODK-STATUSKODER                                        
020500     CALL CBLTDLI USING XRST MSG-PCB                                      
020600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020700                        CHKP-AREA-LENGTH CHKP-AREA                        
020800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020900     PERFORM IMS-STATUSKONTROLL                                           
021000     .                                                                    
021100     EJECT                                                                
021200 IMS-CHECKPOINT SECTION.                                                  
021300     SKIP2                                                                
021400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021500     MOVE '  XD' TO GODK-STATUSKODER                                      
021600     CALL CBLTDLI USING CHKP MSG-PCB                                      
021700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021800                        CHKP-AREA-LENGTH CHKP-AREA                        
021900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022000     PERFORM IMS-STATUSKONTROLL                                           
022100                                                                          
022200     IF IMS-EJ-OK                                                         
022300       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
022400       DISPLAY FELTEXT                                                    
022500       CALL FELLOG                                                        
022600     END-IF                                                               
022700     .                                                                    
022800     EJECT                                                                
022900 IMS-STATUSKONTROLL SECTION.                                              
023000     SKIP2                                                                
023100     SET STATUS-IX TO 1                                                   
023200     SEARCH GODK-STATUS                                                   
023300       AT END                                                             
023400         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
023500         DISPLAY FELTEXT                                                  
023600         CALL FELLOG                                                      
023700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
023800         CONTINUE                                                         
023900     END-SEARCH                                                           
024000     .                                                                    
