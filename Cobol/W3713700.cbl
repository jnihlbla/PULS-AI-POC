000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3713700.                                                
000400*AUTHOR.         RONNY STENHOLM.                                          
000500*DATE-WRITTEN.   92/08/26.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        ALLA RAPPORTER MED STATUS 9 OCH GODKÄNNANDEDATUM                 
001100*        ÄLDRE ÄN TVÅ VECKOR TAS BORT.                                    
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WLBYTF (WDM6)                              
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
002700*          --- FIL MED NYCKLAR FÖR ATT TA BORT RAPPORTERNA                
002800     SELECT W371ST                     ASSIGN TO W37137D1.                
002810*          --- FILEN SÄTTER KDBYTBEK TILL KLAR I RAPPORTERNA              
002820     SELECT W37154                     ASSIGN TO W37137D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W371ST                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700     SKIP2                                                                
003800*01  -COPY W371STAT      -L.                                              
003810     SKIP3                                                                
003820 FD  W37154                                                               
003830     RECORDING       F                                                    
003840     BLOCK CONTAINS  0.                                                   
003850     SKIP2                                                                
003860*01  -COPY W37154        -L.                                              
003900     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100     SKIP2                                                                
004101*    -- CHECKED BY WY2000                                                 
004110     SKIP3                                                                
004200 77  IDPGM                       PIC X(8)    VALUE 'W3713700'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004410 77  WS-KDBYTBEK                 PIC X       VALUE ' '.                   
004500     SKIP2                                                                
004510 01  CHKP-VAR.                                                            
004520 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004530 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004540 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004550 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004551 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004553 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
004590     SKIP2                                                                
004600 01  FELTEXT.                                                             
004700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004900                                                                          
005000 77  W371ST-EOF-SW               PIC X       VALUE 'N'.                   
005100     88  END-OF-W371ST                       VALUE 'J'.                   
005101                                                                          
005110 77  W37154-EOF-SW               PIC X       VALUE 'N'.                   
005120     88  END-OF-W37154                       VALUE 'J'.                   
005200     EJECT                                                                
006000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES DAGENS-DATUM.                                       
006200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006500     EJECT                                                                
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007200     EJECT                                                                
007300*    - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
007400*                                                                         
007500*01  -COPY W0005   -PRE  POSTSUM-                                         
007600     EJECT                                                                
008100 01  IN-AREA-START               PIC X(24)   VALUE                        
008200                                             'IN-AREA-START'.             
008300     SKIP2                                                                
008400                                                                          
008500*01  AREA -COPY W371STAT     -PRE IN-                                     
008600*                                                                         
008610 01  IN2-AREA-START              PIC X(24)   VALUE                        
008620                                             'IN2-AREA-START'.            
008630     SKIP2                                                                
008640                                                                          
008650*01  AREA -COPY W37154       -PRE IN2-                                    
008660*                                                                         
008700     EJECT                                                                
008800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008900     SKIP3                                                                
009000 01  NYCKLAR-TILL-DLI.                                                    
009100     03  W-WDM601KY-X.                                                    
009110         05  W-IDDISTR           PIC S9(5)    VALUE ZERO COMP-3.          
009200         05  W-IDBYTRAP          PIC S9(7)    VALUE ZERO COMP-3.          
009500     SKIP2                                                                
009600*    --- STATUS-KOD FRÅN IMS                                              
009700 01  STATUS-WS                   PIC XX.                                  
009800     88  SEGMENT-FINNS                       VALUE '  '.                  
009900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
010000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010200     88  IMS-EJ-OK                           VALUE 'XD'.                  
010300     SKIP2                                                                
010400 01  GODK-STATUSKODER.                                                    
010500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010600     SKIP3                                                                
010700 01  SSA1                        PIC X(64).                               
010800 01  SSA2                        PIC X(64).                               
010900     EJECT                                                                
011000*    --- IMS FUNKTIONSKODER                                               
011100*01  -COPY W0003                                                          
011200     EJECT                                                                
011300*    ---  DLI INPUT-OUTPUT AREA                                           
011400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
011500     SKIP3                                                                
011600 01  DLI-IO-AREA.                                                         
011700     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
011800     SKIP3                                                                
011900     03  WLBYTF01 REDEFINES IO-AREA.                                      
012000*        05  -COPY WDM601  -PRE BYTF-                                     
012100     SKIP3                                                                
012400     EJECT                                                                
012500 LINKAGE SECTION.                                                         
012600                                                                          
012700*01  -COPY W0009   -PRE MSG-                                              
012810     EJECT                                                                
012900*01  -COPY W0008  -PRE BYTF-                                              
013000     05  FILLER                  PIC X.                                   
013100     EJECT                                                                
013200 PROCEDURE DIVISION  USING MSG-PCB BYTF-PCB.                              
013300     ENTRY 'DLITCBL' USING MSG-PCB BYTF-PCB.                              
013400                                                                          
013500     SKIP2                                                                
013510*                                                                         
013600     PERFORM A-INIT                                                       
013610*                                                                         
013700     PERFORM S03-LAES-W37154                                              
013701     PERFORM UNTIL END-OF-W37154                                          
013702       IF CHKP-ANT > CHKP-MAX                                             
013703          PERFORM S02-TAG-CHECKPOINT                                      
013704       END-IF                                                             
013705       PERFORM C-UPPDATERA                                                
013706       PERFORM S03-LAES-W37154                                            
013707     END-PERFORM                                                          
013708*                                                                         
013710     PERFORM S01-LAES-W371ST                                              
013800     PERFORM UNTIL END-OF-W371ST                                          
013810       IF CHKP-ANT > CHKP-MAX                                             
013820          PERFORM S02-TAG-CHECKPOINT                                      
013830       END-IF                                                             
013900       PERFORM B-UPPDATERA                                                
014500       PERFORM S01-LAES-W371ST                                            
014600     END-PERFORM                                                          
014610*                                                                         
014700                                                                          
014800                                                                          
014900     PERFORM Z-FINIT                                                      
015000                                                                          
015100     MOVE ZERO TO RETURN-CODE                                             
015200     GOBACK                                                               
015300     .                                                                    
015400     EJECT                                                                
015500 A-INIT SECTION.                                                          
015600     SKIP2                                                                
015700                                                                          
015800     OPEN INPUT W371ST                                                    
015810                W37154                                                    
015900                                                                          
016000     ACCEPT DAGENS-DATUM       FROM DATE                                  
016100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016110     PERFORM IMS-RESTART                                                  
016200     .                                                                    
016300                                                                          
016400                                                                          
019301 B-UPPDATERA SECTION.                                                     
019303     SKIP2                                                                
019304                                                                          
019305     MOVE IN-IDDISTR       TO W-IDDISTR                                   
019306     MOVE IN-IDBYTRAP      TO W-IDBYTRAP                                  
019307     PERFORM IMS-GHU-BYTF                                                 
019308     IF SEGMENT-FINNS                                                     
019310       PERFORM IMS-DLET-BYTF                                              
019311       ADD +1 TO  CHKP-ANT                                                
019312     END-IF                                                               
019313     .                                                                    
019320     EJECT                                                                
019330                                                                          
019340 C-UPPDATERA SECTION.                                                     
019350     SKIP2                                                                
019360                                                                          
019370     MOVE IN2-IDDISTR       TO W-IDDISTR                                  
019380     MOVE IN2-IDBYTRAP      TO W-IDBYTRAP                                 
019381     MOVE IN2-KDBYTBEK      TO WS-KDBYTBEK                                
019390     PERFORM IMS-GHU-BYTF                                                 
019391     IF SEGMENT-FINNS                                                     
019392        MOVE WS-KDBYTBEK TO BYTF-RAPP-KDBYTBEK                            
019393        PERFORM IMS-REPL-BYTF                                             
019394        ADD +1 TO  CHKP-ANT                                               
019395     END-IF                                                               
019396     .                                                                    
019397     EJECT                                                                
019400 Z-FINIT SECTION.                                                         
019500                                                                          
019600                                                                          
019700     CLOSE W371ST                                                         
019710           W37154                                                         
019800     SKIP2                                                                
019900     MOVE 'S' TO POSTSUM-OPKOD                                            
020000     CALL POSTSUM USING POSTSUM-PARM                                      
020100     .                                                                    
020200     EJECT                                                                
020300 S01-LAES-W371ST  SECTION.                                                
020400     SKIP2                                                                
020500     READ W371ST INTO IN-AREA                                             
020600     AT END                                                               
020800        SET END-OF-W371ST TO TRUE                                         
020900                                                                          
021000     NOT AT END                                                           
021100        MOVE 'W371ST' TO POSTSUM-FDNAMN                                   
021200        MOVE 'W37137D1' TO POSTSUM-DDNAMN2                                
021300        MOVE 'STAT' TO POSTSUM-TRANSTYP                                   
021400        CALL POSTSUM USING POSTSUM-PARM                                   
021500     END-READ                                                             
021600     .                                                                    
021700     EJECT                                                                
021710 S02-TAG-CHECKPOINT SECTION.                                              
021717                                                                          
021720     PERFORM IMS-CHECKPOINT                                               
021722     MOVE ZERO TO  CHKP-ANT                                               
021723                                                                          
021725     .                                                                    
021730     EJECT                                                                
021740 S03-LAES-W37154  SECTION.                                                
021750     SKIP2                                                                
021760     READ W37154 INTO IN2-AREA                                            
021770     AT END                                                               
021780        SET END-OF-W37154 TO TRUE                                         
021790                                                                          
021791     NOT AT END                                                           
021792        MOVE 'W37154' TO POSTSUM-FDNAMN                                   
021793        MOVE 'W37137D2' TO POSTSUM-DDNAMN2                                
021794        MOVE 'KDBT' TO POSTSUM-TRANSTYP                                   
021795        CALL POSTSUM USING POSTSUM-PARM                                   
021796     END-READ                                                             
021797     .                                                                    
021798     EJECT                                                                
021800* --- IMS SEKTIONER ---                                                   
021900     SKIP3                                                                
022000     EJECT                                                                
022100 IMS-GHU-BYTF SECTION.                                                    
022200     STRING 'WLBYTF01(WDM601KY =' W-WDM601KY-X ')'                        
022300          DELIMITED BY SIZE INTO SSA1                                     
022400     MOVE '  GE' TO GODK-STATUSKODER                                      
022500     CALL CBLTDLI USING GHU BYTF-PCB DLI-IO-AREA SSA1                     
022600     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
022700     PERFORM IMS-STATUSKONTROLL                                           
022800     .                                                                    
022900     EJECT                                                                
023900 IMS-DLET-BYTF SECTION.                                                   
024000                                                                          
024100     MOVE '  ' TO GODK-STATUSKODER                                        
024200     CALL CBLTDLI USING DLET BYTF-PCB DLI-IO-AREA                         
024300     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
024400     PERFORM IMS-STATUSKONTROLL                                           
024500     .                                                                    
024600     EJECT                                                                
024601 IMS-REPL-BYTF SECTION.                                                   
024602                                                                          
024603     MOVE '  ' TO GODK-STATUSKODER                                        
024604     CALL CBLTDLI USING REPL BYTF-PCB DLI-IO-AREA                         
024605     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
024606     PERFORM IMS-STATUSKONTROLL                                           
024607     .                                                                    
024608     SKIP3                                                                
024610 IMS-RESTART SECTION.                                                     
024620     SKIP2                                                                
024630     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024640     MOVE '  ' TO GODK-STATUSKODER                                        
024650     CALL CBLTDLI USING XRST MSG-PCB                                      
024660                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024670                        CHKP-AREA-LENGTH CHKP-AREA                        
024680     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024690     PERFORM IMS-STATUSKONTROLL                                           
024691     .                                                                    
024697 IMS-CHECKPOINT SECTION.                                                  
024699     SKIP2                                                                
024700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024701     MOVE '  XD' TO GODK-STATUSKODER                                      
024702     CALL CBLTDLI USING CHKP MSG-PCB                                      
024703                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024704                        CHKP-AREA-LENGTH CHKP-AREA                        
024705     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024707     PERFORM IMS-STATUSKONTROLL                                           
024708                                                                          
024709     IF IMS-EJ-OK                                                         
024710       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
024711       DISPLAY FELTEXT                                                    
024712       CALL FELLOG                                                        
024713     END-IF                                                               
024716     .                                                                    
024717     EJECT                                                                
024720 IMS-STATUSKONTROLL SECTION.                                              
024800     SKIP2                                                                
024900     SET STATUS-IX TO 1                                                   
025000     SEARCH GODK-STATUS                                                   
025100       AT END                                                             
025200         MOVE 'XXXIMSABENDXXX' TO FELTEXT-STR                             
025300         DISPLAY FELTEXT                                                  
025400         CALL FELLOG                                                      
025500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
025600         CONTINUE                                                         
025700     END-SEARCH                                                           
025800     .                                                                    
