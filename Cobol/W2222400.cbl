000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2222400.                                                
000400*AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000500*DATE-WRITTEN.   JANUARI 2005.                                            
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        UPPDATERAR KVPB-HIST PÅ WDK6                                     
001410*                                                                         
001500*        PROGRAMMET UPPDATERAR WDK6                                       
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- SASONGSINDEX                                               
003000     SELECT W22224IN                   ASSIGN TO W22224D1.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
004070                                                                          
004080 FD  W22224IN                                                             
004090     RECORDING       F                                                    
004091     BLOCK CONTAINS  0.                                                   
004092                                                                          
004093*01  POST -COPY W22223  -PRE IN-    -L.                                   
004094                                                                          
004110     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W2222400'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004610 77  WS-NOLL                     PIC S9(9)   VALUE ZERO COMP-3.           
004620 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
004621 77  WS-ANTAL-WDK711             PIC S9(9)   VALUE ZERO COMP-3.           
004622 77  WS-ANTAL-TOT                PIC S9(9)   VALUE ZERO COMP-3.           
004623 77  WS-ANTAL-0                  PIC S9(9)   VALUE ZERO COMP-3.           
004624 77  WS-ANTAL-0-5                PIC S9(9)   VALUE ZERO COMP-3.           
004625 77  WS-ANTAL-5-10               PIC S9(9)   VALUE ZERO COMP-3.           
004626 77  WS-ANTAL-10-15              PIC S9(9)   VALUE ZERO COMP-3.           
004627 77  WS-ANTAL-15-100             PIC S9(9)   VALUE ZERO COMP-3.           
004700     SKIP2                                                                
004701 01  CHKP-VAR.                                                            
004702 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004703 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004704 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004705 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004706 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004710 03  CHKP-MAX                    PIC S9(3)   VALUE +500.                  
004720                                                                          
004802                                                                          
004803 01  RESULTAT-RAKNARE.                                                    
004804     03  WS-ANTAL-REPL           PIC S9(9)  VALUE ZERO COMP-3.            
004805     03  WS-ANTAL-ISRT           PIC S9(9)  VALUE ZERO COMP-3.            
004816                                                                          
004817                                                                          
004820 01  FELTEXT.                                                             
004900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005100                                                                          
005200 77  W22224-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W22224                       VALUE 'J'.                   
005301                                                                          
005400     EJECT                                                                
005500                                                                          
005600 01      WS-CURRENT-DATE.                                                 
005700         05  WS-DAGENS-TIAAAA    PIC 9(4)   VALUE ZERO.                   
005800         05  FILLER              PIC 9(4)   VALUE ZERO.                   
005900         05  FILLER              PIC 9(6)   VALUE ZERO.                   
005910                                                                          
005920 01      FILLER REDEFINES WS-CURRENT-DATE.                                
005930*-----   INKLUSIVE SEKEL                                                  
005940         05  WS-DAGENS-DATUM     PIC 9(8).                                
005950         05  WS-DAGENS-TID.                                               
005960             07 WS-DAGENS-TIMME  PIC 9(2).                                
005970             07 WS-DAGENS-MINUT  PIC 9(2).                                
005980             07 WS-DAGENS-SEKUND PIC 9(2).                                
006000     EJECT                                                                
006010*      --- VALID IDDC CODES                                               
006020*                                                                         
006030*01    -COPY WWDC99                                                       
006040       EJECT                                                              
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200*                                                                         
006300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006600     EJECT                                                                
006700*    --- PARAMETRAR TILL POSTSUM                                          
006800*                                                                         
006900*01  -COPY W0005   -PRE  POSTSUM-                                         
007000     EJECT                                                                
007100 01  IN-AREA-START               PIC X(24)   VALUE                        
007200                                             'IN-AREA-START'.             
007300     SKIP2                                                                
007400                                                                          
007500*01  AREA -COPY W22223     -PRE IN-                                       
007600*                                                                         
007610     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  NYCKLAR-TILL-DLI.                                                    
008100     03  W-IDARTNR-X.                                                     
008200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008300     03  W-IDDC-X.                                                        
008400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
008500     SKIP2                                                                
008600*    --- STATUS-KOD FRÅN IMS                                              
008700 01  STATUS-WS                   PIC XX.                                  
008800     88  SEGMENT-FINNS                       VALUE '  '.                  
008900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009200     88  IMS-EJ-OK                           VALUE 'XD'.                  
009300     SKIP2                                                                
009400 01  GODK-STATUSKODER.                                                    
009500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009600     SKIP3                                                                
009700 01  SSA1                        PIC X(64).                               
009800 01  SSA2                        PIC X(64).                               
009810 01  SSA3                        PIC X(64).                               
009900     EJECT                                                                
010000*    --- IMS FUNKTIONSKODER                                               
010100*01  -COPY W0003                                                          
010200     EJECT                                                                
010300*    ---  DLI INPUT-OUTPUT AREA                                           
010310     EJECT                                                                
010320 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
010330     SKIP3                                                                
010340 01  DLI-IO-WDK611.                                                       
010350*    03  -COPY WDK611                                                     
011491     EJECT                                                                
011500 LINKAGE SECTION.                                                         
011600                                                                          
011700*01  -COPY W0009   -PRE MSG-                                              
011800     EJECT                                                                
011900*01  -COPY W0008  -PRE WDK6-                                              
012000     05  FILLER                  PIC X.                                   
012100     EJECT                                                                
012200 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
012300     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
012400                                                                          
012600     PERFORM A-INIT                                                       
012700     PERFORM S01-LAES-W22224                                              
012710     PERFORM UNTIL END-OF-W22224                                          
012770                                                                          
012793       PERFORM B-BEHANDLA-POSTER                                          
012796       PERFORM S01-LAES-W22224                                            
012797                                                                          
012798     END-PERFORM                                                          
013200     PERFORM Z-FINIT                                                      
013300                                                                          
013400     MOVE ZERO TO RETURN-CODE                                             
013500     GOBACK                                                               
013600     .                                                                    
013700     EJECT                                                                
013800 A-INIT SECTION.                                                          
013900     SKIP2                                                                
014000                                                                          
014001     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE                        
014010                                                                          
014100     OPEN INPUT  W22224IN                                                 
014310                                                                          
014400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014410     PERFORM IMS-RESTART                                                  
014500     .                                                                    
014600     EJECT                                                                
014700 B-BEHANDLA-POSTER SECTION.                                               
014800                                                                          
014900     MOVE IN-IDARTNR TO W-IDARTNR                                         
014910                                                                          
014920     PERFORM IMS-GHU-K611                                                 
015128                                                                          
015151     MOVE IN-KVPB-HIST       TO CLAG-KVPB-HIST                            
015152     MOVE IN-TIPBDAT         TO CLAG-TIPBDAT                              
015154     PERFORM IMS-REPL-K611                                                
015155     ADD +1 TO CHKP-ANT                                                   
015156     IF CHKP-ANT > CHKP-MAX                                               
015157       PERFORM IMS-CHECKPOINT                                             
015158       MOVE +0 TO CHKP-ANT                                                
015159     END-IF                                                               
015160     ADD +1                  TO WS-ANTAL-REPL                             
015300     .                                                                    
015400     EJECT                                                                
015500 Z-FINIT SECTION.                                                         
015700                                                                          
015710     DISPLAY 'ANTAL REPL : ' WS-ANTAL-REPL                                
015800     CLOSE W22224IN                                                       
015900     SKIP2                                                                
016000     MOVE 'S' TO POSTSUM-OPKOD                                            
016100     CALL POSTSUM USING POSTSUM-PARM                                      
016200     .                                                                    
016300     EJECT                                                                
016400 S01-LAES-W22224  SECTION.                                                
016500     SKIP2                                                                
016600     READ W22224IN INTO IN-AREA                                           
016700     AT END                                                               
016900        SET END-OF-W22224 TO TRUE                                         
017000                                                                          
017100     NOT AT END                                                           
017200        MOVE 'W22224' TO POSTSUM-FDNAMN                                   
017300        MOVE 'W22224D1' TO POSTSUM-DDNAMN2                                
017500        CALL POSTSUM USING POSTSUM-PARM                                   
017600     END-READ                                                             
017700     .                                                                    
017800     EJECT                                                                
017908                                                                          
017910* --- IMS SEKTIONER ---                                                   
018000     SKIP3                                                                
021008 IMS-GHU-K611 SECTION.                                                    
021009     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
021010          DELIMITED BY SIZE INTO SSA1                                     
021011     MOVE 'WDK611  (KDSEGKEY =1)' TO SSA2                                 
021013     MOVE '  ' TO GODK-STATUSKODER                                        
021014     CALL CBLTDLI USING GHU                                               
021015                      WDK6-PCB DLI-IO-WDK611 SSA1 SSA2                    
021016     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
021017     PERFORM IMS-STATUSKONTROLL                                           
021018     .                                                                    
021019     EJECT                                                                
021033 IMS-REPL-K611 SECTION.                                                   
021034                                                                          
021035     MOVE '  ' TO GODK-STATUSKODER                                        
021036     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
021037     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
021038     PERFORM IMS-STATUSKONTROLL                                           
021039     .                                                                    
021041     EJECT                                                                
021042 IMS-RESTART SECTION.                                                     
021043     SKIP2                                                                
021044     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021045     MOVE '  ' TO GODK-STATUSKODER                                        
021046     CALL CBLTDLI USING XRST MSG-PCB                                      
021047                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021048                        CHKP-AREA-LENGTH CHKP-AREA                        
021049     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021050     PERFORM IMS-STATUSKONTROLL                                           
021051     .                                                                    
021052     EJECT                                                                
021053 IMS-CHECKPOINT SECTION.                                                  
021054     SKIP2                                                                
021055     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021056     MOVE '  XD' TO GODK-STATUSKODER                                      
021057     CALL CBLTDLI USING CHKP MSG-PCB                                      
021058                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021059                        CHKP-AREA-LENGTH CHKP-AREA                        
021060     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021061     PERFORM IMS-STATUSKONTROLL                                           
021062                                                                          
021063     IF IMS-EJ-OK                                                         
021064       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
021065       DISPLAY FELTEXT                                                    
021066       CALL FELLOG                                                        
021067     END-IF                                                               
021068     .                                                                    
021069     EJECT                                                                
021070 IMS-STATUSKONTROLL SECTION.                                              
021100     SKIP2                                                                
021200     SET STATUS-IX TO 1                                                   
021300     SEARCH GODK-STATUS                                                   
021400       AT END                                                             
021500         MOVE 'FEL STATUSKOD' TO FELTEXT-STR                              
021600         DISPLAY FELTEXT                                                  
021700         CALL FELLOG                                                      
021800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
021900         CONTINUE                                                         
022000     END-SEARCH                                                           
022100     .                                                                    
