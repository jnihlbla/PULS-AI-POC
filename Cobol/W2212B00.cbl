001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W2212B00.                                                
001400 AUTHOR.         KJELLSON GÖRAN.                                          
001500 DATE-WRITTEN.   14/03/26.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNKTION:                                                            
002000*        SKAPAR HÄNDELSE 2203/2204 PÅ WDG3                                
002100*                                                                         
002210*        PROGRAMMET UPPDATERAR WDG3                                       
002300*                                                                         
002400                                                                          
002500                                                                          
002600 ENVIRONMENT DIVISION.                                                    
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003101     SKIP2                                                                
003102*          --- UPPDATERINGSPOSTER TILL WDG3 2203/04                       
003110     SELECT W2212A                     ASSIGN TO W2212BD1.                
003300                                                                          
003400 DATA DIVISION.                                                           
003600 FILE SECTION.                                                            
003701                                                                          
003702 FD  W2212A                                                               
003703     RECORDING       F                                                    
003704     BLOCK CONTAINS  0.                                                   
003705                                                                          
003710*01  -COPY W2242204      -L.                                              
003800                                                                          
003810                                                                          
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'W2212B00'.            
004200*01  CHKP-VAR.                                                            
004300*    03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004400*    03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004500*    03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004600*    03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004700*    03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004800*    03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100                                                                          
005200 01  FELTEXT.                                                             
005300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005601                                                                          
005602 77  W2212A-EOF-SW               PIC X       VALUE 'N'.                   
005610     88  END-OF-W2212A                       VALUE 'J'.                   
005900                                                                          
005910                                                                          
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007101                                                                          
007102                                                                          
007103*    --- PARAMETRAR TILL POSTSUM                                          
007104                                                                          
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007401                                                                          
007402                                                                          
007403 01  IN-AREA-START               PIC X(24)   VALUE                        
007404                                             'IN-AREA-START'.             
007406                                                                          
007410*01  AREA -COPY W2242204     -PRE IN-                                     
007500*                                                                         
007600                                                                          
007610                                                                          
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800                                                                          
007900 01  NYCKLAR-TILL-DLI.                                                    
008001     03  W-WDGX2203-X.                                                    
008002         05  W-IDHTYP-2203       PIC X(04)    VALUE '2203'.               
008003         05  W-IDDC-2203         PIC X(02)    VALUE SPACE.                
008004         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
008005     03  W-IDARTNR-X.                                                     
008010         05  W-IDARTNR           PIC S9(7)   VALUE ZERO COMP-3.           
008100                                                                          
008110                                                                          
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008800     88  IMS-EJ-OK                           VALUE 'XD'.                  
008900                                                                          
009000 01  GODK-STATUSKODER.                                                    
009100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200                                                                          
009300 01  SSA1                        PIC X(64).                               
009400 01  SSA2                        PIC X(64).                               
009500                                                                          
009510                                                                          
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800                                                                          
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100                                                                          
010201 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG301'.                      
010202 01  DLI-IO-WDG301.                                                       
010203*    03  -COPY WDG301                                                     
010204                                                                          
010205 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2204'.                    
010206 01  DLI-IO-WDGX2204.                                                     
010210*    03  -COPY WDGX2204                                                   
010300                                                                          
010700                                                                          
010800 LINKAGE SECTION.                                                         
010900                                                                          
011000*01  -COPY W0009   -PRE MSG-                                              
011101                                                                          
011102*01  -COPY W0008   -PRE WDG3-                                             
011110     05  FILLER                  PIC X.                                   
011400                                                                          
011500                                                                          
011501 PROCEDURE DIVISION  USING MSG-PCB WDG3-PCB.                              
011502 MAIN SECTION.                                                            
011510     ENTRY 'DLITCBL' USING MSG-PCB WDG3-PCB.                              
011600                                                                          
011800                                                                          
011900     PERFORM A-INIT                                                       
012010     PERFORM S01-LAES-W2212A                                              
012100     PERFORM UNTIL END-OF-W2212A                                          
012200*      IF CHKP-ANT > CHKP-MAX                                             
012300*        PERFORM X-TAG-CHECKPOINT                                         
012400*      END-IF                                                             
012500                                                                          
012520       MOVE IN-IDDC                  TO W-IDDC-2203                       
012521       MOVE IN-IDARTNR               TO 2204-IDARTNR                      
012530       MOVE IN-KDLPORS               TO 2204-KDLPORS                      
012540       PERFORM IMS-ISRT-WDGX2204                                          
013000                                                                          
013110       PERFORM S01-LAES-W2212A                                            
013200     END-PERFORM                                                          
013300                                                                          
013400                                                                          
013500     PERFORM Z-FINIT                                                      
013600                                                                          
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
014000                                                                          
014010                                                                          
014100 A-INIT SECTION.                                                          
014300                                                                          
014400*    PERFORM IMS-RESTART                                                  
014601                                                                          
014610     OPEN INPUT W2212A                                                    
014900                                                                          
015200                                                                          
015310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015600     .                                                                    
015800                                                                          
015810                                                                          
015900 Z-FINIT SECTION.                                                         
016000                                                                          
016401                                                                          
016410     CLOSE W2212A                                                         
016601                                                                          
016602     MOVE 'S' TO POSTSUM-OPKOD                                            
016610     CALL POSTSUM USING POSTSUM-PARM                                      
016800     .                                                                    
016901                                                                          
016902                                                                          
016903 S01-LAES-W2212A  SECTION.                                                
016904                                                                          
016905     READ W2212A INTO IN-AREA                                             
016906     AT END                                                               
016908        SET END-OF-W2212A TO TRUE                                         
016909                                                                          
016910     NOT AT END                                                           
016911        MOVE 'W2212A'     TO POSTSUM-FDNAMN                               
016912        MOVE 'W2212BD1'   TO POSTSUM-DDNAMN2                              
016913        MOVE '2204'       TO POSTSUM-TRANSTYP                             
016914        CALL POSTSUM USING POSTSUM-PARM                                   
016915                                                                          
016916*       ADD 1 TO W-W2212A-KVPOST-IN                                       
016917     END-READ                                                             
016920     .                                                                    
017200                                                                          
017210                                                                          
017300*X-TAG-CHECKPOINT   SECTION.                                              
017400*                                                                         
018000*    PERFORM IMS-CHECKPOINT                                               
018100*    MOVE ZERO TO CHKP-ANT                                                
018300*    .                                                                    
018400                                                                          
018410                                                                          
018500* --- IMS SEKTIONER ---                                                   
018600                                                                          
018730 IMS-ISRT-WDGX2204 SECTION.                                               
018750                                                                          
018770     STRING 'WDG301  (WDG3KEY  =' W-WDGX2203-X ')'                        
018780          DELIMITED BY SIZE INTO SSA1                                     
018790     MOVE   'WDG302  '        TO SSA2                                     
018800     MOVE '  '                TO GODK-STATUSKODER                         
018810     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDGX2204 SSA1 SSA2           
018820     MOVE WDG3-STATUS-CODE    TO STATUS-WS                                
018830     PERFORM IMS-STATUSKONTROLL                                           
018840     .                                                                    
018850                                                                          
018900*IMS-RESTART SECTION.                                                     
019000*    SKIP2                                                                
019100*    MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019200*    MOVE '  ' TO GODK-STATUSKODER                                        
019300*    CALL CBLTDLI USING XRST MSG-PCB                                      
019400*                       CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019500*                       CHKP-AREA-LENGTH CHKP-AREA                        
019600*    MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019700*    PERFORM IMS-STATUSKONTROLL                                           
019800*    .                                                                    
019900*                                                                         
019910*                                                                         
020000*IMS-CHECKPOINT SECTION.                                                  
020100*                                                                         
020200*    MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020300*    MOVE '  XD' TO GODK-STATUSKODER                                      
020400*    CALL CBLTDLI USING CHKP MSG-PCB                                      
020500*                       CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020600*                       CHKP-AREA-LENGTH CHKP-AREA                        
020700*    MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020800*    PERFORM IMS-STATUSKONTROLL                                           
020900*                                                                         
021000*    IF IMS-EJ-OK                                                         
021100*      MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
021200*      DISPLAY FELTEXT                                                    
021300*      CALL FELLOG                                                        
021400*    END-IF                                                               
021500*    .                                                                    
021600                                                                          
021610                                                                          
021700 IMS-STATUSKONTROLL SECTION.                                              
021800                                                                          
021900     SET STATUS-IX TO 1                                                   
022000     SEARCH GODK-STATUS                                                   
022100       AT END                                                             
022200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022300           DELIMITED BY SIZE INTO FELTEXT                                 
022400         DISPLAY FELTEXT                                                  
022500         CALL FELLOG                                                      
022600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
022700         CONTINUE                                                         
022800     END-SEARCH                                                           
022900     .                                                                    
