000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4405C00.                                                
000400*AUTHOR.         LASSI OLGRENER.                                          
000500*DATE-WRITTEN.   93/12/07.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        RENSNING VORKÖNY - WDA6                                          
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDA6                                       
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- RENS.POSTER                                                
002700     SELECT W4405C                     ASSIGN TO W4405CD1.                
002800     SELECT W4405O                     ASSIGN TO W4405CD2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W4405C                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700     SKIP2                                                                
003800*01  -COPY W4405B      -L.                                                
003900                                                                          
004000                                                                          
004100 FD  W4405O                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400     SKIP2                                                                
004500*01  POST -COPY W4405B -PRE OLD-  -L.                                     
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800     SKIP2                                                                
004900                                                                          
005000*    -- CHECKED BY WY2000                                                 
005100 77  IDPGM                       PIC X(8)    VALUE 'W4405C00'.            
005200 01  CHKP-VAR.                                                            
005300 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005400 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005500 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005600 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005700 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005800 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100     SKIP2                                                                
006200 01  FELTEXT.                                                             
006300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006500                                                                          
006600 77  W4405C-EOF-SW               PIC X       VALUE 'N'.                   
006700     88  END-OF-W4405C                       VALUE 'J'.                   
006800                                                                          
006900 77  RENSA-ORDER-SW              PIC X       VALUE 'N'.                   
007000     88  RENSA-ORDER                         VALUE 'J'.                   
007010                                                                          
007020 77  GAMMAL-ORDER-SW             PIC X       VALUE 'N'.                   
007030     88  GAMMAL-ORDER                        VALUE 'J'.                   
007100     EJECT                                                                
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300*                                                                         
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008100     EJECT                                                                
008200 01  IN-AREA-START               PIC X(24)   VALUE                        
008300                                             'IN-AREA-START'.             
008400     SKIP2                                                                
008500                                                                          
008600*01  AREA -COPY W4405B     -PRE IN-                                       
008700*                                                                         
008800                                                                          
008900*01  AREA -COPY W4405B     -PRE OLD-                                      
009000*                                                                         
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009300     SKIP3                                                                
009400 01  NYCKLAR-TILL-DLI.                                                    
009500     03  W-WDA6KEY-X.                                                     
009600         05  WDA6KEY-IDDISTR        PIC S9(5)    COMP-3.                  
009700         05  WDA6KEY-IDKUNDNR       PIC S9(7)    COMP-3.                  
009800         05  WDA6KEY-IDKUNDRF       PIC X(10).                            
009900         05  WDA6KEY-TIREGDAT-URSP  PIC S9(7)    COMP-3.                  
010000         05  WDA6KEY-IDARTNR        PIC S9(9)    COMP-3.                  
010100         05  WDA6KEY-TIREGTID-URSP  PIC S9(9)    COMP-3.                  
010200         05  WDA6KEY-TIREGDAT-AVV   PIC S9(7)    COMP-3.                  
010300         05  WDA6KEY-TIREGTID-AVV   PIC S9(9)    COMP-3.                  
010400     SKIP2                                                                
010500     03  W-WDGXKEY-X.                                                     
010600         05  W-WDGXKEY           PIC X(16)    VALUE SPACE.                
010700     SKIP2                                                                
010800*    --- STATUS-KOD FRÅN IMS                                              
010900 01  STATUS-WS                   PIC XX.                                  
011000     88  SEGMENT-FINNS                       VALUE '  '.                  
011100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011300     88  IMS-EJ-OK                           VALUE 'XD'.                  
011400     SKIP2                                                                
011500 01  GODK-STATUSKODER.                                                    
011600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011700     SKIP3                                                                
011800 01  SSA1                        PIC X(80).                               
011900     EJECT                                                                
012000*    --- IMS FUNKTIONSKODER                                               
012100*01  -COPY W0003                                                          
012200     EJECT                                                                
012300*    ---  DLI INPUT-OUTPUT AREA                                           
012400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012500     SKIP3                                                                
012600 01  DLI-IO-AREA.                                                         
012700*    03  -COPY WDA601                                                     
012800     EJECT                                                                
012900 LINKAGE SECTION.                                                         
013000                                                                          
013100*01  -COPY W0009   -PRE MSG-                                              
013200     EJECT                                                                
013300*01  -COPY W0008  -PRE WDA6-                                              
013400     05  FILLER                  PIC X.                                   
013500     EJECT                                                                
013600 PROCEDURE DIVISION  USING MSG-PCB WDA6-PCB.                              
013700     ENTRY 'DLITCBL' USING MSG-PCB WDA6-PCB.                              
013800                                                                          
013900     PERFORM A-INIT                                                       
014000     PERFORM S01-LAES-W4405C                                              
014100     PERFORM UNTIL END-OF-W4405C                                          
014200                                                                          
014300       IF CHKP-ANT > CHKP-MAX                                             
014400         PERFORM X-TAG-CHECKPOINT                                         
014500       END-IF                                                             
014600                                                                          
014700       IF  IN-IDARTNR = ZERO                                              
014710         IF IN-TIKLAR = 010101                                            
014720            MOVE JA           TO GAMMAL-ORDER-SW                          
014730         ELSE                                                             
014731            MOVE NEJ          TO GAMMAL-ORDER-SW                          
014740         END-IF                                                           
014750                                                                          
014800         IF IN-KVWORKD > 20                                               
014900           MOVE JA            TO RENSA-ORDER-SW                           
015000         ELSE                                                             
015100           MOVE NEJ           TO RENSA-ORDER-SW                           
015200         END-IF                                                           
015300       END-IF                                                             
015400                                                                          
015500       IF  IN-IDARTNR > ZERO                                              
015610         IF GAMMAL-ORDER                                                  
015620            MOVE IN-AREA TO OLD-AREA                                      
015630            PERFORM S02-SKRIV-W4405O                                      
015631*           PERFORM B-RENSA-ORDER                                         
015632* INITIALT SKRIVER VI BARA UT DESSA POSTER PÅ UTFILEN                     
015633* PÅ SIKT SKALL ÄVEN DESSA POSTER RENSAS                                  
015640         ELSE                                                             
015650            IF RENSA-ORDER                                                
015660               PERFORM B-RENSA-ORDER                                      
017001            END-IF                                                        
017010         END-IF                                                           
017100       END-IF                                                             
017200                                                                          
017300                                                                          
017400       PERFORM S01-LAES-W4405C                                            
017500     END-PERFORM                                                          
017600                                                                          
017700     PERFORM Z-FINIT                                                      
017800                                                                          
017900     MOVE ZERO TO RETURN-CODE                                             
018000     GOBACK                                                               
018100     .                                                                    
018200     EJECT                                                                
018300 A-INIT SECTION.                                                          
018400                                                                          
018500     PERFORM IMS-RESTART                                                  
018600                                                                          
018700     OPEN INPUT  W4405C                                                   
018800     OPEN OUTPUT W4405O                                                   
018900                                                                          
019000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019100     .                                                                    
019200     EJECT                                                                
019300 B-RENSA-ORDER SECTION.                                                   
019400                                                                          
019401     MOVE IN-IDDISTR       TO WDA6KEY-IDDISTR                             
019402     MOVE IN-IDKUNDNR      TO WDA6KEY-IDKUNDNR                            
019403     MOVE IN-IDKUNDRF      TO WDA6KEY-IDKUNDRF                            
019404     MOVE IN-TIREGDAT-URSP TO WDA6KEY-TIREGDAT-URSP                       
019405     MOVE IN-IDARTNR       TO WDA6KEY-IDARTNR                             
019406     MOVE IN-TIREGTID-URSP TO WDA6KEY-TIREGTID-URSP                       
019407     MOVE IN-TIREGDAT-AVV  TO WDA6KEY-TIREGDAT-AVV                        
019408     MOVE IN-TIREGTID-AVV  TO WDA6KEY-TIREGTID-AVV                        
019409     PERFORM IMS-GET-WDA601                                               
019410                                                                          
019411     IF SEGMENT-FINNS                                                     
019412        PERFORM IMS-DLET-WDA6                                             
019413        ADD 1 TO CHKP-ANT                                                 
019414     END-IF                                                               
019415     .                                                                    
019420     EJECT                                                                
019430 Z-FINIT SECTION.                                                         
019440                                                                          
019500     CLOSE W4405C W4405O                                                  
019600     SKIP2                                                                
019700     MOVE 'S' TO POSTSUM-OPKOD                                            
019800     CALL POSTSUM USING POSTSUM-PARM                                      
019900     .                                                                    
020000     EJECT                                                                
020100 S01-LAES-W4405C  SECTION.                                                
020200     SKIP2                                                                
020300     READ W4405C INTO IN-AREA                                             
020400     AT END                                                               
020500        SET END-OF-W4405C TO TRUE                                         
020600                                                                          
020700     NOT AT END                                                           
020800        MOVE 'W4405C'   TO POSTSUM-FDNAMN                                 
020900        MOVE 'W4405CD1' TO POSTSUM-DDNAMN2                                
021000        MOVE SPACE      TO POSTSUM-TRANSTYP                               
021100        CALL POSTSUM USING POSTSUM-PARM                                   
021200     END-READ                                                             
021300     .                                                                    
021310     EJECT                                                                
021320 S02-SKRIV-W4405O  SECTION.                                               
021330     SKIP2                                                                
021331                                                                          
021332     WRITE OLD-POST FROM OLD-AREA                                         
021333                                                                          
021334     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
021335     MOVE 'W4405O'   TO POSTSUM-FDNAMN                                    
021336     MOVE 'W4405CD2' TO POSTSUM-DDNAMN2                                   
021337     CALL POSTSUM USING POSTSUM-PARM                                      
021395     .                                                                    
021400     EJECT                                                                
021500 X-TAG-CHECKPOINT   SECTION.                                              
021600                                                                          
021700     PERFORM IMS-CHECKPOINT                                               
021800     MOVE ZERO TO CHKP-ANT                                                
021900     .                                                                    
022000     EJECT                                                                
022100* --- IMS SEKTIONER ---                                                   
022200     SKIP3                                                                
022300 IMS-GET-WDA601 SECTION.                                                  
022400     STRING 'WDA601  (WDA601KY =' W-WDA6KEY-X ')'                         
022500          DELIMITED BY SIZE INTO SSA1                                     
022600     MOVE '  GE' TO GODK-STATUSKODER                                      
022700     CALL CBLTDLI USING GHU WDA6-PCB DLI-IO-AREA SSA1                     
022800     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
022900     PERFORM IMS-STATUSKONTROLL                                           
023000     .                                                                    
023100     SKIP3                                                                
023200 IMS-DLET-WDA6 SECTION.                                                   
023300                                                                          
023400     MOVE '  ' TO GODK-STATUSKODER                                        
023500     CALL CBLTDLI USING DLET WDA6-PCB DLI-IO-AREA                         
023600     MOVE WDA6-STATUS-CODE TO STATUS-WS                                   
023700     PERFORM IMS-STATUSKONTROLL                                           
023800     .                                                                    
023900     EJECT                                                                
024000 IMS-RESTART SECTION.                                                     
024100     SKIP2                                                                
024200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024300     MOVE '  ' TO GODK-STATUSKODER                                        
024400     CALL CBLTDLI USING XRST MSG-PCB                                      
024500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024600                        CHKP-AREA-LENGTH CHKP-AREA                        
024700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024800     PERFORM IMS-STATUSKONTROLL                                           
024900     .                                                                    
025000     EJECT                                                                
025100 IMS-CHECKPOINT SECTION.                                                  
025200     SKIP2                                                                
025300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
025400     MOVE '  XD' TO GODK-STATUSKODER                                      
025500     CALL CBLTDLI USING CHKP MSG-PCB                                      
025600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
025700                        CHKP-AREA-LENGTH CHKP-AREA                        
025800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
025900     PERFORM IMS-STATUSKONTROLL                                           
026000                                                                          
026100     IF IMS-EJ-OK                                                         
026200       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
026300       DISPLAY FELTEXT                                                    
026400       CALL FELLOG                                                        
026500     END-IF                                                               
026600     .                                                                    
026700     EJECT                                                                
026800 IMS-STATUSKONTROLL SECTION.                                              
026900     SKIP2                                                                
027000     SET STATUS-IX TO 1                                                   
027100     SEARCH GODK-STATUS                                                   
027200       AT END                                                             
027300         MOVE 'IMS BANG..' TO FELTEXT-STR                                 
027400         DISPLAY FELTEXT                                                  
027500         CALL FELLOG                                                      
027600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
027700         CONTINUE                                                         
027800     END-SEARCH                                                           
027900     .                                                                    
