001100 ID DIVISION.                                                             
001300 PROGRAM-ID.     W1228000.                                                
001400 AUTHOR.         BODIL LINDAHL.                                           
001500 DATE-WRITTEN.   95/07/11.                                                
001510 DATE-COMPILED.                                                           
001600                                                                          
001900*    FUNKTION:                                                            
002000*        PROGRAMMET LÄSER W12207 SOM SKAPAS I PGM W12204.                 
002100*        BÅDE ARTIKLAR SOM SKA SKALAS/RENSAS WDK6 TAS BORT                
002200*        FRÅN WDK7.                                                       
002300*                                                                         
002410*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
002500*                                                                         
003000                                                                          
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 INPUT-OUTPUT SECTION.                                                    
003500                                                                          
003600 FILE-CONTROL.                                                            
003701     SKIP2                                                                
003702*          --- ARTIKLAR SOM SKA RENSAS/SKALAS WDK6                        
003710     SELECT W12207                     ASSIGN TO W12280D1.                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004301     SKIP3                                                                
004302 FD  W12207                                                               
004303     RECORDING       F                                                    
004304     BLOCK CONTAINS  0.                                                   
004305                                                                          
004310*01  -COPY W12207      -L.                                                
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600     SKIP2                                                                
004601                                                                          
004610*    -- CHECKED BY WY2000                                                 
004700 77  IDPGM                       PIC X(8)    VALUE 'W1228000'.            
004800 01  CHKP-VAR.                                                            
004900 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005000 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005100 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005200 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005300 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005400 03  CHKP-MAX                    PIC S9(3)   VALUE +50.                   
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005600 77  W-DLET-WDK701               PIC 9(7)    VALUE ZERO.                  
005700     SKIP2                                                                
005800 01  FELTEXT.                                                             
005900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006201                                                                          
006202 77  W12207-EOF-SW               PIC X       VALUE 'N'.                   
006210     88  END-OF-W12207                       VALUE 'J'.                   
006500     EJECT                                                                
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300*                                                                         
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007610     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007701     EJECT                                                                
007702*    --- PARAMETRAR TILL POSTSUM                                          
007703*                                                                         
007710*01  -COPY W0005   -PRE  POSTSUM-                                         
008001     EJECT                                                                
008002 01  IN-AREA-START               PIC X(24)   VALUE                        
008003                                             'IN-AREA-START'.             
008004     SKIP2                                                                
008005                                                                          
008010*01  AREA -COPY W12207     -PRE IN-                                       
008100*                                                                         
008200     EJECT                                                                
008300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008400     SKIP3                                                                
008500 01  NYCKLAR-TILL-DLI.                                                    
008601     03  W-IDARTNR-X.                                                     
008610         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008700     SKIP2                                                                
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
010600*    ---  DLI INPUT-OUTPUT AREA                                           
010700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010800     SKIP3                                                                
010900 01  DLI-IO-AREA.                                                         
011000     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
011101     SKIP3                                                                
011102     03  WLARTS01 REDEFINES IO-AREA.                                      
011110*        05  -COPY WDK701  -PRE ARTS-                                     
011200*    ---  DLI INPUT-OUTPUT AREA                                           
011300 01  FILLER                      PIC X(16)                                
011400                             VALUE 'DLI-IO-AREA-2'.                       
012100     EJECT                                                                
012200 LINKAGE SECTION.                                                         
012400*01  -COPY W0009   -PRE MSG-                                              
012501                                                                          
012502*01  -COPY W0008  -PRE ARTS-                                              
012510     05  FILLER                  PIC X.                                   
012800     EJECT                                                                
012901 PROCEDURE DIVISION  USING MSG-PCB ARTS-PCB.                              
012902 MAIN SECTION.                                                            
012910     ENTRY 'DLITCBL' USING MSG-PCB ARTS-PCB.                              
013000                                                                          
013300     PERFORM A-INIT                                                       
013400                                                                          
013410     PERFORM S01-LAES-W12207                                              
013500     PERFORM UNTIL END-OF-W12207                                          
013801       IF IN-UTFIL-TYP = 'S' OR 'B'                                       
013810         MOVE IN-IDARTNR TO W-IDARTNR                                     
013900         PERFORM IMS-GET-ARTS01                                           
014000         IF SEGMENT-FINNS                                                 
014100            PERFORM IMS-DLET-ARTS                                         
                  ADD 1 TO W-DLET-WDK701                                        
014200            ADD +1 TO CHKP-ANT                                            
014300         END-IF                                                           
014310         IF CHKP-ANT > CHKP-MAX                                           
014320           PERFORM X-TAG-CHECKPOINT                                       
014330         END-IF                                                           
014400       END-IF                                                             
014510       PERFORM S01-LAES-W12207                                            
014600     END-PERFORM                                                          
014800                                                                          
014900     PERFORM Z-FINIT                                                      
015100     MOVE ZERO TO RETURN-CODE                                             
015200     GOBACK                                                               
015300     .                                                                    
015400     EJECT                                                                
015500 A-INIT SECTION.                                                          
015700                                                                          
015800     PERFORM IMS-RESTART                                                  
016010     OPEN INPUT W12207                                                    
016710     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017000     .                                                                    
017200     EJECT                                                                
017300 Z-FINIT SECTION.                                                         
017801                                                                          
           DISPLAY 'ANTAL BORTTAGNA WDK701: ' W-DLET-WDK701                     
017810     CLOSE W12207                                                         
018002     MOVE 'S' TO POSTSUM-OPKOD                                            
018010     CALL POSTSUM USING POSTSUM-PARM                                      
018200     .                                                                    
018301     EJECT                                                                
018302 S01-LAES-W12207  SECTION.                                                
018303     SKIP2                                                                
018304     READ W12207 INTO IN-AREA                                             
018305     AT END                                                               
018307        SET END-OF-W12207 TO TRUE                                         
018308                                                                          
018309     NOT AT END                                                           
018310        MOVE 'W12207'     TO POSTSUM-FDNAMN                               
018311        MOVE 'W12280D1'   TO POSTSUM-DDNAMN2                              
018312        MOVE IN-UTFIL-TYP TO POSTSUM-TRANSTYP                             
018313        CALL POSTSUM USING POSTSUM-PARM                                   
018316     END-READ                                                             
018320     .                                                                    
018600     EJECT                                                                
018700 X-TAG-CHECKPOINT SECTION.                                                
018800                                                                          
019400     PERFORM IMS-CHECKPOINT                                               
019500     MOVE ZERO TO CHKP-ANT                                                
019700     .                                                                    
019800     EJECT                                                                
019900* --- IMS SEKTIONER ---                                                   
020000     SKIP3                                                                
020102 IMS-GET-ARTS01 SECTION.                                                  
020104     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
020105          DELIMITED BY SIZE INTO SSA1                                     
020106     MOVE '  GE' TO GODK-STATUSKODER                                      
020107     CALL CBLTDLI USING GHU ARTS-PCB DLI-IO-AREA SSA1                     
020108     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
020109     PERFORM IMS-STATUSKONTROLL                                           
020110     .                                                                    
020120     SKIP3                                                                
020121 IMS-DLET-ARTS SECTION.                                                   
020123     MOVE '  ' TO GODK-STATUSKODER                                        
020124     CALL CBLTDLI USING DLET ARTS-PCB DLI-IO-AREA                         
020125     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
020126     PERFORM IMS-STATUSKONTROLL                                           
020130     .                                                                    
020200     EJECT                                                                
020300 IMS-RESTART SECTION.                                                     
020400     SKIP2                                                                
020500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020600     MOVE '  ' TO GODK-STATUSKODER                                        
020700     CALL CBLTDLI USING XRST MSG-PCB                                      
020800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020900                        CHKP-AREA-LENGTH CHKP-AREA                        
021000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021100     PERFORM IMS-STATUSKONTROLL                                           
021200     .                                                                    
021300     SKIP3                                                                
021400 IMS-CHECKPOINT SECTION.                                                  
021600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021700     MOVE '  XD' TO GODK-STATUSKODER                                      
021800     CALL CBLTDLI USING CHKP MSG-PCB                                      
021900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
022000                        CHKP-AREA-LENGTH CHKP-AREA                        
022100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
022200     PERFORM IMS-STATUSKONTROLL                                           
022400     IF IMS-EJ-OK                                                         
022500       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
022600       DISPLAY FELTEXT                                                    
022700       CALL FELLOG                                                        
022800     END-IF                                                               
022900     .                                                                    
023000     EJECT                                                                
023100 IMS-STATUSKONTROLL SECTION.                                              
023200     SKIP2                                                                
023300     SET STATUS-IX TO 1                                                   
023400     SEARCH GODK-STATUS                                                   
023500       AT END                                                             
023800         CALL FELLOG                                                      
023900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
024000         CONTINUE                                                         
024100     END-SEARCH                                                           
024200     .                                                                    
