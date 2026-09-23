000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2222800.                                                
000400*AUTHOR.         STEFAN ANDREASSON.                                       
000500*DATE-WRITTEN.   FEB 2000.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        UPPDATERAR FÖRÄNDRAD PROGNOS PÅ WDG3                             
001010*        FÖR SATSARTIKLAR                                                 
001100*        INGET ÅTERSTARTSREGISTER BEHÖVS                                  
001400*                                                                         
001500*        PROGRAMMET UPPDATERAR WDG3                                       
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
002900*          --- PROGNOS SOM SKA FÖRÄNDRAS FÖR SATSARTIKLAR                 
003000     SELECT W22228                     ASSIGN TO W22228D1.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W22228                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  -COPY WDG32202    -L.                                                
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W2222800'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004620 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
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
004826                                                                          
004830 01  FELTEXT.                                                             
004900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005100                                                                          
005200 77  W22228-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W22228                       VALUE 'J'.                   
005301                                                                          
005400     EJECT                                                                
005500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES DAGENS-DATUM.                                       
005700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006000     EJECT                                                                
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
007500*01  AREA -COPY WDG32202   -PRE IN-                                       
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007900     SKIP3                                                                
008000 01  NYCKLAR-TILL-DLI.                                                    
008100     03  W-IDARTNR-X.                                                     
008200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008300     03  W-IDDC-X.                                                        
008400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
008420     03  W-WDG3KEY.                                                       
008430         05  W-IDHTYP        PIC X(4)    VALUE '2202'.                    
008440         05  W-NYCKEL-VALFRI PIC X(26)   VALUE LOW-VALUE.                 
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
009900     EJECT                                                                
010000*    --- IMS FUNKTIONSKODER                                               
010100*01  -COPY W0003                                                          
010200     EJECT                                                                
010300*    ---  DLI INPUT-OUTPUT AREA                                           
010360     EJECT                                                                
010370 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDG303'.             
010380     SKIP3                                                                
010390 01  DLI-IO-AREA-WDG303.                                                  
010400*        05   -COPY WDG32202                                              
011400     EJECT                                                                
011500 LINKAGE SECTION.                                                         
011600                                                                          
011700*01  -COPY W0009   -PRE MSG-                                              
011800     EJECT                                                                
011900*01  -COPY W0008  -PRE WDG3-                                              
012000     05  FILLER                  PIC X.                                   
012100     EJECT                                                                
012200 PROCEDURE DIVISION  USING MSG-PCB WDG3-PCB.                              
012300     ENTRY 'DLITCBL' USING MSG-PCB WDG3-PCB.                              
012400                                                                          
012600     PERFORM A-INIT                                                       
012700     PERFORM S01-LAES-W22228                                              
012800     PERFORM UNTIL END-OF-W22228                                          
012820                                                                          
012900       PERFORM B-BEHANDLA-POSTER                                          
012910                                                                          
013000       PERFORM S01-LAES-W22228                                            
013060                                                                          
013100     END-PERFORM                                                          
013200     PERFORM Z-FINIT                                                      
013300                                                                          
013400     MOVE ZERO TO RETURN-CODE                                             
013500     GOBACK                                                               
013600     .                                                                    
013700     EJECT                                                                
013800 A-INIT SECTION.                                                          
013900     SKIP2                                                                
014000                                                                          
014001     ACCEPT DAGENS-DATUM FROM DATE                                        
014010                                                                          
014100     OPEN INPUT W22228                                                    
014300                                                                          
014400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014410     PERFORM IMS-RESTART                                                  
014500     .                                                                    
014600     EJECT                                                                
014700 B-BEHANDLA-POSTER SECTION.                                               
014800                                                                          
014810                                                                          
014840     MOVE IN-AREA            TO WDG32202                                  
014880     PERFORM IMS-ISRT-R2202                                               
014890     ADD +1 TO CHKP-ANT                                                   
014900     IF CHKP-ANT > CHKP-MAX                                               
015000       PERFORM IMS-CHECKPOINT                                             
015100       MOVE +0 TO CHKP-ANT                                                
015110     END-IF                                                               
015190     .                                                                    
015400     EJECT                                                                
015500 Z-FINIT SECTION.                                                         
015700                                                                          
015800     CLOSE W22228                                                         
015900     SKIP2                                                                
016000     MOVE 'S' TO POSTSUM-OPKOD                                            
016100     CALL POSTSUM USING POSTSUM-PARM                                      
016101                                                                          
016200     .                                                                    
016300     EJECT                                                                
016400 S01-LAES-W22228  SECTION.                                                
016500     SKIP2                                                                
016600     READ W22228 INTO IN-AREA                                             
016700     AT END                                                               
016900        SET END-OF-W22228 TO TRUE                                         
017000                                                                          
017100     NOT AT END                                                           
017200        MOVE 'W22228' TO POSTSUM-FDNAMN                                   
017300        MOVE 'W22228D1' TO POSTSUM-DDNAMN2                                
017500        CALL POSTSUM USING POSTSUM-PARM                                   
017600     END-READ                                                             
017700     .                                                                    
017800     EJECT                                                                
017900 IMS-ISRT-R2202 SECTION.                                                  
018000                                                                          
018100     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY ')'                           
018200             DELIMITED BY SIZE INTO SSA1                                  
018300     MOVE 'WDG303  ' TO SSA2                                              
018400     MOVE '  ' TO GODK-STATUSKODER                                        
018500     CALL CBLTDLI USING ISRT WDG3-PCB                                     
018600                                 DLI-IO-AREA-WDG303 SSA1 SSA2             
018700     MOVE WDG3-STATUS-CODE TO STATUS-WS                                   
018800     PERFORM IMS-STATUSKONTROLL                                           
018900     .                                                                    
018910     EJECT                                                                
019100 IMS-RESTART SECTION.                                                     
019200     SKIP2                                                                
019300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019400     MOVE '  ' TO GODK-STATUSKODER                                        
019500     CALL CBLTDLI USING XRST MSG-PCB                                      
019600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019700                        CHKP-AREA-LENGTH CHKP-AREA                        
019800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019900     PERFORM IMS-STATUSKONTROLL                                           
020000     .                                                                    
020100     EJECT                                                                
020200 IMS-CHECKPOINT SECTION.                                                  
020300     SKIP2                                                                
020400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020500     MOVE '  XD' TO GODK-STATUSKODER                                      
020600     CALL CBLTDLI USING CHKP MSG-PCB                                      
020700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020800                        CHKP-AREA-LENGTH CHKP-AREA                        
020900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021000     PERFORM IMS-STATUSKONTROLL                                           
021001                                                                          
021002     IF IMS-EJ-OK                                                         
021003       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
021004       DISPLAY FELTEXT                                                    
021005       CALL FELLOG                                                        
021006     END-IF                                                               
021007     .                                                                    
021008     EJECT                                                                
021010 IMS-STATUSKONTROLL SECTION.                                              
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
