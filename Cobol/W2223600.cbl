000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2223600.                                                
000400*AUTHOR.         STEFAN ANDREASSON.                                       
000500*DATE-WRITTEN.   JANUARI 2000.                                            
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        UPPDATERAR URSPÅRAD PROGNOS PÅ WDK6                              
001100*        INGET ÅTERSTARTSREGISTER BEHÖVS                                  
001400*                                                                         
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
002900*          --- PROGNOS SOM SKA FÖRÄNDRAS                                  
003000     SELECT W22236                     ASSIGN TO W22236D1.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W22236                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  -COPY W22235      -L.                                                
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300     SKIP2                                                                
004301                                                                          
004310*    -- CHECKED BY WY2000                                                 
004400 77  IDPGM                       PIC X(8)    VALUE 'W2223600'.            
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
005200 77  W22236-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W22236                       VALUE 'J'.                   
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
007500*01  AREA -COPY W22235     -PRE IN-                                       
007600*                                                                         
007700     EJECT                                                                
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
011400     EJECT                                                                
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
012700     PERFORM S01-LAES-W22236                                              
012800     PERFORM UNTIL END-OF-W22236                                          
012820                                                                          
012900       PERFORM B-BEHANDLA-POSTER                                          
012910                                                                          
013000       PERFORM S01-LAES-W22236                                            
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
014100     OPEN INPUT W22236                                                    
014300                                                                          
014400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014410     PERFORM IMS-RESTART                                                  
014500     .                                                                    
014600     EJECT                                                                
014700 B-BEHANDLA-POSTER SECTION.                                               
014800                                                                          
014900     MOVE IN-IDARTNR TO W-IDARTNR                                         
015120     PERFORM IMS-GHU-K611                                                 
015121                                                                          
015161     MOVE IN-RVPROURS        TO CLAG-RVPROURS                             
015170     PERFORM IMS-REPL-K611                                                
015180     ADD +1 TO CHKP-ANT                                                   
015181     IF CHKP-ANT > CHKP-MAX                                               
015182       PERFORM IMS-CHECKPOINT                                             
015183       MOVE +0 TO CHKP-ANT                                                
015184     END-IF                                                               
015190     .                                                                    
015400     EJECT                                                                
015500 Z-FINIT SECTION.                                                         
015700                                                                          
015800     CLOSE W22236                                                         
015900     SKIP2                                                                
016000     MOVE 'S' TO POSTSUM-OPKOD                                            
016100     CALL POSTSUM USING POSTSUM-PARM                                      
016101                                                                          
016200     .                                                                    
016300     EJECT                                                                
016400 S01-LAES-W22236  SECTION.                                                
016500     SKIP2                                                                
016600     READ W22236 INTO IN-AREA                                             
016700     AT END                                                               
016900        SET END-OF-W22236 TO TRUE                                         
017000                                                                          
017100     NOT AT END                                                           
017200        MOVE 'W22236' TO POSTSUM-FDNAMN                                   
017300        MOVE 'W22236D1' TO POSTSUM-DDNAMN2                                
017500        CALL POSTSUM USING POSTSUM-PARM                                   
017600     END-READ                                                             
017700     .                                                                    
017800     EJECT                                                                
020920 IMS-GHU-K611 SECTION.                                                    
020930     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
020940          DELIMITED BY SIZE INTO SSA1                                     
020950     MOVE 'WDK611  '       TO SSA2                                        
020960     MOVE '  GE' TO GODK-STATUSKODER                                      
020970     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
020980     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
020990     PERFORM IMS-STATUSKONTROLL                                           
020991     .                                                                    
020992     EJECT                                                                
020993 IMS-REPL-K611 SECTION.                                                   
020994                                                                          
020995     MOVE '  ' TO GODK-STATUSKODER                                        
020996     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
020997     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
020998     PERFORM IMS-STATUSKONTROLL                                           
020999     .                                                                    
021000     EJECT                                                                
021001 IMS-RESTART SECTION.                                                     
021002     SKIP2                                                                
021003     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021004     MOVE '  ' TO GODK-STATUSKODER                                        
021005     CALL CBLTDLI USING XRST MSG-PCB                                      
021006                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021007                        CHKP-AREA-LENGTH CHKP-AREA                        
021008     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021009     PERFORM IMS-STATUSKONTROLL                                           
021010     .                                                                    
021011     EJECT                                                                
021012 IMS-CHECKPOINT SECTION.                                                  
021013     SKIP2                                                                
021014     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
021015     MOVE '  XD' TO GODK-STATUSKODER                                      
021016     CALL CBLTDLI USING CHKP MSG-PCB                                      
021017                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
021018                        CHKP-AREA-LENGTH CHKP-AREA                        
021019     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021020     PERFORM IMS-STATUSKONTROLL                                           
021021                                                                          
021022     IF IMS-EJ-OK                                                         
021023       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
021024       DISPLAY FELTEXT                                                    
021025       CALL FELLOG                                                        
021026     END-IF                                                               
021027     .                                                                    
021028     EJECT                                                                
021030 IMS-STATUSKONTROLL SECTION.                                              
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
