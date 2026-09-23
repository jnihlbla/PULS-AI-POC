000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3711800.                                                
000400*AUTHOR.         RONNY STENHOLM.                                          
000500*DATE-WRITTEN.   92/07/27.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001200*                                                                         
001400*        PROGRAMMET UPPDATERAR STATUS PÅ WLBYTF (WDM6)                    
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- GODKÄNDA RAPPORTER SOM SKALL                               
002900*          --- ÄNDRA STATUS FRÅN 4 TILL 9.                                
003000     SELECT W37118                     ASSIGN TO W37118D1.                
003100     SKIP2                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W37118                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900     SKIP2                                                                
004000*01  POST -COPY W371STAT -PRE    IN-  -L.                                 
004100     SKIP3                                                                
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401*    -- CHECKED BY WY2000                                                 
004410     SKIP3                                                                
004500 77  IDPGM                       PIC X(8)    VALUE 'W3711800'.            
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500 77  SPAR-IDDISTR                PIC S9(5)   VALUE ZERO COMP-3.           
005600 77  SPAR-IDBYTRAP               PIC S9(7)   VALUE ZERO COMP-3.           
005700 77  INDX                        PIC S9(9)  VALUE ZERO COMP SYNC.         
005710 SKIP2                                                                    
005800 01  CHKP-VAR.                                                            
005810 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
005820 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
005830 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
005840 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
005850 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005860 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005870     SKIP2                                                                
005900     SKIP2                                                                
006000 77  W37118-EOF-SW                 PIC X       VALUE 'N'.                 
006100     88  END-OF-W37118                         VALUE 'J'.                 
006200 01  FELTEXT.                                                             
006300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
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
007600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008100     EJECT                                                                
008200 01  IN-AREA-START            PIC X(24)   VALUE                           
008300                                             'IN-AREA-START'.             
008400     SKIP2                                                                
008500                                                                          
008600*01  AREA -COPY W371STAT   -PRE IN-                                       
008700     EJECT                                                                
008800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008900     SKIP3                                                                
009000 01  NYCKLAR-TILL-DLI.                                                    
009100     SKIP3                                                                
009200     03  W-WDM601KY-X.                                                    
009300         05  W-IDDISTR           PIC S9(5)    VALUE ZERO COMP-3.          
009400         05  W-IDBYTRAP          PIC S9(7)    VALUE ZERO COMP-3.          
009500                                                                          
009600     03  W-WDGXKEY-X.                                                     
009700         05  FILLER              PIC X(4)   VALUE '3141'.                 
009800         05  FILLER              PIC X(26)  VALUE LOW-VALUE.              
009900     03  W-KDSEGKEY-X.                                                    
010000         05  W-KDSEGKEY          PIC X(30)    VALUE SPACE.                
010100     SKIP2                                                                
010200*    --- STATUS-KOD FRÅN IMS                                              
010300 01  STATUS-WS                   PIC XX.                                  
010400     88  SEGMENT-FINNS                       VALUE '  '.                  
010500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
010600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010700     88  IMS-EJ-OK                           VALUE 'XD'.                  
010800     SKIP2                                                                
010900 01  GODK-STATUSKODER.                                                    
011000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011100     SKIP3                                                                
011200 01  SSA1                        PIC X(64).                               
011300 01  SSA2                        PIC X(64).                               
011400     EJECT                                                                
011500*    --- IMS FUNKTIONSKODER                                               
011600*01  -COPY W0003                                                          
011700     EJECT                                                                
011800*    ---  DLI INPUT-OUTPUT AREA                                           
011900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012000     SKIP3                                                                
012100 01  DLI-IO-AREA.                                                         
012200     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
012300     SKIP3                                                                
013000     03  WLBYTF01 REDEFINES IO-AREA.                                      
013100*        05  -COPY WDM601  -PRE BYTF-                                     
013200     EJECT                                                                
013300 LINKAGE SECTION.                                                         
013400                                                                          
013500*01  -COPY W0009   -PRE MSG-                                              
013600     EJECT                                                                
014000*01  -COPY W0008  -PRE BYTF-                                              
014100     05  FILLER                  PIC X.                                   
014200     EJECT                                                                
014300 PROCEDURE DIVISION  USING MSG-PCB BYTF-PCB.                              
014400     ENTRY 'DLITCBL' USING MSG-PCB BYTF-PCB.                              
014500                                                                          
014600     SKIP2                                                                
014700     PERFORM A-INIT                                                       
014900     PERFORM S01-LAES-INFIL                                               
015000     PERFORM UNTIL END-OF-W37118                                          
015100       IF CHKP-ANT > CHKP-MAX                                             
015200         PERFORM X-TAG-CHECKPOINT                                         
015300       END-IF                                                             
015400       PERFORM D-UPPDATERA                                                
015410       ADD  +1 TO CHKP-ANT                                                
015500       PERFORM S01-LAES-INFIL                                             
015600     END-PERFORM                                                          
015700                                                                          
015800     PERFORM Z-FINIT                                                      
015900                                                                          
016000     MOVE ZERO TO RETURN-CODE                                             
016100     GOBACK                                                               
016200     .                                                                    
016300     EJECT                                                                
016400 A-INIT SECTION.                                                          
016500     SKIP2                                                                
016600                                                                          
016800     OPEN INPUT W37118                                                    
016900                                                                          
016910     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017000     ACCEPT DAGENS-DATUM       FROM DATE                                  
017100     PERFORM IMS-RESTART                                                  
017300     .                                                                    
017400     EJECT                                                                
020500 S01-LAES-INFIL SECTION.                                                  
020600     SKIP2                                                                
020700     READ W37118 INTO IN-AREA                                             
020800     AT END                                                               
020900        SET END-OF-W37118 TO TRUE                                         
021000                                                                          
021100     NOT AT END                                                           
021200        MOVE 'W371ST' TO POSTSUM-FDNAMN                                   
021300        MOVE 'W37122D1' TO POSTSUM-DDNAMN2                                
021400        MOVE 'IN'      TO POSTSUM-TRANSTYP                                
021500        CALL POSTSUM USING POSTSUM-PARM                                   
021600                                                                          
021700     END-READ                                                             
021800     .                                                                    
021900     EJECT                                                                
022000                                                                          
022100 D-UPPDATERA SECTION.                                                     
022200     SKIP2                                                                
022300                                                                          
022400     MOVE IN-IDDISTR       TO W-IDDISTR                                   
022500     MOVE IN-IDBYTRAP      TO W-IDBYTRAP                                  
022600     PERFORM IMS-GHU-BYTF-RAPP                                            
022700     IF SEGMENT-FINNS                                                     
022800       PERFORM S03-AENDRA-STATUS                                          
022900     END-IF                                                               
023000     .                                                                    
023100     EJECT                                                                
023200 Z-FINIT               SECTION.                                           
023300                                                                          
023400                                                                          
023500     CLOSE W37118                                                         
023600                                                                          
023700     SKIP2                                                                
023800     MOVE 'S' TO POSTSUM-OPKOD                                            
023900     CALL POSTSUM USING POSTSUM-PARM                                      
024000     .                                                                    
024100     EJECT                                                                
025500 S03-AENDRA-STATUS SECTION.                                               
025600     SKIP2                                                                
025700     MOVE '9' TO BYTF-RAPP-KDBYTSTA-RAPP                                  
025800     PERFORM IMS-REPL-BYTF-RAPP                                           
026200     .                                                                    
026300     EJECT                                                                
026310 X-TAG-CHECKPOINT   SECTION.                                              
026320                                                                          
026330* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
026340* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
026350     PERFORM IMS-CHECKPOINT                                               
026360     MOVE ZERO TO CHKP-ANT                                                
026380     .                                                                    
026390     EJECT                                                                
026400* --- IMS SEKTIONER ---                                                   
026500     SKIP3                                                                
026600 IMS-RESTART SECTION.                                                     
026700     SKIP2                                                                
026800     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
026900     MOVE '  ' TO GODK-STATUSKODER                                        
027000     CALL CBLTDLI USING XRST MSG-PCB                                      
027100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027200                        CHKP-AREA-LENGTH CHKP-AREA                        
027300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027400     PERFORM IMS-STATUSKONTROLL                                           
027500     .                                                                    
027600     EJECT                                                                
027700 IMS-CHECKPOINT SECTION.                                                  
027800     SKIP2                                                                
027900     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
028000     MOVE '  XD' TO GODK-STATUSKODER                                      
028100     CALL CBLTDLI USING CHKP MSG-PCB                                      
028200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
028300                        CHKP-AREA-LENGTH CHKP-AREA                        
028400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
028500     PERFORM IMS-STATUSKONTROLL                                           
028600                                                                          
028700     IF IMS-EJ-OK                                                         
028800       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
028900       DISPLAY FELTEXT                                                    
029000       CALL FELLOG                                                        
029100     END-IF                                                               
029110     .                                                                    
029120     EJECT                                                                
029200 IMS-GHU-BYTF-RAPP  SECTION.                                              
029300     STRING 'WLBYTF01(WDM601KY =' W-WDM601KY-X ')'                        
029400          DELIMITED BY SIZE INTO SSA1                                     
029500     MOVE '  ' TO GODK-STATUSKODER                                        
029600     CALL CBLTDLI USING GHU BYTF-PCB DLI-IO-AREA SSA1                     
029700     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
029800     PERFORM IMS-STATUSKONTROLL                                           
029900     .                                                                    
030100 IMS-REPL-BYTF-RAPP SECTION.                                              
030200                                                                          
030300     MOVE '  ' TO GODK-STATUSKODER                                        
030400     CALL CBLTDLI USING REPL BYTF-PCB DLI-IO-AREA                         
030500     MOVE BYTF-STATUS-CODE TO STATUS-WS                                   
030600     PERFORM IMS-STATUSKONTROLL                                           
030700     .                                                                    
033600 IMS-STATUSKONTROLL SECTION.                                              
033700     SKIP2                                                                
033800     SET STATUS-IX TO 1                                                   
033900     SEARCH GODK-STATUS                                                   
034000       AT END                                                             
034100         MOVE 'AKTUELL POST DIST RAPPNR ' TO FELTEXT-STR                  
034101         DISPLAY FELTEXT                                                  
034110         DISPLAY IN-IDDISTR                                               
034120         DISPLAY IN-IDBYTRAP                                              
034300         CALL FELLOG                                                      
034400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034500         CONTINUE                                                         
034600     END-SEARCH                                                           
034700     .                                                                    
034800     EJECT                                                                
