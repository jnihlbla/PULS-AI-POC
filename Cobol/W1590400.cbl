000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1590400.                                                
000300 AUTHOR.         CONNY EGHOLT.                                            
000400 DATE-WRITTEN.   02/03/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        BMP ELLER BATCH (Om BATCH, körs job W159J104)                    
000900*                                                                         
001000*        LÄSER EN EXTRAKTFILEN FRÅN WDD3 FÖR BENÄMNINGAR SOM              
001100*        HAR FLAENDR="J" I ROTEN OCH SÄTTER DEN NORMALT TILL ="N"         
001200*                                                                         
001300*        PGM läser in värdet på nya FLAENDR från sysin (JCL).             
001400*        Detta för att kunna backa flaggorna från en viss körning.        
001500*        Rätt generation av infil måste då anges i JCL (override).        
001600*                                                                         
001700*        PROGRAMMET UPPDATERAR  WDD3                                      
001800*                   LÄSER FIL   W15903                                    
001900*                                                                         
001910*    ÄNDRING:                                                             
001920*        Flaggan släcks även för KDBENSTAT = 3 (rensad TECHLA)            
001921*        eTracker case 1937335, 2005-04-11  /ConnyE                       
001930*                                                                         
001940*    ÄNDRING: 2006-05-15                                                  
001950*        Programmet ändras till att läsa den speciellt framtagna          
001960*        filen W15903 för backning av FLAENDR.                            
001961*        Den innehåller endast D01-poster.                                
001970*                                                                         
002000*    ABENDKODER:                                                          
002100*        U0016 -  . . . .                                                 
002200*        U1000 -  . . . .                                                 
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*          --- Alla benämningar som skickas till NEVIS                    
003210*          --- i fil W15904                                               
003300     SELECT W15903                     ASSIGN TO W15904D1.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W15903                                                               
004000     RECORDING      F                                                     
004100     BLOCK CONTAINS 0.                                                    
004300*01  FILLER    -COPY W1590301  -L.                                        
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'W1590400'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200 77  OCH                         PIC X       VALUE '&'.                   
005300 77  INX-FLAENDR                 PIC X       VALUE 'N'.                   
005400                                                                          
005500 77  BMP-SW                     PIC X       VALUE 'N'.                    
005600     88  SW-BMP                             VALUE 'J'.                    
005700     88  SW-EJ-BMP                          VALUE 'N'.                    
005800                                                                          
005900 77  W15903-EOF-SW              PIC X       VALUE 'N'.                    
006000     88  END-OF-W15903                      VALUE 'J'.                    
006100     EJECT                                                                
006200*    --- KONV.FEL ELLER TRUNK.FEL                                         
006300 77  W-FELTYP                    PIC X(15)   VALUE SPACE.                 
006400     EJECT                                                                
006500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006600 01  FILLER REDEFINES DAGENS-DATUM.                                       
006700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007000     EJECT                                                                
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200*                                                                         
007300     03  VIMSREGT                PIC X(8)    VALUE 'VIMSREGT'.            
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007800     SKIP2                                                                
007900*    --- PARAMETRAR TILL ABEND                                            
008000                                                                          
008100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008400     SKIP2                                                                
008500 01  FELTEXT.                                                             
008600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008800     EJECT                                                                
008900 01  FILLER              PIC X(24)   VALUE 'W005-START   '.               
009000*01  -COPY W0005   -PRE  POSTSUM-                                         
009100     EJECT                                                                
009200 01  FILLER              PIC X(24)   VALUE 'IN-AREA-START  '.             
009300 01  IN-AREA.                                                             
009800*    03 -COPY W1590301 -PRE IN01-                                         
010200     EJECT                                                                
010300 01  FILLER              PIC X(24)   VALUE 'HJALP-AREA-START'.            
010400 01  HJALP-AREA.                                                          
010500     03 3-SPACE                  PIC X(3)   VALUE SPACE.                  
010600     03 5-SPACE                  PIC X(5)   VALUE SPACE.                  
010700     03 25-SPACE                 PIC X(25)  VALUE SPACE.                  
010800     03 WS-IDBENNR               PIC 9(7)  VALUE ZERO.                    
010900     03 IX                       PIC S9(4)  BINARY.                       
011000                                                                          
011100     EJECT                                                                
011200*                                                                         
011300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011500     SKIP3                                                                
011600 01  FILLER                      PIC X(16)   VALUE 'CHKP'.                
011700 01  CHKP-VAR.                                                            
011800     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
011900     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
012000     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
012100     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
012200     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
012300     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
012400                                                                          
012500     SKIP3                                                                
012600 01  FILLER                      PIC X(16)   VALUE 'DLI-KEY'.             
012700 01  NYCKLAR-TILL-DLI.                                                    
012800                                                                          
012900     03  W-IDBENNR-X.                                                     
013000         05  W-IDBENNR           PIC S9(7)   VALUE ZERO COMP-3.           
013100                                                                          
013200     SKIP2                                                                
013300*    --- STATUS-KOD FRÅN IMS                                              
013400 01  FILLER                      PIC X(16)   VALUE 'STATUS-WS'.           
013500 01  STATUS-WS                   PIC XX.                                  
013600     88  SEGMENT-FINNS                       VALUE '  '.                  
013700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013800     88  IMS-EJ-OK                           VALUE 'XD'.                  
013900     SKIP2                                                                
014000 01  GODK-STATUSKODER.                                                    
014100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014200     SKIP3                                                                
014300 01  FILLER                      PIC X(16)   VALUE 'SSA'.                 
014400 01  SSA1                        PIC X(64).                               
014500 01  SSA2                        PIC X(64).                               
014600 01  SSA3                        PIC X(64).                               
014700     EJECT                                                                
014800*    --- IMS FUNKTIONSKODER                                               
014900*01  -COPY W0003                                                          
015000     EJECT                                                                
015100*    ---  DLI INPUT-OUTPUT AREA                                           
015200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD301'.                      
015300 01  DLI-IO-WDD301.                                                       
015400*    03  -COPY WDD301                                                     
015500     EJECT                                                                
015600 LINKAGE SECTION.                                                         
015700*01  -COPY W0009  -PRE MSG-                                               
015800     EJECT                                                                
015900*01  -COPY W0008  -PRE WDD3-                                              
016000     05  FILLER                  PIC X.                                   
016100     EJECT                                                                
016200                                                                          
016300 PROCEDURE DIVISION  USING MSG-PCB WDD3-PCB.                              
016400 MAIN SECTION.                                                            
016500     ENTRY 'DLITCBL' USING MSG-PCB WDD3-PCB.                              
016600                                                                          
016700     PERFORM A-INIT                                                       
016800                                                                          
016900     PERFORM S01-LAES-W15903                                              
017000                                                                          
017100     PERFORM UNTIL END-OF-W15903                                          
017200                                                                          
017700       IF NOT END-OF-W15903                                               
017800*        --- Återställ Ändringsflaggan                                    
017900         MOVE IN01-IDBENNR TO W-IDBENNR                                   
018000                                                                          
018100         PERFORM IMS-GHU-WDD301                                           
018200         IF SEGMENT-FINNS                                                 
018300           MOVE INX-FLAENDR TO BEN-FLAENDR                                
018400           PERFORM IMS-REPL-WDD3                                          
018500*          --- Om pgm t.ex omstartats efter en abend, kan en del          
018600*          --- redan vara satta till NEJ.                                 
018700                                                                          
018800           IF SW-BMP                                                      
018900             ADD +1 TO CHKP-ANT                                           
019000             IF CHKP-ANT > CHKP-MAX                                       
019100               PERFORM IMS-CHECKPOINT                                     
019200               MOVE ZERO TO CHKP-ANT                                      
019300             END-IF                                                       
019400           END-IF                                                         
019410         ELSE                                                             
019411           IF IN01-KDBENSTAT = 3                                          
019412*            --- Benämningen har rensats i W121B1                         
019420             CONTINUE                                                     
019430           ELSE                                                           
019431*            --- Benämningen borde funnits på WDD301                      
019432             STRING ' Benämning borde funnits. KDBENSTAT='                
019433                                     IN01-KDBENSTAT                       
019434               DELIMITED BY SIZE INTO FELTEXT-STR                         
019435             DISPLAY FELTEXT                                              
019436             CALL FELLOG                                                  
019510           END-IF                                                         
019520         END-IF                                                           
019600         PERFORM S01-LAES-W15903                                          
019700       END-IF                                                             
019800     END-PERFORM                                                          
019900                                                                          
020000     PERFORM Z-FINIT                                                      
020100                                                                          
020200     MOVE ZERO TO RETURN-CODE                                             
020300     GOBACK                                                               
020400     .                                                                    
020500     EJECT                                                                
020600 A-INIT SECTION.                                                          
020700                                                                          
020800     OPEN  INPUT W15903                                                   
020900                                                                          
021000     ACCEPT DAGENS-DATUM  FROM DATE                                       
021100                                                                          
021200*    --- HÄMTA  INPUT FÖR KÖRNINGEN                                       
021300     ACCEPT INX-FLAENDR  FROM SYSIN                                       
021400                                                                          
021500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
021600                                                                          
021700     CALL VIMSREGT                                                        
021800     IF RETURN-CODE = 8                                                   
021900       SET SW-BMP TO TRUE                                                 
022000       PERFORM IMS-RESTART                                                
022100     ELSE                                                                 
022200       SET SW-EJ-BMP TO TRUE                                              
022300     END-IF                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 S01-LAES-W15903  SECTION.                                                
022700     SKIP2                                                                
022800     READ W15903 INTO IN-AREA                                             
022900     AT END                                                               
023000        SET END-OF-W15903 TO TRUE                                         
023100                                                                          
023200     NOT AT END                                                           
023300       MOVE '03-'      TO POSTSUM-TRANSTYP                                
023400       MOVE 'W15903'   TO POSTSUM-FDNAMN                                  
023500       MOVE 'W15904D1' TO POSTSUM-DDNAMN2                                 
023600       CALL POSTSUM USING POSTSUM-PARM                                    
023700     END-READ                                                             
023800     .                                                                    
023900     EJECT                                                                
024000*S99-ABEND SECTION.                                                       
024100*                                                                         
024200*    SKIP2                                                                
024300*    MOVE 'S' TO POSTSUM-OPKOD                                            
024400*    CALL POSTSUM USING POSTSUM-PARM                                      
024500*    CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
024600*    .                                                                    
024700*    EJECT                                                                
024800 Z-FINIT SECTION.                                                         
024900     CLOSE W15903                                                         
025000     SKIP2                                                                
025100     MOVE 'S' TO POSTSUM-OPKOD                                            
025200     CALL POSTSUM USING POSTSUM-PARM                                      
025300     .                                                                    
025400     EJECT                                                                
025500     SKIP3                                                                
025600* --- IMS SEKTIONER ---                                                   
025700                                                                          
025800     EJECT                                                                
025900 IMS-RESTART SECTION.                                                     
026000     SKIP2                                                                
026100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
026200     MOVE '  ' TO GODK-STATUSKODER                                        
026300     CALL CBLTDLI USING XRST MSG-PCB                                      
026400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
026500                        CHKP-AREA-LENGTH CHKP-AREA                        
026600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
026700     PERFORM IMS-STATUSKONTROLL                                           
026800     .                                                                    
026900     SKIP3                                                                
027000 IMS-CHECKPOINT SECTION.                                                  
027100     SKIP2                                                                
027200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
027300     MOVE '  XD' TO GODK-STATUSKODER                                      
027400     CALL CBLTDLI USING CHKP MSG-PCB                                      
027500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
027600                        CHKP-AREA-LENGTH CHKP-AREA                        
027700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
027800     PERFORM IMS-STATUSKONTROLL                                           
027900     .                                                                    
028000     EJECT                                                                
028100 IMS-GHU-WDD301      SECTION.                                             
028200                                                                          
028300     STRING 'WDD301  (IDBENNR  =' W-IDBENNR-X ')'                         
028400            DELIMITED BY SIZE INTO SSA1                                   
028500     MOVE '  GE' TO GODK-STATUSKODER                                      
028600     CALL CBLTDLI USING GHU WDD3-PCB DLI-IO-WDD301 SSA1                   
028700     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
028800     PERFORM IMS-STATUSKONTROLL                                           
028900     .                                                                    
029000     SKIP2                                                                
029100 IMS-REPL-WDD3 SECTION.                                                   
029200     MOVE '  ' TO GODK-STATUSKODER                                        
029300     CALL CBLTDLI USING REPL WDD3-PCB DLI-IO-WDD301                       
029400     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
029500     PERFORM IMS-STATUSKONTROLL                                           
029600     .                                                                    
029700     EJECT                                                                
029800 IMS-STATUSKONTROLL SECTION.                                              
029900                                                                          
030000     SET STATUS-IX TO 1                                                   
030100     SEARCH GODK-STATUS                                                   
030200       AT END                                                             
030300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
030400           DELIMITED BY SIZE INTO FELTEXT-STR                             
030500         DISPLAY FELTEXT                                                  
030600         CALL FELLOG                                                      
030700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
030800         CONTINUE                                                         
030900     END-SEARCH                                                           
031000     .                                                                    
