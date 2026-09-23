000100 ID DIVISION.                                                             
000200 PROGRAM-ID.         W4124600.                                            
000300 AUTHOR.             GÖRAN KJELLSON  GUIDE                                
000400 DATE-WRITTEN.       SEPTEMBER 2007                                       
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET RENSAR WDQ2-UPPFÖLJNINGSPOSTER SÅ BARA                
000900*        TVINGANDE TILLÄGGSORDER GÅR VIDARE TILL UTFILEN                  
001000*                                                                         
001100*        PROGRAMMET LÄSER      WDB2                                       
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300                                                                          
002400*          --- ORDER REGISTRERADE UNDER VECKAN                            
002500     SELECT W41245                     ASSIGN TO W41246D1.                
002600                                                                          
002700*          --- TVINGANDE TILLÄGGSORDER                                    
002800*          --- REGISTRERADE UNDER VECKAN                                  
002900     SELECT W41246                     ASSIGN TO W41246D2.                
003000                                                                          
003100                                                                          
003200 DATA DIVISION.                                                           
003300 FILE SECTION.                                                            
003400                                                                          
003500 FD  W41245                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W41245       -L.                                               
004000                                                                          
004100                                                                          
004200 FD  W41246                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  POST -COPY W41245  -PRE  UT-  -L.                                    
004700                                                                          
004800                                                                          
004900 WORKING-STORAGE SECTION.                                                 
005000                                                                          
005100                                                                          
005200 77  IDPGM                       PIC X(8)    VALUE 'W4124600'.            
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500                                                                          
005600 77  CURRENT-SECTION             PIC X(16)   VALUE 'MAIN'.                
005700 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005800                                                                          
005900 77  W41245-EOF-SW               PIC X       VALUE 'N'.                   
006000     88  END-OF-W41245                       VALUE 'J'.                   
006100                                                                          
006200                                                                          
006300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006400 01  FILLER REDEFINES DAGENS-DATUM.                                       
006500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006800                                                                          
006900                                                                          
007000 01  DYNAMISKA-SUBPROGRAM.                                                
007100*                                                                         
007200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007600                                                                          
007700*    --- PARAMETRAR TILL ABEND                                            
007800                                                                          
008200 01  FELTEXT.                                                             
008300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008500                                                                          
008600                                                                          
008700*    --- PARAMETRAR TILL POSTSUM                                          
008800*                                                                         
008900*01  -COPY W0005   -PRE  POSTSUM-                                         
009000     EJECT                                                                
009100 01  IN-AREA-START               PIC X(24)   VALUE                        
009200                                 'IN-AREA-START  '.                       
009300*01  AREA -COPY W41245      -PRE IN-                                      
009400                                                                          
009500 01  UT-AREA-START               PIC X(24)   VALUE                        
009600                                 'UT-AREA-START  '.                       
009700*01  AREA -COPY W41245      -PRE UT-                                      
009800                                                                          
009900                                                                          
010000                                                                          
010100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010200*                                                                         
010300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010400 01  NYCKLAR-TILL-DLI.                                                    
010500     03  W-IDGMT-X.                                                       
010600         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
010700         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
010800                                                                          
010900*    --- STATUS-KOD FRÅN IMS                                              
011000 01  STATUS-WS                   PIC XX.                                  
011100     88  SEGMENT-FINNS                       VALUE '  '.                  
011200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011300                                                                          
011400 01  GODK-STATUSKODER.                                                    
011500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011600                                                                          
011700 01  SSA1                        PIC X(64).                               
011800                                                                          
011900                                                                          
012000*    --- IMS FUNKTIONSKODER                                               
012100*01  -COPY W0003                                                          
012200                                                                          
012300*    ---  DLI INPUT-OUTPUT AREA                                           
012400 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012500 01  DLI-IO-AREA.                                                         
012600*  03  -COPY WDB201                                                       
012700                                                                          
012800                                                                          
012900                                                                          
013000 LINKAGE SECTION.                                                         
013100*01  -COPY W0008  -PRE WDB2-                                              
013200     05  FILLER                  PIC X.                                   
013300                                                                          
013400                                                                          
013500 PROCEDURE DIVISION  USING WDB2-PCB.                                      
013600 MAIN SECTION.                                                            
013700                                                                          
013800     ENTRY 'DLITCBL' USING WDB2-PCB.                                      
013900                                                                          
014000     PERFORM A-INIT                                                       
014100                                                                          
014200     PERFORM S01-LAES-W41245                                              
014600                                                                          
014700     PERFORM UNTIL END-OF-W41245                                          
014800                                                                          
014810        IF IN-KDTILTYP = 1                                                
014900           PERFORM B-SKRIV-TVINGANDE-TILLAGG                              
014901        ELSE                                                              
014902           PERFORM C-SKRIV-DROPLINE-ORDER                                 
014903        END-IF                                                            
014910        PERFORM S01-LAES-W41245                                           
015200                                                                          
015300     END-PERFORM                                                          
015400                                                                          
015500     PERFORM Z-FINIT                                                      
015600                                                                          
015700     MOVE ZERO TO RETURN-CODE                                             
015800     GOBACK                                                               
016000     .                                                                    
016100                                                                          
016200                                                                          
016300                                                                          
016400 A-INIT SECTION.                                                          
016500     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
016600                                                                          
016700     OPEN  INPUT W41245                                                   
016800     OPEN OUTPUT W41246                                                   
016900                                                                          
017000     ACCEPT DAGENS-DATUM  FROM DATE                                       
017100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017200     .                                                                    
017300     EJECT                                                                
017400 B-SKRIV-TVINGANDE-TILLAGG           SECTION.                             
017500     MOVE 'B-SKRIV-TVINGANDE' TO CURRENT-SECTION                          
017600                                                                          
018100     IF IN-IDDISTR NOT = W-IDDISTR                                        
018200     OR IN-IDKUNDNR NOT = W-IDKUNDNR                                      
018300                                                                          
018400        MOVE IN-IDDISTR  TO W-IDDISTR                                     
018500        MOVE IN-IDKUNDNR TO W-IDKUNDNR                                    
018600                                                                          
018700        PERFORM IMS-01-GU-WDB201                                          
018800                                                                          
018900     END-IF                                                               
019000                                                                          
019100     IF (IN-KDORDKL = '1' AND                                             
019200         GMT-FLORDTIL-KL1 = JA)                                           
019300     OR (IN-KDORDKL = '2' AND                                             
019400         GMT-FLORDTIL-KL2 = JA)                                           
019500     OR (IN-KDORDKL = '3' AND                                             
019600         GMT-FLORDTIL-KL3 = JA)                                           
019700     OR (IN-KDORDKL = '4' AND                                             
019800         GMT-FLORDTIL-KL4 = JA)                                           
019900                                                                          
020000         MOVE IN-AREA     TO UT-AREA                                      
020100         PERFORM S02-SKRIV-W41246                                         
020200                                                                          
020300     END-IF                                                               
020600     .                                                                    
020700                                                                          
020710 C-SKRIV-DROPLINE-ORDER    SECTION.                                       
020720     MOVE 'B-SKRIV-TVINGANDE' TO CURRENT-SECTION                          
020730                                                                          
020804     MOVE IN-AREA     TO UT-AREA                                          
020805     PERFORM S02-SKRIV-W41246                                             
020808     .                                                                    
020809                                                                          
020810                                                                          
020900 Z-FINIT SECTION.                                                         
021000     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
021100                                                                          
021200     CLOSE W41245                                                         
021210           W41246                                                         
021300                                                                          
021400     MOVE 'S' TO POSTSUM-OPKOD                                            
021500     CALL POSTSUM USING POSTSUM-PARM                                      
021600     .                                                                    
021700                                                                          
021800                                                                          
021810 S01-LAES-W41245 SECTION.                                                 
021820     MOVE 'S01-LAES-W41245 ' TO CURRENT-SECTION                           
021821                                                                          
021830     READ W41245 INTO IN-AREA                                             
021840       AT END                                                             
021850          MOVE JA TO W41245-EOF-SW                                        
021860     END-READ                                                             
021870                                                                          
021880     IF NOT END-OF-W41245                                                 
021890        MOVE 'W41245'    TO POSTSUM-FDNAMN                                
021891        MOVE 'W41245D1'  TO POSTSUM-DDNAMN2                               
021892        MOVE  'IN'       TO POSTSUM-TRANSTYP                              
021893        CALL POSTSUM USING POSTSUM-PARM                                   
021894     END-IF                                                               
021895     .                                                                    
021896                                                                          
021897                                                                          
021900 S02-SKRIV-W41246 SECTION.                                                
022000     MOVE 'S02-SKRIV-W41246' TO CURRENT-SECTION                           
022100                                                                          
022200     WRITE UT-POST FROM UT-AREA                                           
022300                                                                          
022400     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
022500     MOVE 'W41246'   TO POSTSUM-FDNAMN                                    
022600     MOVE 'W41246D2' TO POSTSUM-DDNAMN2                                   
022700     CALL POSTSUM USING POSTSUM-PARM                                      
022800     .                                                                    
022900                                                                          
023000                                                                          
024800* --- IMS SEKTIONER ---                                                   
024900                                                                          
025000 IMS-01-GU-WDB201 SECTION.                                                
025100     MOVE 'IMS-01-GU-WDB201' TO CURRENT-IMS-SECTION                       
025200                                                                          
025300     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
025400          DELIMITED BY SIZE INTO SSA1                                     
025500     MOVE '  '   TO GODK-STATUSKODER                                      
025600     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA SSA1                      
025700     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
025800     PERFORM IMS-STATUSKONTROLL                                           
025900     .                                                                    
026000                                                                          
026100                                                                          
026200 IMS-STATUSKONTROLL SECTION.                                              
026300                                                                          
026400     SET STATUS-IX TO 1                                                   
026500     SEARCH GODK-STATUS                                                   
026600       AT END                                                             
026700         STRING ' FELAKTIG RETURKOD FRÅN IMS: ' STATUS-WS                 
026800           DELIMITED BY SIZE INTO FELTEXT-STR                             
026900         DISPLAY FELTEXT                                                  
027000         CALL FELLOG                                                      
027100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
027200         CONTINUE                                                         
027300     END-SEARCH                                                           
027400     .                                                                    
