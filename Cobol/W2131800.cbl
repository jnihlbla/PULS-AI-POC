000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2131800.                                                
000300 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000400 DATE-WRITTEN.   98/11/27.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER LEVERANTÖRBAS                                              
000900*        LISTAR LANDKOD ADR MM                                            
001000*                                                                         
001100*        PROGRAMMET LÄSER      WLLEVA (WDF1)                              
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- LEVERANTÖRDATA                                             
002600     SELECT W21319                     ASSIGN TO W21318D1.                
002700     SKIP2                                                                
002800*          --- LEVERANTÖRDATA WXTR                                        
002900     SELECT W21320                     ASSIGN TO W21318D2.                
003000     SKIP2                                                                
003100*          --- LEVERANTÖRDATA AZURE                                       
003200     SELECT W21320X                    ASSIGN TO W21318D3.                
003300     SKIP2                                                                
003400*          --- SORTERINGSFIL                                              
003500     SELECT SORTFIL                    ASSIGN TO W21318DS.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W21319                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  POST -COPY W21319 -PRE  UT-  -L.                                     
004600     SKIP3                                                                
004700 FD  W21320                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  POST -COPY W21320 -PRE  WXTR-  -L.                                   
005200     EJECT                                                                
005300 FD  W21320X                                                              
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700*01  POST -COPY W21320X -PRE  OUT-  -L.                                   
005800     EJECT                                                                
005900 SD  SORTFIL.                                                             
006000                                                                          
006100*01  POST -COPY W21319 -PRE  SORT- .                                      
006200     EJECT                                                                
006300 WORKING-STORAGE SECTION.                                                 
006400                                                                          
006500                                                                          
006600*    -- CHECKED BY WY2000                                                 
006700 77  IDPGM                       PIC X(8)    VALUE 'W2131800'.            
006800 77  JA                          PIC X       VALUE 'J'.                   
006900 77  NEJ                         PIC X       VALUE 'N'.                   
007000                                                                          
007100 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
007200     88  END-OF-SORTFIL                      VALUE 'J'.                   
007300 77  SW-TRAEFF                   PIC X       VALUE 'N'.                   
007400     88  TRAEFF                              VALUE 'J'.                   
007500     EJECT                                                                
007600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007700 01  FILLER REDEFINES DAGENS-DATUM.                                       
007800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008100     EJECT                                                                
008200 01  DYNAMISKA-SUBPROGRAM.                                                
008300*                                                                         
008400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008800     SKIP2                                                                
008900*    --- PARAMETRAR TILL ABEND                                            
009000                                                                          
009100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009400     SKIP2                                                                
009500 01  FELTEXT.                                                             
009600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009800     EJECT                                                                
009900*    --- PARAMETRAR TILL POSTSUM                                          
010000*                                                                         
010100*01  -COPY W0005   -PRE  POSTSUM-                                         
010200     EJECT                                                                
010300 01  UT-AREA-START               PIC X(24)   VALUE                        
010400                                 'UT-AREA-START  '.                       
010500     SKIP2                                                                
010600                                                                          
010700*01  AREA -COPY W21319     -PRE UT-                                       
010800     EJECT                                                                
010900 01  WXTR-AREA-START             PIC X(24)   VALUE                        
011000                                 'WXTR-AREA-START  '.                     
011100     SKIP2                                                                
011200                                                                          
011300*01  AREA -COPY W21320     -PRE WXTR-                                     
011400     EJECT                                                                
011500 01  W21320X-AREA-START             PIC X(24)   VALUE                     
011510                                 'W21320X-AREA-START  '.                  
011520     SKIP2                                                                
011530                                                                          
011540*01  AREA -COPY W21320X    -PRE OUT-                                      
011550     EJECT                                                                
011600 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
011700                                                                          
011800*01  AREA -COPY W21319     -PRE SORTWS-                                   
011900     EJECT                                                                
012000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012100*                                                                         
012200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012300     SKIP3                                                                
012400 01  NYCKLAR-TILL-DLI.                                                    
012500     03  W-IDLEVNR-X.                                                     
012600         05  W-IDLEVNR           PIC  X(5)   VALUE SPACE.                 
012700     03  W-IDLEVSUF-X.                                                    
012800         05  W-IDLEVSUF          PIC S9(1)   VALUE ZERO COMP-3.           
012900     SKIP2                                                                
013000*    --- STATUS-KOD FRÅN IMS                                              
013100 01  STATUS-WS                   PIC XX.                                  
013200     88  SEGMENT-FINNS                       VALUE '  '.                  
013300     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
013400     SKIP2                                                                
013500 01  GODK-STATUSKODER.                                                    
013600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013700     SKIP3                                                                
013800 01  SSA1                        PIC X(64).                               
013900 01  SSA2                        PIC X(64).                               
014000     EJECT                                                                
014100*    --- IMS FUNKTIONSKODER                                               
014200*01  -COPY W0003                                                          
014300     EJECT                                                                
014400*    ---  DLI INPUT-OUTPUT AREA                                           
014500 01  FILLER         PIC X(16) VALUE 'DLI-IO-AREA'.                        
014600 01  DLI-IO-AREA.                                                         
014700     03  WLLEVA14.                                                        
014800*    05  -COPY WDF106                                                     
014900     03  WLLEVA01  REDEFINES WLLEVA14.                                    
015000*    05  -COPY WDF101                                                     
015100     EJECT                                                                
015200 LINKAGE SECTION.                                                         
015300                                                                          
015400                                                                          
015500*01  -COPY W0008  -PRE LEVA-                                              
015600     05  FILLER                  PIC X.                                   
015700     EJECT                                                                
015800 PROCEDURE DIVISION  USING LEVA-PCB.                                      
015900 MAIN SECTION.                                                            
016000     ENTRY 'DLITCBL' USING LEVA-PCB.                                      
016100                                                                          
016200                                                                          
016300     PERFORM A-INIT                                                       
016400                                                                          
016500     SORT SORTFIL ASCENDING KEY SORT-IDLEVNR                              
016600                  INPUT PROCEDURE B-SORT-INPUT                            
016700                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
016800                                                                          
016900     IF SORT-RETURN NOT = 0                                               
017000       MOVE SORT-RETURN TO SORT-RETURN-X                                  
017100       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
017200           DELIMITED BY SIZE                                              
017300           INTO FELTEXT-STR                                               
017400       DISPLAY FELTEXT                                                    
017500       MOVE RKOD-ABEND-UTAN-DUMP  TO RKOD-ABEND                           
017600       PERFORM S99-ABEND                                                  
017700     ELSE                                                                 
017800       PERFORM Z-FINIT                                                    
017900                                                                          
018000       MOVE ZERO TO RETURN-CODE                                           
018100       GOBACK                                                             
018200     END-IF                                                               
018300                                                                          
018400     .                                                                    
018500     EJECT                                                                
018600 A-INIT SECTION.                                                          
018700                                                                          
018800     OPEN OUTPUT W21319                                                   
018900                 W21320                                                   
018910                 W21320X                                                  
019000                                                                          
019100     ACCEPT DAGENS-DATUM  FROM DATE                                       
019200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019300     .                                                                    
019400     EJECT                                                                
019500 B-SORT-INPUT  SECTION.                                                   
019600                                                                          
019700     PERFORM IMS-GET-LEVA                                                 
019800     PERFORM UNTIL SEGMENT-SAKNAS                                         
019900       IF TRAEFF                                                          
020000          PERFORM S31-SORT-RELEASE                                        
020100          MOVE NEJ TO SW-TRAEFF                                           
020200       END-IF                                                             
020300                                                                          
020400       EVALUATE LEVA-SEG-NAME-FB                                          
020500         WHEN 'WDF101'                                                    
020600           PERFORM BA-FLYTTA-LEV                                          
020700         WHEN 'WDF106'                                                    
020800           MOVE JA TO SW-TRAEFF                                           
020900           PERFORM BB-FLYTTA-ADR                                          
021000       END-EVALUATE                                                       
021100       PERFORM IMS-GET-LEVA                                               
021200     END-PERFORM                                                          
021300                                                                          
021400       IF TRAEFF                                                          
021500          PERFORM S31-SORT-RELEASE                                        
021600          MOVE NEJ TO SW-TRAEFF                                           
021700       END-IF                                                             
021800     .                                                                    
021900     EJECT                                                                
022000 BA-FLYTTA-LEV SECTION.                                                   
022100                                                                          
022200     MOVE LEV-IDLEVNR          TO SORTWS-IDLEVNR                          
022300     MOVE LEV-KVDAGAR-TTC1     TO SORTWS-KVDAGAR-TTC1                     
022400     MOVE LEV-KVVECKOR-LT      TO SORTWS-KVVECKOR-LT                      
022500     MOVE LEV-KVVECKOR-AT      TO SORTWS-KVVECKOR-AT                      
022600     .                                                                    
022700     EJECT                                                                
022800 BB-FLYTTA-ADR SECTION.                                                   
022900                                                                          
023000     MOVE ADR-BELEV            TO SORTWS-BELEV                            
023100     MOVE ADR-ADLEV-RAD1       TO SORTWS-ADLEV-RAD1                       
023200     MOVE ADR-ADLEV-RAD2       TO SORTWS-ADLEV-RAD2                       
023300     MOVE ADR-ADLEV-ORT        TO SORTWS-ADLEV-ORT                        
023400     MOVE ADR-ADLEVLND         TO SORTWS-ADLEVLND                         
023500     MOVE ADR-IDLEVTLF         TO SORTWS-IDLEVTLF                         
023600     MOVE ADR-IDLEVFAX         TO SORTWS-IDLEVFAX                         
023700     MOVE ADR-IDLANDX2         TO SORTWS-IDLANDX2                         
023800     .                                                                    
023900     EJECT                                                                
024000 C-SORT-OUTPUT SECTION.                                                   
024100                                                                          
024200     PERFORM S32-SORT-RETURN                                              
024300     PERFORM UNTIL END-OF-SORTFIL                                         
024400       PERFORM CA-FLYTTA-TILL-W21319                                      
024500       PERFORM CB-FLYTTA-TILL-W21320                                      
024510       PERFORM CC-FLYTTA-TILL-W21320X                                     
024600       PERFORM S32-SORT-RETURN                                            
024700     END-PERFORM                                                          
024800     .                                                                    
024900     EJECT                                                                
025000 CA-FLYTTA-TILL-W21319 SECTION.                                           
025100                                                                          
025200     MOVE SORTWS-IDLEVNR       TO UT-IDLEVNR                              
025300     MOVE SORTWS-KVDAGAR-TTC1  TO UT-KVDAGAR-TTC1                         
025400     MOVE SORTWS-KVVECKOR-LT   TO UT-KVVECKOR-LT                          
025500     MOVE SORTWS-KVVECKOR-AT   TO UT-KVVECKOR-AT                          
025600     MOVE SORTWS-BELEV         TO UT-BELEV                                
025700     MOVE SORTWS-ADLEV-RAD1    TO UT-ADLEV-RAD1                           
025800     MOVE SORTWS-ADLEV-RAD2    TO UT-ADLEV-RAD2                           
025900     MOVE SORTWS-ADLEV-ORT     TO UT-ADLEV-ORT                            
026000     MOVE SORTWS-ADLEVLND      TO UT-ADLEVLND                             
026100     MOVE SORTWS-IDLEVTLF      TO UT-IDLEVTLF                             
026200     MOVE SORTWS-IDLEVFAX      TO UT-IDLEVFAX                             
026300     MOVE SORTWS-IDLANDX2      TO UT-IDLANDX2                             
026400                                                                          
026500     PERFORM S11-SKRIV-W21319                                             
026600     .                                                                    
026700     EJECT                                                                
026800 CB-FLYTTA-TILL-W21320 SECTION.                                           
026900                                                                          
027000     MOVE SORTWS-IDLEVNR       TO WXTR-IDLEVNR                            
027100     MOVE SORTWS-KVDAGAR-TTC1  TO WXTR-KVDAGAR-TTC1                       
027200     MOVE SORTWS-KVVECKOR-LT   TO WXTR-KVVECKOR-LT                        
027300     MOVE SORTWS-KVVECKOR-AT   TO WXTR-KVVECKOR-AT                        
027400     MOVE SORTWS-BELEV         TO WXTR-BELEV                              
027500     MOVE SORTWS-ADLEV-RAD1    TO WXTR-ADLEV-RAD1                         
027600     MOVE SORTWS-ADLEV-RAD2    TO WXTR-ADLEV-RAD2                         
027700     MOVE SORTWS-ADLEV-ORT     TO WXTR-ADLEV-ORT                          
027800     MOVE SORTWS-ADLEVLND      TO WXTR-ADLEVLND                           
027900     MOVE SORTWS-IDLEVTLF      TO WXTR-IDLEVTLF                           
028000     MOVE SORTWS-IDLEVFAX      TO WXTR-IDLEVFAX                           
028100     MOVE SORTWS-IDLANDX2      TO WXTR-IDLANDX2                           
028200                                                                          
028300     PERFORM S12-SKRIV-W21320                                             
028400     .                                                                    
028500     EJECT                                                                
028510 CC-FLYTTA-TILL-W21320X SECTION.                                          
028520                                                                          
028530     MOVE SORTWS-IDLEVNR       TO OUT-IDLEVNR                             
028540     MOVE SORTWS-KVDAGAR-TTC1  TO OUT-KVDAGAR-TTC1                        
028550     MOVE SORTWS-KVVECKOR-LT   TO OUT-KVVECKOR-LT                         
028560     MOVE SORTWS-KVVECKOR-AT   TO OUT-KVVECKOR-AT                         
028570     MOVE SORTWS-BELEV         TO OUT-BELEV                               
028580     MOVE SORTWS-ADLEV-RAD1    TO OUT-ADLEV-RAD1                          
028590     MOVE SORTWS-ADLEV-RAD2    TO OUT-ADLEV-RAD2                          
028591     MOVE SORTWS-ADLEV-ORT     TO OUT-ADLEV-ORT                           
028592     MOVE SORTWS-ADLEVLND      TO OUT-ADLEVLND                            
028593     MOVE SORTWS-IDLEVTLF      TO OUT-IDLEVTLF                            
028594     MOVE SORTWS-IDLEVFAX      TO OUT-IDLEVFAX                            
028595     MOVE SORTWS-IDLANDX2      TO OUT-IDLANDX2                            
028596                                                                          
028597     PERFORM S13-SKRIV-W21320X                                            
028598     .                                                                    
028599     EJECT                                                                
028600 Z-FINIT SECTION.                                                         
028700                                                                          
028800     CLOSE W21319                                                         
028900           W21320                                                         
028910           W21320X                                                        
029000     SKIP2                                                                
029100     MOVE 'S' TO POSTSUM-OPKOD                                            
029200     CALL POSTSUM USING POSTSUM-PARM                                      
029300     .                                                                    
029400     EJECT                                                                
029500 S11-SKRIV-W21319 SECTION.                                                
029600                                                                          
029700     WRITE UT-POST FROM UT-AREA                                           
029800                                                                          
029900     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
030000     MOVE 'W21319'   TO POSTSUM-FDNAMN                                    
030100     MOVE 'W21318D1' TO POSTSUM-DDNAMN2                                   
030200     CALL POSTSUM USING POSTSUM-PARM                                      
030300     .                                                                    
030400     EJECT                                                                
030500 S12-SKRIV-W21320 SECTION.                                                
030600                                                                          
030700     WRITE WXTR-POST FROM WXTR-AREA                                       
030800                                                                          
030900     MOVE 'WXTR'     TO POSTSUM-TRANSTYP                                  
031000     MOVE 'W21320'   TO POSTSUM-FDNAMN                                    
031100     MOVE 'W21318D2' TO POSTSUM-DDNAMN2                                   
031200     CALL POSTSUM USING POSTSUM-PARM                                      
031300     .                                                                    
031400     EJECT                                                                
031410 S13-SKRIV-W21320X SECTION.                                               
031420                                                                          
031430     WRITE OUT-POST FROM OUT-AREA                                         
031440                                                                          
031450     MOVE 'AZURE'    TO POSTSUM-TRANSTYP                                  
031460     MOVE 'W21320X'  TO POSTSUM-FDNAMN                                    
031470     MOVE 'W21318D3' TO POSTSUM-DDNAMN2                                   
031480     CALL POSTSUM USING POSTSUM-PARM                                      
031490     .                                                                    
031491     EJECT                                                                
031500 S31-SORT-RELEASE  SECTION.                                               
031600                                                                          
031700     RELEASE SORT-POST FROM SORTWS-AREA                                   
031800     .                                                                    
031900     EJECT                                                                
032000 S32-SORT-RETURN  SECTION.                                                
032100                                                                          
032200     RETURN SORTFIL INTO SORTWS-AREA                                      
032300     AT END                                                               
032400         SET END-OF-SORTFIL TO TRUE                                       
032500     .                                                                    
032600     EJECT                                                                
032700 S99-ABEND SECTION.                                                       
032800                                                                          
032900     SKIP2                                                                
033000     MOVE 'S' TO POSTSUM-OPKOD                                            
033100     CALL POSTSUM USING POSTSUM-PARM                                      
033200     CALL ABEND USING RKOD-ABEND                                          
033300     .                                                                    
033400     EJECT                                                                
033500* --- IMS SEKTIONER ---                                                   
033600                                                                          
033700                                                                          
033800 IMS-GET-LEVA   SECTION.                                                  
033900                                                                          
034000     CALL CBLTDLI USING GN LEVA-PCB DLI-IO-AREA                           
034100     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
034200     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
034300     PERFORM IMS-STATUSKONTROLL                                           
034400     .                                                                    
034500     EJECT                                                                
034600 IMS-STATUSKONTROLL SECTION.                                              
034700                                                                          
034800     SET STATUS-IX TO 1                                                   
034900     SEARCH GODK-STATUS                                                   
035000       AT END                                                             
035100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
035200           DELIMITED BY SIZE INTO FELTEXT                                 
035300         DISPLAY FELTEXT                                                  
035400         CALL FELLOG                                                      
035500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
035600         CONTINUE                                                         
035700     END-SEARCH                                                           
035800     .                                                                    
