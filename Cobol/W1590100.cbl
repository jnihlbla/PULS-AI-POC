000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1590100.                                                
000300 AUTHOR.         CONNY EGHOLT.                                            
000400 DATE-WRITTEN.   02/04/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        SKANNAR FILEN W01160 OCH SKRIVER UT ARTIKLAR SOM KLARAR          
001000*        URVALSKRITERIER FÖR ATT SKICKAS TILL NEVIS.                      
001100*                                                                         
001200*        UTFIL W15901 GÅR VIDARE TILL ETT MATCHPROGRAM W1590200           
001300*        SOM MATCHAR HELA ARTIKELPOSTEN MOT ETT REGISTER W159K6,          
001400*        SOM INNEHÅLLER DE ARTIKLAR SOM REDAN SKICKATS TILL NEVIS.        
001500*        ALLA NYA, ÄNDRADE OCH BORTTAGNA ARTIKLAR RAPPORTERAS.            
001600*                                                                         
001700*        PROGRAMMET LÄSER     W01160  CLAG-EXTRAKT                        
001800*                               WDK6                                      
001900*                                                                         
002000*                                                                         
002100* ÄNDRING: 2004-04-01  IDINK BLIR PIC X(4)I PULS, MEN NEVIS SKALL         
002200*        FORTFARANDE HA PIC 9(3) TILLSVIDARE.                             
002300* +------------------------------------------------------------+          
002400* +OBSERVERA:                                                  +          
002500* +COPYTEXT W15901 OCH W15902 INNEHÅLLER IDINK-OLD MED         +          
002600* +     DEN GAMLA VERSIONEN PIC 9(3)                           +          
002700* +                                                            +          
002800* +NÄR NEVIS VILL HA DEN NYA IDINK, MÅSTE OCKSÅ REGISTER       +          
002900* +W159.QASE.W159K6(+0) KONVERTERAS TILL ALFANUM IDINK.        +          
003000* +                                                            +          
003100* +DÄREFTER MÅSTE CTX W15902 ÅXÅ GENERERAS FRÅN DATAMANAGER.   +          
003200* +------------------------------------------------------------+          
003300*                                                                         
003400*                                                                         
003500* ÄNDRING: 2005-10-24  TVÅ NYA TEARTNOT-FÄLT TILLKOMMER.                  
003600*                     DATABASLÄSNING AV WDK625 TILLKOMMER.                
003700*                     INSTALLERAT 2006-03-15 (INS06AIT)                   
003800*                                                                         
003900*    ABENDKODER:                                                          
004000*        U0016 -  . . . .                                                 
004100*        U1000 -  . . . .                                                 
004200*                                                                         
004300                                                                          
004400     SKIP3                                                                
004500 ENVIRONMENT DIVISION.                                                    
004600     SKIP2                                                                
004700 INPUT-OUTPUT SECTION.                                                    
004800                                                                          
004900 FILE-CONTROL.                                                            
005000     SKIP2                                                                
005100*          --- DAGLAGERBAND                                               
005200     SELECT W01160                     ASSIGN TO W15901D1.                
005300     SKIP2                                                                
005400*          --- ALLA NEVIS-INTRESSANTA ARTIKLAR                            
005500     SELECT W15901                     ASSIGN TO W15901D2.                
005600     EJECT                                                                
005700 DATA DIVISION.                                                           
005800     SKIP3                                                                
005900 FILE SECTION.                                                            
006000     SKIP3                                                                
006100 FD  W01160                                                               
006200     RECORDING       F                                                    
006300     BLOCK CONTAINS  0.                                                   
006400                                                                          
006500*01  -COPY W01160      -L.                                                
006600     SKIP3                                                                
006700 FD  W15901                                                               
006800     RECORDING       F                                                    
006900     BLOCK CONTAINS  0.                                                   
007000                                                                          
007100*01  POST -COPY W15907 -PRE  UT-  -L.                                     
007200     EJECT                                                                
007300 WORKING-STORAGE SECTION.                                                 
007400                                                                          
007500 77  IDPGM                       PIC X(8)    VALUE 'W1590100'.            
007600 77  JA                          PIC X       VALUE 'J'.                   
007700 77  NEJ                         PIC X       VALUE 'N'.                   
007800 77  PACKAT                      PIC X(126)  VALUE                        
007900      X'000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C        
008000-      '1D1E1F202122232425262728292A2B2C2D2E2F30313233343536373839        
008100-      '3A3B3C3D3E3F4142434445464748494A4C4E4F50515253545556575859        
008200-      '5A5C5F62636465666768696C6D6E6F70717273747576777879808A8B8C        
008300-      '8D8E8F9A9B9C9D9E9FA0'.                                            
008400                                                                          
008500*                                                                         
008600*01  -COPY WWPRODSL                                                       
008700*                                                                         
008800                                                                          
008900 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
009000     88  END-OF-W01160                       VALUE 'J'.                   
009100                                                                          
009200     EJECT                                                                
009300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009400 01  FILLER REDEFINES DAGENS-DATUM.                                       
009500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009800     EJECT                                                                
009900 01  DYNAMISKA-SUBPROGRAM.                                                
010000*                                                                         
010100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010500     SKIP2                                                                
010600*    --- PARAMETRAR TILL ABEND                                            
010700                                                                          
010800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011100     SKIP2                                                                
011200 01  FELTEXT.                                                             
011300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011500     EJECT                                                                
011600*    --- PARAMETRAR TILL POSTSUM                                          
011700*                                                                         
011800*01  -COPY W0005   -PRE  POSTSUM-                                         
011900     EJECT                                                                
012000 01  IN-AREA-START               PIC X(24)   VALUE                        
012100                                 'IN-AREA-START  '.                       
012200*01  AREA -COPY W01160     -PRE IN-                                       
012300                                                                          
012400                                                                          
012500     EJECT                                                                
012600 01  UT-AREA-START               PIC X(24)   VALUE                        
012700                                 'UT-AREA-START  '.                       
012800*01  AREA -COPY W15907     -PRE UT-                                       
012900                                                                          
013000     EJECT                                                                
013100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013300     SKIP3                                                                
013400 01  FILLER                      PIC X(16)   VALUE 'DLI-KEY'.             
013500 01  NYCKLAR-TILL-DLI.                                                    
013600                                                                          
013700     03  W-IDARTNR-X.                                                     
013800         05  W-IDARTNR           PIC S9(9)  VALUE ZERO  COMP-3.           
013900     03  W-KDNOTTYP-X.                                                    
014000         05  W-KDNOTTYP          PIC S9      VALUE ZERO COMP-3.           
014100                                                                          
014200     SKIP2                                                                
014300*    --- STATUS-KOD FRÅN IMS                                              
014400 01  FILLER                      PIC X(16)   VALUE 'STATUS-WS'.           
014500 01  STATUS-WS                   PIC XX.                                  
014600     88  SEGMENT-FINNS                       VALUE '  '.                  
014700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014800     SKIP2                                                                
014900 01  GODK-STATUSKODER.                                                    
015000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015100     SKIP3                                                                
015200 01  FILLER                      PIC X(16)   VALUE 'SSA'.                 
015300 01  SSA1                        PIC X(64).                               
015400 01  SSA2                        PIC X(64).                               
015500 01  SSA3                        PIC X(64).                               
015600     EJECT                                                                
015700*    --- IMS FUNKTIONSKODER                                               
015800*01  -COPY W0003                                                          
015900     EJECT                                                                
016000*    ---  DLI INPUT-OUTPUT AREA                                           
016100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK625'.                      
016200 01  DLI-IO-WDK625.                                                       
016300*    03  -COPY WDK625 -PRE WDK6-                                          
016400     EJECT                                                                
016500 LINKAGE SECTION.                                                         
016600*01  -COPY W0008  -PRE WDK6-                                              
016700     05  FILLER                  PIC X.                                   
016800     EJECT                                                                
016900 PROCEDURE DIVISION  USING  WDK6-PCB.                                     
017000 MAIN SECTION.                                                            
017100     ENTRY 'DLITCBL' USING  WDK6-PCB.                                     
017200     SKIP2                                                                
017300     PERFORM A-INIT                                                       
017400     PERFORM S01-LAES-W01160                                              
017500                                                                          
017600     PERFORM UNTIL END-OF-W01160                                          
017700                                                                          
017800       MOVE IN-CLAG-KDPRODSL    TO TEST-KDPRODSL                          
017900                                                                          
018000       IF  ( ( IN-CLAG-FLLSRDEL = JA OR SPACE )                           
018100          OR ( IN-CLAG-FLLSRDEL = NEJ AND IN-CLAG-FLIART = JA ) )         
018200       AND ( KDPRODSL-VOLVO-UTAN-EMB )                                    
018210*-- NOTE: THE SAME PRODSL-S SHOULD BE ALLOWED IN PGM W15903 ALSO          
018300                                                                          
018400         MOVE IN-CLAG-IDARTNR     TO UT-IDARTNR                           
018500                                      W-IDARTNR                           
018600         MOVE IN-CLAG-REKSIFFR    TO UT-REKSIFFR                          
018700         MOVE IN-CLAG-FLLSRDEL    TO UT-FLLSRDEL                          
018800         MOVE IN-CLAG-IDFKNGRP    TO UT-IDFKNGRP                          
018900         MOVE IN-CLAG-FLIART      TO UT-FLIART                            
019000         MOVE IN-CLAG-IDBERED     TO UT-IDBERED                           
019100         MOVE IN-CLAG-IDAO(1)     TO UT-IDAO-1                            
019200         INSPECT UT-IDAO-1 CONVERTING PACKAT TO SPACE                     
019300         MOVE IN-CLAG-IDAO(2)     TO UT-IDAO-2                            
019400         INSPECT UT-IDAO-2 CONVERTING PACKAT TO SPACE                     
019500         MOVE IN-CLAG-IDAO(3)     TO UT-IDAO-3                            
019600         INSPECT UT-IDAO-3 CONVERTING PACKAT TO SPACE                     
019700         MOVE IN-CLAG-IDAO(4)     TO UT-IDAO-4                            
019800         INSPECT UT-IDAO-4 CONVERTING PACKAT TO SPACE                     
019900         MOVE IN-CLAG-IDAO(5)     TO UT-IDAO-5                            
020000         INSPECT UT-IDAO-5 CONVERTING PACKAT TO SPACE                     
020100*********MOVE IN-CLAG-IDINK       TO UT-IDINK-OLD                         
020200*        HÄR MÅSTE KOLLAS SÅ ATT NEVIS VERKLIGEN FÅR ETT NUMERISKT        
020300*        VÄRDE I SITT UT-IDINK-OLD.                                       
020400*        RISK FINNS ATT TVÅSTÄLLIGA ID-NR FINNS                           
020500*                                                                         
020600         IF IN-CLAG-IDINK (1:1) NUMERIC                                   
020700            IF IN-CLAG-IDINK (1:3) NUMERIC                                
020800               MOVE IN-CLAG-IDINK (1:3)  TO UT-IDINK-OLD                  
020900            ELSE                                                          
021000               IF IN-CLAG-IDINK (1:2) NUMERIC                             
021100                  MOVE IN-CLAG-IDINK (1:2) TO UT-IDINK-OLD                
021200               ELSE                                                       
021300                  MOVE ZERO              TO UT-IDINK-OLD                  
021400               END-IF                                                     
021500            END-IF                                                        
021600         ELSE                                                             
021700            IF IN-CLAG-IDINK (2:3) NUMERIC                                
021800               MOVE IN-CLAG-IDINK (2:3) TO UT-IDINK-OLD                   
021900            ELSE                                                          
022000               IF IN-CLAG-IDINK (2:2) NUMERIC                             
022100                  MOVE IN-CLAG-IDINK (2:2) TO UT-IDINK-OLD                
022200               ELSE                                                       
022300                  MOVE ZERO              TO UT-IDINK-OLD                  
022400               END-IF                                                     
022500            END-IF                                                        
022600         END-IF                                                           
022700         MOVE IN-CLAG-IDPROENH(1) TO UT-IDPROENH-1                        
022800         MOVE IN-CLAG-IDPROENH(2) TO UT-IDPROENH-2                        
022900         MOVE IN-CLAG-IDPROENH(3) TO UT-IDPROENH-3                        
023000         MOVE IN-CLAG-IDPROJ      TO UT-IDPROJ                            
023100         MOVE IN-CLAG-IDPROJUP    TO UT-IDPROJUP                          
023200         MOVE IN-CLAG-IDRITN      TO UT-IDRITN                            
023300         MOVE IN-CLAG-KDBPSR      TO UT-KDBPSR                            
023400         MOVE IN-CLAG-KDERS       TO UT-KDERS                             
023500         MOVE IN-CLAG-KDERS-UTG TO UT-KDERS-UTG                           
023600         MOVE IN-CLAG-KDPRODSL    TO UT-KDPRODSL                          
023700         MOVE IN-CLAG-KDSORT      TO UT-KDSORT                            
023800         MOVE IN-CLAG-KDUART      TO UT-KDUART                            
023900         MOVE IN-CLAG-TIERSDAT    TO UT-TIERSDAT                          
024000         MOVE IN-CLAG-TIFINLV     TO UT-TIFINLV                           
024100         MOVE IN-CLAG-TIREGDAT    TO UT-TIREGDAT                          
024200                                                                          
024300         MOVE +3 TO W-KDNOTTYP                                            
024400         PERFORM IMS-GU-WDK625                                            
024500         IF SEGMENT-FINNS                                                 
024600            MOVE WDK6-NOT-TEARTNOT TO UT-TEARTNOT-3                       
024700         ELSE                                                             
024800            MOVE SPACE TO UT-TEARTNOT-3                                   
024900         END-IF                                                           
025000                                                                          
025100         MOVE +7 TO W-KDNOTTYP                                            
025200         PERFORM IMS-GU-WDK625                                            
025300         IF SEGMENT-FINNS                                                 
025400            MOVE WDK6-NOT-TEARTNOT TO UT-TEARTNOT-7                       
025500         ELSE                                                             
025600            MOVE SPACE TO UT-TEARTNOT-7                                   
025700         END-IF                                                           
025800         MOVE SPACE TO UT-FLNOSTOCK                                       
025900                                                                          
026000         PERFORM S11-SKRIV-W15901                                         
026100*                                                                         
026200       END-IF                                                             
026300       PERFORM S01-LAES-W01160                                            
026400     END-PERFORM                                                          
026500     PERFORM Z-FINIT                                                      
026600                                                                          
026700     MOVE ZERO TO RETURN-CODE                                             
026800     GOBACK                                                               
026900     .                                                                    
027000     EJECT                                                                
027100 A-INIT SECTION.                                                          
027200                                                                          
027300     OPEN INPUT  W01160                                                   
027400     OPEN OUTPUT W15901                                                   
027500     SKIP2                                                                
027600     INITIALIZE UT-AREA                                                   
027700     SKIP2                                                                
027800     ACCEPT DAGENS-DATUM  FROM DATE                                       
027900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
028000     .                                                                    
028100     EJECT                                                                
028200 Z-FINIT SECTION.                                                         
028300     CLOSE W01160                                                         
028400           W15901                                                         
028500     SKIP2                                                                
028600     MOVE 'S' TO POSTSUM-OPKOD                                            
028700     CALL POSTSUM USING POSTSUM-PARM                                      
028800     .                                                                    
028900     EJECT                                                                
029000 S01-LAES-W01160  SECTION.                                                
029100     READ W01160 INTO IN-AREA                                             
029200     AT END                                                               
029300        MOVE HIGH-VALUE TO IN-AREA                                        
029400        SET END-OF-W01160 TO TRUE                                         
029500                                                                          
029600     NOT AT END                                                           
029700        MOVE 'W01160'   TO POSTSUM-FDNAMN                                 
029800        MOVE 'W15901D1' TO POSTSUM-DDNAMN2                                
029900        MOVE SPACE      TO POSTSUM-TRANSTYP                               
030000        CALL POSTSUM USING POSTSUM-PARM                                   
030100     END-READ                                                             
030200     .                                                                    
030300     EJECT                                                                
030400 S11-SKRIV-W15901 SECTION.                                                
030500                                                                          
030600     WRITE UT-POST FROM UT-AREA                                           
030700                                                                          
030800     MOVE UT-FLLSRDEL  TO POSTSUM-TRANSTYP                                
030900     MOVE 'W15901'     TO POSTSUM-FDNAMN                                  
031000     MOVE 'W15901D2'   TO POSTSUM-DDNAMN2                                 
031100     CALL POSTSUM USING POSTSUM-PARM                                      
031200     .                                                                    
031300     EJECT                                                                
031400* IMS SEKTIONER                                                           
031500     SKIP3                                                                
031600 IMS-GU-WDK625 SECTION.                                                   
031700                                                                          
031800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
031900            DELIMITED BY SIZE INTO SSA1                                   
032000     MOVE   'WDK611  '          TO SSA2                                   
032100     STRING 'WDK625  (KDNOTTYP =' W-KDNOTTYP-X ')'                        
032200            DELIMITED BY SIZE INTO SSA3                                   
032300     MOVE '  GE' TO GODK-STATUSKODER                                      
032400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK625 SSA1 SSA2 SSA3          
032500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
032600     PERFORM IMS-STATUSKONTROLL                                           
032700     .                                                                    
032800     SKIP3                                                                
032900 IMS-STATUSKONTROLL SECTION.                                              
033000                                                                          
033100     SET STATUS-IX TO 1                                                   
033200     SEARCH GODK-STATUS                                                   
033300       AT END                                                             
033400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033500           DELIMITED BY SIZE INTO FELTEXT                                 
033600         DISPLAY FELTEXT                                                  
033700         CALL FELLOG                                                      
033800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033900         CONTINUE                                                         
034000     END-SEARCH                                                           
034100     .                                                                    
