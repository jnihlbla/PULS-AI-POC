000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5016200.                                                
000300 AUTHOR.         RANDI BERG.                                              
000400 DATE-WRITTEN.   98/01/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        SÖKFUNKTION FÖR WDL9, SALDODATABASEN                             
000900*                                                                         
001000*        PROGRAMMET LÄSER      WLLOGA (WDL9)                              
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W5T162                                              
001400*                     W5T162U                                             
001500*        MID:         W5I16201                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W5O16201                                            
001900                                                                          
002000 ENVIRONMENT DIVISION.                                                    
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W5016200'.            
002700                                                                          
002800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003300                                                                          
003400*01 -COPY WWDCKONS                                                        
003500                                                                          
003600 77  WS-TEST-IDARTNR             PIC X(9).                                
003700 77  WS-IDARTNR                  PIC X(9).                                
003800                                                                          
003900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004100 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
004200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004300                                                                          
004400                                                                          
004500 77  ALLT-SW                     PIC X       VALUE 'J'.                   
004600     88  ALLT-OK                             VALUE 'J'.                   
004700     88  ALLT-NOT-OK                         VALUE 'N'.                   
004800                                                                          
004900 77  BYT-SW                      PIC X       VALUE 'N'.                   
005000     88  BYT-BILD                            VALUE 'J'.                   
005100     88  BYT-EJ-BILD                         VALUE 'N'.                   
005200                                                                          
005300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005400     88  NYCKLAR-OK                          VALUE 'J'.                   
005500     88  NYCKLAR-FEL                         VALUE 'N'.                   
005600                                                                          
005700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005800     88  INDATA-OK                           VALUE 'J'.                   
005900     88  INDATA-FEL                          VALUE 'N'.                   
006000                                                                          
006100 77  EXTRACT-SW                  PIC X       VALUE 'J'.                   
006200     88  EXTRACT-OK                          VALUE 'J'.                   
006300     88  EXTRACT-FEL                         VALUE 'N'.                   
006400                                                                          
006500 77    WDB6-A-SW                 PIC X       VALUE 'J'.                   
006600       88  WDB6-A-FINNS                      VALUE 'J'.                   
006700       88  WDB6-A-SAKNAS                     VALUE 'N'.                   
006800                                                                          
006900 77    WDB6-B-SW         PIC X               VALUE 'J'.                   
007000       88  WDB6-B-FINNS                      VALUE 'J'.                   
007100       88  WDB6-B-SAKNAS                     VALUE 'N'.                   
007200                                                                          
007300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007400     88  EGEN-MID                            VALUE '5162'.                
007500     88  GODK-MID                            VALUE '5162' '5163'          
007600                                                   '5164'.                
007700     88  DETALJ-MID                          VALUE '5164'.                
007800     88  HELP-MID                            VALUE '0551'.                
007900     EJECT                                                                
008000                                                                          
008100     EJECT                                                                
008200*    --- ARBETSFÄLT                                                       
008300 01  W-AREA-DATUM.                                                        
008400     03  W-DAREGDAT.                                                      
008500         05  W-DAREGDAT-AAR      PIC 9(2).                                
008600         05  W-DAREGDAT-TI       PIC 9(6).                                
008700     03  WS-DAREGDAT             PIC 9(8).                                
008800     03  W-DAREGDAT-KONV         PIC 9(6).                                
008900     03  W-DAREGDAT-FROM         PIC 9(8).                                
009000     03  W-DAREGDAT-TOM          PIC 9(8).                                
009100     03  W-DIFF                  PIC 9(8).                                
009200     03  W-TIREGDAT-IN1          PIC 9(8).                                
009300     03  W-TIREGDAT-IN2          PIC 9(8).                                
009400     03  DAGENS-DATUM            PIC 9(8).                                
009500     03  W-FORSTA-DATUM          PIC 9(6).                                
009600     03  W-SISTA-DATUM           PIC 9(6).                                
009700     03  DATUM-SIFFRA            PIC S9(9).                               
009800 01  W-AREA-NYCKLAR.                                                      
009900     03  W-IDARTNR               PIC S9(9).                               
010000     03  W-IDDC                  PIC X(2).                                
010100     03  W-TIKLOCK               PIC S9(9)             COMP-3.            
010200     03  W-IDSEKVNR              PIC S9(3)             COMP-3.            
010300 01  W-AREA-INDEX.                                                        
010400     03  W-INDX                  PIC S9(4)  VALUE +0   COMP SYNC.         
010500 01  W-AREA-ACKUMULERAD.                                                  
010600     03  W-KVTOTAL               PIC 9(7)   VALUE ZERO.                   
010700     03  W-KVCHUP                PIC S9(5)9 VALUE ZERO.                   
010800     03  W-KVCHDO                PIC S9(5)9 VALUE ZERO.                   
010900     03  W-KVLSTOTAL             PIC 9(7)   VALUE ZERO.                   
011000     EJECT                                                                
011100                                                                          
011200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011300 01  GENERELLA-SUBPROGRAM.                                                
011400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011900                                                                          
012000*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012100*01 -COPY WMEDAREA                                                        
012200                                                                          
012300 01  MESSAGE-CODES.                                                       
012400     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
012500     03  ERR-RECORD-MISSING      PIC X(3)    VALUE '029'.                 
012600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
012700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012800     03  WRONG-INPUT-FIELD       PIC X(3)    VALUE '194'.                 
012900     EJECT                                                                
013000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013100*                                                                         
013200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013300                                                                          
013400*01 -COPY WMSGINIT                                                        
013500     EJECT                                                                
013600*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
013700                                                                          
013800 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
013900                                                                          
014000*01  -COPY WDATAREA.                                                      
014100                                                                          
014200*    --- BMP PARAMETRAR                                                   
014300 01  FILLER                      PIC X(16)   VALUE 'BMP-PRM '.            
014400 01  BMP-PARAMETRAR.                                                      
014500     03  SKICKA-IDARTNR      PIC 9(9).                                    
014600     03  SKICKA-DAREGDAT-MAX PIC 9(8).                                    
014700     03  SKICKA-DAREGDAT-MIN PIC 9(8).                                    
014800     03  SKICKA-IDDC         PIC X(2).                                    
014900     03  SKICKA-IDHUVTYP     PIC X(4).                                    
015000     03  SKICKA-IDSUBTYP     PIC X(3).                                    
015100     03  SKICKA-IDTRANS      PIC X(4).                                    
015200     03  SKICKA-IDUSER       PIC X(7).                                    
015300     03  SKICKA-IDLAND       PIC X(3).                                    
015400     EJECT                                                                
015500                                                                          
015600*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
015700*                                                                         
015800 01  SPAR-AREA.                                                           
015900     03  SPAR-WDL901KY.                                                   
016000         05  SPAR-IDARTNR         PIC  S9(9) COMP-3.                      
016100         05  SPAR-DAREGDAT        PIC  9(8).                              
016200         05  SPAR-TIKLOCK         PIC  S9(9) COMP-3.                      
016300         05  SPAR-IDSEKVNR        PIC  S9(3) COMP-3.                      
016400     03  SPAR-FILLER              PIC  X(34).                             
016500     03  SPAR-TABELL.                                                     
016600       05  SPAR-WDL901   OCCURS 11.                                       
016700         07  SPAR-WDL901KY-TAB.                                           
016800           09  SPAR-IDARTNR-TAB     PIC  9(9).                            
016900           09  SPAR-DAREGDAT-TAB    PIC  9(8).                            
017000           09  SPAR-TIKLOCK-TAB     PIC  9(9).                            
017100           09  SPAR-IDSEKVNR-TAB    PIC  9(3).                            
017200     03  SPAR-IDTRANS             PIC X(4)    VALUE '5162'.               
017300     03  SPAR-IDARTNR-ENTER       PIC S9(9)        COMP-3.                
017400     03  SPAR-IDARTNR-NEXT        PIC S9(9)        COMP-3.                
017500     03  SPAR-DAREGDAT-ENTER      PIC  9(8).                              
017600     03  SPAR-DAREGDAT-NEXT       PIC  9(8).                              
017700     03  SPAR-TIKLOCK-ENTER       PIC S9(9)        COMP-3.                
017800     03  SPAR-TIKLOCK-NEXT        PIC S9(9)        COMP-3.                
017900     03  SPAR-IDSEKVNR-ENTER      PIC S9(3)        COMP-3.                
018000     03  SPAR-IDSEKVNR-NEXT       PIC S9(3)        COMP-3.                
018100     03  SPAR-KVTOTAL             PIC  9(7).                              
018200     03  SPAR-KVCHUP              PIC  S9(5)9.                            
018300     03  SPAR-KVCHDO              PIC  S9(5)9.                            
018400     03  SPAR-KVLSTOTAL           PIC  9(7).                              
018500     03  SPAR-BILD                PIC X(4).                               
018600                                                                          
018700     EJECT                                                                
018800*                                                                         
018900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
019000                                                                          
019100*01  MID -COPY W5I16201                                                   
019200     EJECT                                                                
019300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019400                                                                          
019500*01  -COPY WMSGAREA                                                       
019600     EJECT                                                                
019700     03  MOD REDEFINES MSG-AREA.                                          
019800*      05  -COPY W5O16201                                                 
019900     EJECT                                                                
020000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
020100                                                                          
020200*01  -COPY WMFSAREA                                                       
020300     EJECT                                                                
020400                                                                          
020500 01  W-PROG-TO-PROG-SW1.                                                  
020600*    03    -COPY  WMSGSOP                                                 
020700                                                                          
020800 01  W-PROG-TO-PROG-SW2.                                                  
020900     03  M-SW-LL                 PIC S9(4)   VALUE +240 COMP SYNC.        
021000     03  M-SW-Z1-Z2              PIC X(2)    VALUE LOW-VALUE.             
021100     03  M-SW-KDTRANS            PIC X(8)    VALUE 'W5T164  '.            
021200     03  M-SW-IDTRANS            PIC X(4)    VALUE '5162'.                
021300     03  M-SW-KDMFSTYP           PIC X(1)    VALUE '2'.                   
021400                                                                          
021500*    03  MID -COPY W5I16401 -PRE 5164-                                    
021600                                                                          
021700                                                                          
021800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021900                                                                          
022000     EJECT                                                                
022100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022200                                                                          
022300 01  NYCKLAR-TILL-DLI.                                                    
022400     03  W-WDL901KY-X.                                                    
022500         05  W-IDARTNR-X    PIC S9(9) COMP-3.                             
022600         05  W-DAREGDAT-X   PIC 9(8).                                     
022700         05  W-TIKLOCK-X    PIC S9(9) COMP-3.                             
022800         05  W-IDSEKVNR-X   PIC S9(3) COMP-3.                             
022900     03  W-WDL901KY-MIN-X.                                                
023000         05  W-IDARTNR-MIN  PIC S9(9) COMP-3  VALUE ZERO.                 
023100         05  W-DAREGDAT-MIN PIC 9(8)          VALUE ZERO.                 
023200         05  W-TIKLOCK-MIN  PIC S9(9) COMP-3  VALUE ZERO.                 
023300         05  W-IDSEKVNR-MIN PIC S9(3) COMP-3  VALUE ZERO.                 
023400     03  W-WDL901KY-MAX-X.                                                
023500         05  W-IDARTNR-MAX  PIC S9(9) COMP-3  VALUE 999999999.            
023600         05  W-DAREGDAT-MAX PIC 9(8)          VALUE 99999999.             
023700         05  W-TIKLOCK-MAX  PIC S9(9) COMP-3  VALUE 999999999.            
023800         05  W-IDSEKVNR-MAX PIC S9(3) COMP-3  VALUE 999.                  
023900     03  W-WDL901-OVRIGA-MIN-X.                                           
024000         05  W-IDDC-MIN     PIC X(2)          VALUE LOW-VALUE.            
024100         05  W-IDDC-MIN2    PIC X(2)          VALUE LOW-VALUE.            
024200         05  W-IDHUVTYP-MIN PIC X(4)          VALUE LOW-VALUE.            
024300         05  W-IDSUBTYP-MIN PIC X(3)          VALUE LOW-VALUE.            
024400         05  W-IDTRANS-MIN  PIC X(4)          VALUE LOW-VALUE.            
024500     03  W-WDL901-OVRIGA-MAX-X.                                           
024600         05  W-IDDC-MAX     PIC X(2)          VALUE HIGH-VALUE.           
024700         05  W-IDDC-MAX2    PIC X(2)          VALUE HIGH-VALUE.           
024800         05  W-IDHUVTYP-MAX PIC X(4)          VALUE HIGH-VALUE.           
024900         05  W-IDSUBTYP-MAX PIC X(3)          VALUE HIGH-VALUE.           
025000         05  W-IDTRANS-MAX  PIC X(4)          VALUE HIGH-VALUE.           
025100                                                                          
025200     03  W-IDDC-B6-X.                                                     
025300         05  W-IDDC-B6      PIC X(2)          VALUE SPACE.                
025400                                                                          
025500*                                                                         
025600*    --- STATUS-KOD FRÅN IMS                                              
025700 01  STATUS-WS                   PIC XX.                                  
025800     88  SEGMENT-FINNS                       VALUE '  '.                  
025900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026100     88  BASEN-SLUT                          VALUE 'GB'.                  
026200                                                                          
026300 01  GODK-STATUSKODER.                                                    
026400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
026500                                                                          
026600 01  SSA1                        PIC X(512).                              
026700 01  SSA2                        PIC X(64).                               
026800     EJECT                                                                
026900*    --- IMS FUNKTIONSKODER                                               
027000*01  -COPY W0003                                                          
027100     EJECT                                                                
027200*    ---  DLI INPUT-OUTPUT AREA                                           
027300                                                                          
027400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLOGA01'.                    
027500 01  DLI-IO-WLLOGA01.                                                     
027600*    03  -COPY WDL901                                                     
027700     EJECT                                                                
027800 01  FILLER         PIC X(16)   VALUE 'WDB601-A AREA'.                    
027900 01   DLI-IO-AREA-B601-A.                                                 
028000*     03  -COPY WDB601 -PRE A-                                            
028100     EJECT                                                                
028200 01  FILLER         PIC X(16)   VALUE 'WDB601-B AREA'.                    
028300 01   DLI-IO-AREA-B601-B.                                                 
028400*     03  -COPY WDB601 -PRE B-                                            
028500     EJECT                                                                
028600 LINKAGE SECTION.                                                         
028700*01  -COPY W0009   -PRE MSG-                                              
028800                                                                          
028900*01  -COPY W0009   -PRE ALT1-                                             
029000                                                                          
029100*01  -COPY W0009   -PRE ALT2-                                             
029200                                                                          
029300*01  -COPY W0008   -PRE USEA-                                             
029400     05  FILLER                  PIC X.                                   
029500     EJECT                                                                
029600*01  -COPY W0008  -PRE LOGA-                                              
029700     05  FILLER                  PIC X.                                   
029800     EJECT                                                                
029900*01  -COPY W0008  -PRE WDB6-                                              
030000     05  FILLER                  PIC X.                                   
030100     EJECT                                                                
030200 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB ALT2-PCB USEA-PCB             
030300                           LOGA-PCB WDB6-PCB.                             
030400 MAIN SECTION.                                                            
030500     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB ALT2-PCB USEA-PCB             
030600                           LOGA-PCB WDB6-PCB.                             
030700                                                                          
030800     PERFORM IMS-GET-MSG                                                  
030900     IF SEGMENT-FINNS                                                     
031000       PERFORM A-INIT                                                     
031100       PERFORM B-KOLLA-NYCKLAR                                            
031200       IF NYCKLAR-OK                                                      
031300         IF MFS-UPDATE                                                    
031400           PERFORM I-KOLLA-EXTRACT                                        
031500           IF EXTRACT-OK                                                  
031600             PERFORM J-STARTA-BMP                                         
031700             MOVE 'BMP STARTED'     TO MOD-TEMFSINF                       
031800           ELSE                                                           
031900             MOVE 'BMP NOT STARTED' TO MOD-TEMFSINF                       
032000           END-IF                                                         
032100         ELSE                                                             
032200           IF MFS-FIRST AND NOT DETALJ-MID                                
032300             PERFORM C-FOERSTA-SIDA                                       
032400             PERFORM S04-FLYTTA-TILL-MOD                                  
032500**** LOGA IS ONE INTERVALL LOGA2 IS TWO INTERVALLS                        
032600             IF A-DCS-NDC-CN OR A-DCS-NDC-OTHERS                          
032700             OR (A-DCS-NDC-PF AND NOT A-DCS-FTG-PV)                       
032800               PERFORM IMS-GN-LOGA                                        
032900             ELSE                                                         
033000               PERFORM IMS-GN-LOGA2                                       
033100             END-IF                                                       
033200           ELSE                                                           
033300             IF MFS-NEXT                                                  
033400               PERFORM D-NAESTA-SIDA                                      
033500               PERFORM IMS-GU-LOGA                                        
033600             ELSE                                                         
033700               PERFORM E-SAMMA-SIDA                                       
033800               IF NOT BYT-BILD                                            
033900                 PERFORM IMS-GU-LOGA                                      
034000               ELSE                                                       
034100                 PERFORM H-BYT-BILD                                       
034200               END-IF                                                     
034300             END-IF                                                       
034400           END-IF                                                         
034500           IF ALLT-OK                                                     
034600             PERFORM F-LAES-VISA-INFO                                     
034700           END-IF                                                         
034800         END-IF                                                           
034900       END-IF                                                             
035000       IF BYT-EJ-BILD                                                     
035100*    --- OM ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
035200         COMPUTE MSG-KVLL = LENGTH OF MOD-W5O16201 + 4                    
035300         PERFORM IMS-INSERT-MSG                                           
035400       END-IF                                                             
035500     END-IF                                                               
035600                                                                          
035700     MOVE ZERO TO RETURN-CODE                                             
035800     GOBACK                                                               
035900     .                                                                    
036000     EJECT                                                                
036100 A-INIT SECTION.                                                          
036200                                                                          
036300     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
036400     MOVE DAGENS-DATUM(3:6)          TO W-SISTA-DATUM                     
036500                                                                          
036600     MOVE DAGENS-DATUM(3:2)          TO W-FORSTA-DATUM(1:2)               
036700     MOVE 0101                       TO W-FORSTA-DATUM(3:4)               
036800                                                                          
036900     IF MSG-DUBBLA-TRANSKODER                                             
037000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I16201                 
037100       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
037200       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
037300     ELSE                                                                 
037400       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W5I16201                 
037500       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
037600       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
037700     END-IF                                                               
037800                                                                          
037900     MOVE MSG-KDTRTYP     TO MFS-KDTRTYP                                  
038000     MOVE MSG-IDPFK       TO MFS-IDPFK                                    
038100     MOVE MFS-IDTRANS     TO W-IDTRANS                                    
038200                                                                          
038300     MOVE LOW-VALUE       TO MSG-AREA                                     
038400     MOVE 'W5O162N1'      TO MFS-IDMOD                                    
038500     MOVE '5162'          TO MOD-IDTRANS                                  
038600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
038700                                                                          
038800     IF EGEN-MID OR HELP-MID                                              
038900       CONTINUE                                                           
039000     ELSE                                                                 
039100       MOVE SPACE TO MFS-KDTRTYP                                          
039200       MOVE '7'   TO MFS-IDPFK                                            
039300     END-IF                                                               
039400     MOVE 'GB'    TO MED-IDSKYLT                                          
039500     .                                                                    
039600     EJECT                                                                
039700 B-KOLLA-NYCKLAR SECTION.                                                 
039800                                                                          
039900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
040000     MOVE '001'             TO MSGI-KDCALL                                
040100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
040200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
040300     MOVE '5162'            TO MSGI-IDTRANS                               
040400                                                                          
040500     IF EGEN-MID AND MID-IDARTNR-IN NOT = ALL '+'                         
040600       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
040700     END-IF                                                               
040800                                                                          
040900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
041000                                                                          
041100     MOVE ZERO           TO SPAR-IDARTNR-ENTER                            
041200                            SPAR-IDARTNR-NEXT                             
041300     IF GODK-MID                                                          
041400       MOVE MSGI-SPAR-AREA           TO SPAR-AREA                         
041500       MOVE SPACE                    TO SPAR-BILD                         
041600     END-IF                                                               
041700                                                                          
041800     MOVE JA                         TO NYCKLAR-SW                        
041900*    -- KONTROLL AV SELECT-RAD                                            
042000     MOVE +1                         TO INDX                              
042100     PERFORM UNTIL INDX > MAX-INDX                                        
042200**** SELECT-RADEN GÄLLER BARA EGEN OCH DETALJ-BILD                        
042300       IF NOT (DETALJ-MID OR EGEN-MID)                                    
042400         MOVE ' '                    TO MID-SELECT-RAD(INDX)              
042500       END-IF                                                             
042600       IF MID-SELECT-RAD(INDX) NOT = '+' AND ' '                          
042700         IF MID-SELECT-RAD(INDX) = 'S'                                    
042800           MOVE INDX                 TO W-INDX                            
042900           MOVE 12                   TO INDX                              
043000           MOVE JA                   TO BYT-SW                            
043100         ELSE                                                             
043200          IF NOT DETALJ-MID                                               
043300           MOVE NEJ                TO INDATA-SW                           
043400                                      NYCKLAR-SW                          
043500            MOVE MID-SELECT-RAD(INDX) TO MOD-SELECT-RAD(INDX)             
043600            MOVE MFS-ALFA-FAELT-FEL   TO MOD-SELECT-RAD-ATTR(INDX)        
043700          END-IF                                                          
043800         END-IF                                                           
043900       END-IF                                                             
044000       ADD 1                         TO INDX                              
044100     END-PERFORM                                                          
044200                                                                          
044300     PERFORM BA-PREPARERA-FAELT                                           
044400     PERFORM BB-KOLLA-INDATA                                              
044500     PERFORM S04-FLYTTA-TILL-MOD                                          
044600                                                                          
044700     IF INDATA-FEL                                                        
044800       MOVE WRONG-INPUT-FIELD TO MED-IDMFSFEL                             
044900       CALL WMEDKONV USING MED-WMEDAREA                                   
045000       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
045100       PERFORM MFS-ROER-EJ-FAELT-UT                                       
045200     ELSE                                                                 
045300       IF NYCKLAR-FEL                                                     
045400         CALL WMEDKONV USING MED-WMEDAREA                                 
045500         MOVE MED-MFSFEL      TO MOD-TEMFSFEL                             
045600         PERFORM MFS-RENSA-FAELT-IN                                       
045700         PERFORM MFS-RENSA-FAELT-UT                                       
045800       END-IF                                                             
045900     END-IF                                                               
046000     .                                                                    
046100     EJECT                                                                
046200 BA-PREPARERA-FAELT SECTION.                                              
046300                                                                          
046400     IF NOT EGEN-MID AND                                                  
046500        NOT DETALJ-MID                                                    
046600         MOVE MSGI-IDARTNR   TO MID-IDARTNR-IN                            
046700     ELSE                                                                 
046800       IF MID-IDARTNR-IN = ALL '+' AND                                    
046900         BYT-SW NOT = JA           AND                                    
047000         NOT MFS-NEXT              AND                                    
047100         NOT MFS-FIRST                                                    
047200         MOVE MID-IDARTNR-UT TO MID-IDARTNR-IN                            
047300       END-IF                                                             
047400     END-IF                                                               
047500                                                                          
047600     IF NOT GODK-MID                                                      
047700       MOVE SPACE            TO MID-IDDC-IN                               
047800                                MID-IDHUVTYP-IN                           
047900                                MID-IDSUBTYP-IN                           
048000                                MID-IDTRANS-IN                            
048100     END-IF                                                               
048200                                                                          
048300     IF W-IDTRANS = '5163'                                                
048400       MOVE SPACE            TO MID-IDTRANS-IN                            
048500     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800 BB-KOLLA-INDATA SECTION.                                                 
048900                                                                          
049000*    -- KONTROLL AV IDARTNR                                               
049100                                                                          
049200     MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR-IN                            
049300     IF MID-IDARTNR-IN NOT = ALL '+' AND NOT                              
049400        MFS-UPDATE                   AND NOT                              
049500        MFS-NEXT                                                          
049600        MOVE '7'             TO MFS-IDPFK                                 
049700        MOVE SPACE           TO MFS-KDTRTYP                               
049800     END-IF                                                               
049900                                                                          
050000     IF MID-IDARTNR-IN = ALL '+' AND                                      
050100        MID-IDARTNR-UT NOT = 0                                            
050200       IF GODK-MID                                                        
050300         MOVE MID-IDARTNR-UT   TO MID-IDARTNR-IN                          
050400       ELSE                                                               
050500         MOVE MSGI-IDARTNR     TO MID-IDARTNR-IN                          
050600       END-IF                                                             
050700     END-IF                                                               
050800                                                                          
050900     IF MID-IDARTNR-IN NOT = ALL '+'                                      
051000       INSPECT MID-IDARTNR-IN REPLACING LEADING SPACE BY ZERO             
051100       IF MID-IDARTNR-IN NUMERIC                                          
051200         MOVE MID-IDARTNR-IN TO WS-IDARTNR                                
051300                                                                          
051400         IF WS-IDARTNR NUMERIC AND                                        
051500            WS-IDARTNR > '000000000'                                      
051600           MOVE WS-IDARTNR   TO W-IDARTNR-MAX                             
051700                                W-IDARTNR-MIN                             
051800                                W-IDARTNR                                 
051900                                MSGI-IDARTNR                              
052000                                MID-IDARTNR-IN                            
052100         ELSE                                                             
052200           MOVE NEJ          TO NYCKLAR-SW                                
052300         END-IF                                                           
052400       ELSE                                                               
052500         MOVE NEJ            TO NYCKLAR-SW                                
052600       END-IF                                                             
052700     ELSE                                                                 
052800       MOVE NEJ              TO NYCKLAR-SW                                
052900     END-IF                                                               
053000                                                                          
053100*    -- KONTROLL AV IDDC                                                  
053200                                                                          
053300     IF MID-IDDC-IN = ALL '+' AND                                         
053400        MID-IDDC-UT NOT = SPACE                                           
053500       MOVE MID-IDDC-UT        TO MID-IDDC-IN                             
053600     END-IF                                                               
053700                                                                          
053800     IF MID-IDDC-IN NOT = ALL '+' AND                                     
053900        MID-IDDC-IN NOT = ' '                                             
054000       IF MID-IDDC-IN NOT = '**'                                          
054100         MOVE MID-IDDC-IN      TO W-IDDC-B6                               
054200                                                                          
054300         MOVE JA               TO WDB6-A-SW                               
054400         PERFORM IMS-GU-WDB601-A                                          
054500         IF SEGMENT-SAKNAS                                                
054600           MOVE NEJ            TO WDB6-A-SW                               
054700         END-IF                                                           
054800                                                                          
054900         MOVE MSGI-IDDC        TO W-IDDC-B6                               
055000                                                                          
055100         MOVE JA               TO WDB6-B-SW                               
055200         PERFORM IMS-GU-WDB601-B                                          
055300         IF SEGMENT-SAKNAS                                                
055400           MOVE NEJ            TO WDB6-B-SW                               
055500         END-IF                                                           
055600                                                                          
055700         IF WDB6-A-FINNS AND NOT A-DCS-DDC AND WDB6-B-FINNS               
055800**** CDC AND SDC AND LDC BELONGING TO SEPV                                
055900           IF ((A-DCS-CDC OR A-DCS-CDC-TR OR A-DCS-SDC)                   
056000           AND (B-DCS-CDC OR B-DCS-CDC-TR OR B-DCS-SDC))                  
056100**** NON VCC AND NA                                                       
056200           OR (A-DCS-NDC AND (B-DCS-NDC OR B-DCS-CDC))                    
056300              MOVE MID-IDDC-IN TO W-IDDC                                  
056400              MOVE W-IDDC      TO W-IDDC-MIN                              
056500                                  W-IDDC-MAX                              
056600                                  W-IDDC-MIN2                             
056700                                  W-IDDC-MAX2                             
056800           ELSE                                                           
056900             MOVE NEJ          TO NYCKLAR-SW                              
057000           END-IF                                                         
057100         ELSE                                                             
057200           MOVE NEJ            TO NYCKLAR-SW                              
057300         END-IF                                                           
057400       END-IF                                                             
057500     ELSE                                                                 
057600       MOVE MSGI-IDDC          TO W-IDDC-B6                               
057700       PERFORM IMS-GU-WDB601-A                                            
057800       IF A-DCS-NDC-NA                                                    
057900**** DC 41 TO 51 AND 92                                                   
058000         MOVE WC-NDC-US-RU     TO W-IDDC-MIN                              
058100         MOVE WC-NDC-CA        TO W-IDDC-MAX                              
058200         MOVE WC-NDC-US-BAT    TO W-IDDC-MIN2                             
058300         MOVE WC-NDC-US-BAT    TO W-IDDC-MAX2                             
058400       ELSE                                                               
058500**** DC 7A TO 74                                                          
058600         IF A-DCS-NDC-CN                                                  
058700           MOVE WC-LDC-CN-7A     TO W-IDDC-MIN                            
058800           MOVE WC-NDC-CN-74     TO W-IDDC-MAX                            
058900         ELSE                                                             
059000**** ONLY ONE DC                                                          
059100           IF A-DCS-NDC-OTHERS                                            
059200           OR (A-DCS-NDC-PF AND NOT A-DCS-FTG-PV)                         
059300             MOVE A-DCS-IDDC     TO W-IDDC-MIN                            
059400             MOVE A-DCS-IDDC     TO W-IDDC-MAX                            
059500           ELSE                                                           
059600**** CDC 1A TO 4D, 6A TO 62                                               
059700**** SHOULD ALSO BE 91                                                    
059800             MOVE WC-LDC-SE-1A TO W-IDDC-MIN                              
059900             MOVE '40'         TO W-IDDC-MAX                              
060000             MOVE WC-NDC-JP-6A TO W-IDDC-MIN2                             
060100             MOVE WC-NDC-AU    TO W-IDDC-MAX2                             
060200           END-IF                                                         
060300         END-IF                                                           
060400       END-IF                                                             
060500     END-IF                                                               
060600                                                                          
060700*    -- KONTROLL AV LOGGTYP                                               
060800                                                                          
060900     IF MID-IDHUVTYP-IN = ALL '+' AND                                     
061000        MID-IDHUVTYP-UT NOT = SPACE                                       
061100       MOVE MID-IDHUVTYP-UT TO MID-IDHUVTYP-IN                            
061200     END-IF                                                               
061300     IF MID-IDSUBTYP-IN = ALL '+' AND                                     
061400        MID-IDSUBTYP-UT NOT = SPACE                                       
061500       MOVE MID-IDSUBTYP-UT TO MID-IDSUBTYP-IN                            
061600     END-IF                                                               
061700                                                                          
061800     IF MID-IDHUVTYP-IN = ALL '+' AND                                     
061900        MID-IDSUBTYP-IN NOT = ALL '+'                                     
062000       MOVE NEJ             TO NYCKLAR-SW                                 
062100     END-IF                                                               
062200                                                                          
062300     IF MID-IDHUVTYP-IN NOT = ALL '+' AND                                 
062400        MID-IDHUVTYP-IN NOT = ' '                                         
062500       MOVE MID-IDHUVTYP-IN TO W-IDHUVTYP-MIN                             
062600                               W-IDHUVTYP-MAX                             
062700     END-IF                                                               
062800                                                                          
062900     IF MID-IDSUBTYP-IN NOT = ALL '+' AND                                 
063000        MID-IDSUBTYP-IN NOT = ' '                                         
063100       MOVE MID-IDSUBTYP-IN TO W-IDSUBTYP-MIN                             
063200                               W-IDSUBTYP-MAX                             
063300     END-IF                                                               
063400                                                                          
063500*    -- KONTROLL AV TIREGDAT                                              
063600                                                                          
063700     IF MID-TIREGDAT-IN1 = ALL '+' AND                                    
063800        MID-TIREGDAT-UT1 NOT = 0                                          
063900       MOVE MID-TIREGDAT-UT1   TO MID-TIREGDAT-IN1                        
064000     END-IF                                                               
064100     IF MID-TIREGDAT-IN2 = ALL '+' AND                                    
064200        MID-TIREGDAT-UT2 NOT = 0                                          
064300       MOVE MID-TIREGDAT-UT2   TO MID-TIREGDAT-IN2                        
064400     END-IF                                                               
064500                                                                          
064600     IF NOT GODK-MID                                                      
064700       MOVE W-SISTA-DATUM      TO MID-TIREGDAT-IN2                        
064800                                  MOD-TIREGDAT-UT2                        
064900       MOVE W-FORSTA-DATUM     TO MID-TIREGDAT-IN1                        
065000                                  MOD-TIREGDAT-UT1                        
065100     ELSE                                                                 
065200       MOVE 'AAMMDD'           TO DAT-KDDATFORM                           
065300       IF MID-TIREGDAT-IN1 = ALL '+'                                      
065400         MOVE W-FORSTA-DATUM   TO MID-TIREGDAT-IN1                        
065500                                                                          
065600       ELSE                                                               
065700         MOVE MID-TIREGDAT-IN1 TO DAT-I-TIDATUM                           
065800         CALL WDATKONV USING DAT-KDDATFORM,                               
065900                             DAT-I-TIDATUM,                               
066000                             DAT-O-TIDATUM,                               
066100                             DAT-KDSVAR                                   
066200         IF DAT-KDSVAR = 'F'                                              
066300           MOVE NEJ            TO NYCKLAR-SW                              
066400         END-IF                                                           
066500       END-IF                                                             
066600                                                                          
066700       IF MID-TIREGDAT-IN2 = ALL '+'                                      
066800         MOVE W-SISTA-DATUM    TO MID-TIREGDAT-IN2                        
066900       ELSE                                                               
067000         MOVE MID-TIREGDAT-IN2 TO DAT-I-TIDATUM                           
067100         CALL WDATKONV USING DAT-KDDATFORM,                               
067200                             DAT-I-TIDATUM,                               
067300                             DAT-O-TIDATUM,                               
067400                             DAT-KDSVAR                                   
067500         IF DAT-KDSVAR = 'F'                                              
067600           MOVE NEJ TO NYCKLAR-SW                                         
067700         END-IF                                                           
067800       END-IF                                                             
067900     END-IF                                                               
068000                                                                          
068100     IF NYCKLAR-OK                                                        
068200       PERFORM S01-RAEKNA-OM-DATUM-IN                                     
068300       MOVE W-TIREGDAT-IN1 TO W-DAREGDAT-MAX                              
068400       MOVE W-TIREGDAT-IN2 TO W-DAREGDAT-MIN                              
068500       COMPUTE W-DIFF = W-DAREGDAT-TOM - W-DAREGDAT-FROM                  
068600       END-COMPUTE                                                        
068700       IF W-DIFF > 10000                                                  
068800         MOVE NEJ          TO NYCKLAR-SW                                  
068900       END-IF                                                             
069000       IF W-TIREGDAT-IN1 < W-TIREGDAT-IN2                                 
069100         MOVE NEJ          TO NYCKLAR-SW                                  
069200       END-IF                                                             
069300     END-IF                                                               
069400                                                                          
069500*    -- KONTROLL AV IDTRANS                                               
069600                                                                          
069700     IF MID-IDTRANS-IN = ALL '+' AND                                      
069800        MID-IDTRANS-UT NOT = SPACE                                        
069900       MOVE MID-IDTRANS-UT   TO MID-IDTRANS-IN                            
070000     END-IF                                                               
070100                                                                          
070200     IF MID-IDTRANS-IN NOT = ALL '+'                                      
070300       IF MID-IDTRANS-IN NOT = ' '                                        
070400         MOVE MID-IDTRANS-IN TO W-IDTRANS-MIN                             
070500                                W-IDTRANS-MAX                             
070600       END-IF                                                             
070700     END-IF                                                               
070800     .                                                                    
070900     EJECT                                                                
071000 C-FOERSTA-SIDA SECTION.                                                  
071100                                                                          
071200     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
071300     CALL WMEDKONV USING MED-WMEDAREA                                     
071400     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
071500     .                                                                    
071600     EJECT                                                                
071700 D-NAESTA-SIDA SECTION.                                                   
071800                                                                          
071900     IF SPAR-IDTRANS = '5162'                                             
072000       IF SPAR-IDARTNR-NEXT  NUMERIC                                      
072100         MOVE SPAR-IDARTNR-NEXT        TO W-IDARTNR-X                     
072200       ELSE                                                               
072300         MOVE  ZERO                    TO W-IDARTNR-X                     
072400       END-IF                                                             
072500       MOVE SPAR-DAREGDAT-NEXT TO W-DAREGDAT-X                            
072600       MOVE SPAR-TIKLOCK-NEXT  TO W-TIKLOCK-X                             
072700       MOVE SPAR-IDSEKVNR-NEXT TO W-IDSEKVNR-X                            
072800     ELSE                                                                 
072900       PERFORM MFS-RENSA-FAELT-IN                                         
073000     END-IF                                                               
073100     .                                                                    
073200     EJECT                                                                
073300 E-SAMMA-SIDA SECTION.                                                    
073400                                                                          
073500     IF SPAR-IDTRANS = '5162' OR '0551'                                   
073600       IF SPAR-IDARTNR-ENTER NUMERIC                                      
073700         MOVE SPAR-IDARTNR-ENTER       TO W-IDARTNR-X                     
073800       ELSE                                                               
073900         MOVE  ZERO                    TO W-IDARTNR-X                     
074000       END-IF                                                             
074100       MOVE SPAR-DAREGDAT-ENTER TO W-DAREGDAT-X                           
074200       MOVE SPAR-TIKLOCK-ENTER  TO W-TIKLOCK-X                            
074300       MOVE SPAR-IDSEKVNR-ENTER TO W-IDSEKVNR-X                           
074400     ELSE                                                                 
074500       PERFORM MFS-RENSA-FAELT-IN                                         
074600     END-IF                                                               
074700     .                                                                    
074800     EJECT                                                                
074900 F-LAES-VISA-INFO SECTION.                                                
075000                                                                          
075100     IF SEGMENT-SAKNAS                                                    
075200       MOVE ERR-RECORD-MISSING  TO MED-IDMFSFEL                           
075300       CALL WMEDKONV USING MED-WMEDAREA                                   
075400       MOVE MED-MFSFEL          TO MOD-TEMFSFEL                           
075500       PERFORM MFS-RENSA-FAELT-UT                                         
075600     ELSE                                                                 
075700       MOVE +1 TO INDX                                                    
075800       IF SEGMENT-FINNS                                                   
075900         MOVE ZERO                 TO SPAR-IDARTNR-NEXT                   
076000                                      SPAR-DAREGDAT-NEXT                  
076100                                      SPAR-TIKLOCK-NEXT                   
076200                                      SPAR-IDSEKVNR-NEXT                  
076300         MOVE LOGG-IDARTNR         TO SPAR-IDARTNR-ENTER                  
076400         MOVE LOGG-DAREGDAT-9KOMPL TO SPAR-DAREGDAT-ENTER                 
076500         MOVE LOGG-TIKLOCK-9KOMPL  TO SPAR-TIKLOCK-ENTER                  
076600         MOVE LOGG-IDSEKVNR        TO SPAR-IDSEKVNR-ENTER                 
076700       ELSE                                                               
076800         MOVE W-IDARTNR-MIN     TO SPAR-IDARTNR-ENTER                     
076900         MOVE W-DAREGDAT-MIN    TO SPAR-DAREGDAT-ENTER                    
077000         MOVE W-TIKLOCK-MIN     TO SPAR-TIKLOCK-ENTER                     
077100         MOVE W-IDSEKVNR-MIN    TO SPAR-IDSEKVNR-ENTER                    
077200       END-IF                                                             
077300                                                                          
077400       PERFORM UNTIL INDX > MAX-INDX OR                                   
077500                      SEGMENT-SAKNAS OR                                   
077600                      BASEN-SLUT                                          
077700        IF SEGMENT-FINNS                                                  
077800            IF MFS-FIRST AND NOT DETALJ-MID                               
077900              PERFORM S03-RAEKNA-ACKUMULERAT                              
078000            END-IF                                                        
078100            MOVE LOGG-IDARTNR         TO SPAR-IDARTNR-TAB (INDX)          
078200            MOVE LOGG-DAREGDAT-9KOMPL TO SPAR-DAREGDAT-TAB (INDX)         
078300            MOVE LOGG-TIKLOCK-9KOMPL  TO SPAR-TIKLOCK-TAB (INDX)          
078400            MOVE LOGG-IDSEKVNR        TO SPAR-IDSEKVNR-TAB (INDX)         
078500            PERFORM S02-RAEKNA-OM-DATUM-UT                                
078600            IF DETALJ-MID                                                 
078700              IF SPAR-IDARTNR  = LOGG-IDARTNR        AND                  
078800                SPAR-DAREGDAT = LOGG-DAREGDAT-9KOMPL AND                  
078900                SPAR-TIKLOCK  = LOGG-TIKLOCK-9KOMPL  AND                  
079000                SPAR-IDSEKVNR = LOGG-IDSEKVNR                             
079100                MOVE 'S'              TO MOD-SELECT-RAD (INDX)            
079200              END-IF                                                      
079300            END-IF                                                        
079400            MOVE LOGG-IDDC            TO MOD-IDDC (INDX)                  
079500            MOVE LOGG-IDHUVTYP        TO MOD-IDHUVTYP (INDX)              
079600            MOVE LOGG-IDSUBTYP        TO MOD-IDSUBTYP (INDX)              
079700            MOVE W-DAREGDAT-KONV      TO MOD-TIREGDAT (INDX)              
079800            MOVE LOGG-KVART-SALDO     TO MOD-KVART-SALDO (INDX)           
079900            MOVE LOGG-IDTECKEN-KVAKS-PAV                                  
080000                               TO MOD-IDTECKEN-KVAKS-PAV (INDX)           
080100            MOVE LOGG-KVAKS-PAV       TO MOD-KVAKS-PAV (INDX)             
080200            MOVE LOGG-IDTECKEN-KVAKS  TO MOD-IDTECKEN-KVAKS (INDX)        
080300            MOVE LOGG-KVAKS           TO MOD-KVAKS (INDX)                 
080400            MOVE LOGG-IDTECKEN-KVEFRS TO MOD-IDTECKEN-KVEFRS(INDX)        
080500            MOVE LOGG-KVEFRS          TO MOD-KVEFRS (INDX)                
080600            MOVE LOGG-IDTECKEN-KVLS   TO MOD-IDTECKEN-KVLS (INDX)         
080700            MOVE LOGG-KVLS            TO MOD-KVLS (INDX)                  
080800            ADD 1 TO INDX                                                 
080900          IF A-DCS-NDC-CN OR A-DCS-NDC-OTHERS                             
081000          OR (A-DCS-NDC-PF AND NOT A-DCS-FTG-PV)                          
081100            PERFORM IMS-GN-LOGA                                           
081200          ELSE                                                            
081300            PERFORM IMS-GN-LOGA2                                          
081400          END-IF                                                          
081500        ELSE                                                              
081600          MOVE MFS-RENSA-FAELT TO MOD-IDDC (INDX)                         
081700                                  MOD-IDHUVTYP (INDX)                     
081800                                  MOD-IDSUBTYP (INDX)                     
081900                                  MOD-TIREGDAT (INDX)                     
082000                                  MOD-KVART-SALDO (INDX)                  
082100                                  MOD-IDTECKEN-KVAKS-PAV (INDX)           
082200                                  MOD-KVAKS-PAV (INDX)                    
082300                                  MOD-IDTECKEN-KVAKS (INDX)               
082400                                  MOD-KVAKS (INDX)                        
082500                                  MOD-IDTECKEN-KVEFRS (INDX)              
082600                                  MOD-KVEFRS (INDX)                       
082700                                  MOD-IDTECKEN-KVLS (INDX)                
082800                                  MOD-KVLS (INDX)                         
082900                                  SPAR-WDL901KY-TAB (INDX)                
083000          ADD 1 TO INDX                                                   
083100        END-IF                                                            
083200       END-PERFORM                                                        
083300                                                                          
083400       IF SEGMENT-FINNS                                                   
083500         MOVE LOGG-IDARTNR         TO SPAR-IDARTNR-NEXT                   
083600         MOVE LOGG-DAREGDAT-9KOMPL TO SPAR-DAREGDAT-NEXT                  
083700         MOVE LOGG-TIKLOCK-9KOMPL  TO SPAR-TIKLOCK-NEXT                   
083800         MOVE LOGG-IDSEKVNR        TO SPAR-IDSEKVNR-NEXT                  
083900         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
084000         CALL WMEDKONV USING MED-WMEDAREA                                 
084100         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
084200         IF (MFS-FIRST AND NOT DETALJ-MID)                                
084300           PERFORM UNTIL SEGMENT-SAKNAS OR                                
084400                         BASEN-SLUT                                       
084500           PERFORM S03-RAEKNA-ACKUMULERAT                                 
084600             IF A-DCS-NDC-CN OR A-DCS-NDC-OTHERS                          
084700             OR (A-DCS-NDC-PF AND NOT A-DCS-FTG-PV)                       
084800               PERFORM IMS-GN-LOGA                                        
084900             ELSE                                                         
085000               PERFORM IMS-GN-LOGA2                                       
085100             END-IF                                                       
085200           END-PERFORM                                                    
085300         END-IF                                                           
085400       END-IF                                                             
085500                                                                          
085600       IF (MFS-FIRST AND NOT DETALJ-MID)                                  
085700         MOVE W-KVTOTAL      TO MOD-KVTOTAL                               
085800                                SPAR-KVTOTAL                              
085900         MOVE W-KVCHUP       TO MOD-KVCHUP                                
086000                                SPAR-KVCHUP                               
086100         MOVE W-KVCHDO       TO MOD-KVCHDO                                
086200                                SPAR-KVCHDO                               
086300         MOVE W-KVLSTOTAL    TO MOD-KVLSTOTAL                             
086400                                SPAR-KVLSTOTAL                            
086500       ELSE                                                               
086600         MOVE SPAR-KVTOTAL   TO MOD-KVTOTAL                               
086700         MOVE SPAR-KVCHUP    TO MOD-KVCHUP                                
086800         MOVE SPAR-KVCHDO    TO MOD-KVCHDO                                
086900         MOVE SPAR-KVLSTOTAL TO MOD-KVLSTOTAL                             
087000       END-IF                                                             
087100                                                                          
087200       MOVE '002'            TO MSGI-KDCALL                               
087300       MOVE '5162'           TO SPAR-IDTRANS                              
087400       MOVE SPAR-AREA        TO MSGI-SPAR-AREA                            
087500       MOVE MID-IDARTNR-UT   TO MSGI-IDARTNR                              
087600       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
087700     END-IF                                                               
087800     .                                                                    
087900     EJECT                                                                
088000 H-BYT-BILD SECTION.                                                      
088100                                                                          
088200     MOVE '5162'                    TO SPAR-BILD                          
088300                                                                          
088400* ---HÄMTAR RÄTT RAD-VÄRDE TILL 5164-BILDEN                               
088500     MOVE W-INDX TO INDX                                                  
088600     MOVE SPAR-IDARTNR-TAB  (INDX)  TO SPAR-IDARTNR                       
088700     MOVE SPAR-DAREGDAT-TAB (INDX)  TO SPAR-DAREGDAT                      
088800     MOVE SPAR-TIKLOCK-TAB  (INDX)  TO SPAR-TIKLOCK                       
088900     MOVE SPAR-IDSEKVNR-TAB (INDX)  TO SPAR-IDSEKVNR                      
089000                                                                          
089100     MOVE '002'               TO MSGI-KDCALL                              
089200     MOVE '5162'              TO SPAR-IDTRANS                             
089300     MOVE SPAR-AREA           TO MSGI-SPAR-AREA                           
089400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
089500                                                                          
089600* ---SKICKAR VÄRDE TILL 5164-MID FÖR ATT SEDAN                            
089700* ---STARTA UPP DETTA PROGRAM                                             
089800     MOVE LOW-VALUE           TO 5164-MID-W5I16401                        
089900     MOVE MID-IDARTNR-UT      TO 5164-MID-IDARTNR-UT                      
090000     MOVE MID-IDDC-UT         TO 5164-MID-IDDC-UT                         
090100     MOVE MID-IDHUVTYP-UT     TO 5164-MID-IDHUVTYP-UT                     
090200     MOVE MID-IDSUBTYP-UT     TO 5164-MID-IDSUBTYP-UT                     
090300     MOVE MID-TIREGDAT-UT1    TO 5164-MID-TIREGDAT-FOM-UT                 
090400     MOVE MID-TIREGDAT-UT2    TO 5164-MID-TIREGDAT-TOM-UT                 
090500     MOVE MID-IDTRANS-UT      TO 5164-MID-IDTRANS-UT                      
090600     COMPUTE MSG-KVLL = LENGTH OF MOD-W5O16201 + 17                       
090700     PERFORM IMS-INSERT-ALT2-MSG                                          
090800     MOVE NEJ                 TO ALLT-SW                                  
090900     .                                                                    
091000     EJECT                                                                
091100 I-KOLLA-EXTRACT.                                                         
091200                                                                          
091300     PERFORM MFS-ROER-EJ-FAELT-UT                                         
091400     IF MID-FLEXTRAKT = 'Y' OR ' ' OR 'N'                                 
091500       MOVE MFS-ROER-EJ-FAELT    TO MOD-FLEXTRAKT-ATTR                    
091600       MOVE MFS-RENSA-FAELT      TO MOD-FLEXTRAKT                         
091700       IF MID-FLEXTRAKT = ' ' OR 'N'                                      
091800         MOVE NEJ                TO EXTRACT-SW                            
091900       END-IF                                                             
092000     ELSE                                                                 
092100       MOVE NEJ                  TO EXTRACT-SW                            
092200       MOVE WRONG-INPUT-FIELD    TO MED-IDMFSFEL                          
092300       CALL WMEDKONV USING MED-WMEDAREA                                   
092400       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
092500       MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLEXTRAKT-ATTR                    
092600     END-IF                                                               
092700     .                                                                    
092800     EJECT                                                                
092900 J-STARTA-BMP  SECTION.                                                   
093000*                                                                         
093100*  STARTA BMP W555B3                                                      
093200*                                                                         
093300     MOVE W-IDARTNR       TO SKICKA-IDARTNR                               
093400     MOVE MID-IDDC-IN     TO SKICKA-IDDC                                  
093500     MOVE MID-IDHUVTYP-IN TO SKICKA-IDHUVTYP                              
093600     MOVE MID-IDSUBTYP-IN TO SKICKA-IDSUBTYP                              
093700     MOVE W-DAREGDAT-MIN  TO SKICKA-DAREGDAT-MIN                          
093800     MOVE W-DAREGDAT-MAX  TO SKICKA-DAREGDAT-MAX                          
093900     MOVE MID-IDTRANS-IN  TO SKICKA-IDTRANS                               
094000     MOVE MSGI-IDUSER     TO SKICKA-IDUSER                                
094100     MOVE MSGI-IDLAND-SPR TO SKICKA-IDLAND                                
094200                                                                          
094300     MOVE '5162'          TO MSGSOP-IDTRANS                               
094400     MOVE MFS-KDMFSFOR    TO MSGSOP-KDMFSFOR                              
094500     MOVE 'W555B3'        TO MSGSOP-IDPROCESS                             
094600     MOVE 'O'             TO MSGSOP-KDSOPFUNK                             
094700                                                                          
094800     STRING 'IDARTNR(' SKICKA-IDARTNR ')                                  
094900-           'IDDC(' SKICKA-IDDC ')                                        
095000-           'HUVTYP(' SKICKA-IDHUVTYP ')                                  
095100-           'SUBTYP(' SKICKA-IDSUBTYP ')                                  
095200-           'MAXDAT(' SKICKA-DAREGDAT-MAX ')                              
095300-           'MINDAT(' SKICKA-DAREGDAT-MIN ')                              
095400-           'IDTRANS(' SKICKA-IDTRANS ')                                  
095500-           'IDUSER(' SKICKA-IDUSER ')                                    
095600-           'IDLAND(' SKICKA-IDLAND ')'                                   
095700          DELIMITED BY SIZE INTO MSGSOP-TESYMBV                           
095800     PERFORM IMS-INSERT-ALT1-MSG                                          
095900     .                                                                    
096000     EJECT                                                                
096100 S01-RAEKNA-OM-DATUM-IN SECTION.                                          
096200                                                                          
096300     MOVE MID-TIREGDAT-IN1 TO W-DAREGDAT-KONV                             
096400     IF W-DAREGDAT-KONV < 500000                                          
096500       MOVE 20             TO W-DAREGDAT-AAR                              
096600     ELSE                                                                 
096700       MOVE 19             TO W-DAREGDAT-AAR                              
096800     END-IF                                                               
096900     MOVE W-DAREGDAT-KONV  TO W-DAREGDAT-TI                               
097000     MOVE W-DAREGDAT       TO WS-DAREGDAT                                 
097100                              W-DAREGDAT-FROM                             
097200     COMPUTE W-TIREGDAT-IN1 = 99999999 - WS-DAREGDAT                      
097300                                                                          
097400     MOVE MID-TIREGDAT-IN2 TO W-DAREGDAT-KONV                             
097500     IF W-DAREGDAT-KONV < 500000                                          
097600       MOVE 20             TO W-DAREGDAT-AAR                              
097700     ELSE                                                                 
097800       MOVE 19             TO W-DAREGDAT-AAR                              
097900     END-IF                                                               
098000     MOVE W-DAREGDAT-KONV  TO W-DAREGDAT-TI                               
098100     MOVE W-DAREGDAT       TO WS-DAREGDAT                                 
098200                              W-DAREGDAT-TOM                              
098300     COMPUTE W-TIREGDAT-IN2 = 99999999 - WS-DAREGDAT                      
098400     MOVE ZERO             TO W-DAREGDAT                                  
098500                              WS-DAREGDAT                                 
098600     .                                                                    
098700     EJECT                                                                
098800 S02-RAEKNA-OM-DATUM-UT SECTION.                                          
098900                                                                          
099000     MOVE LOGG-DAREGDAT-9KOMPL TO WS-DAREGDAT                             
099100     COMPUTE WS-DAREGDAT = WS-DAREGDAT - 99999999                         
099200     MOVE WS-DAREGDAT          TO W-DAREGDAT                              
099300     MOVE W-DAREGDAT-TI        TO W-DAREGDAT-KONV                         
099400     .                                                                    
099500     EJECT                                                                
099600 S03-RAEKNA-ACKUMULERAT SECTION.                                          
099700                                                                          
099800     ADD 1                               TO W-KVTOTAL                     
099900     IF LOGG-IDTECKEN-KVLS NOT = SPACE                                    
100000       ADD 1                             TO W-KVLSTOTAL                   
100100       IF LOGG-IDTECKEN-KVLS  = '+' AND                                   
100200          LOGG-KVART-SALDO > 0                                            
100300         ADD LOGG-KVART-SALDO            TO W-KVCHUP                      
100400       ELSE                                                               
100500         IF LOGG-IDTECKEN-KVLS  = '-' AND                                 
100600            LOGG-KVART-SALDO > 0                                          
100700           ADD LOGG-KVART-SALDO          TO W-KVCHDO                      
100800         ELSE                                                             
100900           IF LOGG-IDTECKEN-KVLS = '+' AND                                
101000              LOGG-KVART-SALDO < 0                                        
101100             SUBTRACT LOGG-KVART-SALDO   FROM W-KVCHDO                    
101200           ELSE                                                           
101300             IF LOGG-IDTECKEN-KVLS = '-' AND                              
101400                LOGG-KVART-SALDO < 0                                      
101500               SUBTRACT LOGG-KVART-SALDO FROM W-KVCHUP                    
101600             END-IF                                                       
101700           END-IF                                                         
101800         END-IF                                                           
101900       END-IF                                                             
102000     END-IF                                                               
102100     .                                                                    
102200     EJECT                                                                
102300 S04-FLYTTA-TILL-MOD SECTION.                                             
102400                                                                          
102500     IF MID-IDARTNR-IN NOT = ALL '+'                                      
102600       MOVE MID-IDARTNR-IN   TO MOD-IDARTNR-UT                            
102700       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
102800     END-IF                                                               
102900                                                                          
103000     IF MID-IDDC-IN NOT = ALL '+'                                         
103100       MOVE MID-IDDC-IN      TO MOD-IDDC-UT                               
103200     END-IF                                                               
103300                                                                          
103400     IF MID-IDHUVTYP-IN NOT = ALL '+'                                     
103500       MOVE MID-IDHUVTYP-IN  TO MOD-IDHUVTYP-UT                           
103600     END-IF                                                               
103700                                                                          
103800     IF MID-IDSUBTYP-IN NOT = ALL '+'                                     
103900       MOVE MID-IDSUBTYP-IN  TO MOD-IDSUBTYP-UT                           
104000     END-IF                                                               
104100                                                                          
104200     IF MID-TIREGDAT-IN1 NOT = ALL '+'                                    
104300       MOVE MID-TIREGDAT-IN1 TO MOD-TIREGDAT-UT1                          
104400     END-IF                                                               
104500                                                                          
104600     IF MID-TIREGDAT-IN2 NOT = ALL '+'                                    
104700       MOVE MID-TIREGDAT-IN2 TO MOD-TIREGDAT-UT2                          
104800     END-IF                                                               
104900                                                                          
105000     IF MID-IDTRANS-IN NOT = ALL '+'                                      
105100       MOVE MID-IDTRANS-IN TO MOD-IDTRANS-UT                              
105200     END-IF                                                               
105300     .                                                                    
105400     EJECT                                                                
105500 MFS-RENSA-FAELT-UT SECTION.                                              
105600                                                                          
105700     MOVE +1 TO INDX                                                      
105800     PERFORM UNTIL INDX > MAX-INDX                                        
105900       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
106000       ADD +1 TO INDX                                                     
106100     END-PERFORM                                                          
106200     .                                                                    
106300     EJECT                                                                
106400 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
106500                                                                          
106600*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
106700     MOVE MFS-RENSA-FAELT TO MOD-SELECT-RAD         (INDX)                
106800                             MOD-IDDC               (INDX)                
106900                             MOD-IDHUVTYP           (INDX)                
107000                             MOD-IDSUBTYP           (INDX)                
107100                             MOD-TIREGDAT           (INDX)                
107200                             MOD-KVART-SALDO        (INDX)                
107300                             MOD-IDTECKEN-KVAKS-PAV (INDX)                
107400                             MOD-KVAKS-PAV          (INDX)                
107500                             MOD-IDTECKEN-KVAKS     (INDX)                
107600                             MOD-KVAKS              (INDX)                
107700                             MOD-IDTECKEN-KVEFRS    (INDX)                
107800                             MOD-KVEFRS             (INDX)                
107900                             MOD-IDTECKEN-KVLS      (INDX)                
108000                             MOD-KVLS               (INDX)                
108100     .                                                                    
108200     EJECT                                                                
108300 MFS-RENSA-FAELT-IN SECTION.                                              
108400                                                                          
108500*    --- ALLA INDATA-FÄLT                                                 
108600     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
108700                             MOD-IDDC-IN                                  
108800                             MOD-IDHUVTYP-IN                              
108900                             MOD-IDSUBTYP-IN                              
109000                             MOD-TIREGDAT-IN1                             
109100                             MOD-TIREGDAT-IN2                             
109200                             MOD-IDTRANS-IN                               
109300     .                                                                    
109400     EJECT                                                                
109500 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
109600                                                                          
109700*    --- ALLA UTDATA-FÄLT                                                 
109800*    --- OCH RAD-DATA                                                     
109900     MOVE MFS-ROER-EJ-FAELT TO MOD-KVTOTAL                                
110000                               MOD-KVCHUP                                 
110100                               MOD-KVCHDO                                 
110200                               MOD-KVLSTOTAL                              
110300                               MOD-FLEXTRAKT                              
110400                               MOD-TEMFSINF                               
110500     MOVE +1 TO INDX                                                      
110600     PERFORM UNTIL INDX > MAX-INDX                                        
110700       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
110800       ADD +1 TO INDX                                                     
110900     END-PERFORM                                                          
111000     .                                                                    
111100     EJECT                                                                
111200 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
111300                                                                          
111400*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
111500     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC               (INDX)              
111600                               MOD-IDHUVTYP           (INDX)              
111700                               MOD-IDSUBTYP           (INDX)              
111800                               MOD-TIREGDAT           (INDX)              
111900                               MOD-KVART-SALDO        (INDX)              
112000                               MOD-IDTECKEN-KVAKS-PAV (INDX)              
112100                               MOD-KVAKS-PAV          (INDX)              
112200                               MOD-IDTECKEN-KVAKS     (INDX)              
112300                               MOD-KVAKS              (INDX)              
112400                               MOD-IDTECKEN-KVEFRS    (INDX)              
112500                               MOD-KVEFRS             (INDX)              
112600                               MOD-IDTECKEN-KVLS      (INDX)              
112700                               MOD-KVLS               (INDX)              
112800     .                                                                    
112900     EJECT                                                                
113000                                                                          
113100* --- IMS SEKTIONER ---                                                   
113200                                                                          
113300 IMS-GET-MSG SECTION.                                                     
113400                                                                          
113500     MOVE '  QC' TO GODK-STATUSKODER                                      
113600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
113700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
113800     PERFORM IMS-STATUSKONTROLL                                           
113900     .                                                                    
114000                                                                          
114100 IMS-INSERT-MSG SECTION.                                                  
114200                                                                          
114300     IF ENGLISH-TEXT                                                      
114400       MOVE 'N' TO MFS-KDHUVOMR                                           
114500     END-IF                                                               
114600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
114700     MOVE SPACE TO GODK-STATUSKODER                                       
114800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
114900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
115000     PERFORM IMS-STATUSKONTROLL                                           
115100     .                                                                    
115200     EJECT                                                                
115300 IMS-INSERT-ALT1-MSG SECTION.                                             
115400                                                                          
115500     MOVE '  ' TO GODK-STATUSKODER                                        
115600     CALL CBLTDLI USING ISRT ALT1-PCB W-PROG-TO-PROG-SW1                  
115700     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
115800     PERFORM IMS-STATUSKONTROLL                                           
115900     .                                                                    
116000     EJECT                                                                
116100 IMS-INSERT-ALT2-MSG SECTION.                                             
116200                                                                          
116300     MOVE SPACE TO GODK-STATUSKODER                                       
116400     CALL CBLTDLI USING ISRT ALT2-PCB W-PROG-TO-PROG-SW2                  
116500     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
116600     PERFORM IMS-STATUSKONTROLL                                           
116700     .                                                                    
116800     EJECT                                                                
116900 IMS-GN-LOGA SECTION.                                                     
117000                                                                          
117100     STRING 'WLLOGA01(WDL901KY>=' W-WDL901KY-MIN-X                        
117200                    '&WDL901KY<=' W-WDL901KY-MAX-X                        
117300                    '&IDDC    >=' W-IDDC-MIN                              
117400                    '&IDDC    <=' W-IDDC-MAX                              
117500                    '&IDHUVTYP>=' W-IDHUVTYP-MIN                          
117600                    '&IDHUVTYP<=' W-IDHUVTYP-MAX                          
117700                    '&IDSUBTYP>=' W-IDSUBTYP-MIN                          
117800                    '&IDSUBTYP<=' W-IDSUBTYP-MAX                          
117900                    '&IDTRANS >=' W-IDTRANS-MIN                           
118000                    '&IDTRANS <=' W-IDTRANS-MAX ')'                       
118100          DELIMITED BY SIZE INTO SSA1                                     
118200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
118300     CALL CBLTDLI USING GN LOGA-PCB DLI-IO-WLLOGA01 SSA1                  
118400     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
118500     PERFORM IMS-STATUSKONTROLL                                           
118600     .                                                                    
118700     EJECT                                                                
118800 IMS-GN-LOGA2 SECTION.                                                    
118900                                                                          
119000     STRING 'WLLOGA01(WDL901KY>=' W-WDL901KY-MIN-X                        
119100                    '&WDL901KY<=' W-WDL901KY-MAX-X                        
119200                    '&IDDC    >=' W-IDDC-MIN                              
119300                    '&IDDC    <=' W-IDDC-MAX                              
119400                    '&IDHUVTYP>=' W-IDHUVTYP-MIN                          
119500                    '&IDHUVTYP<=' W-IDHUVTYP-MAX                          
119600                    '&IDSUBTYP>=' W-IDSUBTYP-MIN                          
119700                    '&IDSUBTYP<=' W-IDSUBTYP-MAX                          
119800                    '&IDTRANS >=' W-IDTRANS-MIN                           
119900                    '&IDTRANS <=' W-IDTRANS-MAX                           
120000                    '+WDL901KY>=' W-WDL901KY-MIN-X                        
120100                    '&WDL901KY<=' W-WDL901KY-MAX-X                        
120200                    '&IDDC    >=' W-IDDC-MIN2                             
120300                    '&IDDC    <=' W-IDDC-MAX2                             
120400                    '&IDHUVTYP>=' W-IDHUVTYP-MIN                          
120500                    '&IDHUVTYP<=' W-IDHUVTYP-MAX                          
120600                    '&IDSUBTYP>=' W-IDSUBTYP-MIN                          
120700                    '&IDSUBTYP<=' W-IDSUBTYP-MAX                          
120800                    '&IDTRANS >=' W-IDTRANS-MIN                           
120900                    '&IDTRANS <=' W-IDTRANS-MAX ')'                       
121000          DELIMITED BY SIZE INTO SSA1                                     
121100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
121200     CALL CBLTDLI USING GN LOGA-PCB DLI-IO-WLLOGA01 SSA1                  
121300     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
121400     PERFORM IMS-STATUSKONTROLL                                           
121500     .                                                                    
121600     EJECT                                                                
121700 IMS-GU-LOGA SECTION.                                                     
121800                                                                          
121900     STRING 'WLLOGA01(WDL901KY =' W-WDL901KY-X ')'                        
122000          DELIMITED BY SIZE INTO SSA1                                     
122100     MOVE '  GE' TO GODK-STATUSKODER                                      
122200     CALL CBLTDLI USING GU LOGA-PCB DLI-IO-WLLOGA01 SSA1                  
122300     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
122400     PERFORM IMS-STATUSKONTROLL                                           
122500     .                                                                    
122600     EJECT                                                                
122700 IMS-GU-WDB601-A  SECTION.                                                
122800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
122900          DELIMITED BY SIZE INTO SSA1                                     
123000     MOVE '  GE' TO GODK-STATUSKODER                                      
123100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-A SSA1               
123200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
123300     PERFORM IMS-STATUSKONTROLL                                           
123400     .                                                                    
123500     EJECT                                                                
123600 IMS-GU-WDB601-B  SECTION.                                                
123700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
123800          DELIMITED BY SIZE INTO SSA1                                     
123900     MOVE '  GE' TO GODK-STATUSKODER                                      
124000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-B SSA1               
124100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
124200     PERFORM IMS-STATUSKONTROLL                                           
124300     .                                                                    
124400     EJECT                                                                
124500 IMS-STATUSKONTROLL SECTION.                                              
124600                                                                          
124700     SET STATUS-IX TO 1                                                   
124800     SEARCH GODK-STATUS                                                   
124900       AT END                                                             
125000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
125100         DELIMITED BY SIZE INTO FELTEXT                                   
125200         CALL FELLOG                                                      
125300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
125400         CONTINUE                                                         
125500     END-SEARCH                                                           
125600     .                                                                    
