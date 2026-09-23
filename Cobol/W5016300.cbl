000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5016300.                                                
000300 AUTHOR.         RANDI BERG.                                              
000400 DATE-WRITTEN.   98/02/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        SÖKFUNKTION FÖR WDL9, SALDODATABASEN                             
000900*                                                                         
001000*        PROGRAMMET LÄSER      WLLOGA (WDL9)                              
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W5T163                                              
001400*        MID:         W5I16301                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W5O16301                                            
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400*    -- CHECKED BY WY2000                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W5016300'.            
002600                                                                          
002700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002900                                                                          
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003200                                                                          
003300*01  -COPY WWDCKONS                                                       
003400                                                                          
003500 77  WS-TEST-IDARTNR             PIC X(9).                                
003600 77  WS-IDARTNR                  PIC X(9).                                
003700                                                                          
003800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004000 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
004100                                                                          
004200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
004300     88  ALLT-OK                             VALUE 'J'.                   
004400     88  ALLT-NOT-OK                         VALUE 'N'.                   
004500                                                                          
004600 77  BYT-SW                      PIC X       VALUE 'N'.                   
004700     88  BYT-BILD                            VALUE 'J'.                   
004800     88  BYT-EJ-BILD                         VALUE 'N'.                   
004900                                                                          
005000 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005100     88  NYCKLAR-OK                          VALUE 'J'.                   
005200     88  NYCKLAR-FEL                         VALUE 'N'.                   
005300                                                                          
005400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005500     88  INDATA-OK                           VALUE 'J'.                   
005600     88  INDATA-FEL                          VALUE 'N'.                   
005700                                                                          
005800 77  EXTRACT-SW                  PIC X       VALUE 'J'.                   
005900     88  EXTRACT-OK                          VALUE 'J'.                   
006000     88  EXTRACT-FEL                         VALUE 'N'.                   
006100                                                                          
006200 77    WDB6-A-SW                 PIC X       VALUE 'J'.                   
006300       88  WDB6-A-FINNS                      VALUE 'J'.                   
006400       88  WDB6-A-SAKNAS                     VALUE 'N'.                   
006500                                                                          
006600 77    WDB6-B-SW                 PIC X       VALUE 'J'.                   
006700       88  WDB6-B-FINNS                      VALUE 'J'.                   
006800       88  WDB6-B-SAKNAS                     VALUE 'N'.                   
006900                                                                          
007000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007100     88  EGEN-MID                            VALUE '5163'.                
007200     88  GODK-MID                            VALUE '5162' '5163'          
007300                                                   '5164'.                
007400     88  DETALJ-MID                          VALUE '5164'.                
007500     88  HELP-MID                            VALUE '0551'.                
007600     EJECT                                                                
007700*    --- ARBETSFÄLT                                                       
007800 01  W-AREA-DATUM.                                                        
007900     03  W-DAREGDAT.                                                      
008000         05  W-DAREGDAT-AAR      PIC 9(2).                                
008100         05  W-DAREGDAT-TI       PIC 9(6).                                
008200     03  WS-DAREGDAT             PIC 9(8).                                
008300     03  W-DAREGDAT-KONV         PIC 9(6).                                
008400     03  W-DAREGDAT-FROM         PIC 9(8).                                
008500     03  W-DAREGDAT-TOM          PIC 9(8).                                
008600     03  W-DIFF                  PIC 9(8).                                
008700     03  W-TIREGDAT-IN1          PIC 9(8).                                
008800     03  W-TIREGDAT-IN2          PIC 9(8).                                
008900     03  DAGENS-DATUM            PIC 9(8).                                
009000     03  W-FORSTA-DATUM.                                                  
009100         05 W-FORSTA-DATUM-AA    PIC 9(2).                                
009200         05 W-FORSTA-DATUM-MMDD  PIC 9(4).                                
009300     03  W-SISTA-DATUM          PIC 9(6).                                 
009400     03  DATUM-SIFFRA            PIC S9(9).                               
009500 01  W-AREA-NYCKLAR.                                                      
009600     03  W-IDARTNR               PIC S9(9).                               
009700     03  W-IDDC                  PIC X(2).                                
009800     03  W-TIKLOCK               PIC S9(9)             COMP-3.            
009900     03  W-IDSEKVNR              PIC S9(3)             COMP-3.            
010000 01  W-AREA-INDEX.                                                        
010100     03  W-INDX                  PIC S9(4)  VALUE +0   COMP SYNC.         
010200 01  W-AREA-ACKUMULERAD.                                                  
010300     03  W-KVCHUP                PIC S9(5)9  VALUE ZERO.                  
010400     03  W-KVCHDO                PIC S9(5)9  VALUE ZERO.                  
010500     03  W-KVTOTAL               PIC 9(7)   VALUE ZERO.                   
010600     EJECT                                                                
010700                                                                          
010800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010900 01  GENERELLA-SUBPROGRAM.                                                
011000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011500                                                                          
011600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011700*01 -COPY WMEDAREA                                                        
011800                                                                          
011900 01  MESSAGE-CODES.                                                       
012000     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
012100     03  ERR-RECORD-MISSING      PIC X(3)    VALUE '029'.                 
012200     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
012300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012400     03  WRONG-INPUT-FIELD       PIC X(3)    VALUE '194'.                 
012500     EJECT                                                                
012600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
012700*                                                                         
012800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012900                                                                          
013000*01 -COPY WMSGINIT                                                        
013100     EJECT                                                                
013200*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
013300                                                                          
013400 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
013500                                                                          
013600*01  -COPY WDATAREA.                                                      
013700                                                                          
013800*    --- BMP PARAMETRAR                                                   
013900 01  FILLER                      PIC X(16)   VALUE 'BMP-PRM '.            
014000 01  BMP-PARAMETRAR.                                                      
014100     03  SKICKA-IDARTNR      PIC 9(9).                                    
014200     03  SKICKA-DAREGDAT-MAX PIC 9(8).                                    
014300     03  SKICKA-DAREGDAT-MIN PIC 9(8).                                    
014400     03  SKICKA-IDDC         PIC X(2).                                    
014500     03  SKICKA-IDHUVTYP     PIC X(4).                                    
014600     03  SKICKA-IDSUBTYP     PIC X(3).                                    
014700     03  SKICKA-IDTRANS      PIC X(4).                                    
014800     03  SKICKA-IDUSER       PIC X(7).                                    
014900     03  SKICKA-IDLAND       PIC X(3).                                    
015000     EJECT                                                                
015100                                                                          
015200*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
015300*                                                                         
015400 01  SPAR-AREA.                                                           
015500     03  SPAR-WDL901KY.                                                   
015600         05  SPAR-IDARTNR         PIC  S9(9) COMP-3.                      
015700         05  SPAR-DAREGDAT        PIC  9(8).                              
015800         05  SPAR-TIKLOCK         PIC  S9(9) COMP-3.                      
015900         05  SPAR-IDSEKVNR        PIC  S9(3) COMP-3.                      
016000     03  SPAR-FILLER              PIC  X(34).                             
016100     03  SPAR-TABELL.                                                     
016200       05  SPAR-WDL901   OCCURS 11.                                       
016300         07  SPAR-WDL901KY-TAB.                                           
016400           09  SPAR-IDARTNR-TAB     PIC  9(9).                            
016500           09  SPAR-DAREGDAT-TAB    PIC  9(8).                            
016600           09  SPAR-TIKLOCK-TAB     PIC  9(9).                            
016700           09  SPAR-IDSEKVNR-TAB    PIC  9(3).                            
016800     03  SPAR-IDTRANS             PIC X(4)    VALUE '5163'.               
016900     03  SPAR-IDARTNR-ENTER       PIC S9(9)        COMP-3.                
017000     03  SPAR-IDARTNR-NEXT        PIC S9(9)        COMP-3.                
017100     03  SPAR-DAREGDAT-ENTER      PIC  9(8).                              
017200     03  SPAR-DAREGDAT-NEXT       PIC  9(8).                              
017300     03  SPAR-TIKLOCK-ENTER       PIC S9(9)        COMP-3.                
017400     03  SPAR-TIKLOCK-NEXT        PIC S9(9)        COMP-3.                
017500     03  SPAR-IDSEKVNR-ENTER      PIC S9(3)        COMP-3.                
017600     03  SPAR-IDSEKVNR-NEXT       PIC S9(3)        COMP-3.                
017700     03  SPAR-KVTOTAL             PIC 9(7).                               
017800     03  SPAR-KVCHUP              PIC S9(5)9.                             
017900     03  SPAR-KVCHDO              PIC S9(5)9.                             
018000     03  SPAR-KVLSTOTAL           PIC 9(7).                               
018100     03  SPAR-BILD                PIC X(4).                               
018200                                                                          
018300     EJECT                                                                
018400*                                                                         
018500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
018600                                                                          
018700*01  MID -COPY W5I16301                                                   
018800     EJECT                                                                
018900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
019000                                                                          
019100*01  -COPY WMSGAREA                                                       
019200     EJECT                                                                
019300     03  MOD REDEFINES MSG-AREA.                                          
019400*      05  -COPY W5O16301                                                 
019500     EJECT                                                                
019600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
019700                                                                          
019800*01  -COPY WMFSAREA                                                       
019900     EJECT                                                                
020000                                                                          
020100 01  W-PROG-TO-PROG-SW1.                                                  
020200*    03    -COPY  WMSGSOP                                                 
020300                                                                          
020400 01  W-PROG-TO-PROG-SW2.                                                  
020500     03  M-SW-LL                 PIC S9(4)   VALUE +240 COMP SYNC.        
020600     03  M-SW-Z1-Z2              PIC X(2)    VALUE LOW-VALUE.             
020700     03  M-SW-KDTRANS            PIC X(8)    VALUE 'W5T164  '.            
020800     03  M-SW-IDTRANS            PIC X(4)    VALUE '5163'.                
020900     03  M-SW-KDMFSTYP           PIC X(1)    VALUE '2'.                   
021000                                                                          
021100*    03  MID -COPY W5I16401 -PRE 5164-                                    
021200                                                                          
021300                                                                          
021400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021500                                                                          
021600     EJECT                                                                
021700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021800                                                                          
021900 01  NYCKLAR-TILL-DLI.                                                    
022000     03  W-WDL901KY-X.                                                    
022100         05  W-IDARTNR-X    PIC S9(9) COMP-3.                             
022200         05  W-DAREGDAT-X   PIC 9(8).                                     
022300         05  W-TIKLOCK-X    PIC S9(9) COMP-3.                             
022400         05  W-IDSEKVNR-X   PIC S9(3) COMP-3.                             
022500     03  W-WDL901KY-MIN-X.                                                
022600         05  W-IDARTNR-MIN  PIC S9(9) COMP-3  VALUE ZERO.                 
022700         05  W-DAREGDAT-MIN PIC 9(8)          VALUE ZERO.                 
022800         05  W-TIKLOCK-MIN  PIC S9(9) COMP-3  VALUE ZERO.                 
022900         05  W-IDSEKVNR-MIN PIC S9(3) COMP-3  VALUE ZERO.                 
023000     03  W-WDL901KY-MAX-X.                                                
023100         05  W-IDARTNR-MAX  PIC S9(9) COMP-3  VALUE 999999999.            
023200         05  W-DAREGDAT-MAX PIC 9(8)          VALUE 99999999.             
023300         05  W-TIKLOCK-MAX  PIC S9(9) COMP-3  VALUE 999999999.            
023400         05  W-IDSEKVNR-MAX PIC S9(3) COMP-3  VALUE 999.                  
023500     03  W-WDL901-OVRIGA-MIN-X.                                           
023600         05  W-IDDC-MIN     PIC X(2)          VALUE LOW-VALUE.            
023700         05  W-IDDC-MIN2    PIC X(2)          VALUE LOW-VALUE.            
023800         05  W-IDHUVTYP-MIN PIC X(4)          VALUE LOW-VALUE.            
023900         05  W-IDSUBTYP-MIN PIC X(3)          VALUE LOW-VALUE.            
024000     03  W-WDL901-OVRIGA-MAX-X.                                           
024100         05  W-IDDC-MAX     PIC X(2)          VALUE HIGH-VALUE.           
024200         05  W-IDDC-MAX2    PIC X(2)          VALUE HIGH-VALUE.           
024300         05  W-IDHUVTYP-MAX PIC X(4)          VALUE HIGH-VALUE.           
024400         05  W-IDSUBTYP-MAX PIC X(3)          VALUE HIGH-VALUE.           
024500                                                                          
024600     03  W-IDDC-B6-X.                                                     
024700         05  W-IDDC-B6      PIC X(2)          VALUE SPACE.                
024800*                                                                         
024900*    --- STATUS-KOD FRÅN IMS                                              
025000 01  STATUS-WS                   PIC XX.                                  
025100     88  SEGMENT-FINNS                       VALUE '  '.                  
025200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
025300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025400     88  BASEN-SLUT                          VALUE 'GB'.                  
025500                                                                          
025600 01  GODK-STATUSKODER.                                                    
025700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025800                                                                          
025900 01  SSA1                        PIC X(512).                              
026000 01  SSA2                        PIC X(64).                               
026100     EJECT                                                                
026200*    --- IMS FUNKTIONSKODER                                               
026300*01  -COPY W0003                                                          
026400     EJECT                                                                
026500*    ---  DLI INPUT-OUTPUT AREA                                           
026600                                                                          
026700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLOGA01'.                    
026800 01  DLI-IO-WLLOGA01.                                                     
026900*    03  -COPY WDL901                                                     
027000     EJECT                                                                
027100 01  FILLER         PIC X(16)   VALUE 'WDB601-A AREA'.                    
027200 01   DLI-IO-AREA-B601-A.                                                 
027300*     03  -COPY WDB601 -PRE A-                                            
027400     EJECT                                                                
027500 01  FILLER         PIC X(16)   VALUE 'WDB601-B AREA'.                    
027600 01   DLI-IO-AREA-B601-B.                                                 
027700*     03  -COPY WDB601 -PRE B-                                            
027800     EJECT                                                                
027900 LINKAGE SECTION.                                                         
028000*01  -COPY W0009   -PRE MSG-                                              
028100                                                                          
028200*01  -COPY W0009   -PRE ALT1-                                             
028300                                                                          
028400*01  -COPY W0009   -PRE ALT2-                                             
028500                                                                          
028600*01  -COPY W0008   -PRE USEA-                                             
028700     05  FILLER                  PIC X.                                   
028800     EJECT                                                                
028900*01  -COPY W0008  -PRE LOGA-                                              
029000     05  FILLER                  PIC X.                                   
029100     EJECT                                                                
029200*01  -COPY W0008  -PRE WDB6-                                              
029300     05  FILLER                  PIC X.                                   
029400     EJECT                                                                
029500 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB ALT2-PCB USEA-PCB             
029600                           LOGA-PCB WDB6-PCB.                             
029700 MAIN SECTION.                                                            
029800     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB ALT2-PCB USEA-PCB             
029900                           LOGA-PCB WDB6-PCB.                             
030000                                                                          
030100     PERFORM IMS-GET-MSG                                                  
030200     IF SEGMENT-FINNS                                                     
030300       PERFORM A-INIT                                                     
030400       PERFORM B-KOLLA-NYCKLAR                                            
030500       IF NYCKLAR-OK                                                      
030600         IF MFS-UPDATE                                                    
030700           PERFORM I-KOLLA-EXTRACT                                        
030800           IF EXTRACT-OK                                                  
030900             PERFORM J-STARTA-BMP                                         
031000             MOVE 'BMP STARTED' TO MOD-TEMFSINF                           
031100           ELSE                                                           
031200             MOVE 'BMP NOT STARTED' TO MOD-TEMFSINF                       
031300           END-IF                                                         
031400         ELSE                                                             
031500           IF MFS-FIRST AND NOT DETALJ-MID                                
031600             PERFORM C-FOERSTA-SIDA                                       
031700             PERFORM S04-FLYTTA-TILL-MOD                                  
031710**** LOGA IS ONE INTERVALL LOGA2 IS TWO INTERVALLS                        
031720             IF A-DCS-NDC-CN OR A-DCS-NDC-OTHERS                          
031730             OR (A-DCS-NDC-PF AND NOT A-DCS-FTG-PV)                       
031900               PERFORM IMS-GN-LOGA                                        
032000             ELSE                                                         
032100               PERFORM IMS-GN-LOGA2                                       
032200             END-IF                                                       
032300           ELSE                                                           
032400             IF MFS-NEXT                                                  
032500               PERFORM D-NAESTA-SIDA                                      
032600               PERFORM IMS-GU-LOGA                                        
032700             ELSE                                                         
032800               PERFORM E-SAMMA-SIDA                                       
032900               IF NOT BYT-BILD                                            
033000                 PERFORM IMS-GU-LOGA                                      
033100               ELSE                                                       
033200                 PERFORM H-BYT-BILD                                       
033300               END-IF                                                     
033400             END-IF                                                       
033500           END-IF                                                         
033600           IF ALLT-OK                                                     
033700             PERFORM F-LAES-VISA-INFO                                     
033800           END-IF                                                         
033900         END-IF                                                           
034000       END-IF                                                             
034100       IF BYT-EJ-BILD                                                     
034200*    --- OM ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
034300         COMPUTE MSG-KVLL = LENGTH OF MOD-W5O16301 + 4                    
034400         PERFORM IMS-INSERT-MSG                                           
034500       END-IF                                                             
034600     END-IF                                                               
034700                                                                          
034800     MOVE ZERO TO RETURN-CODE                                             
034900     GOBACK                                                               
035000     .                                                                    
035100     EJECT                                                                
035200 A-INIT SECTION.                                                          
035300                                                                          
035400     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
035500     MOVE DAGENS-DATUM               TO W-SISTA-DATUM                     
035600                                                                          
035700     MOVE DAGENS-DATUM (3:6)         TO W-FORSTA-DATUM                    
035800     IF W-FORSTA-DATUM-AA = 98                                            
035900       MOVE 0601                     TO W-FORSTA-DATUM-MMDD               
036000     ELSE                                                                 
036100       MOVE 0101                     TO W-FORSTA-DATUM-MMDD               
036200     END-IF                                                               
036300                                                                          
036400     IF MSG-DUBBLA-TRANSKODER                                             
036500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I16301                 
036600       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
036700       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
036800     ELSE                                                                 
036900       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W5I16301                 
037000       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
037100       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
037200     END-IF                                                               
037300                                                                          
037400     MOVE MSG-KDTRTYP     TO MFS-KDTRTYP                                  
037500     MOVE MSG-IDPFK       TO MFS-IDPFK                                    
037600     MOVE MFS-IDTRANS     TO W-IDTRANS                                    
037700                                                                          
037800     MOVE LOW-VALUE       TO MSG-AREA                                     
037900     MOVE 'W5O163N1'      TO MFS-IDMOD                                    
038000     MOVE '5163'          TO MOD-IDTRANS                                  
038100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
038200                                                                          
038300     IF EGEN-MID OR HELP-MID                                              
038400       CONTINUE                                                           
038500     ELSE                                                                 
038600       MOVE SPACE         TO MFS-KDTRTYP                                  
038700       MOVE '7'           TO MFS-IDPFK                                    
038800     END-IF                                                               
038900     MOVE 'GB'            TO MED-IDSKYLT                                  
039000     .                                                                    
039100     EJECT                                                                
039200 B-KOLLA-NYCKLAR SECTION.                                                 
039300                                                                          
039400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
039500     MOVE '001'             TO MSGI-KDCALL                                
039600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
039700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
039800     MOVE '5163'            TO MSGI-IDTRANS                               
039900                                                                          
040000     IF EGEN-MID AND MID-IDARTNR-IN NOT = ALL '+'                         
040100       MOVE MID-IDARTNR-IN  TO MSGI-IDARTNR                               
040200     END-IF                                                               
040300                                                                          
040400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
040500                                                                          
040600     IF GODK-MID                                                          
040700       MOVE MSGI-SPAR-AREA  TO SPAR-AREA                                  
040800       MOVE SPACE           TO SPAR-BILD                                  
040900     END-IF                                                               
041000                                                                          
041100     MOVE JA                TO NYCKLAR-SW                                 
041200                                                                          
041300*    -- KONTROLL AV SELECT-RAD                                            
041400     MOVE +1                         TO INDX                              
041500     PERFORM UNTIL INDX > MAX-INDX                                        
041600       IF MID-SELECT-RAD(INDX) NOT = '+' AND ' '                          
041700         IF MID-SELECT-RAD(INDX) = 'S'                                    
041800           MOVE INDX                 TO W-INDX                            
041900           MOVE 12                   TO INDX                              
042000           MOVE JA                   TO BYT-SW                            
042100         ELSE                                                             
042200           IF NOT DETALJ-MID                                              
042300             MOVE NEJ                TO INDATA-SW                         
042400                                        NYCKLAR-SW                        
042500             MOVE MID-SELECT-RAD(INDX)                                    
042600                                     TO MOD-SELECT-RAD(INDX)              
042700             MOVE MFS-ALFA-FAELT-FEL TO MOD-SELECT-RAD-ATTR(INDX)         
042800           END-IF                                                         
042900         END-IF                                                           
043000       END-IF                                                             
043100       ADD 1                         TO INDX                              
043200     END-PERFORM                                                          
043300                                                                          
043400     PERFORM BA-PREPARERA-FAELT                                           
043500     PERFORM BB-KOLLA-INDATA                                              
043600     PERFORM S04-FLYTTA-TILL-MOD                                          
043700                                                                          
043800     IF INDATA-FEL                                                        
043900       MOVE WRONG-INPUT-FIELD TO MED-IDMFSFEL                             
044000       CALL WMEDKONV USING MED-WMEDAREA                                   
044100       MOVE MED-MFSFEL        TO MOD-TEMFSFEL                             
044200       PERFORM MFS-ROER-EJ-FAELT-UT                                       
044300     ELSE                                                                 
044400       IF NYCKLAR-FEL                                                     
044500         MOVE ERR-WRONG-KEY   TO MED-IDMFSFEL                             
044600         CALL WMEDKONV USING MED-WMEDAREA                                 
044700         MOVE MED-MFSFEL      TO MOD-TEMFSFEL                             
044800         PERFORM MFS-RENSA-FAELT-IN                                       
044900         PERFORM MFS-RENSA-FAELT-UT                                       
045000       END-IF                                                             
045100     END-IF                                                               
045200     .                                                                    
045300     EJECT                                                                
045400 BA-PREPARERA-FAELT SECTION.                                              
045500                                                                          
045600     IF NOT EGEN-MID AND                                                  
045700        NOT DETALJ-MID                                                    
045800       MOVE MSGI-IDARTNR     TO MID-IDARTNR-IN                            
045900     ELSE                                                                 
046000       IF MID-IDARTNR-IN = ALL '+' AND                                    
046100         BYT-SW NOT = JA           AND                                    
046200         NOT MFS-NEXT              AND                                    
046300         NOT MFS-FIRST                                                    
046400         MOVE MID-IDARTNR-UT TO MID-IDARTNR-IN                            
046500       END-IF                                                             
046600     END-IF                                                               
046700                                                                          
046800     IF NOT GODK-MID                                                      
046900       MOVE SPACE            TO MID-IDDC-IN                               
047000                                MID-IDHUVTYP-IN                           
047100                                MID-IDSUBTYP-IN                           
047200                                MID-IDSALDO-IN                            
047300     END-IF                                                               
047400     .                                                                    
047500     EJECT                                                                
047600 BB-KOLLA-INDATA SECTION.                                                 
047700                                                                          
047800*    -- KONTROLL AV IDARTNR                                               
047900                                                                          
048000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
048100     IF MID-IDARTNR-IN NOT = ALL '+' AND NOT MFS-UPDATE                   
048200       MOVE '7'         TO MFS-IDPFK                                      
048300       MOVE SPACE       TO MFS-KDTRTYP                                    
048400     END-IF                                                               
048500                                                                          
048600     IF MID-IDARTNR-IN = ALL '+' AND                                      
048700        MID-IDARTNR-UT NOT = 0                                            
048800       IF GODK-MID                                                        
048900         MOVE MID-IDARTNR-UT TO MID-IDARTNR-IN                            
049000       ELSE                                                               
049100         MOVE MSGI-IDARTNR     TO MID-IDARTNR-IN                          
049200       END-IF                                                             
049300     END-IF                                                               
049400                                                                          
049500     IF MID-IDARTNR-IN NOT = ALL '+'                                      
049600       INSPECT MID-IDARTNR-IN REPLACING LEADING SPACE BY ZERO             
049700       IF MID-IDARTNR-IN NUMERIC                                          
049800         MOVE MID-IDARTNR-IN     TO WS-IDARTNR                            
049900                                                                          
050000         IF WS-IDARTNR NUMERIC AND                                        
050100            WS-IDARTNR > '000000000'                                      
050200           MOVE WS-IDARTNR       TO W-IDARTNR-MAX                         
050300                                    W-IDARTNR-MIN                         
050400                                    W-IDARTNR                             
050500                                    MSGI-IDARTNR                          
050600                                    MID-IDARTNR-IN                        
050700         ELSE                                                             
050800           MOVE NEJ              TO NYCKLAR-SW                            
050900         END-IF                                                           
051000       ELSE                                                               
051100         MOVE NEJ                TO NYCKLAR-SW                            
051200       END-IF                                                             
051300     ELSE                                                                 
051400       MOVE NEJ                  TO NYCKLAR-SW                            
051500     END-IF                                                               
051600                                                                          
051700*    -- KONTROLL AV IDDC                                                  
051800                                                                          
051900     IF MID-IDDC-IN = ALL '+' AND                                         
052000        MID-IDDC-UT NOT = SPACE                                           
052100       MOVE MID-IDDC-UT         TO MID-IDDC-IN                            
052200     END-IF                                                               
052300                                                                          
052400     IF MID-IDDC-IN NOT = ALL '+' AND                                     
052500        MID-IDDC-IN NOT = ' '                                             
052600       IF MID-IDDC-IN NOT = '**'                                          
052700         MOVE MID-IDDC-IN      TO W-IDDC-B6                               
052800         MOVE JA               TO WDB6-A-SW                               
052900         PERFORM IMS-GU-WDB601-A                                          
053000         IF SEGMENT-SAKNAS                                                
053100           MOVE NEJ            TO WDB6-A-SW                               
053200         END-IF                                                           
053300                                                                          
053400         MOVE MSGI-IDDC        TO W-IDDC-B6                               
053500                                                                          
053600         MOVE JA               TO WDB6-B-SW                               
053700         PERFORM IMS-GU-WDB601-B                                          
053800         IF SEGMENT-SAKNAS                                                
053900           MOVE NEJ            TO WDB6-B-SW                               
054000         END-IF                                                           
054100                                                                          
054200         IF WDB6-A-FINNS AND NOT A-DCS-DDC AND WDB6-B-FINNS               
054210**** CDC AND SDC AND LDC BELONGING TO SEPV                                
054300           IF ((A-DCS-CDC OR A-DCS-CDC-TR OR A-DCS-SDC)                   
054400           AND (B-DCS-CDC OR B-DCS-CDC-TR OR B-DCS-SDC))                  
054410**** NON VCC AND NA                                                       
054420           OR (A-DCS-NDC AND (B-DCS-NDC OR B-DCS-CDC))                    
054800             MOVE MID-IDDC-IN  TO W-IDDC                                  
054900             MOVE W-IDDC       TO W-IDDC-MIN                              
055000                                  W-IDDC-MAX                              
055100                                  W-IDDC-MIN2                             
055200                                  W-IDDC-MAX2                             
055300           ELSE                                                           
055400             MOVE NEJ          TO NYCKLAR-SW                              
055500           END-IF                                                         
055600         ELSE                                                             
055700           MOVE NEJ            TO NYCKLAR-SW                              
055800         END-IF                                                           
055900       END-IF                                                             
056000     ELSE                                                                 
056100       MOVE MSGI-IDDC          TO W-IDDC-B6                               
056200       PERFORM IMS-GU-WDB601-A                                            
056210**** DC 41 TO 51 AND 92                                                   
056300       IF A-DCS-NDC-NA                                                    
056400         MOVE WC-NDC-US-RU     TO W-IDDC-MIN                              
056500         MOVE WC-NDC-CA        TO W-IDDC-MAX                              
056510         MOVE WC-NDC-US-BAT    TO W-IDDC-MIN2                             
056520         MOVE WC-NDC-US-BAT    TO W-IDDC-MAX2                             
056600       ELSE                                                               
056601**** DC 7A TO 74                                                          
056610         IF A-DCS-NDC-CN                                                  
056620           MOVE WC-LDC-CN-7A     TO W-IDDC-MIN                            
056630           MOVE WC-NDC-CN-74     TO W-IDDC-MAX                            
056640         ELSE                                                             
056641**** ONLY ONE DC                                                          
056651           IF A-DCS-NDC-OTHERS                                            
056652           OR (A-DCS-NDC-PF AND NOT A-DCS-FTG-PV)                         
056653             MOVE A-DCS-IDDC     TO W-IDDC-MIN                            
056654             MOVE A-DCS-IDDC     TO W-IDDC-MAX                            
056660           ELSE                                                           
056670**** CDC 1A TO 4D, 6A TO 62                                               
056680**** SHOULD ALSO BE 91                                                    
056700             MOVE WC-LDC-SE-1A   TO W-IDDC-MIN                            
056900             MOVE '40'           TO W-IDDC-MAX                            
057000             MOVE WC-NDC-JP-6A   TO W-IDDC-MIN2                           
057100             MOVE WC-NDC-AU      TO W-IDDC-MAX2                           
057200           END-IF                                                         
057201         END-IF                                                           
057210       END-IF                                                             
057300     END-IF                                                               
057400                                                                          
057500*    -- KONTROLL AV LOGGTYP                                               
057600                                                                          
057700     IF MID-IDHUVTYP-IN = ALL '+' AND                                     
057800        MID-IDHUVTYP-UT NOT = SPACE                                       
057900       MOVE MID-IDHUVTYP-UT TO MID-IDHUVTYP-IN                            
058000     END-IF                                                               
058100     IF MID-IDSUBTYP-IN = ALL '+' AND                                     
058200        MID-IDSUBTYP-UT NOT = SPACE                                       
058300       MOVE MID-IDSUBTYP-UT TO MID-IDSUBTYP-IN                            
058400     END-IF                                                               
058500                                                                          
058600     IF MID-IDHUVTYP-IN = ALL '+' AND                                     
058700        MID-IDSUBTYP-IN NOT = ALL '+'                                     
058800       MOVE NEJ TO NYCKLAR-SW                                             
058900     END-IF                                                               
059000                                                                          
059100     IF MID-IDHUVTYP-IN NOT = ALL '+' AND                                 
059200        MID-IDHUVTYP-IN NOT = ' '                                         
059300       MOVE MID-IDHUVTYP-IN TO W-IDHUVTYP-MIN                             
059400                               W-IDHUVTYP-MAX                             
059500     END-IF                                                               
059600                                                                          
059700     IF MID-IDSUBTYP-IN NOT = ALL '+' AND                                 
059800        MID-IDSUBTYP-IN NOT = ' '                                         
059900       MOVE MID-IDSUBTYP-IN TO W-IDSUBTYP-MIN                             
060000                               W-IDSUBTYP-MAX                             
060100     END-IF                                                               
060200                                                                          
060300*    -- KONTROLL AV TIREGDAT                                              
060400                                                                          
060500     IF MID-TIREGDAT-IN1 = ALL '+' AND                                    
060600        MID-TIREGDAT-UT1 NOT = 0                                          
060700       MOVE MID-TIREGDAT-UT1 TO MID-TIREGDAT-IN1                          
060800     END-IF                                                               
060900     IF MID-TIREGDAT-IN2 = ALL '+' AND                                    
061000        MID-TIREGDAT-UT2 NOT = 0                                          
061100       MOVE MID-TIREGDAT-UT2 TO MID-TIREGDAT-IN2                          
061200     END-IF                                                               
061300                                                                          
061400     IF NOT GODK-MID                                                      
061500       MOVE W-SISTA-DATUM      TO MID-TIREGDAT-IN2                        
061600                                  MOD-TIREGDAT-UT2                        
061700       MOVE W-FORSTA-DATUM     TO MID-TIREGDAT-IN1                        
061800                                  MOD-TIREGDAT-UT1                        
061900     ELSE                                                                 
062000       MOVE 'AAMMDD'           TO DAT-KDDATFORM                           
062100       IF MID-TIREGDAT-IN1 = ALL '+'                                      
062200         MOVE W-FORSTA-DATUM   TO MID-TIREGDAT-IN1                        
062300                                                                          
062400       ELSE                                                               
062500         MOVE MID-TIREGDAT-IN1 TO DAT-I-TIDATUM                           
062600         CALL WDATKONV USING DAT-KDDATFORM,                               
062700                             DAT-I-TIDATUM,                               
062800                             DAT-O-TIDATUM,                               
062900                             DAT-KDSVAR                                   
063000         IF DAT-KDSVAR = 'F'                                              
063100           MOVE NEJ            TO NYCKLAR-SW                              
063200         END-IF                                                           
063300       END-IF                                                             
063400                                                                          
063500       IF MID-TIREGDAT-IN2 = ALL '+'                                      
063600         MOVE W-SISTA-DATUM    TO MID-TIREGDAT-IN2                        
063700       ELSE                                                               
063800         MOVE MID-TIREGDAT-IN2 TO DAT-I-TIDATUM                           
063900         CALL WDATKONV USING DAT-KDDATFORM,                               
064000                             DAT-I-TIDATUM,                               
064100                             DAT-O-TIDATUM,                               
064200                             DAT-KDSVAR                                   
064300         IF DAT-KDSVAR = 'F'                                              
064400           MOVE NEJ            TO NYCKLAR-SW                              
064500         END-IF                                                           
064600       END-IF                                                             
064700     END-IF                                                               
064800                                                                          
064900     IF NYCKLAR-OK                                                        
065000       PERFORM S01-RAEKNA-OM-DATUM-IN                                     
065100       MOVE W-TIREGDAT-IN1 TO W-DAREGDAT-MAX                              
065200       MOVE W-TIREGDAT-IN2 TO W-DAREGDAT-MIN                              
065300       COMPUTE W-DIFF = W-DAREGDAT-TOM - W-DAREGDAT-FROM                  
065400       END-COMPUTE                                                        
065500       IF W-DIFF > 10000                                                  
065600         MOVE NEJ          TO NYCKLAR-SW                                  
065700       END-IF                                                             
065800       IF W-TIREGDAT-IN1 < W-TIREGDAT-IN2                                 
065900         MOVE NEJ          TO NYCKLAR-SW                                  
066000       END-IF                                                             
066100     END-IF                                                               
066200                                                                          
066300*    -- KONTROLL AV IDSALDO                                               
066400                                                                          
066500     IF MID-IDSALDO-IN = ALL '+' AND                                      
066600        MID-IDSALDO-UT NOT = SPACE                                        
066700       MOVE MID-IDSALDO-UT TO MID-IDSALDO-IN                              
066800     END-IF                                                               
066900                                                                          
067000     IF MID-IDSALDO-IN NOT = 'ST'   AND                                   
067100                             'AKS'  AND                                   
067200                             'AKSO' AND                                   
067300                             'EFRS'                                       
067400       MOVE NEJ TO NYCKLAR-SW                                             
067500     END-IF                                                               
067600                                                                          
067700     .                                                                    
067800     EJECT                                                                
067900 C-FOERSTA-SIDA SECTION.                                                  
068000                                                                          
068100     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
068200     CALL WMEDKONV USING MED-WMEDAREA                                     
068300     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
068400     .                                                                    
068500     EJECT                                                                
068600 D-NAESTA-SIDA SECTION.                                                   
068700                                                                          
068800     IF SPAR-IDTRANS = '5163'                                             
068900       MOVE SPAR-IDARTNR-NEXT  TO W-IDARTNR-X                             
069000       MOVE SPAR-DAREGDAT-NEXT TO W-DAREGDAT-X                            
069100       MOVE SPAR-TIKLOCK-NEXT  TO W-TIKLOCK-X                             
069200       MOVE SPAR-IDSEKVNR-NEXT TO W-IDSEKVNR-X                            
069300     ELSE                                                                 
069400       PERFORM MFS-RENSA-FAELT-IN                                         
069500     END-IF                                                               
069600     .                                                                    
069700     EJECT                                                                
069800 E-SAMMA-SIDA SECTION.                                                    
069900                                                                          
070000     IF SPAR-IDTRANS = '5163' OR '0551'                                   
070100       MOVE SPAR-IDARTNR-ENTER  TO W-IDARTNR-X                            
070200       MOVE SPAR-DAREGDAT-ENTER TO W-DAREGDAT-X                           
070300       MOVE SPAR-TIKLOCK-ENTER  TO W-TIKLOCK-X                            
070400       MOVE SPAR-IDSEKVNR-ENTER TO W-IDSEKVNR-X                           
070500     ELSE                                                                 
070600       PERFORM MFS-RENSA-FAELT-IN                                         
070700     END-IF                                                               
070800     .                                                                    
070900     EJECT                                                                
071000 F-LAES-VISA-INFO SECTION.                                                
071100                                                                          
071200     IF SEGMENT-SAKNAS                                                    
071300       MOVE ERR-RECORD-MISSING     TO MED-IDMFSFEL                        
071400       CALL WMEDKONV USING MED-WMEDAREA                                   
071500       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
071600       PERFORM MFS-RENSA-FAELT-UT                                         
071700     ELSE                                                                 
071800       MOVE +1                     TO INDX                                
071900       IF SEGMENT-FINNS                                                   
072000         MOVE ZERO                 TO SPAR-IDARTNR-NEXT                   
072100                                      SPAR-DAREGDAT-NEXT                  
072200                                      SPAR-TIKLOCK-NEXT                   
072300                                      SPAR-IDSEKVNR-NEXT                  
072400         MOVE LOGG-IDARTNR         TO SPAR-IDARTNR-ENTER                  
072500         MOVE LOGG-DAREGDAT-9KOMPL TO SPAR-DAREGDAT-ENTER                 
072600         MOVE LOGG-TIKLOCK-9KOMPL  TO SPAR-TIKLOCK-ENTER                  
072700         MOVE LOGG-IDSEKVNR        TO SPAR-IDSEKVNR-ENTER                 
072800         CONTINUE                                                         
072900       ELSE                                                               
073000         MOVE W-IDARTNR-MIN        TO SPAR-IDARTNR-ENTER                  
073100         MOVE W-DAREGDAT-MIN       TO SPAR-DAREGDAT-ENTER                 
073200         MOVE W-TIKLOCK-MIN        TO SPAR-TIKLOCK-ENTER                  
073300         MOVE W-IDSEKVNR-MIN       TO SPAR-IDSEKVNR-ENTER                 
073400       END-IF                                                             
073500                                                                          
073600       PERFORM UNTIL INDX > MAX-INDX OR                                   
073700                     SEGMENT-SAKNAS  OR                                   
073800                     BASEN-SLUT                                           
073900         IF SEGMENT-FINNS                                                 
074200             IF (MFS-FIRST AND NOT DETALJ-MID)                            
074300               PERFORM S03-RAEKNA-ACKUMULERAT                             
074400             END-IF                                                       
074500             MOVE LOGG-IDARTNR         TO SPAR-IDARTNR-TAB(INDX)          
074600             MOVE LOGG-DAREGDAT-9KOMPL TO SPAR-DAREGDAT-TAB(INDX)         
074700             MOVE LOGG-TIKLOCK-9KOMPL  TO SPAR-TIKLOCK-TAB(INDX)          
074800             MOVE LOGG-IDSEKVNR        TO SPAR-IDSEKVNR-TAB(INDX)         
074900             PERFORM S02-RAEKNA-OM-DATUM-UT                               
075000             IF DETALJ-MID                                                
075100               IF SPAR-IDARTNR  = LOGG-IDARTNR         AND                
075200                  SPAR-DAREGDAT = LOGG-DAREGDAT-9KOMPL AND                
075300                  SPAR-TIKLOCK  = LOGG-TIKLOCK-9KOMPL  AND                
075400                  SPAR-IDSEKVNR = LOGG-IDSEKVNR                           
075500                  MOVE 'S'             TO MOD-SELECT-RAD (INDX)           
075600               END-IF                                                     
075700             END-IF                                                       
075800             IF MID-IDSALDO-IN = 'AKSO' AND                               
075900                LOGG-IDTECKEN-KVAKS-PAV NOT = SPACE                       
076000               MOVE LOGG-IDDC              TO MOD-IDDC (INDX)             
076100               MOVE LOGG-IDHUVTYP       TO MOD-IDHUVTYP (INDX)            
076200               MOVE LOGG-IDSUBTYP       TO MOD-IDSUBTYP (INDX)            
076300               MOVE W-DAREGDAT-KONV     TO MOD-TIREGDAT (INDX)            
076400               MOVE LOGG-KVART-SALDO    TO MOD-KVART-SALDO (INDX)         
076500               MOVE LOGG-IDTECKEN-KVAKS-PAV                               
076600                                        TO MOD-IDTECKEN (INDX)            
076700               MOVE LOGG-KVAKS-PAV      TO MOD-KVSALDO (INDX)             
076800               ADD 1 TO INDX                                              
076900             ELSE                                                         
077000             IF MID-IDSALDO-IN = 'AKS ' AND                               
077100                LOGG-IDTECKEN-KVAKS NOT = SPACE                           
077200               MOVE LOGG-IDDC           TO MOD-IDDC (INDX)                
077300               MOVE LOGG-IDHUVTYP       TO MOD-IDHUVTYP (INDX)            
077400               MOVE LOGG-IDSUBTYP       TO MOD-IDSUBTYP (INDX)            
077500               MOVE W-DAREGDAT-KONV     TO MOD-TIREGDAT (INDX)            
077600               MOVE LOGG-KVART-SALDO    TO MOD-KVART-SALDO(INDX)          
077700               MOVE LOGG-IDTECKEN-KVAKS TO MOD-IDTECKEN(INDX)             
077800               MOVE LOGG-KVAKS          TO MOD-KVSALDO(INDX)              
077900               ADD 1 TO INDX                                              
078000             ELSE                                                         
078100             IF MID-IDSALDO-IN = 'EFRS' AND                               
078200                LOGG-IDTECKEN-KVEFRS NOT = SPACE                          
078300               MOVE LOGG-IDDC           TO MOD-IDDC (INDX)                
078400               MOVE LOGG-IDHUVTYP       TO MOD-IDHUVTYP (INDX)            
078500               MOVE LOGG-IDSUBTYP       TO MOD-IDSUBTYP (INDX)            
078600               MOVE W-DAREGDAT-KONV     TO MOD-TIREGDAT (INDX)            
078700               MOVE LOGG-KVART-SALDO    TO MOD-KVART-SALDO (INDX)         
078800               MOVE LOGG-IDTECKEN-KVEFRS TO MOD-IDTECKEN (INDX)           
078900               MOVE LOGG-KVEFRS         TO MOD-KVSALDO (INDX)             
079000               ADD 1 TO INDX                                              
079100             ELSE                                                         
079200             IF MID-IDSALDO-IN = 'ST  ' AND                               
079300                LOGG-IDTECKEN-KVLS NOT = SPACE                            
079400               MOVE LOGG-IDDC           TO MOD-IDDC (INDX)                
079500               MOVE LOGG-IDHUVTYP       TO MOD-IDHUVTYP (INDX)            
079600               MOVE LOGG-IDSUBTYP       TO MOD-IDSUBTYP (INDX)            
079700               MOVE W-DAREGDAT-KONV     TO MOD-TIREGDAT (INDX)            
079800               MOVE LOGG-KVART-SALDO    TO MOD-KVART-SALDO (INDX)         
079900               MOVE LOGG-IDTECKEN-KVLS  TO MOD-IDTECKEN (INDX)            
080000               MOVE LOGG-KVLS           TO MOD-KVSALDO (INDX)             
080100               ADD 1 TO INDX                                              
080200             END-IF                                                       
080300             END-IF                                                       
080400             END-IF                                                       
080500             END-IF                                                       
080700           IF A-DCS-NDC-CN OR A-DCS-NDC-OTHERS                            
080710           OR (A-DCS-NDC-PF AND NOT A-DCS-FTG-PV)                         
080800             PERFORM IMS-GN-LOGA                                          
080900           ELSE                                                           
081000             PERFORM IMS-GN-LOGA2                                         
081100           END-IF                                                         
081200         ELSE                                                             
081300           MOVE MFS-RENSA-FAELT TO MOD-IDDC (INDX)                        
081400                                   MOD-IDHUVTYP (INDX)                    
081500                                   MOD-IDSUBTYP (INDX)                    
081600                                   MOD-TIREGDAT (INDX)                    
081700                                   MOD-KVART-SALDO (INDX)                 
081800                                   MOD-IDTECKEN (INDX)                    
081900                                   MOD-KVSALDO (INDX)                     
082000                                   SPAR-WDL901KY-TAB (INDX)               
082100         END-IF                                                           
082200       END-PERFORM                                                        
082300                                                                          
082400       IF SEGMENT-FINNS                                                   
082500         MOVE LOGG-IDARTNR         TO SPAR-IDARTNR-NEXT                   
082600         MOVE LOGG-DAREGDAT-9KOMPL TO SPAR-DAREGDAT-NEXT                  
082700         MOVE LOGG-TIKLOCK-9KOMPL  TO SPAR-TIKLOCK-NEXT                   
082800         MOVE LOGG-IDSEKVNR        TO SPAR-IDSEKVNR-NEXT                  
082900         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
083000         CALL WMEDKONV USING MED-WMEDAREA                                 
083100         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
083200         IF MFS-FIRST AND NOT DETALJ-MID                                  
083300           PERFORM UNTIL SEGMENT-SAKNAS OR                                
083400                         BASEN-SLUT                                       
083500             PERFORM S03-RAEKNA-ACKUMULERAT                               
083600             IF A-DCS-NDC-CN OR A-DCS-NDC-OTHERS                          
083610             OR (A-DCS-NDC-PF AND NOT A-DCS-FTG-PV)                       
083700               PERFORM IMS-GN-LOGA                                        
083800             ELSE                                                         
083900               PERFORM IMS-GN-LOGA2                                       
084000             END-IF                                                       
084100           END-PERFORM                                                    
084200         END-IF                                                           
084300       END-IF                                                             
084400                                                                          
084500       IF MFS-FIRST AND NOT DETALJ-MID                                    
084600         MOVE W-KVTOTAL      TO MOD-KVTOTAL                               
084700                                SPAR-KVTOTAL                              
084800         MOVE W-KVCHUP       TO MOD-KVCHUP                                
084900                                SPAR-KVCHUP                               
085000         MOVE W-KVCHDO       TO MOD-KVCHDO                                
085100                                SPAR-KVCHDO                               
085200       ELSE                                                               
085300         MOVE SPAR-KVTOTAL   TO MOD-KVTOTAL                               
085400         MOVE SPAR-KVCHUP    TO MOD-KVCHUP                                
085500         MOVE SPAR-KVCHDO    TO MOD-KVCHDO                                
085600       END-IF                                                             
085700                                                                          
085800       MOVE '002'            TO MSGI-KDCALL                               
085900       MOVE '5163'           TO SPAR-IDTRANS                              
086000       MOVE SPAR-AREA        TO MSGI-SPAR-AREA                            
086100       MOVE MID-IDARTNR-UT   TO MSGI-IDARTNR                              
086200       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
086300     END-IF                                                               
086400                                                                          
086500     IF INDX = +1                                                         
086600       IF MOD-IDDC (INDX) = SPACE                                         
086700         MOVE ERR-RECORD-MISSING  TO MED-IDMFSFEL                         
086800         CALL WMEDKONV USING MED-WMEDAREA                                 
086900         MOVE MED-MFSFEL          TO MOD-TEMFSFEL                         
087000         PERFORM MFS-RENSA-FAELT-UT                                       
087100       END-IF                                                             
087200     END-IF                                                               
087300     .                                                                    
087400     EJECT                                                                
087500 H-BYT-BILD SECTION.                                                      
087600                                                                          
087700     MOVE '5163'                    TO SPAR-BILD                          
087800                                                                          
087900* ---HÄMTAR RÄTT RAD-VÄRDE TILL 5164-BILDEN                               
088000     MOVE W-INDX TO INDX                                                  
088100     MOVE SPAR-IDARTNR-TAB  (INDX)  TO SPAR-IDARTNR                       
088200     MOVE SPAR-DAREGDAT-TAB (INDX)  TO SPAR-DAREGDAT                      
088300     MOVE SPAR-TIKLOCK-TAB  (INDX)  TO SPAR-TIKLOCK                       
088400     MOVE SPAR-IDSEKVNR-TAB (INDX)  TO SPAR-IDSEKVNR                      
088500                                                                          
088600     MOVE '002'               TO MSGI-KDCALL                              
088700     MOVE '5163'              TO SPAR-IDTRANS                             
088800     MOVE SPAR-AREA           TO MSGI-SPAR-AREA                           
088900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
089000                                                                          
089100* ---SKICKAR VÄRDE TILL 5164-MID FÖR ATT SEDAN                            
089200* ---STARTA UPP DETTA PROGRAM                                             
089300     MOVE LOW-VALUE           TO 5164-MID-W5I16401                        
089400     MOVE MID-IDARTNR-UT      TO 5164-MID-IDARTNR-UT                      
089500     MOVE MID-IDDC-UT         TO 5164-MID-IDDC-UT                         
089600     MOVE MID-IDHUVTYP-UT     TO 5164-MID-IDHUVTYP-UT                     
089700     MOVE MID-IDSUBTYP-UT     TO 5164-MID-IDSUBTYP-UT                     
089800     MOVE MID-TIREGDAT-UT1    TO 5164-MID-TIREGDAT-FOM-UT                 
089900     MOVE MID-TIREGDAT-UT2    TO 5164-MID-TIREGDAT-TOM-UT                 
090000     MOVE MID-IDSALDO-UT      TO 5164-MID-IDTRANS-UT                      
090100     COMPUTE MSG-KVLL = LENGTH OF MOD-W5O16301 + 17                       
090200     PERFORM IMS-INSERT-ALT2-MSG                                          
090300     MOVE NEJ TO ALLT-SW                                                  
090400     .                                                                    
090500     EJECT                                                                
090600 I-KOLLA-EXTRACT.                                                         
090700                                                                          
090800     PERFORM MFS-ROER-EJ-FAELT-UT                                         
090900     IF MID-FLEXTRAKT = 'Y' OR ' ' OR 'N'                                 
091000       MOVE MFS-ROER-EJ-FAELT    TO MOD-FLEXTRAKT-ATTR                    
091100       MOVE MFS-RENSA-FAELT      TO MOD-FLEXTRAKT                         
091200       IF MID-FLEXTRAKT = ' ' OR 'N'                                      
091300         MOVE NEJ TO EXTRACT-SW                                           
091400       END-IF                                                             
091500     ELSE                                                                 
091600       MOVE NEJ TO EXTRACT-SW                                             
091700       MOVE WRONG-INPUT-FIELD    TO MED-IDMFSFEL                          
091800       CALL WMEDKONV USING MED-WMEDAREA                                   
091900       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
092000       MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLEXTRAKT-ATTR                    
092100     END-IF                                                               
092200     .                                                                    
092300     EJECT                                                                
092400 J-STARTA-BMP  SECTION.                                                   
092500*                                                                         
092600*  STARTA BMP W555B3                                                      
092700*                                                                         
092800     MOVE W-IDARTNR       TO SKICKA-IDARTNR                               
092900     MOVE MID-IDDC-IN     TO SKICKA-IDDC                                  
093000     MOVE MID-IDHUVTYP-IN TO SKICKA-IDHUVTYP                              
093100     MOVE MID-IDSUBTYP-IN TO SKICKA-IDSUBTYP                              
093200     MOVE W-DAREGDAT-MIN  TO SKICKA-DAREGDAT-MIN                          
093300     MOVE W-DAREGDAT-MAX  TO SKICKA-DAREGDAT-MAX                          
093400     MOVE '++++'          TO SKICKA-IDTRANS                               
093500     MOVE MSGI-IDUSER     TO SKICKA-IDUSER                                
093600     MOVE MSGI-IDLAND-SPR TO SKICKA-IDLAND                                
093700                                                                          
093800     MOVE '5163'          TO MSGSOP-IDTRANS                               
093900     MOVE MFS-KDMFSFOR    TO MSGSOP-KDMFSFOR                              
094000     MOVE 'W555B3'        TO MSGSOP-IDPROCESS                             
094100     MOVE 'O'             TO MSGSOP-KDSOPFUNK                             
094200                                                                          
094300     STRING 'IDARTNR(' SKICKA-IDARTNR ')                                  
094400-           'IDDC(' SKICKA-IDDC ')                                        
094500-           'HUVTYP(' SKICKA-IDHUVTYP ')                                  
094600-           'SUBTYP(' SKICKA-IDSUBTYP ')                                  
094700-           'MAXDAT(' SKICKA-DAREGDAT-MAX ')                              
094800-           'MINDAT(' SKICKA-DAREGDAT-MIN ')                              
094900-           'IDTRANS(' SKICKA-IDTRANS ')                                  
095000-           'IDUSER(' SKICKA-IDUSER ')                                    
095100-           'IDLAND(' SKICKA-IDLAND ')'                                   
095200          DELIMITED BY SIZE INTO MSGSOP-TESYMBV                           
095300     PERFORM IMS-INSERT-ALT1-MSG                                          
095400     .                                                                    
095500     EJECT                                                                
095600 S01-RAEKNA-OM-DATUM-IN SECTION.                                          
095700                                                                          
095800     MOVE MID-TIREGDAT-IN1 TO W-DAREGDAT-KONV                             
095900     IF W-DAREGDAT-KONV < 500000                                          
096000       MOVE 20             TO W-DAREGDAT-AAR                              
096100     ELSE                                                                 
096200       MOVE 19             TO W-DAREGDAT-AAR                              
096300     END-IF                                                               
096400     MOVE W-DAREGDAT-KONV  TO W-DAREGDAT-TI                               
096500     MOVE W-DAREGDAT       TO WS-DAREGDAT                                 
096600                              W-DAREGDAT-FROM                             
096700     COMPUTE W-TIREGDAT-IN1 = 99999999 - WS-DAREGDAT                      
096800                                                                          
096900     MOVE MID-TIREGDAT-IN2 TO W-DAREGDAT-KONV                             
097000     IF W-DAREGDAT-KONV < 500000                                          
097100       MOVE 20             TO W-DAREGDAT-AAR                              
097200     ELSE                                                                 
097300       MOVE 19             TO W-DAREGDAT-AAR                              
097400     END-IF                                                               
097500     MOVE W-DAREGDAT-KONV  TO W-DAREGDAT-TI                               
097600     MOVE W-DAREGDAT       TO WS-DAREGDAT                                 
097700                              W-DAREGDAT-TOM                              
097800     COMPUTE W-TIREGDAT-IN2 = 99999999 - WS-DAREGDAT                      
097900     MOVE ZERO             TO W-DAREGDAT                                  
098000                              WS-DAREGDAT                                 
098100     .                                                                    
098200     EJECT                                                                
098300 S02-RAEKNA-OM-DATUM-UT SECTION.                                          
098400                                                                          
098500     MOVE LOGG-DAREGDAT-9KOMPL TO WS-DAREGDAT                             
098600     COMPUTE WS-DAREGDAT = WS-DAREGDAT - 99999999                         
098700     MOVE WS-DAREGDAT          TO W-DAREGDAT                              
098800     MOVE W-DAREGDAT-TI        TO W-DAREGDAT-KONV                         
098900     .                                                                    
099000     EJECT                                                                
099100 S03-RAEKNA-ACKUMULERAT SECTION.                                          
099200                                                                          
099300     IF MID-IDSALDO-IN = 'EFRS'                                           
099400       IF LOGG-IDTECKEN-KVEFRS NOT = SPACE                                
099500         ADD 1                             TO W-KVTOTAL                   
099600         IF LOGG-IDTECKEN-KVEFRS = '+' AND                                
099700            LOGG-KVART-SALDO > 0                                          
099800           ADD LOGG-KVART-SALDO            TO W-KVCHUP                    
099900         ELSE                                                             
100000           IF LOGG-IDTECKEN-KVEFRS = '-' AND                              
100100              LOGG-KVART-SALDO > 0                                        
100200             ADD LOGG-KVART-SALDO          TO W-KVCHDO                    
100300           ELSE                                                           
100400             IF LOGG-IDTECKEN-KVEFRS = '+' AND                            
100500                LOGG-KVART-SALDO < 0                                      
100600               SUBTRACT LOGG-KVART-SALDO   FROM W-KVCHDO                  
100700             ELSE                                                         
100800               IF LOGG-IDTECKEN-KVEFRS = '-' AND                          
100900                  LOGG-KVART-SALDO < 0                                    
101000                 SUBTRACT LOGG-KVART-SALDO FROM W-KVCHUP                  
101100               END-IF                                                     
101200             END-IF                                                       
101300           END-IF                                                         
101400         END-IF                                                           
101500       END-IF                                                             
101600     ELSE                                                                 
101700     IF MID-IDSALDO-IN = 'AKSO'                                           
101800       IF LOGG-IDTECKEN-KVAKS-PAV NOT = SPACE                             
101900         ADD 1 TO W-KVTOTAL                                               
102000         IF LOGG-IDTECKEN-KVAKS-PAV = '+' AND                             
102100            LOGG-KVART-SALDO > 0                                          
102200           ADD LOGG-KVART-SALDO TO W-KVCHUP                               
102300         ELSE                                                             
102400           IF LOGG-IDTECKEN-KVAKS-PAV = '-' AND                           
102500              LOGG-KVART-SALDO > 0                                        
102600             ADD LOGG-KVART-SALDO TO W-KVCHDO                             
102700           ELSE                                                           
102800             IF LOGG-IDTECKEN-KVAKS-PAV = '+' AND                         
102900                LOGG-KVART-SALDO < 0                                      
103000               SUBTRACT LOGG-KVART-SALDO FROM W-KVCHDO                    
103100             ELSE                                                         
103200               IF LOGG-IDTECKEN-KVAKS-PAV = '-' AND                       
103300                  LOGG-KVART-SALDO < 0                                    
103400                 SUBTRACT LOGG-KVART-SALDO FROM W-KVCHUP                  
103500               END-IF                                                     
103600             END-IF                                                       
103700           END-IF                                                         
103800         END-IF                                                           
103900       END-IF                                                             
104000     ELSE                                                                 
104100     IF MID-IDSALDO-IN = 'AKS '                                           
104200        IF LOGG-IDTECKEN-KVAKS NOT = SPACE                                
104300           ADD 1                                 TO W-KVTOTAL             
104400           IF LOGG-IDTECKEN-KVAKS = '+' AND                               
104500              LOGG-KVART-SALDO > 0                                        
104600              ADD LOGG-KVART-SALDO               TO W-KVCHUP              
104700           ELSE                                                           
104800              IF LOGG-IDTECKEN-KVAKS = '-' AND                            
104900                 LOGG-KVART-SALDO > 0                                     
105000                 ADD LOGG-KVART-SALDO            TO W-KVCHDO              
105100              ELSE                                                        
105200                 IF LOGG-IDTECKEN-KVAKS = '+' AND                         
105300                    LOGG-KVART-SALDO < 0                                  
105400                    SUBTRACT LOGG-KVART-SALDO    FROM W-KVCHDO            
105500                 ELSE                                                     
105600                    IF LOGG-IDTECKEN-KVAKS = '-' AND                      
105700                       LOGG-KVART-SALDO < 0                               
105800                       SUBTRACT LOGG-KVART-SALDO FROM W-KVCHUP            
105900                    END-IF                                                
106000                 END-IF                                                   
106100              END-IF                                                      
106200           END-IF                                                         
106300        END-IF                                                            
106400     ELSE                                                                 
106500     IF MID-IDSALDO-IN = 'ST  '                                           
106600       IF LOGG-IDTECKEN-KVLS NOT = SPACE                                  
106700         ADD 1                             TO W-KVTOTAL                   
106800         IF LOGG-IDTECKEN-KVLS = '+' AND                                  
106900            LOGG-KVART-SALDO > 0                                          
107000           ADD LOGG-KVART-SALDO            TO W-KVCHUP                    
107100         ELSE                                                             
107200           IF LOGG-IDTECKEN-KVLS = '-' AND                                
107300              LOGG-KVART-SALDO > 0                                        
107400             ADD LOGG-KVART-SALDO          TO W-KVCHDO                    
107500           ELSE                                                           
107600             IF LOGG-IDTECKEN-KVLS = '+' AND                              
107700                LOGG-KVART-SALDO < 0                                      
107800               SUBTRACT LOGG-KVART-SALDO   FROM W-KVCHDO                  
107900             ELSE                                                         
108000               IF LOGG-IDTECKEN-KVLS = '-' AND                            
108100                  LOGG-KVART-SALDO < 0                                    
108200                 SUBTRACT LOGG-KVART-SALDO FROM W-KVCHUP                  
108300               END-IF                                                     
108400             END-IF                                                       
108500           END-IF                                                         
108600         END-IF                                                           
108700       END-IF                                                             
108800     END-IF                                                               
108900     END-IF                                                               
109000     END-IF                                                               
109100     END-IF                                                               
109200     .                                                                    
109300     EJECT                                                                
109400 S04-FLYTTA-TILL-MOD SECTION.                                             
109500                                                                          
109600*    -- FLYTTA TILL MOD-AREA                                              
109700     IF MID-IDARTNR-IN NOT = ALL '+'                                      
109800       MOVE MID-IDARTNR-IN   TO MOD-IDARTNR-UT                            
109900       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
110000     END-IF                                                               
110100                                                                          
110200     IF MID-IDDC-IN NOT = ALL '+'                                         
110300       MOVE MID-IDDC-IN      TO MOD-IDDC-UT                               
110400     END-IF                                                               
110500                                                                          
110600     IF MID-IDHUVTYP-IN NOT = ALL '+'                                     
110700       MOVE MID-IDHUVTYP-IN  TO MOD-IDHUVTYP-UT                           
110800     END-IF                                                               
110900                                                                          
111000     IF MID-IDSUBTYP-IN NOT = ALL '+'                                     
111100       MOVE MID-IDSUBTYP-IN  TO MOD-IDSUBTYP-UT                           
111200     END-IF                                                               
111300                                                                          
111400     IF MID-TIREGDAT-IN1 NOT = ALL '+'                                    
111500       MOVE MID-TIREGDAT-IN1 TO MOD-TIREGDAT-UT1                          
111600     END-IF                                                               
111700                                                                          
111800     IF MID-TIREGDAT-IN2 NOT = ALL '+'                                    
111900       MOVE MID-TIREGDAT-IN2 TO MOD-TIREGDAT-UT2                          
112000     END-IF                                                               
112100                                                                          
112200     IF MID-IDSALDO-IN NOT = ALL '+'                                      
112300       MOVE MID-IDSALDO-IN   TO MOD-IDSALDO-UT                            
112400     END-IF                                                               
112500     .                                                                    
112600     EJECT                                                                
112700 MFS-RENSA-FAELT-UT SECTION.                                              
112800                                                                          
112900     MOVE +1 TO INDX                                                      
113000     PERFORM UNTIL INDX > MAX-INDX                                        
113100       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
113200       ADD +1 TO INDX                                                     
113300     END-PERFORM                                                          
113400     .                                                                    
113500                                                                          
113600 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
113700                                                                          
113800*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
113900     MOVE MFS-RENSA-FAELT TO MOD-SELECT-RAD         (INDX)                
114000                             MOD-IDDC               (INDX)                
114100                             MOD-IDHUVTYP           (INDX)                
114200                             MOD-IDSUBTYP           (INDX)                
114300                             MOD-TIREGDAT           (INDX)                
114400                             MOD-KVART-SALDO        (INDX)                
114500                             MOD-IDTECKEN           (INDX)                
114600                             MOD-KVSALDO            (INDX)                
114700     .                                                                    
114800                                                                          
114900 MFS-RENSA-FAELT-IN SECTION.                                              
115000                                                                          
115100*    --- ALLA INDATA-FÄLT                                                 
115200     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
115300                             MOD-IDDC-IN                                  
115400                             MOD-IDHUVTYP-IN                              
115500                             MOD-IDSUBTYP-IN                              
115600                             MOD-TIREGDAT-IN1                             
115700                             MOD-TIREGDAT-IN2                             
115800                             MOD-IDSALDO-IN                               
115900     .                                                                    
116000     EJECT                                                                
116100 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
116200                                                                          
116300*    --- ALLA UTDATA-FÄLT                                                 
116400*    --- OCH RAD-DATA                                                     
116500     MOVE MFS-ROER-EJ-FAELT TO MOD-KVTOTAL                                
116600                               MOD-KVCHUP                                 
116700                               MOD-KVCHDO                                 
116800                               MOD-FLEXTRAKT                              
116900                               MOD-TEMFSINF                               
117000     MOVE +1 TO INDX                                                      
117100     PERFORM UNTIL INDX > MAX-INDX                                        
117200       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
117300       ADD +1 TO INDX                                                     
117400     END-PERFORM                                                          
117500     .                                                                    
117600     EJECT                                                                
117700 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
117800                                                                          
117900*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
118000     MOVE MFS-ROER-EJ-FAELT TO MOD-SELECT-RAD         (INDX)              
118100                               MOD-IDDC               (INDX)              
118200                               MOD-IDHUVTYP           (INDX)              
118300                               MOD-IDSUBTYP           (INDX)              
118400                               MOD-TIREGDAT           (INDX)              
118500                               MOD-KVART-SALDO        (INDX)              
118600                               MOD-IDTECKEN           (INDX)              
118700                               MOD-KVSALDO            (INDX)              
118800     .                                                                    
118900     EJECT                                                                
119000* --- IMS SEKTIONER ---                                                   
119100                                                                          
119200 IMS-GET-MSG SECTION.                                                     
119300                                                                          
119400     MOVE '  QC' TO GODK-STATUSKODER                                      
119500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
119600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
119700     PERFORM IMS-STATUSKONTROLL                                           
119800     .                                                                    
119900                                                                          
120000 IMS-INSERT-MSG SECTION.                                                  
120100                                                                          
120200     IF ENGLISH-TEXT                                                      
120300       MOVE 'N' TO MFS-KDHUVOMR                                           
120400     END-IF                                                               
120500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
120600     MOVE SPACE TO GODK-STATUSKODER                                       
120700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
120800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
120900     PERFORM IMS-STATUSKONTROLL                                           
121000     .                                                                    
121100     EJECT                                                                
121200 IMS-INSERT-ALT1-MSG SECTION.                                             
121300     MOVE '  ' TO GODK-STATUSKODER                                        
121400     CALL CBLTDLI USING ISRT ALT1-PCB W-PROG-TO-PROG-SW1                  
121500     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
121600     PERFORM IMS-STATUSKONTROLL                                           
121700     .                                                                    
121800     EJECT                                                                
121900 IMS-INSERT-ALT2-MSG SECTION.                                             
122000                                                                          
122100     MOVE SPACE TO GODK-STATUSKODER                                       
122200     CALL CBLTDLI USING ISRT ALT2-PCB W-PROG-TO-PROG-SW2                  
122300     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
122400     PERFORM IMS-STATUSKONTROLL                                           
122500     .                                                                    
122600     EJECT                                                                
122700                                                                          
122800 IMS-GN-LOGA SECTION.                                                     
122900                                                                          
123000     STRING 'WLLOGA01(WDL901KY>=' W-WDL901KY-MIN-X                        
123100                    '&WDL901KY<=' W-WDL901KY-MAX-X                        
123200                    '&IDDC    >=' W-IDDC-MIN                              
123300                    '&IDDC    <=' W-IDDC-MAX                              
123400                    '&IDHUVTYP>=' W-IDHUVTYP-MIN                          
123500                    '&IDHUVTYP<=' W-IDHUVTYP-MAX                          
123600                    '&IDSUBTYP>=' W-IDSUBTYP-MIN                          
123700                    '&IDSUBTYP<=' W-IDSUBTYP-MAX ')'                      
123800          DELIMITED BY SIZE INTO SSA1                                     
123900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
124000     CALL CBLTDLI USING GN LOGA-PCB DLI-IO-WLLOGA01 SSA1                  
124100     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
124200     PERFORM IMS-STATUSKONTROLL                                           
124300     .                                                                    
124400     EJECT                                                                
124500 IMS-GN-LOGA2 SECTION.                                                    
124600                                                                          
124700     STRING 'WLLOGA01(WDL901KY>=' W-WDL901KY-MIN-X                        
124800                    '&WDL901KY<=' W-WDL901KY-MAX-X                        
124900                    '&IDDC    >=' W-IDDC-MIN                              
125000                    '&IDDC    <=' W-IDDC-MAX                              
125100                    '&IDHUVTYP>=' W-IDHUVTYP-MIN                          
125200                    '&IDHUVTYP<=' W-IDHUVTYP-MAX                          
125300                    '&IDSUBTYP>=' W-IDSUBTYP-MIN                          
125400                    '&IDSUBTYP<=' W-IDSUBTYP-MAX                          
125500                    '+WDL901KY>=' W-WDL901KY-MIN-X                        
125600                    '&WDL901KY<=' W-WDL901KY-MAX-X                        
125700                    '&IDDC    >=' W-IDDC-MIN2                             
125800                    '&IDDC    <=' W-IDDC-MAX2                             
125900                    '&IDHUVTYP>=' W-IDHUVTYP-MIN                          
126000                    '&IDHUVTYP<=' W-IDHUVTYP-MAX                          
126100                    '&IDSUBTYP>=' W-IDSUBTYP-MIN                          
126200                    '&IDSUBTYP<=' W-IDSUBTYP-MAX ')'                      
126300          DELIMITED BY SIZE INTO SSA1                                     
126400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
126500     CALL CBLTDLI USING GN LOGA-PCB DLI-IO-WLLOGA01 SSA1                  
126600     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
126700     PERFORM IMS-STATUSKONTROLL                                           
126800     .                                                                    
126900     EJECT                                                                
127000 IMS-GU-LOGA SECTION.                                                     
127100                                                                          
127200     STRING 'WLLOGA01(WDL901KY =' W-WDL901KY-X ')'                        
127300          DELIMITED BY SIZE INTO SSA1                                     
127400     MOVE '  GE' TO GODK-STATUSKODER                                      
127500     CALL CBLTDLI USING GU LOGA-PCB DLI-IO-WLLOGA01 SSA1                  
127600     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
127700     PERFORM IMS-STATUSKONTROLL                                           
127800     .                                                                    
127900     EJECT                                                                
128000 IMS-GU-WDB601-A  SECTION.                                                
128100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
128200          DELIMITED BY SIZE INTO SSA1                                     
128300     MOVE '  GE' TO GODK-STATUSKODER                                      
128400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-A SSA1               
128500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
128600     PERFORM IMS-STATUSKONTROLL                                           
128700     .                                                                    
128800     EJECT                                                                
128900 IMS-GU-WDB601-B  SECTION.                                                
129000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
129100          DELIMITED BY SIZE INTO SSA1                                     
129200     MOVE '  GE' TO GODK-STATUSKODER                                      
129300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-B SSA1               
129400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
129500     PERFORM IMS-STATUSKONTROLL                                           
129600     .                                                                    
129700     EJECT                                                                
129800 IMS-STATUSKONTROLL SECTION.                                              
129900                                                                          
130000     SET STATUS-IX TO 1                                                   
130100     SEARCH GODK-STATUS                                                   
130200       AT END                                                             
130300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
130400         DELIMITED BY SIZE INTO FELTEXT                                   
130500         CALL FELLOG                                                      
130600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
130700         CONTINUE                                                         
130800     END-SEARCH                                                           
130900     .                                                                    
