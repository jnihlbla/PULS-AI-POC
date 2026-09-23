000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4058100.                                                
000400 AUTHOR.         BERT ANDERSSON.                                          
000500 DATE-WRITTEN.   90/02/08.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET ÄR ETT FRÅGE-/UPPDATERINGS- PROGRAM MOT               
001100*        TRANSPORTREGISTRET. PROGRAMMET LÄSER TRANSPORTID                 
001200*        FRÅN DB. (WLXXKA) OCH TRANSPORTAVGÅNG FRÅN DB. (WLXXKB).         
001300*        DET FINNS MÖJLIGHET ATT BLÄDDRA OM INTE SAMTLIGA                 
001400*        AVGÅNGAR FÖR EN TRANSPORT FÅR PLATS PÅ SKÄRMEN.                  
001500*        I PROGRAMMET FINNS MÖJLIGHET ATT:                                
001600*        - SÖKA TRANSPORTID.                                              
001700*        - ÄNDRA/UPPDATERA OCH NYREGISTRERA TRANSPORTID.                  
001800*        -    -   II  -     -  II  -        TRANSPORTAVGÅNG.              
001900*        - BLÄDDRA GENOM TRANSPORTAVGÅNGAR.                               
002000*        PROGRAMMET ÄR ETT FRÅGE-MPP.                                     
002100*        PROGRAMMET LÄSER WLXXKA (WDR1).                                  
002200*        PROGRAMMET LÄSER WLXXKB (WDR1).                                  
002300*    INDATA.                                                              
002400*        TRANSAKTION: W4T581                                              
002500*        MID:         W4I58101                                            
002600*    UTDATA.                                                              
002700*        MOD:         W4O58101                                            
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003201                                                                          
003210*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W4058100'.            
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  YES                         PIC X       VALUE 'Y'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700 77  INDX                        PIC S9(4)   VALUE +0   COMP SYNC.        
003800 77  MAX-INDX                    PIC S9(4)   VALUE +10  COMP SYNC.        
003900 77  MAX-VECKA                   PIC  9(2)   VALUE  53.                   
004000 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +600 COMP SYNC.        
004200 77  WS-IDTRP                    PIC X(5)    VALUE SPACE.                 
004300 77  WS-TRPAVG                   PIC X(5)    VALUE SPACE.                 
004400 77  WS-BETRPDST                 PIC X(15)   VALUE SPACE.                 
004500 77  WS-KVLASTTI                 PIC S9(3)V9(2)     COMP-3.               
004600 77  WS-KVADMFL                  PIC S9(3)V9(2)     COMP-3.               
004700 77  WS-KVADMEL                  PIC S9(3)V9(2)     COMP-3.               
004710 77  WS-IDDC                     PIC X(2).                                
004720 77  WS-IDTIDZON                 PIC X(2)    VALUE SPACE.                 
004800                                                                          
004900 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005000     88  NYCKLAR-OK                          VALUE 'J'.                   
005100     88  NYCKLAR-FEL                         VALUE 'N'.                   
005200                                                                          
005300 77  NYA-NYCKLAR-SW              PIC X       VALUE 'J'.                   
005400     88  NYA-NYCKLAR                         VALUE 'J'.                   
005500                                                                          
005600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005700     88  INDATA-OK                           VALUE 'J'.                   
005800     88  INDATA-FEL                          VALUE 'N'.                   
005900                                                                          
006000 77  FLTABORT-SW                 PIC X       VALUE 'N'.                   
006100     88  FLTABORT-KOD-FEL                    VALUE 'J'.                   
006200                                                                          
006300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006400     88  ALLT-OK                             VALUE 'J'.                   
006500                                                                          
006600 77  TRPID-SW                    PIC X       VALUE 'J'.                   
006700     88  TRPID-FINNS                         VALUE 'J'.                   
006800     88  TRPID-SAKNAS                        VALUE 'N'.                   
006900                                                                          
007000 77  UPPDAT-TRPID-SW             PIC X       VALUE 'N'.                   
007100     88  UPPDAT-TRPID-OK                     VALUE 'J'.                   
007200                                                                          
007300 77  TRPAVG-SW                   PIC X       VALUE 'N'.                   
007400     88  TRPAVG-FINNS                        VALUE 'J'.                   
007500                                                                          
007600 77  UPPDAT-SW                   PIC X       VALUE 'N'.                   
007700     88  UPPDAT-OK                           VALUE 'J'.                   
007800                                                                          
007900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008000     88  EGEN-MID                            VALUE '4581'.                
008100     88  GODK-MID                  VALUE '4581' '4582' '4583'.            
008200                                                                          
008300 01  DATUM                       PIC 9(6).                                
008400 01  W REDEFINES DATUM.                                                   
008500     03  W-AR                    PIC 9(2).                                
008600     03  W-VECKA                 PIC 9(2).                                
008700     03  W-DAG                   PIC 9(2).                                
008800                                                                          
008900 01  EGET-DATUM                  PIC 9(4).                                
009000 01  E REDEFINES EGET-DATUM.                                              
009100     03  E-AR                    PIC 9(2).                                
009200     03  E-VECKA                 PIC 9(2).                                
009300                                                                          
009400 01  TRPAVGTID.                                                           
009500     03  TRPAVGTID-VECKA         PIC 9(2).                                
009600         88  VECKA-OK                        VALUE 0 THRU 53.             
009700     03  TRPAVGTID-DAG           PIC 9(1).                                
009800         88  DAG-OK                          VALUE 0 THRU 7.              
009900     03  TRPAVGTID-TIMMA         PIC 9(2).                                
010000         88  TIMMA-OK                        VALUE 0 THRU 23.             
010100     03  TRPAVGTID-MINUT         PIC 9(2).                                
010200         88  MINUT-OK                        VALUE 0 THRU 59.             
010300     EJECT                                                                
010400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010500 01  GENERELLA-SUBPROGRAM.                                                
010600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010800     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
010900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011010     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011021     EJECT                                                                
011030*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011040*01 -COPY WMSGINIT                                                        
011095     EJECT                                                                
011200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011300*   -COPY WMEDAREA                                                        
011500     EJECT                                                                
011600*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
011700*   -COPY WDATAREA                                                        
011900     EJECT                                                                
012000*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
012100*   -COPY WDECAREA                                                        
012300     EJECT                                                                
012400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012500*                                                                         
012600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012700     SKIP3                                                                
012800*01  MID -COPY W4I58101                                                   
013000     EJECT                                                                
013100 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
013200     SKIP3                                                                
013300*01  -COPY WMSGAREA                                                       
013500     EJECT                                                                
013600*    03  MOD -COPY W4O58101   -RED MSG-AREA.                              
013800     EJECT                                                                
013900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014000     SKIP3                                                                
014100*01  -COPY WMFSAREA                                                       
014300     EJECT                                                                
014400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014500*                                                                         
014600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014700     SKIP3                                                                
014800 01  NYCKLAR-TILL-DLI.                                                    
014900     03  W-WDGXKEY-4431-X.                                                
015000         05  W-4431-IDHTYP       PIC X(4)    VALUE '4431'.                
015100         05  W-4431-IDDC         PIC X(2).                                
015200         05  W-4431-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
015300                                                                          
015400     03  W-WDGXKEY-4432-X.                                                
015500         05  W-4432-IDTRP        PIC X(5)    VALUE SPACE.                 
015600                                                                          
015700     03  W-WDGXKEY-4433-X.                                                
015800         05  W-4433-IDHTYP       PIC X(4)    VALUE '4433'.                
015900         05  W-4433-IDDC         PIC X(2).                                
016000         05  W-4433-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
016100                                                                          
016200     03  W-WDGXKEY-4434-X.                                                
016300         05  W-4434-IDTRP        PIC X(5)    VALUE SPACE.                 
016400         05  W-4434-TITRPAVG     PIC S9(7)   COMP-3.                      
016500         05  W-4434-LOW-VALUE    PIC X       VALUE LOW-VALUE.             
016600*    --- STATUS-KOD FRÅN IMS                                              
016700 01  STATUS-WS                   PIC XX.                                  
016800     88  SEGMENT-FINNS                       VALUE '  '.                  
016900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017100     SKIP2                                                                
017200 01  GODK-STATUSKODER.                                                    
017300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017400     SKIP3                                                                
017500 01  SSA1                        PIC X(64).                               
017600 01  SSA2                        PIC X(64).                               
017700     EJECT                                                                
017800*    --- IMS FUNKTIONSKODER                                               
017900*01  -COPY W0003                                                          
018100     EJECT                                                                
018200*    ---  DLI INPUT-OUTPUT AREA                                           
018300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
018400     SKIP3                                                                
018500 01  DLI-IO-AREA.                                                         
018600     03  IO-AREA                 PIC X(30)  VALUE SPACE.                  
018700     SKIP3                                                                
018800*    03  WLXXKA01 -COPY WDGX4431   -RED IO-AREA.                          
019000     EJECT                                                                
019100*    03  WLXXKA11 -COPY WDGX4432   -RED IO-AREA.                          
019300     EJECT                                                                
019400*    03  WLXXKB01 -COPY WDGX4433   -RED IO-AREA.                          
019600     EJECT                                                                
019700*    03  WLXXKB11 -COPY WDGX4434   -RED IO-AREA.                          
019900     EJECT                                                                
020000 LINKAGE SECTION.                                                         
020100                                                                          
020200*01  -COPY W0009      -PRE MSG-                                           
020400     EJECT                                                                
020500*01  -COPY W0008      -PRE USEA-                                          
020700     05  FILLER                  PIC X.                                   
020800     EJECT                                                                
020810*01  -COPY W0008      -PRE XXKA-                                          
020820     05  FILLER                  PIC X.                                   
020830     EJECT                                                                
020900*01  -COPY W0008      -PRE XXKB-                                          
021100     05  FILLER                  PIC X.                                   
021200     EJECT                                                                
021300 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB XXKA-PCB XXKB-PCB.            
021400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB XXKA-PCB XXKB-PCB.            
021500                                                                          
021600     PERFORM IMS-GET-MSG                                                  
021700     IF SEGMENT-FINNS                                                     
021800       PERFORM A-INIT                                                     
021900       PERFORM B-KOLLA-NYCKLAR                                            
022000       IF NYCKLAR-OK                                                      
022100         PERFORM C-LAES-DB-WLXXKA                                         
022200         IF NYA-NYCKLAR                                                   
022300           MOVE ZERO TO W-4434-TITRPAVG                                   
022400         ELSE                                                             
022500            IF MFS-UPDATE                                                 
022600              PERFORM D-KONTROLL-INDATA                                   
022700              IF INDATA-OK                                                
022800                PERFORM E-UPPDATERA                                       
022900              END-IF                                                      
023000            ELSE                                                          
023100              PERFORM F-KONTROLL-TRANSTYP                                 
023200            END-IF                                                        
023300         END-IF                                                           
023400         IF ALLT-OK                                                       
023500           PERFORM G-LAES-VISA-BILD                                       
023600         END-IF                                                           
023700       END-IF                                                             
023800       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
023900       PERFORM IMS-INSERT-MSG                                             
024000     END-IF                                                               
024100                                                                          
024200     MOVE ZERO TO RETURN-CODE                                             
024300     GOBACK                                                               
024400     .                                                                    
024500     EJECT                                                                
024600 A-INIT SECTION.                                                          
024700                                                                          
024800     IF MSG-DUBBLA-TRANSKODER                                             
024900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I58101                 
025000       MOVE MSG-IDTRANS-2       TO MFS-IDTRANS                            
025100       MOVE MSG-KDMFSFOR-2      TO MFS-KDMFSFOR                           
025200     ELSE                                                                 
025300       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W4I58101                 
025400       MOVE MSG-IDTRANS-1       TO MFS-IDTRANS                            
025500       MOVE MSG-KDMFSFOR-1      TO MFS-KDMFSFOR                           
025600     END-IF                                                               
025700                                                                          
025800     MOVE MSG-KDTRTYP           TO MFS-KDTRTYP                            
025900     MOVE MSG-IDPFK TO MFS-IDPFK                                          
026000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
026100                                                                          
026200     MOVE LOW-VALUE TO MSG-AREA                                           
026300     MOVE 'W4O581N1' TO MFS-IDMOD                                         
026400     MOVE '4581' TO MOD-IDTRANS                                           
026500     MOVE MFS-RENSA-FAELT       TO MOD-TEMFSFEL MOD-TEMFSINF              
026600                                                                          
026700     IF NOT EGEN-MID                                                      
026800       MOVE SPACE               TO MFS-KDTRTYP                            
026900       MOVE '7'                 TO MFS-IDPFK                              
027000       PERFORM AA-MOVE-PLUS-TO-INFIELDS                                   
027100     END-IF                                                               
028400     .                                                                    
028500     EJECT                                                                
028600 AA-MOVE-PLUS-TO-INFIELDS SECTION.                                        
028700                                                                          
028800     MOVE '+++++++++++++++++++++++++++++++' TO MID-UPPDAT-TRPID           
028900     MOVE '+++++++++++++++++++++++++++'     TO MID-UPPDAT-TRPAVG          
029000     .                                                                    
029100 B-KOLLA-NYCKLAR SECTION.                                                 
029200                                                                          
029210     MOVE ALL '+'               TO MSGI-WMSGINIT                          
029220     MOVE '001'                 TO MSGI-KDCALL                            
029230     MOVE MSG-SIGNON-USERID     TO MSGI-IDUSER                            
029240     MOVE '4581'                TO MSGI-IDTRANS                           
029250     MOVE MSG-LTERM-NAME        TO MSGI-IDLTERM-USER                      
029260     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
029270                                                                          
029272     MOVE MSGI-IDLAND-SPR       TO MED-IDSKYLT                            
029280     MOVE MSGI-IDTIDZON         TO WS-IDTIDZON                            
029290                                                                          
029300     MOVE JA                    TO NYCKLAR-SW  INDATA-SW  ALLT-SW         
029400     MOVE NEJ                   TO UPPDAT-SW                              
029500     MOVE MFS-RENSA-FAELT       TO MOD-IDTRP-IN                           
029600                                   MOD-BETRPDST-IN                        
029700     IF MID-IDTRP-IN = ALL '+'                                            
029800       MOVE MID-IDTRP-UT        TO WS-IDTRP                               
029900       MOVE NEJ                 TO NYA-NYCKLAR-SW                         
030000     ELSE                                                                 
030100       MOVE MID-IDTRP-IN        TO WS-IDTRP                               
030200       MOVE SPACE               TO MFS-KDTRTYP                            
030300       MOVE JA                  TO NYA-NYCKLAR-SW                         
030400     END-IF                                                               
030500                                                                          
030600     IF WS-IDTRP NUMERIC AND WS-IDTRP IS > ZERO                           
030700       INSPECT WS-IDTRP REPLACING LEADING ZERO BY SPACE                   
030800       MOVE WS-IDTRP            TO W-4432-IDTRP                           
030900     ELSE                                                                 
031000       INSPECT WS-IDTRP REPLACING LEADING ZERO BY SPACE                   
031100       MOVE NEJ                 TO NYCKLAR-SW                             
031200     END-IF                                                               
031201                                                                          
031202     IF MID-BETRPDST-IN = ALL '+'                                         
031203       IF NYA-NYCKLAR                                                     
031204         MOVE MFS-RENSA-FAELT   TO MOD-BETRPDST-UT                        
031205       ELSE                                                               
031206         MOVE MID-BETRPDST-UT   TO WS-BETRPDST                            
031207       END-IF                                                             
031208     ELSE                                                                 
031209       MOVE MID-BETRPDST-IN     TO WS-BETRPDST                            
031210       INSPECT WS-BETRPDST REPLACING LEADING ZERO BY SPACE                
031211     END-IF                                                               
031212                                                                          
031220     IF MID-IDDC-IN = ALL '+'                                             
031221       IF MID-IDDC-UT = SPACE                                             
031222         MOVE NEJ                     TO NYCKLAR-SW                       
031223       ELSE                                                               
031224         MOVE MID-IDDC-UT             TO WS-IDDC                          
031225       END-IF                                                             
031250     ELSE                                                                 
031260       MOVE MID-IDDC-IN         TO WS-IDDC                                
031261       MOVE '7'                 TO MFS-IDPFK                              
031270       MOVE SPACE               TO MFS-KDTRTYP                            
031290     END-IF                                                               
031291                                                                          
031292     IF WS-IDDC IS > SPACE                                                
031295       MOVE WS-IDDC             TO W-4431-IDDC                            
031296                                   W-4433-IDDC                            
031297     ELSE                                                                 
031299       MOVE NEJ                 TO NYCKLAR-SW                             
031300     END-IF                                                               
032400                                                                          
032500     IF GODK-MID                                                          
032600       MOVE WS-IDTRP            TO MOD-IDTRP-UT                           
032700       MOVE WS-BETRPDST         TO MOD-BETRPDST-UT                        
032710       MOVE WS-IDDC             TO MOD-IDDC-UT                            
032800       MOVE MFS-RENSA-FAELT     TO MOD-IDTRP-IN MOD-BETRPDST-IN           
032810                                   MOD-IDDC-IN                            
032900     ELSE                                                                 
033000       MOVE MFS-RENSA-FAELT     TO MOD-IDTRP-UT MOD-BETRPDST-UT           
033010                                   MOD-IDDC-UT                            
033100     END-IF                                                               
033200                                                                          
033300     IF NYCKLAR-FEL                                                       
033400       MOVE '401'               TO MED-IDMFSFEL                           
033500       PERFORM S01-FEL-RUTIN                                              
033600     END-IF                                                               
033700     .                                                                    
033800     EJECT                                                                
033900 C-LAES-DB-WLXXKA SECTION.                                                
034000                                                                          
034100     PERFORM IMS-GET-WLXXKA                                               
034200     IF SEGMENT-FINNS                                                     
034300       MOVE JA              TO TRPID-SW                                   
034400       PERFORM CA-FLYTTA-TRPIDINFO                                        
034500     ELSE                                                                 
034600       MOVE NEJ             TO ALLT-SW TRPID-SW                           
034700       IF MFS-UPDATE                                                      
034800         CONTINUE                                                         
034900       ELSE                                                               
035000         MOVE '005'          TO MED-IDMFSFEL                              
035100         PERFORM S01-FEL-RUTIN                                            
035200       END-IF                                                             
035300     END-IF                                                               
035400     .                                                                    
035500 CA-FLYTTA-TRPIDINFO SECTION.                                             
035600                                                                          
035700       MOVE 4432-IDTRP         TO MOD-IDTRP-UT                            
035800       MOVE 4432-BETRPDST      TO MOD-BETRPDST-UT                         
035900       MOVE 4432-KVLASTTI      TO MOD-KVLASTTI-UT                         
036000       MOVE 4432-KVADMFL       TO MOD-KVADMFL-UT                          
036100       MOVE 4432-KVADMEL       TO MOD-KVADMEL-UT                          
036200       MOVE 4432-BETRPDST      TO MOD-BETRPDST                            
036300       .                                                                  
036400 D-KONTROLL-INDATA SECTION.                                               
036500                                                                          
036600       PERFORM DA-TRPID-GENERELL-KONTROLL                                 
036700       IF MID-UPPDAT-TRPAVG NOT = ALL '+'                                 
036800         PERFORM S04-KONTROLL-TRPAVG                                      
036900       END-IF                                                             
037000       IF INDATA-FEL                                                      
037100         MOVE NEJ               TO ALLT-SW                                
037200         MOVE '409'             TO MED-IDMFSFEL                           
037300         PERFORM S02-FEL-RUTIN-ROER-EJ-FAELT                              
037400       ELSE                                                               
037500         MOVE JA                TO UPPDAT-TRPID-SW                        
037600       END-IF                                                             
037700     .                                                                    
037800     EJECT                                                                
037900 DA-TRPID-GENERELL-KONTROLL SECTION.                                      
038000                                                                          
038100       MOVE WS-IDTRP                     TO  4432-IDTRP                   
038200       IF MID-KVLASTTI-IN = ALL '+'                                       
038300         MOVE ZERO                       TO WS-KVLASTTI                   
038400       ELSE                                                               
038500         MOVE MID-KVLASTTI-IN            TO DEC-IDFRIDATA                 
038600         PERFORM S06-FORMATERING-MED-RDECDATA                             
038700                                                                          
038800         IF DEC-KDSVAR-OK                                                 
038900           MOVE MFS-ALFA-FAELT-RAETT     TO MOD-KVLASTTI-IN-ATTR          
039000           MOVE DEC-IDEDITDATA           TO WS-KVLASTTI                   
039100         ELSE                                                             
039200           MOVE MFS-ALFA-FAELT-FEL       TO MOD-KVLASTTI-IN-ATTR          
039300           MOVE NEJ TO INDATA-SW                                          
039400         END-IF                                                           
039500       END-IF                                                             
039600                                                                          
039700       IF MID-KVADMFL-IN = ALL '+'                                        
039800         MOVE ZERO                       TO WS-KVADMFL                    
039900       ELSE                                                               
040000         MOVE MID-KVADMFL-IN             TO DEC-IDFRIDATA                 
040100         PERFORM S06-FORMATERING-MED-RDECDATA                             
040200                                                                          
040300         IF DEC-KDSVAR-OK                                                 
040400           MOVE MFS-ALFA-FAELT-RAETT   TO MOD-KVADMFL-IN-ATTR             
040500           MOVE DEC-IDEDITDATA           TO WS-KVADMFL                    
040600         ELSE                                                             
040700           MOVE MFS-ALFA-FAELT-FEL     TO MOD-KVADMFL-IN-ATTR             
040800           MOVE NEJ TO INDATA-SW                                          
040900         END-IF                                                           
041000       END-IF                                                             
041100                                                                          
041200       IF MID-KVADMEL-IN = ALL '+'                                        
041300         MOVE ZERO                       TO WS-KVADMEL                    
041400       ELSE                                                               
041500         MOVE MID-KVADMEL-IN             TO DEC-IDFRIDATA                 
041600         PERFORM S06-FORMATERING-MED-RDECDATA                             
041700                                                                          
041800         IF DEC-KDSVAR-OK                                                 
041900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KVADMEL-IN-ATTR               
042000           MOVE DEC-IDEDITDATA           TO WS-KVADMEL                    
042100         ELSE                                                             
042200           MOVE MFS-ALFA-FAELT-FEL    TO MOD-KVADMEL-IN-ATTR              
042300           MOVE NEJ                      TO INDATA-SW                     
042400         END-IF                                                           
042500       END-IF                                                             
042600                                                                          
042700       IF MID-BETRPDST NOT = ALL '+'                                      
042800         MOVE MID-BETRPDST              TO WS-BETRPDST                    
042900       END-IF                                                             
043000       .                                                                  
043100       EJECT                                                              
043200 E-UPPDATERA SECTION.                                                     
043300                                                                          
043400     IF MID-FLTABORT-TRP-IN NOT = '+' OR                                  
043500       MID-FLTABORT-AVG-IN NOT = '+'                                      
043600       PERFORM EA-BORTTAG-TRPID-ELLER-TRPAVG                              
043700     ELSE                                                                 
043800       IF UPPDAT-TRPID-OK AND                                             
043900         MID-UPPDAT-TRPAVG NOT = ALL '+'                                  
044000         PERFORM EB-UPPDATERA-TRPID-OCH-TRPAVG                            
044100       ELSE                                                               
044200         IF UPPDAT-TRPID-OK                                               
044300           PERFORM EC-UPPDATERA-TRPID                                     
044400         ELSE                                                             
044500           IF MID-UPPDAT-TRPAVG NOT = ALL '+'                             
044600             PERFORM ED-UPPDATERA-TRPAVG                                  
044700           ELSE                                                           
044800             MOVE '414'            TO MED-IDMFSINF                        
044900             CALL WMEDKONV USING MED-WMEDAREA                             
045000             MOVE MED-MFSINF       TO MOD-TEMFSINF                        
045100             PERFORM MFS-ROER-EJ-FAELT-UT                                 
045200             PERFORM MFS-ROER-EJ-FAELT-IN                                 
045300           END-IF                                                         
045400         END-IF                                                           
045500       END-IF                                                             
045600     END-IF                                                               
045700                                                                          
045800     IF INDATA-FEL                                                        
045900       MOVE NEJ                      TO ALLT-SW UPPDAT-SW                 
046000       MOVE '409'                    TO MED-IDMFSFEL                      
046100       PERFORM S02-FEL-RUTIN-ROER-EJ-FAELT                                
046200     ELSE                                                                 
046300       IF FLTABORT-KOD-FEL                                                
046400         MOVE NEJ                    TO ALLT-SW UPPDAT-SW                 
046500         PERFORM S02-FEL-RUTIN-ROER-EJ-FAELT                              
046600       ELSE                                                               
046700         MOVE JA                     TO UPPDAT-SW ALLT-SW                 
046800         PERFORM MFS-RENSA-FAELT-IN                                       
046900       END-IF                                                             
047000     END-IF                                                               
047100     .                                                                    
047200     EJECT                                                                
047300 EA-BORTTAG-TRPID-ELLER-TRPAVG SECTION.                                   
047400     IF MID-FLTABORT-AVG-IN NOT = '+'                                     
047500       IF (MID-FLTABORT-AVG-IN = 'J') OR                                  
047600         (MID-FLTABORT-AVG-IN = 'Y')                                      
047700         PERFORM EAA-BORTTAG-AV-TRPAVG                                    
047800       ELSE                                                               
047900         MOVE MFS-ALFA-FAELT-FEL     TO MOD-FLTABORT-AVG-UT-ATTR          
048000         MOVE JA                     TO FLTABORT-SW                       
048100         MOVE '409'                  TO MED-IDMFSFEL                      
048200       END-IF                                                             
048300     END-IF                                                               
048400     IF MID-FLTABORT-TRP-IN NOT = '+'                                     
048500       IF (MID-FLTABORT-TRP-IN = 'J') OR                                  
048600         (MID-FLTABORT-TRP-IN = 'Y')                                      
048700         PERFORM EAB-BORTTAG-AV-TRPID                                     
048800       ELSE                                                               
048900         MOVE MFS-ALFA-FAELT-FEL     TO MOD-FLTABORT-TRP-UT-ATTR          
049000         MOVE JA                     TO FLTABORT-SW                       
049100         MOVE '409'                  TO MED-IDMFSFEL                      
049200       END-IF                                                             
049300     END-IF                                                               
049400     .                                                                    
049500 EAA-BORTTAG-AV-TRPAVG SECTION.                                           
049600     PERFORM S08-LAES-WLXXKB11-4434                                       
049700     IF TRPAVG-FINNS                                                      
049800       PERFORM IMS-DLET-WLXXKB                                            
049900       MOVE MID-TITRPAVG-ENTER       TO W-4434-TITRPAVG                   
050000     ELSE                                                                 
050100       MOVE '005'                    TO MED-IDMFSFEL                      
050200       MOVE JA                       TO FLTABORT-SW                       
050300     END-IF                                                               
050400     .                                                                    
050500 EAB-BORTTAG-AV-TRPID SECTION.                                            
050600     IF TRPID-FINNS                                                       
050700       PERFORM S08-LAES-WLXXKB11-4434                                     
050800       IF TRPAVG-FINNS                                                    
050900         MOVE '007'                  TO MED-IDMFSFEL                      
051000         MOVE JA                     TO FLTABORT-SW                       
051100       ELSE                                                               
051200         PERFORM IMS-GET-WLXXKA                                           
051300         PERFORM IMS-DLET-WLXXKA                                          
051400       END-IF                                                             
051500     END-IF                                                               
051600     .                                                                    
051700 EB-UPPDATERA-TRPID-OCH-TRPAVG SECTION.                                   
051800                                                                          
051900     IF TRPID-FINNS                                                       
052000       PERFORM S08-LAES-WLXXKB11-4434                                     
052100       IF TRPAVG-FINNS                                                    
052200         PERFORM S14-KONTROLL-RAD19                                       
052300       ELSE                                                               
052400         CONTINUE                                                         
052500       END-IF                                                             
052600     END-IF                                                               
052700                                                                          
052800     IF INDATA-OK                                                         
052900       PERFORM IMS-GET-WLXXKA                                             
053000       IF TRPID-FINNS                                                     
053100         PERFORM S09-AENDRA-4432                                          
053200         PERFORM IMS-REPL-WLXXKA                                          
053300       ELSE                                                               
053400         PERFORM S10-NYREG-4432                                           
053500         PERFORM IMS-ISRT-WLXXKA                                          
053600       END-IF                                                             
053700                                                                          
053800       PERFORM IMS-GET-WLXXKB                                             
053900       IF TRPAVG-FINNS                                                    
054000         PERFORM S11-AENDRA-4434                                          
054100         PERFORM IMS-REPL-WLXXKB                                          
054200       ELSE                                                               
054300         PERFORM S12-NYREG-4434                                           
054400         PERFORM IMS-ISRT-WLXXKB                                          
054500       END-IF                                                             
054600     END-IF                                                               
054700     .                                                                    
054800     EJECT                                                                
054900 EC-UPPDATERA-TRPID SECTION.                                              
055000                                                                          
055100     IF TRPID-FINNS                                                       
055200       PERFORM ECA-TRPID-AENDRING                                         
055300     ELSE                                                                 
055400       PERFORM ECB-TRPID-NYREGISTRERING                                   
055500     END-IF                                                               
055600     .                                                                    
055700 ECA-TRPID-AENDRING SECTION.                                              
055800                                                                          
055900     PERFORM S09-AENDRA-4432                                              
056000     PERFORM IMS-REPL-WLXXKA                                              
056100     .                                                                    
056200 ECB-TRPID-NYREGISTRERING SECTION.                                        
056300                                                                          
056400     IF INDATA-OK                                                         
056500       PERFORM S10-NYREG-4432                                             
056600       PERFORM IMS-ISRT-WLXXKA                                            
056700     END-IF                                                               
056800     .                                                                    
056900 ED-UPPDATERA-TRPAVG SECTION.                                             
057000                                                                          
057100     PERFORM S08-LAES-WLXXKB11-4434                                       
057200     IF TRPAVG-FINNS                                                      
057300       PERFORM S14-KONTROLL-RAD19                                         
057400       IF INDATA-OK                                                       
057500         PERFORM EDA-TRPAVG-AENDRING                                      
057600       END-IF                                                             
057700     ELSE                                                                 
057800       PERFORM EDB-TRPAVG-NYREGISTRERING                                  
057900     END-IF                                                               
058000     .                                                                    
058100 EDA-TRPAVG-AENDRING SECTION.                                             
058200                                                                          
058300         PERFORM S11-AENDRA-4434                                          
058400         PERFORM IMS-REPL-WLXXKB                                          
058500     .                                                                    
058600 EDB-TRPAVG-NYREGISTRERING SECTION.                                       
058700                                                                          
058800     PERFORM S04-KONTROLL-TRPAVG                                          
058900     IF INDATA-OK                                                         
059000       PERFORM S12-NYREG-4434                                             
059100       PERFORM IMS-ISRT-WLXXKB                                            
059200     END-IF                                                               
059300     .                                                                    
059400 F-KONTROLL-TRANSTYP SECTION.                                             
059500                                                                          
059600     IF MID-UPPDAT-TRPID = ALL '+' AND                                    
059700       MID-UPPDAT-TRPAVG = ALL '+'                                        
059800       IF MFS-FIRST                                                       
059900         PERFORM FA-FOERSTA-SIDA                                          
060000       ELSE                                                               
060100         IF MFS-NEXT                                                      
060200           PERFORM FB-NAESTA-SIDA                                         
060300         ELSE                                                             
060400           PERFORM FC-SAMMA-SIDA                                          
060500         END-IF                                                           
060600       END-IF                                                             
060700     ELSE                                                                 
060800       MOVE NEJ                      TO INDATA-SW                         
060900       MOVE MID-TITRPAVG-ENTER       TO W-4434-TITRPAVG                   
061000       PERFORM FD-KONTROLL-INDATA-FAELT                                   
061100       MOVE '003' TO MED-IDMFSFEL                                         
061200       PERFORM S02-FEL-RUTIN-ROER-EJ-FAELT                                
061300     END-IF                                                               
061400     .                                                                    
061500     EJECT                                                                
061600 FA-FOERSTA-SIDA SECTION.                                                 
061700                                                                          
061800     MOVE ZERO                       TO W-4434-TITRPAVG                   
061900     .                                                                    
062000     EJECT                                                                
062100 FB-NAESTA-SIDA SECTION.                                                  
062200                                                                          
062300     MOVE MID-TITRPAVG-NEXT          TO W-4434-TITRPAVG                   
062400     .                                                                    
062500 FC-SAMMA-SIDA SECTION.                                                   
062600                                                                          
062700     MOVE MID-TITRPAVG-ENTER         TO W-4434-TITRPAVG                   
062800     .                                                                    
062900 FD-KONTROLL-INDATA-FAELT SECTION.                                        
063000                                                                          
063100       IF MID-KVLASTTI-IN NOT = ALL '+'                                   
063200         MOVE MFS-ADD-LAES-IN-FAELT-HI TO MOD-KVLASTTI-IN-ATTR            
063300       END-IF                                                             
063400                                                                          
063500       IF MID-KVADMFL-IN NOT = ALL '+'                                    
063600         MOVE MFS-ADD-LAES-IN-FAELT-HI TO MOD-KVADMFL-IN-ATTR             
063700       END-IF                                                             
063800                                                                          
063900       IF MID-KVADMEL-IN NOT = ALL '+'                                    
064000         MOVE MFS-ADD-LAES-IN-FAELT-HI TO MOD-KVADMEL-IN-ATTR             
064100       END-IF                                                             
064200                                                                          
064300       IF MID-BETRPDST NOT = ALL '+'                                      
064400         MOVE MFS-ADD-LAES-IN-FAELT-HI TO MOD-BETRPDST-ATTR               
064500       END-IF                                                             
064600                                                                          
064700       IF MID-FLTABORT-TRP-IN NOT = ALL '+'                               
064800         MOVE MFS-ADD-LAES-IN-FAELT-HI TO MOD-FLTABORT-TRP-UT-ATTR        
064900       END-IF                                                             
065000                                                                          
065100       IF MID-TITRPAVG-IN NOT = ALL '+'                                   
065200         MOVE MFS-ADD-LAES-IN-FAELT-HI TO MOD-TITRPAVG-IN-ATTR            
065300       END-IF                                                             
065400                                                                          
065500       IF MID-BETRPFIR-IN NOT = ALL '+'                                   
065600         MOVE MFS-ADD-LAES-IN-FAELT-HI TO MOD-BETRPFIR-IN-ATTR            
065700       END-IF                                                             
065800                                                                          
065900       IF MID-KDFARLIG-IN NOT = ALL '+'                                   
066000         MOVE MFS-ADD-LAES-IN-FAELT-HI TO MOD-KDFARLIG-IN-ATTR            
066100       END-IF                                                             
066200                                                                          
066300       IF MID-VLTRPMIN-IN NOT = ALL '+'                                   
066400         MOVE MFS-ADD-LAES-IN-FAELT-HI TO MOD-VLTRPMIN-IN-ATTR            
066500       END-IF                                                             
066600                                                                          
066700       IF MID-FLTABORT-AVG-IN NOT = ALL '+'                               
066800         MOVE MFS-ADD-LAES-IN-FAELT-HI TO MOD-FLTABORT-AVG-UT-ATTR        
066900       END-IF                                                             
067000       .                                                                  
067100       EJECT                                                              
067200 G-LAES-VISA-BILD SECTION.                                                
067300                                                                          
067400       MOVE +0                       TO INDX                              
067500       MOVE WS-IDTRP                 TO W-4434-IDTRP                      
067600       MOVE LOW-VALUE                TO W-4434-LOW-VALUE                  
067700       PERFORM IMS-GET-GU-WLXXKB                                          
067800       PERFORM IMS-GET-GNP-FIRST-WLXXKB11                                 
067900                                                                          
068000       PERFORM UNTIL INDX > MAX-INDX                                      
068100         IF SEGMENT-FINNS AND                                             
068200           W-4434-IDTRP = 4434-IDTRP                                      
068300           IF INDX = +0                                                   
068400             PERFORM GA-FLYTTA-TRPAVG-RAD1                                
068500           ELSE                                                           
068600             PERFORM GB-FLYTTA-TRPAVG-RAD                                 
068700           END-IF                                                         
068800           PERFORM IMS-GET-GNP-WLXXKB11                                   
068900           MOVE JA  TO TRPAVG-SW                                          
069000         ELSE                                                             
069100           MOVE NEJ TO TRPAVG-SW                                          
069200           IF INDX = +0                                                   
069300             MOVE ZERO               TO MOD-TITRPAVG-ENTER                
069400           ELSE                                                           
069500             MOVE MFS-RENSA-FAELT    TO MOD-TITRPAVG-RAD(INDX)            
069600           END-IF                                                         
069700         END-IF                                                           
069800         ADD +1 TO INDX                                                   
069900       END-PERFORM                                                        
070000                                                                          
070100       IF TRPAVG-FINNS                                                    
070200         IF W-4434-IDTRP = 4434-IDTRP                                     
070300           MOVE 4434-TITRPAVG        TO MOD-TITRPAVG-NEXT                 
070400         ELSE                                                             
070500           MOVE MOD-TITRPAVG-ENTER   TO MOD-TITRPAVG-NEXT                 
070600         END-IF                                                           
070700         IF MFS-FIRST                                                     
070800           MOVE '006'                TO MED-IDMFSINF                      
070900         ELSE                                                             
071000           IF INDX > MAX-INDX                                             
071100             MOVE '105'              TO MED-IDMFSINF                      
071200           END-IF                                                         
071300         END-IF                                                           
071400         CALL WMEDKONV USING MED-WMEDAREA                                 
071500         MOVE MED-MFSINF             TO MOD-TEMFSINF                      
071600       ELSE                                                               
071700         MOVE MOD-TITRPAVG-ENTER     TO MOD-TITRPAVG-NEXT                 
071800       END-IF                                                             
071900                                                                          
072000       IF UPPDAT-OK                                                       
072100         MOVE '101'                  TO MED-IDMFSINF                      
072200         PERFORM S13-INF-RUTIN                                            
072300       END-IF                                                             
072400     .                                                                    
072500     EJECT                                                                
072600 GA-FLYTTA-TRPAVG-RAD1 SECTION.                                           
072601                                                                          
072700       MOVE 4434-TITRPAVG            TO MOD-TITRPAVG-UT                   
072800       MOVE 4434-BETRPFIR            TO MOD-BETRPFIR-UT                   
072900       MOVE 4434-KDFARLIG            TO MOD-KDFARLIG-UT                   
073000       INSPECT MOD-KDFARLIG-UT REPLACING LEADING ZERO BY SPACE            
073100       MOVE 4434-VLTRPMIN            TO MOD-VLTRPMIN-UT                   
073200       MOVE 4434-TITRPAVG            TO MOD-TITRPAVG-ENTER                
073300       .                                                                  
073400 GB-FLYTTA-TRPAVG-RAD SECTION.                                            
073500                                                                          
073600       MOVE 4434-TITRPAVG            TO MOD-TITRPAVG-RAD(INDX)            
073700       MOVE 4434-BETRPFIR            TO MOD-BETRPFIR-RAD(INDX)            
073800       MOVE 4434-KDFARLIG            TO MOD-KDFARLIG-RAD(INDX)            
073900       INSPECT MOD-KDFARLIG-RAD(INDX) REPLACING                           
074000                                      LEADING ZERO BY SPACE               
074100       MOVE 4434-VLTRPMIN            TO MOD-VLTRPMIN-RAD(INDX)            
074200       .                                                                  
074300     EJECT                                                                
074400 S01-FEL-RUTIN SECTION.                                                   
074500       CALL WMEDKONV USING MED-WMEDAREA                                   
074600       MOVE MED-MFSFEL               TO MOD-TEMFSFEL                      
074700       .                                                                  
074800 S02-FEL-RUTIN-ROER-EJ-FAELT SECTION.                                     
074900       CALL WMEDKONV USING MED-WMEDAREA                                   
075000       MOVE MED-MFSFEL               TO MOD-TEMFSFEL                      
075100       PERFORM MFS-ROER-EJ-FAELT-UT                                       
075200       PERFORM MFS-ROER-EJ-FAELT-IN                                       
075300       .                                                                  
075400       EJECT                                                              
075500 S04-KONTROLL-TRPAVG SECTION.                                             
075600                                                                          
075700       IF MID-TITRPAVG-IN = ALL '+'                                       
075800         MOVE NEJ                       TO INDATA-SW                      
075900         MOVE MFS-NUM-FAELT-FEL         TO MOD-TITRPAVG-IN-ATTR           
076000       ELSE                                                               
076100         IF MID-TITRPAVG-IN NUMERIC                                       
076200           MOVE MID-TITRPAVG-IN         TO TRPAVGTID                      
076300           IF TRPAVGTID-VECKA IS > MAX-VECKA                              
076400             MOVE MFS-NUM-FAELT-FEL     TO MOD-TITRPAVG-IN-ATTR           
076500             MOVE NEJ TO INDATA-SW                                        
076600           ELSE                                                           
076700             ACCEPT DATUM  FROM DATE                                      
076800             MOVE 'AAVV  '              TO DAT-KDDATFORM                  
076900             MOVE W-AR                  TO E-AR                           
077000             MOVE TRPAVGTID-VECKA       TO E-VECKA                        
077100             MOVE EGET-DATUM            TO DAT-I-TIDATUM                  
077200             CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM,            
077300                                 DAT-O-TIDATUM, DAT-KDSVAR                
077400             IF DAT-KDSVAR-OK OR TRPAVGTID-VECKA = ZERO                   
077500               MOVE MFS-NUM-FAELT-RAETT TO MOD-TITRPAVG-IN-ATTR           
077600             ELSE                                                         
077700               MOVE MFS-NUM-FAELT-FEL   TO MOD-TITRPAVG-IN-ATTR           
077800               MOVE NEJ TO INDATA-SW                                      
077900             END-IF                                                       
078000           END-IF                                                         
078100         ELSE                                                             
078200           MOVE MFS-NUM-FAELT-FEL       TO MOD-TITRPAVG-IN-ATTR           
078300           MOVE NEJ TO INDATA-SW                                          
078400         END-IF                                                           
078500                                                                          
078600         IF DAG-OK AND TIMMA-OK AND MINUT-OK                              
078700           MOVE MFS-NUM-FAELT-RAETT   TO MOD-TITRPAVG-IN-ATTR             
078800         ELSE                                                             
078900           MOVE MFS-NUM-FAELT-FEL     TO MOD-TITRPAVG-IN-ATTR             
079000           MOVE NEJ TO INDATA-SW                                          
079100         END-IF                                                           
079200       END-IF                                                             
079300                                                                          
079400         IF MID-BETRPFIR-IN = ALL '+'                                     
079500           CONTINUE                                                       
079600         ELSE                                                             
079700           MOVE MFS-ALFA-FAELT-RAETT  TO MOD-BETRPFIR-IN-ATTR             
079800         END-IF                                                           
079900                                                                          
080000         IF MID-KDFARLIG-IN = ALL '+'                                     
080100           CONTINUE                                                       
080200         ELSE                                                             
080300           IF MID-KDFARLIG-IN NUMERIC                                     
080400             MOVE MFS-NUM-FAELT-RAETT TO MOD-KDFARLIG-IN-ATTR             
080500           ELSE                                                           
080600             MOVE MFS-NUM-FAELT-FEL   TO MOD-KDFARLIG-IN-ATTR             
080700             MOVE NEJ TO INDATA-SW                                        
080800           END-IF                                                         
080900         END-IF                                                           
081000                                                                          
081100         IF MID-VLTRPMIN-IN = ALL '+'                                     
081200           CONTINUE                                                       
081300         ELSE                                                             
081400           IF MID-VLTRPMIN-IN NUMERIC                                     
081500             MOVE MFS-NUM-FAELT-RAETT TO MOD-VLTRPMIN-IN-ATTR             
081600           ELSE                                                           
081700             MOVE MFS-NUM-FAELT-FEL   TO MOD-VLTRPMIN-IN-ATTR             
081800             MOVE NEJ TO INDATA-SW                                        
081900           END-IF                                                         
082000         END-IF                                                           
082100       .                                                                  
082200     EJECT                                                                
082300 S06-FORMATERING-MED-RDECDATA SECTION.                                    
082400                                                                          
082500       MOVE +2                        TO DEC-KVHELTAL                     
082600       MOVE +2                        TO DEC-KVDECIMAL                    
082700       CALL WDECEDIT USING DEC-WDECAREA                                   
082800       .                                                                  
082900     EJECT                                                                
084300 S08-LAES-WLXXKB11-4434 SECTION.                                          
084400                                                                          
084500     MOVE WS-IDTRP              TO W-4434-IDTRP                           
084600     IF MID-FLTABORT-TRP-IN = 'J' OR                                      
084700       MID-FLTABORT-TRP-IN = 'Y'                                          
084800       MOVE MID-TITRPAVG-ENTER  TO W-4434-TITRPAVG                        
084900     ELSE                                                                 
085000       MOVE MID-TITRPAVG-IN     TO W-4434-TITRPAVG                        
085100     END-IF                                                               
085200     MOVE LOW-VALUE             TO W-4434-LOW-VALUE                       
085300                                                                          
085400     PERFORM IMS-GET-WLXXKB                                               
085500                                                                          
085600     IF SEGMENT-FINNS                                                     
085700       MOVE JA                  TO TRPAVG-SW                              
085800     ELSE                                                                 
085900       MOVE NEJ                 TO TRPAVG-SW                              
086000     END-IF                                                               
086100     .                                                                    
086200 S09-AENDRA-4432 SECTION.                                                 
086300                                                                          
086400     IF MID-KVLASTTI-IN NOT = ALL '+'                                     
086500       MOVE WS-KVLASTTI TO 4432-KVLASTTI MOD-KVLASTTI-UT                  
086600     END-IF                                                               
086700                                                                          
086800     IF MID-KVADMFL-IN NOT = ALL '+'                                      
086900       MOVE WS-KVADMFL TO 4432-KVADMFL  MOD-KVADMFL-UT                    
087000     END-IF                                                               
087100                                                                          
087200     IF MID-KVADMEL-IN NOT = ALL '+'                                      
087300       MOVE WS-KVADMEL TO 4432-KVADMEL  MOD-KVADMEL-UT                    
087400     END-IF                                                               
087500                                                                          
087600     IF MID-BETRPDST NOT = ALL '+'                                        
087700       MOVE WS-BETRPDST TO 4432-BETRPDST  MOD-BETRPDST                    
087800                           MOD-BETRPDST-UT                                
087900     END-IF                                                               
088000     .                                                                    
088100 S10-NYREG-4432 SECTION.                                                  
088200                                                                          
088300     MOVE WS-KVLASTTI       TO 4432-KVLASTTI MOD-KVLASTTI-UT              
088400     MOVE WS-KVADMFL        TO 4432-KVADMFL  MOD-KVADMFL-UT               
088500     MOVE WS-KVADMEL        TO 4432-KVADMEL  MOD-KVADMEL-UT               
088600     MOVE WS-BETRPDST       TO 4432-BETRPDST MOD-BETRPDST                 
088700     .                                                                    
088800 S11-AENDRA-4434 SECTION.                                                 
088900                                                                          
089000     MOVE WS-IDTRP          TO 4434-IDTRP W-4434-IDTRP                    
089100     MOVE LOW-VALUE         TO 4434-LOW-VALUE                             
089200     MOVE MID-TITRPAVG-IN   TO W-4434-TITRPAVG                            
089300                                                                          
089400     IF MID-BETRPFIR-IN NOT = ALL '+'                                     
089500       MOVE MID-BETRPFIR-IN TO 4434-BETRPFIR                              
089600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-BETRPFIR-UT-ATTR                 
089700     END-IF                                                               
089800                                                                          
089900     IF MID-KDFARLIG-IN NOT = ALL '+'                                     
090000       MOVE MID-KDFARLIG-IN TO 4434-KDFARLIG                              
090100       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFARLIG-UT-ATTR                 
090200     END-IF                                                               
090300                                                                          
090400     IF MID-VLTRPMIN-IN NOT = ALL '+'                                     
090500       MOVE MID-VLTRPMIN-IN TO 4434-VLTRPMIN                              
090600       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-VLTRPMIN-UT-ATTR                 
090700     END-IF                                                               
090800     .                                                                    
090900 S12-NYREG-4434 SECTION.                                                  
091000                                                                          
091100     MOVE WS-IDTRP          TO 4434-IDTRP W-4434-IDTRP                    
091200     MOVE LOW-VALUE         TO 4434-LOW-VALUE                             
091300     MOVE MID-TITRPAVG-IN   TO 4434-TITRPAVG W-4434-TITRPAVG              
091400                               MOD-TITRPAVG-UT MOD-TITRPAVG-ENTER         
091500     IF MID-BETRPFIR-IN = ALL '+'                                         
091600       MOVE SPACE TO 4434-BETRPFIR                                        
091700     ELSE                                                                 
091800       MOVE MID-BETRPFIR-IN TO 4434-BETRPFIR MOD-BETRPFIR-UT              
091900     END-IF                                                               
092000                                                                          
092100     IF MID-KDFARLIG-IN = ALL '+'                                         
092200       MOVE ZERO TO 4434-KDFARLIG                                         
092300     ELSE                                                                 
092400       MOVE MID-KDFARLIG-IN TO 4434-KDFARLIG MOD-KDFARLIG-UT              
092500     END-IF                                                               
092600                                                                          
092700     IF MID-VLTRPMIN-IN = ALL '+'                                         
092800       MOVE ZERO TO 4434-VLTRPMIN                                         
092900     ELSE                                                                 
093000       MOVE MID-VLTRPMIN-IN TO 4434-VLTRPMIN MOD-VLTRPMIN-UT              
093100     END-IF                                                               
093200     MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TITRPAVG-UT-ATTR                   
093300                                   MOD-BETRPFIR-UT-ATTR                   
093400                                   MOD-KDFARLIG-UT-ATTR                   
093500                                   MOD-VLTRPMIN-UT-ATTR                   
093600     .                                                                    
093700 S13-INF-RUTIN SECTION.                                                   
093800       CALL WMEDKONV USING MED-WMEDAREA                                   
093900       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
094000       .                                                                  
094100 S14-KONTROLL-RAD19 SECTION.                                              
094200                                                                          
094300     IF MID-BETRPFIR-IN NOT = ALL '+'                                     
094400       MOVE MFS-ALFA-FAELT-RAETT       TO MOD-BETRPFIR-IN-ATTR            
094500     END-IF                                                               
094600                                                                          
094700     IF MID-KDFARLIG-IN NOT = ALL '+'                                     
094800       MOVE MFS-NUM-FAELT-RAETT        TO MOD-KDFARLIG-IN-ATTR            
094900     END-IF                                                               
095000                                                                          
095100     IF MID-VLTRPMIN-IN NOT = ALL '+'                                     
095200       MOVE MFS-NUM-FAELT-RAETT        TO MOD-VLTRPMIN-IN-ATTR            
095300     END-IF                                                               
095400     .                                                                    
096304       EJECT                                                              
096310* --- MFS SEKTIONER ---                                                   
096400     SKIP2                                                                
096500 MFS-RENSA-FAELT-IN SECTION.                                              
096600                                                                          
096700     MOVE MFS-RENSA-FAELT TO MOD-IDTRP-IN                                 
096800                             MOD-BETRPDST-IN                              
096900                             MOD-KVLASTTI-IN                              
097000                             MOD-KVADMFL-IN                               
097100                             MOD-KVADMEL-IN                               
097200                             MOD-FLTABORT-TRP-UT                          
097300                             MOD-TITRPAVG-IN                              
097400                             MOD-BETRPFIR-IN                              
097500                             MOD-KDFARLIG-IN                              
097600                             MOD-VLTRPMIN-IN                              
097700                             MOD-FLTABORT-AVG-UT                          
097800     .                                                                    
097900     EJECT                                                                
098000 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
098100                                                                          
098200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDTRP-UT                               
098300                               MOD-BETRPDST-UT                            
098400                               MOD-TITRPAVG-ENTER                         
098500                               MOD-TITRPAVG-NEXT                          
098600                               MOD-KVLASTTI-UT                            
098700                               MOD-KVADMFL-UT                             
098800                               MOD-KVADMEL-UT                             
098900                               MOD-BETRPDST                               
099000                               MOD-FLTABORT-TRP-UT                        
099100                               MOD-TITRPAVG-UT                            
099200                               MOD-BETRPFIR-UT                            
099300                               MOD-KDFARLIG-UT                            
099400                               MOD-VLTRPMIN-UT                            
099500                               MOD-FLTABORT-AVG-UT                        
099600     MOVE +1 TO INDX                                                      
099700     PERFORM UNTIL INDX > MAX-INDX                                        
099800       MOVE MFS-ROER-EJ-FAELT TO MOD-TITRPAVG-RAD(INDX)                   
099900                                 MOD-BETRPFIR-RAD(INDX)                   
100000                                 MOD-KDFARLIG-RAD(INDX)                   
100100                                 MOD-VLTRPMIN-RAD(INDX)                   
100200       ADD +1 TO INDX                                                     
100300     END-PERFORM                                                          
100400     .                                                                    
100500     SKIP2                                                                
100600 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
100700                                                                          
100800     MOVE MFS-ROER-EJ-FAELT TO MOD-KVLASTTI-IN                            
100900                               MOD-KVADMFL-IN                             
101000                               MOD-KVADMEL-IN                             
101100                               MOD-TITRPAVG-IN                            
101200                               MOD-BETRPFIR-IN                            
101300                               MOD-KDFARLIG-IN                            
101400                               MOD-VLTRPMIN-IN                            
101500     .                                                                    
101600     EJECT                                                                
101700     SKIP2                                                                
101800* --- IMS SEKTIONER ---                                                   
101900     SKIP2                                                                
102000 IMS-GET-MSG SECTION.                                                     
102100                                                                          
102200     MOVE '  QC' TO GODK-STATUSKODER                                      
102300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
102400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
102500     PERFORM IMS-STATUSKONTROLL                                           
102600     .                                                                    
102700     SKIP2                                                                
102800 IMS-INSERT-MSG SECTION.                                                  
102900                                                                          
103000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
103100       MOVE '0' TO MFS-KDHUVOMR                                           
103200     END-IF                                                               
103300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
103400     MOVE SPACE TO GODK-STATUSKODER                                       
103500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
103600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
103700     PERFORM IMS-STATUSKONTROLL                                           
103800     .                                                                    
103900     EJECT                                                                
104000 IMS-GET-WLXXKA SECTION.                                                  
104100                                                                          
104200     STRING 'WLXXKA01(WDGXKEY  =' W-WDGXKEY-4431-X ')'                    
104300          DELIMITED BY SIZE INTO SSA1                                     
104400     STRING 'WLXXKA11(WDGXKEY  =' W-WDGXKEY-4432-X ')'                    
104500          DELIMITED BY SIZE INTO SSA2                                     
104600     MOVE '  GE' TO GODK-STATUSKODER                                      
104700     CALL CBLTDLI USING GHU XXKA-PCB DLI-IO-AREA SSA1 SSA2                
104800     MOVE XXKA-STATUS-CODE TO STATUS-WS                                   
104900     PERFORM IMS-STATUSKONTROLL                                           
105000     .                                                                    
105100     EJECT                                                                
105200 IMS-GET-WLXXKB SECTION.                                                  
105300                                                                          
105400     STRING 'WLXXKB01(WDGXKEY  =' W-WDGXKEY-4433-X ')'                    
105500          DELIMITED BY SIZE INTO SSA1                                     
105600     STRING 'WLXXKB11(WDGXKEY  =' W-WDGXKEY-4434-X ')'                    
105700          DELIMITED BY SIZE INTO SSA2                                     
105800     MOVE '  GE' TO GODK-STATUSKODER                                      
105900     CALL CBLTDLI USING GHU XXKB-PCB DLI-IO-AREA SSA1 SSA2                
106000     MOVE XXKB-STATUS-CODE TO STATUS-WS                                   
106100     PERFORM IMS-STATUSKONTROLL                                           
106200     .                                                                    
106300     SKIP2                                                                
106400 IMS-GET-GU-WLXXKB SECTION.                                               
106500                                                                          
106600     STRING 'WLXXKB01(WDGXKEY  =' W-WDGXKEY-4433-X ')'                    
106700          DELIMITED BY SIZE INTO SSA1                                     
106800     MOVE '  GE' TO GODK-STATUSKODER                                      
106900     CALL CBLTDLI USING GU XXKB-PCB DLI-IO-AREA SSA1                      
107000     MOVE XXKB-STATUS-CODE TO STATUS-WS                                   
107100     PERFORM IMS-STATUSKONTROLL                                           
107200     .                                                                    
107300     SKIP2                                                                
107400 IMS-GET-GNP-WLXXKB11 SECTION.                                            
107500                                                                          
107600     STRING 'WLXXKB11(WDGXKEY =>' W-WDGXKEY-4434-X ')'                    
107700          DELIMITED BY SIZE INTO SSA1                                     
107800     MOVE '  GE' TO GODK-STATUSKODER                                      
107900     CALL CBLTDLI USING GNP XXKB-PCB DLI-IO-AREA SSA1                     
108000     MOVE XXKB-STATUS-CODE TO STATUS-WS                                   
108100     PERFORM IMS-STATUSKONTROLL                                           
108200     .                                                                    
108300     SKIP2                                                                
108400 IMS-GET-GNP-FIRST-WLXXKB11 SECTION.                                      
108500                                                                          
108600     STRING 'WLXXKB11*F(WDGXKEY =>' W-WDGXKEY-4434-X ')'                  
108700          DELIMITED BY SIZE INTO SSA1                                     
108800     MOVE '  GE' TO GODK-STATUSKODER                                      
108900     CALL CBLTDLI USING GNP XXKB-PCB DLI-IO-AREA SSA1                     
109000     MOVE XXKB-STATUS-CODE TO STATUS-WS                                   
109100     PERFORM IMS-STATUSKONTROLL                                           
109200     .                                                                    
109300     SKIP2                                                                
109400 IMS-ISRT-WLXXKA SECTION.                                                 
109500                                                                          
109600     STRING 'WLXXKA01(WDGXKEY  =' W-WDGXKEY-4431-X ')'                    
109700          DELIMITED BY SIZE INTO SSA1                                     
109800     MOVE 'WLXXKA11' TO SSA2                                              
109900     MOVE '  II' TO GODK-STATUSKODER                                      
110000     CALL CBLTDLI USING ISRT XXKA-PCB DLI-IO-AREA SSA1 SSA2               
110100     MOVE XXKA-STATUS-CODE TO STATUS-WS                                   
110200     PERFORM IMS-STATUSKONTROLL                                           
110300     .                                                                    
110400     SKIP2                                                                
110500 IMS-ISRT-WLXXKB SECTION.                                                 
110600                                                                          
110700     STRING 'WLXXKB01(WDGXKEY  =' W-WDGXKEY-4433-X ')'                    
110800          DELIMITED BY SIZE INTO SSA1                                     
110900     MOVE 'WLXXKB11' TO SSA2                                              
111000     MOVE '  II' TO GODK-STATUSKODER                                      
111100     CALL CBLTDLI USING ISRT XXKB-PCB DLI-IO-AREA SSA1 SSA2               
111200     MOVE XXKB-STATUS-CODE TO STATUS-WS                                   
111300     PERFORM IMS-STATUSKONTROLL                                           
111400     .                                                                    
111500     SKIP2                                                                
111600 IMS-REPL-WLXXKA SECTION.                                                 
111700                                                                          
111800     MOVE '  ' TO GODK-STATUSKODER                                        
111900     CALL CBLTDLI USING REPL XXKA-PCB DLI-IO-AREA                         
112000     MOVE XXKA-STATUS-CODE TO STATUS-WS                                   
112100     PERFORM IMS-STATUSKONTROLL                                           
112200     .                                                                    
112300     SKIP2                                                                
112400 IMS-REPL-WLXXKB SECTION.                                                 
112500                                                                          
112600     MOVE '  ' TO GODK-STATUSKODER                                        
112700     CALL CBLTDLI USING REPL XXKB-PCB DLI-IO-AREA                         
112800     MOVE XXKB-STATUS-CODE TO STATUS-WS                                   
112900     PERFORM IMS-STATUSKONTROLL                                           
113000     .                                                                    
113100     SKIP2                                                                
113200 IMS-DLET-WLXXKA SECTION.                                                 
113300                                                                          
113400     MOVE '  ' TO GODK-STATUSKODER                                        
113500     CALL CBLTDLI USING DLET XXKA-PCB DLI-IO-AREA                         
113600     MOVE XXKA-STATUS-CODE TO STATUS-WS                                   
113700     PERFORM IMS-STATUSKONTROLL                                           
113800     .                                                                    
113900     SKIP2                                                                
114000 IMS-DLET-WLXXKB SECTION.                                                 
114100                                                                          
114200     MOVE '  ' TO GODK-STATUSKODER                                        
114300     CALL CBLTDLI USING DLET XXKB-PCB DLI-IO-AREA                         
114400     MOVE XXKB-STATUS-CODE TO STATUS-WS                                   
114500     PERFORM IMS-STATUSKONTROLL                                           
114600     .                                                                    
114700     SKIP2                                                                
114800 IMS-STATUSKONTROLL SECTION.                                              
114900                                                                          
115000     SET STATUS-IX TO 1                                                   
115100     SEARCH GODK-STATUS                                                   
115200       AT END CALL FELLOG                                                 
115300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
115400     END-SEARCH                                                           
115500     .                                                                    
