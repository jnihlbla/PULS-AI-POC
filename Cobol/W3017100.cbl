000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3017100.                                                
000300 AUTHOR.         THOMAS LARSSON.                                          
000400 DATE-WRITTEN.   92/04/23.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        VISAR ANKOMMANDE GODS SORTERAD PÅ DISTRIKT.                      
000900*                                                                         
001000*        PROGRAMMET LÄSER OCH UPPDATERAR WDM6                             
001100                                                                          
001200*    INDATA.                                                              
001300*        TRANSAKTION: W3T171                                              
001400*        MID:         W3I17101                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W3O17101                                            
001800*                                                                         
001900* ETRACKER 2816396/070404.                                                
002000*                                                                         
002100*                                                                         
002200                                                                          
002300                                                                          
002400 ENVIRONMENT DIVISION.                                                    
002500                                                                          
002600                                                                          
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000*    -- CHECKED BY WY2000                                                 
003100                                                                          
003200 77  IDPGM                       PIC X(08)   VALUE 'W3017100'.            
003300 77  WS-SECTION                  PIC X(25) VALUE SPACE.                   
003400 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003500 77  JA                          PIC X       VALUE 'J'.                   
003600 77  NEJ                         PIC X       VALUE 'N'.                   
003700 77  OK                          PIC X       VALUE ' '.                   
003800*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004000 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004100                                                                          
004200 77  PGNO-INDX                   PIC S9(4)  VALUE +0    COMP SYNC.        
004300                                                                          
004400 77  WS-IDDC-1                   PIC X(2)    VALUE SPACE.                 
004600                                                                          
004700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004800     88  INDATA-OK                           VALUE 'J'.                   
004900     88  INDATA-FEL                          VALUE 'N'.                   
005000                                                                          
005100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005200     88  NYCKLAR-OK                          VALUE 'J'.                   
005300     88  NYCKLAR-FEL                         VALUE 'N'.                   
005400                                                                          
005500 77  W-RENSNINGSDATUM            PIC  9(8).                               
005600 77  ALLT-SW                     PIC X       VALUE 'J'.                   
005700     88  ALLT-OK                             VALUE 'J'.                   
005800                                                                          
005900 77  RAD-KOLL-SW                 PIC X       VALUE 'J'.                   
006000     88  RAD-KOLL-OK                         VALUE 'J'.                   
006100     88  RAD-KOLL-FEL                        VALUE 'N'.                   
006200                                                                          
006300 77  BYT-SW                      PIC X       VALUE 'J'.                   
006400     88  BYT-EJ-BILD                         VALUE 'N'.                   
006500                                                                          
006600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006700     88  EGEN-MID                            VALUE '3171'.                
006800     88  GODK-MID                            VALUE '3171' '3172'          
006900                                                   '3173' '3174'          
007000                                                   '3175' '3176'          
007100                                                   '3177' '3178'          
007200                                                   '3179'.                
007300     88  HELP-MID                            VALUE '0551'.                
007400     EJECT                                                                
007500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007600 01  GENERELLA-SUBPROGRAM.                                                
007700     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
007800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008200     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
008300     EJECT                                                                
008400*    --- PARAMETRAR TILL SUBPROGRAM W005WDK7                              
008500 01  FILLER                       PIC X(8)    VALUE 'W005WDK7'.           
008600*   -COPY W005WDK7                                                        
008700     EJECT                                                                
008800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008900*01 -COPY WMEDAREA                                                        
009000                                                                          
009100                                                                          
009200 01  MESSAGE-CODES.                                                       
009300     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
009400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009600     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
009800     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
009900     03  ERR-UPDATE              PIC X(3)    VALUE '007'.                 
010000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010200     03  REPORT-NOT-REGISTRATED  PIC X(3)    VALUE '210'.                 
010300     03  ERR-REPORT-STATUS       PIC X(3)    VALUE '212'.                 
010400     03  WRONG-STATUS            PIC X(3)    VALUE '079'.                 
010500     03  USER-NOT-ALLOWED        PIC X(3)    VALUE '405'.                 
010600     EJECT                                                                
010610 01  -COPY WWDCKONS                                                       
010620     EJECT                                                                
010700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010800*                                                                         
010900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011000                                                                          
011100*01 -COPY WMSGINIT                                                        
011200     EJECT                                                                
011300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011400*                                                                         
011500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011600                                                                          
011700*01  MID -COPY W3I17101                                                   
011800     EJECT                                                                
011900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012000                                                                          
012100*01  -COPY WMSGAREA                                                       
012200     EJECT                                                                
012300     03  MOD REDEFINES MSG-AREA.                                          
012400*      05  -COPY W3O17101                                                 
012500     EJECT                                                                
012600 01  W-PROG-TO-PROG-SW.                                                   
012700     03  M-SW-LL                 PIC S9(4)   VALUE +367 COMP SYNC.        
012800     03  M-SW-Z1-Z2              PIC X(2)    VALUE LOW-VALUE.             
012900     03  M-SW-KDTRANS            PIC X(8)    VALUE 'W3T172  '.            
013000     03  M-SW-IDTRANS            PIC X(4)    VALUE '3171'.                
013100     03  M-SW-KDMFSFOR           PIC X(1)    VALUE '1'.                   
013200                                                                          
013300*    03  MID -COPY W3I17201 -PRE 3172-                                    
013400                                                                          
013500     EJECT                                                                
013600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013700                                                                          
013800*01  -COPY WDAGAREA                                                       
013900*01  -COPY WMFSAREA                                                       
014000     EJECT                                                                
014100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014300                                                                          
014400 01  SPAR-AREA.                                                           
014500   03  SPAR-IDTRANS              PIC X(4)     VALUE '3171'.               
014600   03  SPAR-PGNO                 PIC 9(2)     VALUE ZERO.                 
014700   03  SPAR-PAGE-DATA.                                                    
014800     05 SPAR-PREV-AREA OCCURS 34 TIMES.                                   
014900       07 W-IDDISTR-PREV         PIC 9(4).                                
015000       07 W-IDBYTRAP-PREV        PIC 9(7).                                
015100       07 W-DAREGDAT-PREV        PIC 9(8).                                
015200       07 W-DAANKDAG-PREV        PIC 9(8).                                
015300       07 W-KDBYTSTA-PREV        PIC X   .                                
015400     05 SPAR-NEXT-AREA.                                                   
015500       07 W-IDDISTR-NEXT         PIC 9(4).                                
015600       07 W-IDBYTRAP-NEXT        PIC 9(7).                                
015700       07 W-DAREGDAT-NEXT        PIC 9(8).                                
015800       07 W-DAANKDAG-NEXT        PIC 9(8).                                
015900       07 W-KDBYTSTA-NEXT        PIC X   .                                
016000                                                                          
016100 01  WS-STYR-LAS.                                                         
016200   03  WS-DISTR                  PIC X(1)    VALUE SPACE.                 
016300   03  WS-KUND                   PIC X(1)    VALUE SPACE.                 
016400   03  WS-RAPP                   PIC X(1)    VALUE SPACE.                 
016500   03  WS-STATUS-2459            PIC X(1)    VALUE SPACE.                 
016600   03  WS-STATUS-36              PIC X(1)    VALUE SPACE.                 
016700                                                                          
016800 01  NYCKLAR-TILL-DLI.                                                    
016900   03  FILLER.                                                            
017000     05 W-IDDC               PIC X(2)    VALUE SPACE.                     
017100     05 W-IDDISTR            PIC S9(5)   VALUE ZERO COMP-3.               
017200     05 W-KDBYTSTA           PIC X       VALUE SPACE.                     
017300                                                                          
017400   03  W-IDBYTRAP-X.                                                      
017500     05 W-IDBYTRAP           PIC S9(7)   VALUE ZERO COMP-3.               
017600   03  W-IDKUNDNR-X.                                                      
017700     05 W-IDKUNDNR           PIC S9(7)   VALUE ZERO COMP-3.               
017800                                                                          
017900   03  W-WDM6ASEQ-MIN-X.                                                  
018000     05  W-KDBYTSTA-A1       PIC X(1)    VALUE SPACE.                     
018100     05  W-IDDC-A1           PIC X(2)    VALUE SPACE.                     
018200     05  W-DAREGDAT-A1       PIC 9(8)    VALUE ZERO.                      
018300     05  W-IDDISTR-A1        PIC S9(5)   VALUE ZERO COMP-3.               
018400     05  W-IDBYTRAP-A1       PIC S9(7)   VALUE ZERO COMP-3.               
018500                                                                          
018600   03  W-WDM6ASEQ-MAX-X.                                                  
018700     05  W-KDBYTSTA-A1-MAX   PIC X(1)    VALUE '9'.                       
018800     05  W-IDDC-A1-MAX       PIC X(2)    VALUE SPACE.                     
018900     05  W-DAREGDAT-A1-MAX   PIC 9(8)    VALUE 99999999.                  
019000     05  W-IDDISTR-A1-MAX    PIC S9(5)   VALUE +99999   COMP-3.           
019100     05  W-IDBYTRAP-A1-MAX   PIC S9(7)   VALUE +9999999 COMP-3.           
019200                                                                          
019300   03  W-WDM6BSEQ-MIN-X.                                                  
019400     05  W-IDDC-B1           PIC X(2)    VALUE SPACE.                     
019500     05  W-KDBYTSTA-B1       PIC X(1)    VALUE SPACE.                     
019600     05  W-DAANKDAG-B1       PIC 9(8)    VALUE ZERO.                      
019700     05  W-IDDISTR-B1        PIC S9(5)   VALUE ZERO COMP-3.               
019800     05  W-IDBYTRAP-B1       PIC S9(7)   VALUE ZERO COMP-3.               
019900                                                                          
020000   03  W-WDM6BSEQ-MAX-X.                                                  
020100     05  W-IDDC-B1-MAX       PIC X(2)    VALUE SPACE.                     
020200     05  W-KDBYTSTA-B1-MAX   PIC X(1)    VALUE '9'.                       
020300     05  W-DAANKDAG-B1-MAX   PIC 9(8)    VALUE 99999999.                  
020400     05  W-IDDISTR-B1-MAX    PIC S9(5)   VALUE +99999   COMP-3.           
020500     05  W-IDBYTRAP-B1-MAX   PIC S9(7)   VALUE +9999999 COMP-3.           
020600                                                                          
020700   03  W-WDM6C1KY-MIN-X.                                                  
020800     05  W-IDDC-C1           PIC X(2)    VALUE SPACE.                     
020900     05  W-IDDISTR-C1        PIC S9(5)   VALUE ZERO COMP-3.               
021000     05  W-KDBYTSTA-C1       PIC X(1)    VALUE SPACE.                     
021100     05  W-DAANKDAG-C1       PIC 9(8)    VALUE ZERO.                      
021200     05  W-IDBYTRAP-C1       PIC S9(7)   VALUE ZERO COMP-3.               
021300                                                                          
021400   03  W-WDM6C1KY-MAX-X.                                                  
021500     05  W-IDDC-C1-MAX       PIC X(2)    VALUE SPACE.                     
021600     05  W-IDDISTR-C1-MAX    PIC S9(5)   VALUE +99999   COMP-3.           
021700     05  W-KDBYTSTA-C1-MAX   PIC X(1)    VALUE '9'.                       
021800     05  W-DAANKDAG-C1-MAX   PIC 9(8)    VALUE 99999999.                  
021900     05  W-IDBYTRAP-C1-MAX   PIC S9(7)   VALUE +9999999 COMP-3.           
022000                                                                          
022100   03  W-WDM6D1KY-MIN-X.                                                  
022200     05  W-IDDC-D1           PIC X(2)    VALUE SPACE.                     
022300     05  W-IDDISTR-D1        PIC S9(5)   VALUE ZERO COMP-3.               
022400     05  W-KDBYTSTA-D1       PIC X(1)    VALUE SPACE.                     
022500     05  W-DAREGDAT-D1       PIC 9(8)    VALUE ZERO.                      
022600     05  W-IDBYTRAP-D1       PIC S9(7)   VALUE ZERO COMP-3.               
022700                                                                          
022800   03  W-WDM6D1KY-MAX-X.                                                  
022900     05  W-IDDC-D1-MAX       PIC X(2)    VALUE SPACE.                     
023000     05  W-IDDISTR-D1-MAX    PIC S9(5)   VALUE +99999   COMP-3.           
023100     05  W-KDBYTSTA-D1-MAX   PIC X(1)    VALUE '9'.                       
023200     05  W-DAREGDAT-D1-MAX   PIC 9(8)    VALUE 99999999.                  
023300     05  W-IDBYTRAP-D1-MAX   PIC S9(7)   VALUE +9999999 COMP-3.           
023400                                                                          
023500   03  W-WDM601KY-X.                                                      
023600     05  W-IDDISTR-UNIK      PIC S9(5)   VALUE ZERO COMP-3.               
023700     05  W-IDBYTRAP-UNIK     PIC S9(7)   VALUE ZERO COMP-3.               
023800                                                                          
023900   03  W-IDDC-B6-X.                                                       
024000       05 W-IDDC-B6          PIC X(2).                                    
024100                                                                          
024200   03  W-IDARTNR-K7-X.                                                    
024300       05  W-IDARTNR-K7      PIC S9(9)  VALUE ZERO COMP-3.                
024400                                                                          
024500   03  W-IDDC-K7-X.                                                       
024600       05 W-IDDC-K7          PIC X(2).                                    
024700                                                                          
024800   03  W-IDARTNR-K6-X.                                                    
024900       05  W-IDARTNR-K6      PIC S9(9)  VALUE ZERO COMP-3.                
025000                                                                          
025100*    --- STATUS-KOD FRÅN IMS                                              
025200 01  STATUS-WS                   PIC XX.                                  
025300     88  SEGMENT-FINNS                       VALUE '  '.                  
025400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025500     88  BAS-SLUT                            VALUE 'GB'.                  
025600     SKIP2                                                                
025700 01  GODK-STATUSKODER.                                                    
025800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025900     SKIP3                                                                
026000 01  SSA1                        PIC X(156).                              
026100 01  SSA2                        PIC X(64).                               
026200                                                                          
026300     EJECT                                                                
026400*    --- IMS FUNKTIONSKODER                                               
026500*01  -COPY W0003                                                          
026600     EJECT                                                                
026700*    ---  DLI INPUT-OUTPUT AREA                                           
026800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
026900                                                                          
027000 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDM601'.        
027100 01  DLI-IO-WDM601.                                                       
027200*  03  -COPY WDM601                                                       
027300     EJECT                                                                
027400                                                                          
027500 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDM611'.        
027600                                                                          
027700 01  DLI-IO-WDM611.                                                       
027800*  03  -COPY WDM611                                                       
027900     EJECT                                                                
028000                                                                          
028100 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDM6C1'.        
028200                                                                          
028300 01  DLI-IO-WDM6C1.                                                       
028400*  03  -COPY WDM6C1                                                       
028500     EJECT                                                                
028600                                                                          
028700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDM6D1'.        
028800                                                                          
028900 01  DLI-IO-WDM6D1.                                                       
029000*  03  -COPY WDM6D1                                                       
029100                                                                          
029200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
029300 01   DLI-IO-AREA-B601.                                                   
029400*     03  -COPY WDB601                                                    
029500                                                                          
029600 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK711'.        
029700     SKIP3                                                                
029800 01  DLI-IO-WDK711.                                                       
029900*    03  -COPY WDK711                                                     
030000     EJECT                                                                
030100 LINKAGE SECTION.                                                         
030200*01  -COPY W0009   -PRE MSG-                                              
030300                                                                          
030400*01  -COPY W0009   -PRE ALT-                                              
030500     EJECT                                                                
030600*01  -COPY W0008   -PRE WDP7-                                             
030700     05  FILLER                  PIC X.                                   
030800                                                                          
030900*01  -COPY W0008  -PRE WDM6-                                              
031000     05  FILLER                  PIC X.                                   
031100*01  -COPY W0008  -PRE WDM6A-                                             
031200     05  FILLER                  PIC X.                                   
031300*01  -COPY W0008  -PRE WDM6B-                                             
031400     05  FILLER                  PIC X.                                   
031500*01  -COPY W0008  -PRE WDM6C-                                             
031600     05  FILLER                  PIC X.                                   
031700*01  -COPY W0008  -PRE WDM6D-                                             
031800     05  FILLER                  PIC X.                                   
031900*01  -COPY W0008  -PRE WDB6-                                              
032000     05  FILLER                  PIC X.                                   
032100*01  -COPY W0008  -PRE WDK7-                                              
032200     05  FILLER                  PIC X.                                   
032300*01  -COPY W0008  -PRE WDK6-                                              
032400     05  FILLER                  PIC X.                                   
032500     EJECT                                                                
032600                                                                          
032700 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP7-PCB WDM6-PCB              
032800                 WDM6A-PCB WDM6B-PCB WDM6C-PCB WDM6D-PCB                  
032900                 WDB6-PCB WDK7-PCB WDK6-PCB.                              
033000 MAIN SECTION.                                                            
033100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP7-PCB WDM6-PCB              
033200                 WDM6A-PCB WDM6B-PCB WDM6C-PCB WDM6D-PCB                  
033300                 WDB6-PCB WDK7-PCB WDK6-PCB.                              
033400                                                                          
033500     PERFORM IMS-GET-MSG                                                  
033600     IF SEGMENT-FINNS                                                     
033700       PERFORM A-INIT                                                     
033800       PERFORM B-KOLLA-NYCKLAR                                            
033900       IF NYCKLAR-OK                                                      
034000         IF MFS-UPDATE                                                    
034100           PERFORM G-KOLLA-INPUT                                          
034200           IF INDATA-OK                                                   
034300             PERFORM H-UPPDATERA                                          
034400           END-IF                                                         
034500         ELSE                                                             
034600           IF MFS-FIRST                                                   
034700             PERFORM C-FOERSTA-SIDA                                       
034800           ELSE                                                           
034900             IF MFS-NEXT                                                  
035000               PERFORM D-NAESTA-SIDA                                      
035100             ELSE                                                         
035200               IF MFS-PREVIOUS                                            
035300                 PERFORM I-PREV-PAGE                                      
035400               ELSE                                                       
035500                 PERFORM E-SAMMA-SIDA                                     
035600               END-IF                                                     
035700             END-IF                                                       
035800           END-IF                                                         
035900           IF ALLT-OK                                                     
036000             PERFORM F-LAES-VISA-INFO                                     
036100           END-IF                                                         
036200         END-IF                                                           
036300       END-IF                                                             
036400       IF BYT-EJ-BILD                                                     
036500         COMPUTE MSG-KVLL = LENGTH OF MOD-W3O17101 + 4                    
036600         PERFORM IMS-INSERT-MSG                                           
036700       END-IF                                                             
036800     END-IF                                                               
036900                                                                          
037000     MOVE ZERO TO RETURN-CODE                                             
037100     GOBACK                                                               
037200     .                                                                    
037300     EJECT                                                                
037400 A-INIT SECTION.                                                          
037500     IF MSG-DUBBLA-TRANSKODER                                             
037600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I17101                 
037700       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
037800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
037900     ELSE                                                                 
038000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I17101                  
038100       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
038200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
038300     END-IF                                                               
038400                                                                          
038500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
038600     MOVE MSG-IDPFK TO MFS-IDPFK                                          
038700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
038800                                                                          
038900     MOVE LOW-VALUE TO MSG-AREA                                           
039000     MOVE 'W3O171N1' TO MFS-IDMOD                                         
039100     MOVE '3171' TO MOD-IDTRANS                                           
039200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
039300                                                                          
039400     IF EGEN-MID OR HELP-MID                                              
039500       CONTINUE                                                           
039600     ELSE                                                                 
039700       MOVE SPACE TO MFS-KDTRTYP                                          
039800       MOVE '7' TO MFS-IDPFK                                              
039900     END-IF                                                               
040000*    FÖRBERED RENSNINGSDATUMANROP ANVÄNDS FÖR ATT BEGRÄNSA VISN           
040100*    AV RADER BAKÅT I TIDEN                                               
040200     ACCEPT DAG-TIAAMMDD-TOM FROM DATE                                    
040300     MOVE 365    TO DAG-KVKALDAG                                          
040400     MOVE 003    TO DAG-KDCALL                                            
040500                                                                          
040600     CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA DAG-KDSVAR             
040700                                                                          
040800     IF DAG-KDSVAR = OK                                                   
040900       MOVE DAG-TIAAMMDD-FOM TO W-RENSNINGSDATUM (3:6)                    
041000       MOVE DAG-TISEKEL-FOM  TO W-RENSNINGSDATUM (1:2)                    
041100     ELSE                                                                 
041200       MOVE ZERO             TO W-RENSNINGSDATUM                          
041300     END-IF                                                               
041400     .                                                                    
041500     EJECT                                                                
041600 B-KOLLA-NYCKLAR SECTION.                                                 
041700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
041800     MOVE '001'             TO MSGI-KDCALL                                
041900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
042000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
042100     MOVE '3171'            TO MSGI-IDTRANS                               
042200     IF EGEN-MID                                                          
042300       MOVE MID-IDDISTR-IN  TO MSGI-IDDISTR                               
042400       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
042500       MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                              
042600       MOVE MID-KDBYTSTA-IN TO MSGI-KDBYTSTA                              
042700       MOVE MID-IDBYTRAP-IN TO MSGI-IDBYTRAP                              
042800     END-IF                                                               
042900     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
043000     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
043100                                                                          
043200     IF MSGI-IDLAND-SPR = 'SE'                                            
043300       MOVE 'S  ' TO MED-IDSKYLT                                          
043400     ELSE                                                                 
043500       MOVE 'GB ' TO MED-IDSKYLT                                          
043600     END-IF                                                               
043700     MOVE JA TO NYCKLAR-SW                                                
043800     MOVE NEJ TO BYT-SW                                                   
043900                                                                          
044000*    -- KONTROLL AV IDDISTR                                               
044100                                                                          
044200     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
044300                                                                          
044400     IF MID-IDDISTR-IN NOT = ALL '+'                                      
044500       MOVE SPACE TO MFS-KDTRTYP                                          
044600       MOVE '7' TO MFS-IDPFK                                              
044700     END-IF                                                               
044800                                                                          
044900     INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO                 
045000     IF MSGI-IDDISTR NUMERIC                                              
045100       MOVE MSGI-IDDISTR TO W-IDDISTR                                     
045200                            W-IDDISTR-C1                                  
045300                            W-IDDISTR-C1-MAX                              
045400                            W-IDDISTR-D1                                  
045500                            W-IDDISTR-D1-MAX                              
045600     ELSE                                                                 
045700       MOVE NEJ TO NYCKLAR-SW                                             
045800     END-IF                                                               
045900                                                                          
046000*    -- KONTROLL AV IDKUNDNR                                              
046100                                                                          
046200     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
046300                                                                          
046400     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
046500       MOVE SPACE TO MFS-KDTRTYP                                          
046600       MOVE '7' TO MFS-IDPFK                                              
046700     END-IF                                                               
046800                                                                          
046900     INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO                
047000     IF MSGI-IDKUNDNR NUMERIC                                             
047100       MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                   
047200     ELSE                                                                 
047300       MOVE NEJ TO NYCKLAR-SW                                             
047400     END-IF                                                               
047500                                                                          
047600*    -- KONTROLL AV IDDC                                                  
047700                                                                          
047800     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
047900                                                                          
048000     IF MID-IDDC-IN NOT = ALL '+'                                         
048100       MOVE SPACE TO MFS-KDTRTYP                                          
048200       MOVE '7' TO MFS-IDPFK                                              
048300     END-IF                                                               
048400                                                                          
048500*    INSPECT MSGI-IDDC-KEY REPLACING LEADING SPACE BY ZERO                
048600                                                                          
048700     MOVE MSGI-IDDC-KEY    TO W-IDDC-B6                                   
048800     PERFORM IMS-GU-WDB601                                                
048900     IF DCS-KDDC = SPACE OR DCS-DDC                                       
049000       MOVE MSGI-IDDC TO MSGI-IDDC-KEY                                    
049100     END-IF                                                               
049200     MOVE MSGI-IDDC-KEY TO W-IDDC                                         
049300     MOVE MSGI-IDDC     TO WS-IDDC-1                                      
049400     IF DCS-SDC AND DCS-ENGLAND                                           
049500       MOVE WC-SDC-NL-ET TO WS-IDDC-1 W-IDDC                              
049600     END-IF                                                               
049700     IF     MSGI-IDUSER = 'MWGB112 '                                      
049800         OR MSGI-IDUSER = 'MWGB216 '                                      
049900         OR MSGI-IDUSER = 'MWGB219 '                                      
050000       MOVE WC-SDC-NL-ET TO WS-IDDC-1 W-IDDC                              
050100     END-IF                                                               
050200                                                                          
050300     MOVE W-IDDC TO W-IDDC-A1                                             
050400                    W-IDDC-A1-MAX                                         
050500                    W-IDDC-B1                                             
050600                    W-IDDC-B1-MAX                                         
050700                    W-IDDC-C1                                             
050800                    W-IDDC-C1-MAX                                         
050900                    W-IDDC-D1                                             
051000                    W-IDDC-D1-MAX                                         
051100                                                                          
051200*    -- KONTROLL AV IDBYTRAP                                              
051300                                                                          
051400     MOVE MFS-RENSA-FAELT TO MOD-IDBYTRAP-IN                              
051500                                                                          
051600     IF MID-IDBYTRAP-IN NOT = ALL '+'                                     
051700       MOVE SPACE TO MFS-KDTRTYP                                          
051800       MOVE '7' TO MFS-IDPFK                                              
051900     END-IF                                                               
052000                                                                          
052100     INSPECT MSGI-IDBYTRAP REPLACING LEADING SPACE BY ZERO                
052200     IF MSGI-IDBYTRAP NUMERIC                                             
052300       MOVE MSGI-IDBYTRAP TO W-IDBYTRAP                                   
052400     ELSE                                                                 
052500       MOVE NEJ TO NYCKLAR-SW                                             
052600     END-IF                                                               
052700                                                                          
052800*    -- KONTROLL AV KDBYTSTA                                              
052900                                                                          
053000     MOVE MFS-RENSA-FAELT TO MOD-KDBYTSTA-IN                              
053100                                                                          
053200     IF MID-KDBYTSTA-IN NOT = ALL '+'                                     
053300       MOVE SPACE TO MFS-KDTRTYP                                          
053400       MOVE '7' TO MFS-IDPFK                                              
053500     END-IF                                                               
053600                                                                          
053700     IF MSGI-KDBYTSTA = '2' OR '3' OR '4' OR '9'                          
053800       MOVE MSGI-KDBYTSTA TO W-KDBYTSTA                                   
053900                             W-KDBYTSTA-A1                                
054000                             W-KDBYTSTA-A1-MAX                            
054100                             W-KDBYTSTA-B1                                
054200                             W-KDBYTSTA-B1-MAX                            
054300                             W-KDBYTSTA-C1                                
054400                             W-KDBYTSTA-C1-MAX                            
054500                             W-KDBYTSTA-D1                                
054600                             W-KDBYTSTA-D1-MAX                            
054700     ELSE                                                                 
054800       MOVE NEJ TO NYCKLAR-SW                                             
054900     END-IF                                                               
055000                                                                          
055100     IF W-KDBYTSTA = '9'                                                  
055200       MOVE W-RENSNINGSDATUM     TO W-DAREGDAT-A1                         
055300                                    W-DAREGDAT-D1                         
055400     END-IF                                                               
055500                                                                          
055600     IF MSGI-IDDISTR = ZERO                                               
055700         AND (MSGI-IDKUNDNR NOT = ZERO                                    
055800           OR MSGI-IDBYTRAP NOT = ZERO)                                   
055900       MOVE NEJ TO NYCKLAR-SW                                             
056000     END-IF                                                               
056100                                                                          
056200     IF EGEN-MID OR NYCKLAR-OK                                            
056300       MOVE MSGI-IDDISTR TO MOD-IDDISTR-UT                                
056400       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
056500       MOVE MSGI-IDKUNDNR TO MOD-IDKUNDNR-UT                              
056600       INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE            
056700       MOVE MSGI-IDBYTRAP TO MOD-IDBYTRAP-UT                              
056800       INSPECT MOD-IDBYTRAP-UT REPLACING LEADING ZERO BY SPACE            
056900       MOVE MSGI-KDBYTSTA TO MOD-KDBYTSTA-UT                              
057000       INSPECT MOD-KDBYTSTA-UT REPLACING LEADING ZERO BY SPACE            
057100       MOVE MSGI-IDDC-KEY TO MOD-IDDC-UT                                  
057200                                                                          
057300       MOVE SPACE TO WS-STYR-LAS                                          
057400       IF MSGI-IDDISTR NOT = ZERO                                         
057500         MOVE 'D' TO WS-DISTR                                             
057600       END-IF                                                             
057700       IF MSGI-IDKUNDNR NOT = ZERO                                        
057800         MOVE 'K' TO WS-KUND                                              
057900       END-IF                                                             
058000       IF MSGI-IDBYTRAP NOT = ZERO                                        
058100         MOVE 'R' TO WS-RAPP                                              
058200       END-IF                                                             
058300       IF MSGI-KDBYTSTA = '2' OR '4' OR '9'                               
058400         MOVE '2' TO WS-STATUS-2459                                       
058500       ELSE                                                               
058600         IF MSGI-KDBYTSTA = '3'                                           
058700           MOVE '3' TO WS-STATUS-36                                       
058800         END-IF                                                           
058900       END-IF                                                             
059000     END-IF                                                               
059100     IF NYCKLAR-FEL                                                       
059200       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
059300       CALL WMEDKONV USING MED-WMEDAREA                                   
059400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
059500       PERFORM MFS-RENSA-FAELT-IN                                         
059600       PERFORM MFS-RENSA-FAELT-UT                                         
059700     END-IF                                                               
059800     .                                                                    
059900     EJECT                                                                
060000 C-FOERSTA-SIDA SECTION.                                                  
060100                                                                          
060200     MOVE JA TO ALLT-SW                                                   
060300     MOVE 1                   TO SPAR-PGNO                                
060400     MOVE INF-FIRST-PAGE         TO MED-IDMFSINF                          
060500     CALL WMEDKONV            USING MED-WMEDAREA                          
060600     MOVE MED-MFSINF             TO MOD-TEMFSFEL                          
060700     .                                                                    
060800     EJECT                                                                
060900 D-NAESTA-SIDA SECTION.                                                   
061000                                                                          
061100     IF SPAR-IDTRANS = '3171'                                             
061200                                                                          
061300       IF SPAR-PAGE-DATA = LOW-VALUE                                      
061400         CONTINUE                                                         
061500       ELSE                                                               
061600         MOVE W-IDDISTR-NEXT  TO W-IDDISTR-A1                             
061700                                 W-IDDISTR-B1                             
061800                                 W-IDDISTR-C1                             
061900                                 W-IDDISTR-D1                             
062000                                                                          
062100         MOVE W-DAREGDAT-NEXT TO W-DAREGDAT-A1                            
062200                                 W-DAREGDAT-D1                            
062300                                                                          
062400         MOVE W-DAANKDAG-NEXT TO W-DAANKDAG-B1                            
062500                                 W-DAANKDAG-C1                            
062600                                                                          
062700         MOVE W-IDBYTRAP-NEXT TO W-IDBYTRAP-A1                            
062800                                 W-IDBYTRAP-B1                            
062900                                 W-IDBYTRAP-C1                            
063000                                 W-IDBYTRAP-D1                            
063100                                                                          
063200         MOVE W-KDBYTSTA-NEXT TO W-KDBYTSTA-A1                            
063300                                 W-KDBYTSTA-B1                            
063400                                 W-KDBYTSTA-C1                            
063500                                 W-KDBYTSTA-D1                            
063600                                                                          
063700         IF SPAR-PREV-AREA (SPAR-PGNO) = SPAR-NEXT-AREA                   
063800           CONTINUE                                                       
063900         ELSE                                                             
064000           COMPUTE SPAR-PGNO = SPAR-PGNO + 1                              
064100           IF SPAR-PGNO > 34                                              
064200             PERFORM VARYING PGNO-INDX FROM 1 BY 1                        
064300             UNTIL PGNO-INDX = 34                                         
064400               MOVE W-KDBYTSTA-PREV(PGNO-INDX + 1 )                       
064500                                 TO W-KDBYTSTA-PREV(PGNO-INDX)            
064600               MOVE W-DAREGDAT-PREV(PGNO-INDX + 1 )                       
064700                                 TO W-DAREGDAT-PREV(PGNO-INDX)            
064800               MOVE W-DAANKDAG-PREV(PGNO-INDX + 1 )                       
064900                                 TO W-DAANKDAG-PREV(PGNO-INDX)            
065000               MOVE W-IDBYTRAP-PREV(PGNO-INDX + 1 )                       
065100                                 TO W-IDBYTRAP-PREV(PGNO-INDX)            
065200               MOVE W-IDDISTR-PREV(PGNO-INDX + 1 )                        
065300                                 TO W-IDDISTR-PREV(PGNO-INDX)             
065400             END-PERFORM                                                  
065500             MOVE PGNO-INDX      TO SPAR-PGNO                             
065600           END-IF                                                         
065700         END-IF                                                           
065800       END-IF                                                             
065900     END-IF                                                               
066000                                                                          
066100     MOVE JA TO ALLT-SW                                                   
066200     .                                                                    
066300     EJECT                                                                
066400                                                                          
066500 E-SAMMA-SIDA SECTION.                                                    
066600                                                                          
066700     MOVE JA TO ALLT-SW                                                   
066800     PERFORM EA-BYT-BILD                                                  
066900     IF BYT-EJ-BILD                                                       
067000       MOVE +1 TO INDX                                                    
067100       PERFORM UNTIL INDX > MAX-INDX                                      
067200         IF MID-KDBYTSTA (INDX) = ALL '+' AND                             
067300            MID-ADBYTANK (INDX) = ALL '+'                                 
067400           ADD +1 TO INDX                                                 
067500         ELSE                                                             
067600           MOVE NEJ TO ALLT-SW                                            
067700           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
067800           CALL WMEDKONV USING MED-WMEDAREA                               
067900           MOVE MED-MFSINF TO MOD-TEMFSINF                                
068000           PERFORM MFS-ROER-EJ-FAELT-IN                                   
068100           PERFORM MFS-ROER-EJ-FAELT-UT                                   
068200           PERFORM MFS-LAES-IN-IGEN                                       
068300           IF HELP-MID                                                    
068400             PERFORM EB-MID-INDATA-TILL-MOD                               
068500           END-IF                                                         
068600           MOVE +9999 TO INDX                                             
068700         END-IF                                                           
068800       END-PERFORM                                                        
068900                                                                          
069000       IF ALLT-OK                                                         
069100         IF SPAR-IDTRANS = '3171' AND                                     
069200            SPAR-PGNO > 0         AND                                     
069300            SPAR-PAGE-DATA NOT = LOW-VALUE                                
069400           MOVE W-IDDISTR-PREV (SPAR-PGNO)                                
069500                                 TO W-IDDISTR-A1                          
069600                                    W-IDDISTR-B1                          
069700                                    W-IDDISTR-C1                          
069800                                    W-IDDISTR-D1                          
069900           MOVE W-DAREGDAT-PREV (SPAR-PGNO)                               
070000                                 TO W-DAREGDAT-A1                         
070100                                    W-DAREGDAT-D1                         
070200           MOVE W-DAANKDAG-PREV (SPAR-PGNO)                               
070300                                 TO W-DAANKDAG-B1                         
070400                                    W-DAANKDAG-C1                         
070500           MOVE W-IDBYTRAP-PREV (SPAR-PGNO)                               
070600                                 TO W-IDBYTRAP-A1                         
070700                                    W-IDBYTRAP-B1                         
070800                                    W-IDBYTRAP-C1                         
070900                                    W-IDBYTRAP-D1                         
071000           MOVE W-KDBYTSTA-PREV (SPAR-PGNO)                               
071100                                 TO W-KDBYTSTA-A1                         
071200                                    W-KDBYTSTA-B1                         
071300                                    W-KDBYTSTA-C1                         
071400                                    W-KDBYTSTA-D1                         
071500                                                                          
071600         END-IF                                                           
071700       END-IF                                                             
071800     END-IF                                                               
071900     .                                                                    
072000     EJECT                                                                
072100                                                                          
072200 EA-BYT-BILD SECTION.                                                     
072300                                                                          
072400     MOVE +1 TO INDX                                                      
072500     PERFORM UNTIL INDX > MAX-INDX                                        
072600       IF MID-KDSVAR (INDX) = '+' OR SPACE                                
072700         ADD +1 TO INDX                                                   
072800       ELSE                                                               
072900         MOVE NEJ TO ALLT-SW                                              
073000         MOVE JA TO BYT-SW                                                
073100                                                                          
073200         IF ENGLISH-TEXT                                                  
073300           MOVE '2' TO M-SW-KDMFSFOR                                      
073400         ELSE                                                             
073500           MOVE '1' TO M-SW-KDMFSFOR                                      
073600         END-IF                                                           
073700         INSPECT MID-IDBYTRAP (INDX)                                      
073800                 REPLACING LEADING SPACE BY ZERO                          
073900         INSPECT MID-IDDISTR (INDX)                                       
074000                 REPLACING LEADING SPACE BY ZERO                          
074100         MOVE ALL '+'   TO 3172-MID-W3I17201                              
074200         MOVE MID-IDDISTR (INDX) TO 3172-MID-IDDISTR-IN                   
074300         MOVE MID-IDBYTRAP (INDX) TO 3172-MID-IDBYTRAP-IN                 
074400         PERFORM IMS-INSERT-ALT-MSG                                       
074500         MOVE +9999 TO INDX                                               
074600       END-IF                                                             
074700     END-PERFORM                                                          
074800     .                                                                    
074900     EJECT                                                                
075000 EB-MID-INDATA-TILL-MOD SECTION.                                          
075100                                                                          
075200     MOVE +1 TO INDX                                                      
075300     PERFORM UNTIL INDX > MAX-INDX                                        
075400       IF MID-KDSVAR (INDX) NOT = ALL '+'                                 
075500         MOVE MID-KDSVAR (INDX) TO MOD-KDSVAR (INDX)                      
075600       END-IF                                                             
075700       IF MID-KDBYTSTA (INDX) NOT = ALL '+'                               
075800         MOVE MID-KDBYTSTA (INDX) TO                                      
075900              MOD-KDBYTSTA (INDX)                                         
076000       END-IF                                                             
076100       IF MID-ADBYTANK (INDX) NOT = ALL '+'                               
076200         MOVE MID-ADBYTANK (INDX) TO                                      
076300              MOD-ADBYTANK (INDX)                                         
076400       END-IF                                                             
076500       ADD +1 TO INDX                                                     
076600     END-PERFORM                                                          
076700     .                                                                    
076800     EJECT                                                                
076900                                                                          
077000 I-PREV-PAGE SECTION.                                                     
077100     IF SPAR-IDTRANS = '3171'                                             
077200       COMPUTE SPAR-PGNO = SPAR-PGNO - 1                                  
077300       IF SPAR-PGNO > 0                                                   
077400         MOVE W-IDDISTR-PREV (SPAR-PGNO)                                  
077500                                 TO W-IDDISTR-A1                          
077600                                    W-IDDISTR-B1                          
077700                                    W-IDDISTR-C1                          
077800                                    W-IDDISTR-D1                          
077900                                                                          
078000         MOVE W-DAREGDAT-PREV (SPAR-PGNO)                                 
078100                                 TO W-DAREGDAT-A1                         
078200                                    W-DAREGDAT-D1                         
078300                                                                          
078400         MOVE W-DAANKDAG-PREV (SPAR-PGNO)                                 
078500                                 TO W-DAANKDAG-B1                         
078600                                    W-DAANKDAG-C1                         
078700                                                                          
078800         MOVE W-IDBYTRAP-PREV (SPAR-PGNO)                                 
078900                                 TO W-IDBYTRAP-A1                         
079000                                    W-IDBYTRAP-B1                         
079100                                    W-IDBYTRAP-C1                         
079200                                    W-IDBYTRAP-D1                         
079300                                                                          
079400         MOVE W-KDBYTSTA-PREV (SPAR-PGNO)                                 
079500                                 TO W-KDBYTSTA-A1                         
079600                                    W-KDBYTSTA-B1                         
079700                                    W-KDBYTSTA-C1                         
079800                                    W-KDBYTSTA-D1                         
079900       ELSE                                                               
080000         MOVE 1                  TO SPAR-PGNO                             
080100         MOVE INF-FIRST-PAGE                                              
080200                                 TO MED-IDMFSINF                          
080300         CALL WMEDKONV        USING MED-WMEDAREA                          
080400         MOVE MED-MFSINF         TO MOD-TEMFSFEL                          
080500       END-IF                                                             
080600     END-IF                                                               
080700                                                                          
080800     MOVE JA TO ALLT-SW                                                   
080900     .                                                                    
081000     SKIP2                                                                
081100 F-LAES-VISA-INFO SECTION.                                                
081200                                                                          
081300     PERFORM FA-LAES-DB                                                   
081400     IF SEGMENT-SAKNAS                                                    
081500       MOVE 1                    TO SPAR-PGNO                             
081600       MOVE LOW-VALUE            TO SPAR-PAGE-DATA                        
081700       MOVE REPORT-NOT-REGISTRATED                                        
081800                                 TO   MED-IDMFSFEL                        
081900       CALL WMEDKONV            USING MED-WMEDAREA                        
082000       MOVE MED-MFSFEL           TO   MOD-TEMFSFEL                        
082100       PERFORM MFS-RENSA-FAELT-UT                                         
082200     ELSE                                                                 
082300       MOVE +1 TO INDX                                                    
082400       MOVE RAPP-KDBYTSTA-RAPP                                            
082500                                 TO W-KDBYTSTA-PREV(SPAR-PGNO)            
082600       MOVE RAPP-DAREGDAT                                                 
082700                                 TO W-DAREGDAT-PREV(SPAR-PGNO)            
082800       MOVE RAPP-DAANKDAG                                                 
082900                                 TO W-DAANKDAG-PREV(SPAR-PGNO)            
083000       MOVE RAPP-IDBYTRAP                                                 
083100                                 TO W-IDBYTRAP-PREV(SPAR-PGNO)            
083200       MOVE RAPP-IDDISTR                                                  
083300                                 TO W-IDDISTR-PREV (SPAR-PGNO)            
083400                                                                          
083500       PERFORM UNTIL INDX > MAX-INDX                                      
083600         IF SEGMENT-FINNS                                                 
083700           PERFORM FC-DATA-TO-MOD                                         
083800           PERFORM FA-LAES-DB                                             
083900         ELSE                                                             
084000           MOVE MFS-RENSA-FAELT    TO MOD-KDSVAR (INDX)                   
084100           PERFORM MFS-RENSA-RAD-FAELT-UT                                 
084200         END-IF                                                           
084300         ADD 1 TO INDX                                                    
084400       END-PERFORM                                                        
084500                                                                          
084600       IF SEGMENT-FINNS                                                   
084700          MOVE INF-MORE-INFO-EXISTS                                       
084800                                 TO MED-IDMFSINF                          
084900          CALL WMEDKONV          USING MED-WMEDAREA                       
085000          MOVE MED-TEMFSINF      TO MOD-TEMFSINF                          
085100          MOVE RAPP-KDBYTSTA-RAPP                                         
085200                                 TO W-KDBYTSTA-NEXT                       
085300          MOVE RAPP-DAREGDAT     TO W-DAREGDAT-NEXT                       
085400          MOVE RAPP-DAANKDAG     TO W-DAANKDAG-NEXT                       
085500          MOVE RAPP-IDBYTRAP     TO W-IDBYTRAP-NEXT                       
085600          MOVE RAPP-IDDISTR      TO W-IDDISTR-NEXT                        
085700       ELSE                                                               
085800          MOVE INF-LAST-PAGE                                              
085900                                 TO MED-IDMFSINF                          
086000          CALL WMEDKONV       USING MED-WMEDAREA                          
086100          MOVE MED-MFSINF        TO MOD-TEMFSFEL                          
086200          MOVE W-KDBYTSTA-PREV(SPAR-PGNO)                                 
086300                                 TO W-KDBYTSTA-NEXT                       
086400          MOVE W-DAREGDAT-PREV(SPAR-PGNO)                                 
086500                                 TO W-DAREGDAT-NEXT                       
086600          MOVE W-DAANKDAG-PREV(SPAR-PGNO)                                 
086700                                 TO W-DAANKDAG-NEXT                       
086800          MOVE W-IDBYTRAP-PREV(SPAR-PGNO)                                 
086900                                 TO W-IDBYTRAP-NEXT                       
087000          MOVE W-IDDISTR-PREV (SPAR-PGNO)                                 
087100                                 TO W-IDDISTR-NEXT                        
087200       END-IF                                                             
087300     END-IF                                                               
087400     MOVE '002'                  TO MSGI-KDCALL                           
087500     MOVE '3171'                 TO SPAR-IDTRANS                          
087600     MOVE SPAR-AREA              TO MSGI-SPAR-AREA                        
087700     CALL W005INIT            USING MSGI-WMSGINIT WDP7-PCB                
087800                                                                          
087900     .                                                                    
088000     EJECT                                                                
088100 FA-LAES-DB SECTION.                                                      
088200     EVALUATE WS-STYR-LAS                                                 
088300       WHEN '   2 '                                                       
088400         PERFORM IMS-GN-WDM601-A1                                         
088500       WHEN '    3'                                                       
088600         PERFORM IMS-GN-WDM601-B1                                         
088700       WHEN 'D   3'                                                       
088800         PERFORM IMS-GN-WDM6C1                                            
088900         IF SEGMENT-FINNS                                                 
089000           MOVE SEQC-IDDISTR      TO W-IDDISTR-UNIK                       
089100           MOVE SEQC-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
089200           PERFORM IMS-GHU-WDM601                                         
089300         END-IF                                                           
089400       WHEN 'D R 3'                                                       
089500         PERFORM IMS-GN-WDM6C1-RAP                                        
089600         IF SEGMENT-FINNS                                                 
089700           MOVE SEQC-IDDISTR      TO W-IDDISTR-UNIK                       
089800           MOVE SEQC-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
089900           PERFORM IMS-GHU-WDM601                                         
090000         END-IF                                                           
090100       WHEN 'DK  3'                                                       
090200         PERFORM IMS-GN-WDM6C1-KUN                                        
090300         IF SEGMENT-FINNS                                                 
090400           MOVE SEQC-IDDISTR      TO W-IDDISTR-UNIK                       
090500           MOVE SEQC-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
090600           PERFORM IMS-GHU-WDM601                                         
090700         END-IF                                                           
090800       WHEN 'DKR 3'                                                       
090900         PERFORM IMS-GN-WDM6C1-KUN-RAP                                    
091000         IF SEGMENT-FINNS                                                 
091100           MOVE SEQC-IDDISTR      TO W-IDDISTR-UNIK                       
091200           MOVE SEQC-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
091300           PERFORM IMS-GHU-WDM601                                         
091400         END-IF                                                           
091500       WHEN 'D  2 '                                                       
091600         PERFORM IMS-GN-WDM6D1                                            
091700         IF SEGMENT-FINNS                                                 
091800           MOVE SEQD-IDDISTR      TO W-IDDISTR-UNIK                       
091900           MOVE SEQD-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
092000           PERFORM IMS-GHU-WDM601                                         
092100         END-IF                                                           
092200       WHEN 'DK 2 '                                                       
092300         PERFORM IMS-GN-WDM6D1-KUN                                        
092400         IF SEGMENT-FINNS                                                 
092500           MOVE SEQD-IDDISTR      TO W-IDDISTR-UNIK                       
092600           MOVE SEQD-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
092700           PERFORM IMS-GHU-WDM601                                         
092800         END-IF                                                           
092900       WHEN 'D R2 '                                                       
093000         PERFORM IMS-GN-WDM6D1-RAP                                        
093100         IF SEGMENT-FINNS                                                 
093200           MOVE SEQD-IDDISTR      TO W-IDDISTR-UNIK                       
093300           MOVE SEQD-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
093400           PERFORM IMS-GHU-WDM601                                         
093500         END-IF                                                           
093600       WHEN 'DKR2 '                                                       
093700         PERFORM IMS-GN-WDM6D1-KUN-RAP                                    
093800         IF SEGMENT-FINNS                                                 
093900           MOVE SEQD-IDDISTR      TO W-IDDISTR-UNIK                       
094000           MOVE SEQD-IDBYTRAP     TO W-IDBYTRAP-UNIK                      
094100           PERFORM IMS-GHU-WDM601                                         
094200         END-IF                                                           
094300     END-EVALUATE                                                         
094400     .                                                                    
094500     EJECT                                                                
094600 FC-DATA-TO-MOD SECTION.                                                  
094700     MOVE MFS-OPEN-NUM-NOMOD       TO MOD-KDBYTSTA-ATTR     (INDX)        
094800     MOVE MFS-OPEN-ALPHA-NOMOD     TO MOD-ADBYTANK-ATTR     (INDX)        
094900     MOVE MFS-OPEN-ALPHA-FIELD     TO MOD-KDSVAR-ATTR       (INDX)        
095000     MOVE MFS-RENSA-FAELT          TO MOD-KDSVAR            (INDX)        
095100     MOVE RAPP-IDDISTR             TO MOD-IDDISTR           (INDX)        
095200     MOVE RAPP-IDKUNDNR            TO MOD-IDKUNDNR          (INDX)        
095300     MOVE RAPP-IDFAKT              TO MOD-IDFAKT            (INDX)        
095400     MOVE RAPP-IDBYTRAP            TO MOD-IDBYTRAP          (INDX)        
095500     MOVE RAPP-KVRETUR-TOT         TO MOD-KVRETUR-TOT       (INDX)        
095600     MOVE RAPP-DAREGDAT            TO MOD-TIREGDAT          (INDX)        
095700     MOVE RAPP-KDBYTSTA-RAPP       TO MOD-KDBYTSTA          (INDX)        
095800     MOVE RAPP-ADBYTANK            TO MOD-ADBYTANK          (INDX)        
095900     MOVE RAPP-DAANKDAG(3:6)       TO MOD-TIANKDAG          (INDX)        
096000     MOVE RAPP-DAREGDAT-GODK       TO MOD-TIREGDAT-GODK     (INDX)        
096100     MOVE RAPP-FLBYTGAR            TO MOD-FLBYTGAR          (INDX)        
096200     IF MSGI-IDLAND-SPR = 'GB'                                            
096300       IF RAPP-FLBYTGAR = 'J'                                             
096400         MOVE 'Y'                  TO MOD-FLBYTGAR          (INDX)        
096500       END-IF                                                             
096600     END-IF                                                               
096700     .                                                                    
096800     EJECT                                                                
096900 G-KOLLA-INPUT SECTION.                                                   
097000******* UPPDATERING FÅR EJ SKE PÅ FELAKTIGT IDDC *******                  
097100                                                                          
097200     MOVE JA  TO INDATA-SW                                                
097300     MOVE +1 TO INDX                                                      
097400     PERFORM UNTIL INDX > MAX-INDX                                        
097500       IF MID-IDDISTR (INDX) = ALL '+' OR SPACE                           
097600         CONTINUE                                                         
097700       ELSE                                                               
097800         INSPECT MID-IDDISTR  (INDX)                                      
097900                 REPLACING LEADING SPACE BY ZERO                          
098000         INSPECT MID-IDBYTRAP (INDX)                                      
098100                 REPLACING LEADING SPACE BY ZERO                          
098200         MOVE MFS-OPEN-NUM-NOMOD   TO MOD-KDBYTSTA-ATTR (INDX)            
098300         MOVE MFS-OPEN-ALPHA-NOMOD TO MOD-ADBYTANK-ATTR (INDX)            
098400         MOVE MFS-OPEN-ALPHA-FIELD TO MOD-KDSVAR-ATTR   (INDX)            
098500         IF MID-KDBYTSTA (INDX) NOT = ALL '+'                             
098600             OR MID-ADBYTANK (INDX) NOT = ALL '+'                         
098700           MOVE MID-IDDISTR (INDX)   TO W-IDDISTR-UNIK                    
098800           MOVE MID-IDBYTRAP (INDX)  TO W-IDBYTRAP-UNIK                   
098900           PERFORM IMS-GHU-WDM601                                         
099000           IF SEGMENT-FINNS                                               
099100             IF WS-IDDC-1 = RAPP-IDDC                                     
099200               PERFORM GA-KOLLA-BYTSTA                                    
099300               IF MID-ADBYTANK (INDX) NOT = ALL '+'                       
099400                 IF RAPP-KDBYTSTA-RAPP = '3'                              
099500                     OR MID-KDBYTSTA (INDX) = '3'                         
099600                   MOVE MFS-ALFA-FAELT-RAETT TO                           
099700                        MOD-ADBYTANK-ATTR (INDX)                          
099800                 ELSE                                                     
099900                   MOVE MFS-ALFA-FAELT-FEL TO                             
100000                        MOD-ADBYTANK-ATTR (INDX)                          
100100                   MOVE NEJ TO INDATA-SW                                  
100200                   MOVE ERR-UPDATE TO MED-IDMFSINF                        
100300                   CALL WMEDKONV USING MED-WMEDAREA                       
100400                   MOVE MED-MFSINF TO MOD-TEMFSINF                        
100500                 END-IF                                                   
100600               END-IF                                                     
100700             ELSE                                                         
100800               MOVE MFS-ALFA-FAELT-FEL TO MOD-ADBYTANK-ATTR (INDX)        
100900               MOVE MFS-NUM-FAELT-FEL  TO MOD-KDBYTSTA-ATTR (INDX)        
101000               MOVE NEJ TO INDATA-SW                                      
101100               MOVE USER-NOT-ALLOWED TO MED-IDMFSINF                      
101200               CALL WMEDKONV USING MED-WMEDAREA                           
101300               MOVE MED-MFSINF TO MOD-TEMFSINF                            
101400             END-IF                                                       
101500           ELSE                                                           
101600             MOVE MFS-ALFA-FAELT-FEL TO MOD-ADBYTANK-ATTR (INDX)          
101700             MOVE MFS-NUM-FAELT-FEL  TO MOD-KDBYTSTA-ATTR (INDX)          
101800             MOVE NEJ TO INDATA-SW                                        
101900             MOVE ERR-UPDATE TO MED-IDMFSINF                              
102000             CALL WMEDKONV USING MED-WMEDAREA                             
102100             MOVE MED-MFSINF TO MOD-TEMFSINF                              
102200           END-IF                                                         
102300         END-IF                                                           
102400       END-IF                                                             
102500       ADD +1 TO INDX                                                     
102600     END-PERFORM                                                          
102700                                                                          
102800                                                                          
102900     IF INDATA-FEL                                                        
103000       MOVE NEJ TO ALLT-SW                                                
103100       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
103200       CALL WMEDKONV USING MED-WMEDAREA                                   
103300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
103400       PERFORM MFS-ROER-EJ-FAELT-UT                                       
103500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
103600     END-IF                                                               
103700     .                                                                    
103800     EJECT                                                                
103900 GA-KOLLA-BYTSTA SECTION.                                                 
104000* --- --- HÄR FÅR MAN INTE ÄNDRA STATUS 4 TILL 3                          
104100* --- --- DET SKER I BILD 3172.                                           
104200                                                                          
104300     IF MID-KDBYTSTA (INDX) NOT = ALL '+'                                 
104400       IF MID-KDBYTSTA (INDX) = '2' OR '3'                                
104500         IF RAPP-KDBYTSTA-RAPP = '2' OR '3'                               
104600           IF RAPP-KDBYTSTA-RAPP = '2'                                    
104700             IF MID-KDBYTSTA (INDX) = '3'                                 
104800               MOVE MFS-NUM-FAELT-RAETT TO MOD-KDBYTSTA-ATTR(INDX)        
104900             ELSE                                                         
105000               MOVE MFS-NUM-FAELT-FEL TO  MOD-KDBYTSTA-ATTR (INDX)        
105100               MOVE MFS-ALFA-FAELT-FEL TO MOD-ADBYTANK-ATTR (INDX)        
105200               MOVE NEJ TO INDATA-SW                                      
105300               MOVE WRONG-STATUS TO MED-IDMFSINF                          
105400               CALL WMEDKONV USING MED-WMEDAREA                           
105500               MOVE MED-MFSINF TO MOD-TEMFSINF                            
105600             END-IF                                                       
105700           ELSE                                                           
105800             IF RAPP-KDBYTSTA-RAPP = '3'                                  
105900               IF MID-KDBYTSTA (INDX) = '2'                               
106000                 PERFORM IMS-GNP-WDM611                                   
106100                 IF SEGMENT-FINNS                                         
106200                   PERFORM UNTIL SEGMENT-SAKNAS OR RAD-KOLL-FEL           
106300                     IF OBJ-KDBYTSTA-OBJ NOT = ' '                        
106400                       MOVE NEJ TO RAD-KOLL-SW                            
106500                     END-IF                                               
106600                     PERFORM IMS-GNP-WDM611                               
106700                   END-PERFORM                                            
106800                   IF RAD-KOLL-OK                                         
106900                     MOVE MFS-ALFA-FAELT-RAETT                            
107000                          TO MOD-KDBYTSTA-ATTR (INDX)                     
107100                     IF MID-ADBYTANK(INDX) NOT = ALL '+'                  
107200                       MOVE MFS-ALFA-FAELT-FEL                            
107300                            TO MOD-ADBYTANK-ATTR (INDX)                   
107400                       MOVE NEJ TO INDATA-SW                              
107500                     END-IF                                               
107600                   ELSE                                                   
107700                     MOVE MFS-ALFA-FAELT-FEL TO                           
107800                          MOD-KDBYTSTA-ATTR (INDX)                        
107900                     MOVE NEJ TO INDATA-SW                                
108000                     IF ENGLISH-TEXT                                      
108100                       MOVE 'LINES ARE CHANGED' TO MOD-TEMFSINF           
108200                     ELSE                                                 
108300                       MOVE 'RADER ÄR ÄNDRADE ' TO MOD-TEMFSINF           
108400                     END-IF                                               
108500                     IF MID-ADBYTANK (INDX) NOT = ALL '+'                 
108600                       MOVE MFS-ALFA-FAELT-RAETT TO                       
108700                            MOD-ADBYTANK-ATTR (INDX)                      
108800                     END-IF                                               
108900                   END-IF                                                 
109000                 END-IF                                                   
109100               ELSE                                                       
109200                 MOVE MFS-NUM-FAELT-FEL TO                                
109300                      MOD-KDBYTSTA-ATTR (INDX)                            
109400                 MOVE NEJ TO INDATA-SW                                    
109500                 IF MID-ADBYTANK (INDX) NOT = ALL '+'                     
109600                   MOVE MFS-ALFA-FAELT-RAETT TO                           
109700                        MOD-ADBYTANK-ATTR (INDX)                          
109800                 END-IF                                                   
109900               END-IF                                                     
110000             END-IF                                                       
110100           END-IF                                                         
110200         ELSE                                                             
110300           MOVE MFS-NUM-FAELT-FEL TO MOD-KDBYTSTA-ATTR (INDX)             
110400           MOVE MFS-ALFA-FAELT-FEL TO MOD-ADBYTANK-ATTR(INDX)             
110500           MOVE NEJ TO INDATA-SW                                          
110600           MOVE ERR-UPDATE  TO MED-IDMFSINF                               
110700           CALL WMEDKONV USING MED-WMEDAREA                               
110800           MOVE MED-MFSINF TO MOD-TEMFSINF                                
110900         END-IF                                                           
111000       ELSE                                                               
111100         MOVE MFS-NUM-FAELT-FEL TO MOD-KDBYTSTA-ATTR (INDX)               
111200         MOVE MFS-ALFA-FAELT-FEL TO MOD-ADBYTANK-ATTR(INDX)               
111300         MOVE NEJ TO INDATA-SW                                            
111400         MOVE ERR-UPDATE  TO MED-IDMFSINF                                 
111500         CALL WMEDKONV USING MED-WMEDAREA                                 
111600         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
111700       END-IF                                                             
111800     END-IF                                                               
111900     .                                                                    
112000     EJECT                                                                
112100 H-UPPDATERA SECTION.                                                     
112200                                                                          
112300     MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSINF                            
112400     MOVE +1 TO INDX                                                      
112500     PERFORM UNTIL INDX > MAX-INDX                                        
112600       IF MID-IDDISTR (INDX) NUMERIC                                      
112700         MOVE MFS-OPEN-NUM-NOMOD   TO MOD-KDBYTSTA-ATTR (INDX)            
112800         MOVE MFS-OPEN-ALPHA-NOMOD TO MOD-ADBYTANK-ATTR (INDX)            
112900         MOVE MFS-OPEN-ALPHA-FIELD TO MOD-KDSVAR-ATTR   (INDX)            
113000         IF MID-KDBYTSTA (INDX) NOT = ALL '+'                             
113100             OR MID-ADBYTANK (INDX) NOT = ALL '+'                         
113200           MOVE MID-IDDISTR (INDX)  TO W-IDDISTR-UNIK                     
113300           MOVE MID-IDBYTRAP (INDX) TO W-IDBYTRAP-UNIK                    
113400           PERFORM IMS-GHU-WDM601                                         
113500           IF MID-KDBYTSTA (INDX) NOT = ALL '+'                           
113600             MOVE MID-KDBYTSTA (INDX) TO RAPP-KDBYTSTA-RAPP               
113700             MOVE MFS-NUM-UPDATE-OK TO MOD-KDBYTSTA-ATTR (INDX)           
113800             IF MID-KDBYTSTA (INDX) = '2'                                 
113900               MOVE SPACE TO RAPP-ADBYTANK                                
114000               MOVE ZERO  TO RAPP-DAANKDAG                                
114100             ELSE                                                         
114110               IF RAPP-IDDC NOT = WC-CDC-SE                               
114200                 MOVE WC-SDC-NL-ET TO W-IDDC-K7                           
114300                 PERFORM IMS-GNP-WDM611                                   
114400                 PERFORM UNTIL SEGMENT-SAKNAS OR BAS-SLUT                 
114500                   MOVE OBJ-IDARTNR-OBJ  TO W-IDARTNR-K7                  
114600                                            W-IDARTNR-K6                  
114700                   PERFORM IMS-GHU-WDK711                                 
114800                   IF SEGMENT-SAKNAS                                      
114900                     PERFORM HA-NYA-SEGMENT-WDK7                          
115000                   END-IF                                                 
115100                   PERFORM IMS-GNP-WDM611                                 
115200                 END-PERFORM                                              
115210               END-IF                                                     
115300               MOVE MID-IDDISTR (INDX)  TO W-IDDISTR-UNIK                 
115400               MOVE MID-IDBYTRAP (INDX) TO W-IDBYTRAP-UNIK                
115500               PERFORM IMS-GHU-WDM601                                     
115600               MOVE MID-KDBYTSTA (INDX) TO RAPP-KDBYTSTA-RAPP             
115700               MOVE MFS-NUM-UPDATE-OK TO MOD-KDBYTSTA-ATTR (INDX)         
115800*******                HÄR SÄTTS ANKOMSTDATUM TILL DAGENS DATUM           
115900               MOVE MSGI-TILOKDAT  TO RAPP-DAANKDAG                       
116000               IF MSGI-TILOKDAT NOT = ZERO                                
116100                 IF MSGI-TILOKDAT < 500000                                
116200                   MOVE 20         TO RAPP-DAANKDAG (1:2)                 
116300                 ELSE                                                     
116400                   IF MSGI-TILOKDAT < 999999                              
116500                     MOVE 19       TO RAPP-DAANKDAG (1:2)                 
116600                   ELSE                                                   
116700                     MOVE 99999999 TO RAPP-DAANKDAG                       
116800                   END-IF                                                 
116900                 END-IF                                                   
117000               END-IF                                                     
117100             END-IF                                                       
117200           END-IF                                                         
117300           IF MID-ADBYTANK (INDX) NOT = ALL '+'                           
117400             MOVE MID-ADBYTANK (INDX) TO RAPP-ADBYTANK                    
117500             MOVE MFS-ALPHA-UPDATE-OK TO MOD-ADBYTANK-ATTR (INDX)         
117600           END-IF                                                         
117700           PERFORM IMS-REPL-WDM601                                        
117800           MOVE INF-UPDATE-DONE TO MED-IDMFSINF                           
117900         END-IF                                                           
118000       END-IF                                                             
118100       ADD +1 TO INDX                                                     
118200     END-PERFORM                                                          
118300                                                                          
118400     CALL WMEDKONV USING MED-WMEDAREA                                     
118500     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
118600     PERFORM MFS-ROER-EJ-FAELT-UT                                         
118700     .                                                                    
118800     EJECT                                                                
118900 HA-NYA-SEGMENT-WDK7 SECTION.                                             
119000                                                                          
119100     MOVE ALL '+'      TO WDK7-W005WDK7                                   
119200     MOVE 'WDK711'     TO WDK7-IDSEGM                                     
119300     MOVE W-IDARTNR-K7 TO WDK7-IDARTNR-KFB                                
119400     MOVE WC-SDC-NL-ET TO WDK7-IDDC-KFB                                   
119500                          WDK7-IDDC                                       
119600                                                                          
119700     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-PCB WDK7-PCB         
119800     .                                                                    
119900     EJECT                                                                
120000                                                                          
120100 MFS-RENSA-FAELT-UT SECTION.                                              
120200                                                                          
120300     MOVE +1 TO INDX                                                      
120400     PERFORM UNTIL INDX > MAX-INDX                                        
120500       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
120600       ADD +1 TO INDX                                                     
120700     END-PERFORM                                                          
120800     .                                                                    
120900     SKIP2                                                                
121000 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
121100                                                                          
121200*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
121300     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR        (INDX)                    
121400                             MOD-IDKUNDNR       (INDX)                    
121500                             MOD-IDFAKT         (INDX)                    
121600                             MOD-IDBYTRAP       (INDX)                    
121700                             MOD-KVRETUR-TOT    (INDX)                    
121800                             MOD-TIREGDAT       (INDX)                    
121900                             MOD-FLBYTGAR       (INDX)                    
122000                             MOD-KDBYTSTA       (INDX)                    
122100                             MOD-ADBYTANK       (INDX)                    
122200                             MOD-TIANKDAG       (INDX)                    
122300                             MOD-TIREGDAT-GODK  (INDX)                    
122400     .                                                                    
122500     SKIP2                                                                
122600 MFS-RENSA-FAELT-IN SECTION.                                              
122700                                                                          
122800*    --- ALLA INDATA-FÄLT                                                 
122900     MOVE +1 TO INDX                                                      
123000     PERFORM UNTIL INDX > MAX-INDX                                        
123100       MOVE MFS-RENSA-FAELT TO MOD-KDSVAR   (INDX)                        
123200                               MOD-KDBYTSTA (INDX)                        
123300                               MOD-ADBYTANK (INDX)                        
123400       ADD +1  TO INDX                                                    
123500     END-PERFORM                                                          
123600     .                                                                    
123700     EJECT                                                                
123800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
123900                                                                          
124000*    --- ALLA UTDATA-FÄLT                                                 
124100     MOVE +1 TO INDX                                                      
124200     PERFORM UNTIL INDX > MAX-INDX                                        
124300       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
124400       ADD +1 TO INDX                                                     
124500     END-PERFORM                                                          
124600     .                                                                    
124700     SKIP2                                                                
124800 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
124900                                                                          
125000*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
125100     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR        (INDX)                  
125200                               MOD-IDKUNDNR       (INDX)                  
125300                               MOD-IDFAKT         (INDX)                  
125400                               MOD-IDBYTRAP       (INDX)                  
125500                               MOD-KVRETUR-TOT    (INDX)                  
125600                               MOD-TIREGDAT       (INDX)                  
125700                               MOD-FLBYTGAR       (INDX)                  
125800                               MOD-KDBYTSTA       (INDX)                  
125900                               MOD-ADBYTANK       (INDX)                  
126000                               MOD-TIANKDAG       (INDX)                  
126100                               MOD-TIREGDAT-GODK  (INDX)                  
126200     .                                                                    
126300     SKIP2                                                                
126400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
126500                                                                          
126600*    --- ALLA INDATA-FÄLT                                                 
126700     MOVE +1 TO INDX                                                      
126800     PERFORM UNTIL INDX > MAX-INDX                                        
126900       MOVE MFS-ROER-EJ-FAELT TO MOD-KDSVAR   (INDX)                      
127000                                 MOD-KDBYTSTA (INDX)                      
127100                                 MOD-ADBYTANK (INDX)                      
127200       ADD +1 TO INDX                                                     
127300     END-PERFORM                                                          
127400     .                                                                    
127500     SKIP2                                                                
127600 MFS-LAES-IN-IGEN SECTION.                                                
127700                                                                          
127800*    --- ALLA INDATA-FÄLT                                                 
127900     MOVE +1 TO INDX                                                      
128000     PERFORM UNTIL INDX > MAX-INDX                                        
128100       IF MID-KDSVAR (INDX) NOT = ALL '+'                                 
128200         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDSVAR-ATTR(INDX)              
128300       END-IF                                                             
128400       IF MID-KDBYTSTA (INDX) NOT = ALL '+'                               
128500         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBYTSTA-ATTR (INDX)           
128600       END-IF                                                             
128700       IF MID-ADBYTANK (INDX)        NOT = ALL '+'                        
128800         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-ADBYTANK-ATTR (INDX)           
128900       END-IF                                                             
129000       ADD +1 TO INDX                                                     
129100     END-PERFORM                                                          
129200     .                                                                    
129300     EJECT                                                                
129400* --- IMS SEKTIONER ---                                                   
129500     SKIP3                                                                
129600 IMS-GET-MSG SECTION.                                                     
129700     MOVE 'GET-MSG'           TO WS-SECTION                               
129800                                                                          
129900     MOVE '  QC' TO GODK-STATUSKODER                                      
130000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
130100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
130200     PERFORM IMS-STATUSKONTROLL                                           
130300     .                                                                    
130400     SKIP2                                                                
130500 IMS-INSERT-MSG SECTION.                                                  
130600     MOVE 'INSERT-MSG    '    TO WS-SECTION                               
130700                                                                          
130800     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
130900       MOVE '0' TO MFS-KDHUVOMR                                           
131000     END-IF                                                               
131100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
131200     MOVE SPACE TO GODK-STATUSKODER                                       
131300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
131400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
131500     PERFORM IMS-STATUSKONTROLL                                           
131600     .                                                                    
131700     SKIP2                                                                
131800 IMS-INSERT-ALT-MSG SECTION.                                              
131900     MOVE 'INSERT-ALT-MSG'    TO WS-SECTION                               
132000     MOVE SPACE TO GODK-STATUSKODER                                       
132100     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
132200     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
132300     PERFORM IMS-STATUSKONTROLL                                           
132400     .                                                                    
132500     EJECT                                                                
132600 IMS-GN-WDM601-A1 SECTION.                                                
132700       MOVE 'GN-WDM601-A1' TO WS-SECTION                                  
132800     STRING 'WDM601  (WDM6ASEQ=>' W-WDM6ASEQ-MIN-X                        
132900                    '&WDM6ASEQ<=' W-WDM6ASEQ-MAX-X ')'                    
133000          DELIMITED BY SIZE INTO SSA1                                     
133100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
133200     CALL CBLTDLI USING GN WDM6A-PCB DLI-IO-WDM601 SSA1                   
133300     MOVE WDM6A-STATUS-CODE TO STATUS-WS                                  
133400     PERFORM IMS-STATUSKONTROLL                                           
133500     .                                                                    
133600     SKIP2                                                                
133700 IMS-GN-WDM601-B1 SECTION.                                                
133800       MOVE 'GN-WDM601-B1' TO WS-SECTION                                  
133900     STRING 'WDM601  (WDM6BSEQ=>' W-WDM6BSEQ-MIN-X                        
134000                    '&WDM6BSEQ<=' W-WDM6BSEQ-MAX-X ')'                    
134100          DELIMITED BY SIZE INTO SSA1                                     
134200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
134300     CALL CBLTDLI USING GN WDM6B-PCB DLI-IO-WDM601 SSA1                   
134400     MOVE WDM6B-STATUS-CODE TO STATUS-WS                                  
134500     PERFORM IMS-STATUSKONTROLL                                           
134600     .                                                                    
134700     EJECT                                                                
134800 IMS-GN-WDM6C1  SECTION.                                                  
134900       MOVE 'GN-WDM6C1' TO WS-SECTION                                     
135000     STRING 'WDM6C1  (WDM6C1KY=>' W-WDM6C1KY-MIN-X                        
135100                    '&WDM6C1KY<=' W-WDM6C1KY-MAX-X ')'                    
135200          DELIMITED BY SIZE INTO SSA1                                     
135300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
135400     CALL CBLTDLI USING GN WDM6C-PCB DLI-IO-WDM6C1 SSA1                   
135500     MOVE WDM6C-STATUS-CODE TO STATUS-WS                                  
135600     PERFORM IMS-STATUSKONTROLL                                           
135700     .                                                                    
135800     SKIP2                                                                
135900 IMS-GN-WDM6C1-RAP  SECTION.                                              
136000       MOVE 'GN-WDM6C1-RAP' TO WS-SECTION                                 
136100     STRING 'WDM6C1  (WDM6C1KY=>' W-WDM6C1KY-MIN-X                        
136200                    '&WDM6C1KY<=' W-WDM6C1KY-MAX-X                        
136300                    '&IDBYTRAP =' W-IDBYTRAP-X ')'                        
136400          DELIMITED BY SIZE INTO SSA1                                     
136500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
136600     CALL CBLTDLI USING GN WDM6C-PCB DLI-IO-WDM6C1 SSA1                   
136700     MOVE WDM6C-STATUS-CODE TO STATUS-WS                                  
136800     PERFORM IMS-STATUSKONTROLL                                           
136900     .                                                                    
137000     EJECT                                                                
137100 IMS-GN-WDM6C1-KUN  SECTION.                                              
137200       MOVE 'GN-WDM6C1-KUN' TO WS-SECTION                                 
137300     STRING 'WDM6C1  (WDM6C1KY=>' W-WDM6C1KY-MIN-X                        
137400                    '&WDM6C1KY<=' W-WDM6C1KY-MAX-X                        
137500                    '&IDKUNDNR =' W-IDKUNDNR-X ')'                        
137600          DELIMITED BY SIZE INTO SSA1                                     
137700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
137800     CALL CBLTDLI USING GN WDM6C-PCB DLI-IO-WDM6C1 SSA1                   
137900     MOVE WDM6C-STATUS-CODE TO STATUS-WS                                  
138000     PERFORM IMS-STATUSKONTROLL                                           
138100     .                                                                    
138200     SKIP2                                                                
138300 IMS-GN-WDM6C1-KUN-RAP  SECTION.                                          
138400       MOVE 'GN-WDM6C1-KUN-RAP' TO WS-SECTION                             
138500     STRING 'WDM6C1  (WDM6C1KY=>' W-WDM6C1KY-MIN-X                        
138600                    '&WDM6C1KY<=' W-WDM6C1KY-MAX-X                        
138700                    '&IDKUNDNR =' W-IDKUNDNR-X                            
138800                    '&IDBYTRAP =' W-IDBYTRAP-X ')'                        
138900          DELIMITED BY SIZE INTO SSA1                                     
139000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
139100     CALL CBLTDLI USING GN WDM6C-PCB DLI-IO-WDM6C1 SSA1                   
139200     MOVE WDM6C-STATUS-CODE TO STATUS-WS                                  
139300     PERFORM IMS-STATUSKONTROLL                                           
139400     .                                                                    
139500     EJECT                                                                
139600 IMS-GN-WDM6D1  SECTION.                                                  
139700       MOVE 'GN-WDM6D1' TO WS-SECTION                                     
139800     STRING 'WDM6D1  (WDM6D1KY=>' W-WDM6D1KY-MIN-X                        
139900                    '&WDM6D1KY<=' W-WDM6D1KY-MAX-X ')'                    
140000          DELIMITED BY SIZE INTO SSA1                                     
140100     MOVE '  GBGE' TO GODK-STATUSKODER                                    
140200     CALL CBLTDLI USING GN WDM6D-PCB DLI-IO-WDM6D1 SSA1                   
140300     MOVE WDM6D-STATUS-CODE TO STATUS-WS                                  
140400     PERFORM IMS-STATUSKONTROLL                                           
140500     .                                                                    
140600     SKIP2                                                                
140700 IMS-GN-WDM6D1-RAP  SECTION.                                              
140800       MOVE 'GN-WDM6D1-RAP' TO WS-SECTION                                 
140900     STRING 'WDM6D1  (WDM6D1KY=>' W-WDM6D1KY-MIN-X                        
141000                    '&WDM6D1KY<=' W-WDM6D1KY-MAX-X                        
141100                    '&IDBYTRAP =' W-IDBYTRAP-X ')'                        
141200          DELIMITED BY SIZE INTO SSA1                                     
141300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
141400     CALL CBLTDLI USING GN WDM6D-PCB DLI-IO-WDM6D1 SSA1                   
141500     MOVE WDM6D-STATUS-CODE TO STATUS-WS                                  
141600     PERFORM IMS-STATUSKONTROLL                                           
141700     .                                                                    
141800     EJECT                                                                
141900 IMS-GN-WDM6D1-KUN  SECTION.                                              
142000       MOVE 'GN-WDM6D1-KUN' TO WS-SECTION                                 
142100     STRING 'WDM6D1  (WDM6D1KY=>' W-WDM6D1KY-MIN-X                        
142200                    '&WDM6D1KY<=' W-WDM6D1KY-MAX-X                        
142300                    '&IDKUNDNR =' W-IDKUNDNR-X ')'                        
142400          DELIMITED BY SIZE INTO SSA1                                     
142500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
142600     CALL CBLTDLI USING GN WDM6D-PCB DLI-IO-WDM6D1 SSA1                   
142700     MOVE WDM6D-STATUS-CODE TO STATUS-WS                                  
142800     PERFORM IMS-STATUSKONTROLL                                           
142900     .                                                                    
143000     SKIP2                                                                
143100 IMS-GN-WDM6D1-KUN-RAP  SECTION.                                          
143200       MOVE 'GN-WDM6D1-KUN-RAP' TO WS-SECTION                             
143300     STRING 'WDM6D1  (WDM6D1KY=>' W-WDM6D1KY-MIN-X                        
143400                    '&WDM6D1KY<=' W-WDM6D1KY-MAX-X                        
143500                    '&IDKUNDNR =' W-IDKUNDNR-X                            
143600                    '&IDBYTRAP =' W-IDBYTRAP-X ')'                        
143700          DELIMITED BY SIZE INTO SSA1                                     
143800     MOVE '  GBGE' TO GODK-STATUSKODER                                    
143900     CALL CBLTDLI USING GN WDM6D-PCB DLI-IO-WDM6D1 SSA1                   
144000     MOVE WDM6D-STATUS-CODE TO STATUS-WS                                  
144100     PERFORM IMS-STATUSKONTROLL                                           
144200     .                                                                    
144300     EJECT                                                                
144400 IMS-GHU-WDM601 SECTION.                                                  
144500       MOVE 'GHU-WDM601' TO WS-SECTION                                    
144600     STRING 'WDM601  (WDM601KY =' W-WDM601KY-X ')'                        
144700          DELIMITED BY SIZE INTO SSA1                                     
144800     MOVE '  GE' TO GODK-STATUSKODER                                      
144900     CALL CBLTDLI USING GHU WDM6-PCB DLI-IO-WDM601 SSA1                   
145000     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
145100     PERFORM IMS-STATUSKONTROLL                                           
145200     .                                                                    
145300     SKIP2                                                                
145400 IMS-GNP-WDM611  SECTION.                                                 
145500       MOVE 'GNP-WDM611' TO WS-SECTION                                    
145600                                                                          
145700     MOVE   'WDM611'          TO SSA1                                     
145800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
145900     CALL CBLTDLI USING GNP  WDM6-PCB DLI-IO-WDM611 SSA1                  
146000     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
146100     PERFORM IMS-STATUSKONTROLL                                           
146200     .                                                                    
146300     SKIP2                                                                
146400 IMS-REPL-WDM601 SECTION.                                                 
146500       MOVE 'REPL-WDM601' TO WS-SECTION                                   
146600                                                                          
146700     MOVE '  ' TO GODK-STATUSKODER                                        
146800     CALL CBLTDLI USING REPL WDM6-PCB DLI-IO-WDM601                       
146900     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
147000     PERFORM IMS-STATUSKONTROLL                                           
147100     .                                                                    
147200     EJECT                                                                
147300 IMS-GU-WDB601    SECTION.                                                
147400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
147500          DELIMITED BY SIZE INTO SSA1                                     
147600     MOVE '  GE' TO GODK-STATUSKODER                                      
147700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
147800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
147900     PERFORM IMS-STATUSKONTROLL                                           
148000     IF SEGMENT-SAKNAS                                                    
148100         MOVE SPACE TO DCS-KDDC                                           
148200     END-IF                                                               
148300     .                                                                    
148400 IMS-GHU-WDK711 SECTION.                                                  
148500                                                                          
148600     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-K7-X ')'                      
148700          DELIMITED BY SIZE INTO SSA1                                     
148800     STRING 'WDK711  (IDDC    = ' W-IDDC-K7-X ')'                         
148900          DELIMITED BY SIZE INTO SSA2                                     
149000     MOVE '  GE' TO GODK-STATUSKODER                                      
149100     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
149200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
149300     PERFORM IMS-STATUSKONTROLL                                           
149400     .                                                                    
149500     EJECT                                                                
149600 IMS-STATUSKONTROLL SECTION.                                              
149700                                                                          
149800     SET STATUS-IX TO 1                                                   
149900     SEARCH GODK-STATUS                                                   
150000       AT END                                                             
150100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
150200         DELIMITED BY SIZE INTO FELTEXT                                   
150300         CALL FELLOG                                                      
150400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
150500         CONTINUE                                                         
150600     END-SEARCH                                                           
150700     .                                                                    
