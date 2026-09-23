000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4074800.                                                
000300 AUTHOR.         SUSANNE OLSSON.                                          
000400 DATE-WRITTEN.   01/07/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET VISAR STATUS FÖR AKTUELLA INLÄGGNINGSLISTOR           
000900*        S.K. I-LISTOR. VEM SOM HAR SKAPAT EN LISTA OCH DATUM NÄR,        
001000*        ÄVEN VEM SOM HAR SKRIVIT UT EN LISTA OCH DATUM NÄR DETTA         
001100*        GJORDES.                                                         
001200*        NY FUNKTION E-TRACKER 829496 MÖJLIGHET ATT PRINTA FLERA          
001300*        I-LISTOR MED KANTKOD X.                                          
001400*                                                                         
001500*        PROGRAMMET LÄSER      WDA2E                                      
001600*        PROGRAMMET LÄSER      WDK6                                       
001700*        PROGRAMMET LÄSER      WDK7                                       
001710*        PROGRAMMET LÄSER      WDR5                                       
001800*        PROGRAMMET UPPDATERAR WDA2                                       
001900*                                                                         
002000*    PGM-ÄNDRING:                                                         
002100*        SCR/ETRACKER NR. 829496  DATUM 2004-09                           
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W4T748                                              
002500*        MID:         W4I74801                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W4O74801                                            
002900                                                                          
003000     SKIP3                                                                
003100 ENVIRONMENT DIVISION.                                                    
003200                                                                          
003300 DATA DIVISION.                                                           
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W4074800'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004200                                                                          
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  WS-KDBEHX                   PIC X       VALUE SPACE.                 
004700 77  W-PRINT                     PIC X       VALUE 'X'.                   
004800                                                                          
004900*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005000 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005100 77  4795-IX                     PIC S9(4)  VALUE +0    COMP SYNC.        
005200 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
005300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005400                                                                          
005500                                                                          
005600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005700     88  NYCKLAR-OK                          VALUE 'J'.                   
005800     88  NYCKLAR-FEL                         VALUE 'N'.                   
005900                                                                          
006000 77  INDATA-SW                   PIC X       VALUE 'J'.                   
006100     88  INDATA-OK                           VALUE 'J'.                   
006200     88  INDATA-FEL                          VALUE 'N'.                   
006300                                                                          
006400 77  SW-KDCMD                    PIC X       VALUE 'N'.                   
006500     88  KDCMD-IFYLLT                        VALUE 'J'.                   
006600                                                                          
006700 77  SW-KDCMD-RAETT-IFYLLD       PIC X       VALUE 'N'.                   
006800     88  KDCMD-RAETT                         VALUE 'J'.                   
006900     88  KDCMD-FEL                           VALUE 'N'.                   
007000                                                                          
007100 77  SW-IDANSTNR-RAETT-IFYLLD    PIC X       VALUE 'J'.                   
007200     88  IDANSTNR-RAETT                      VALUE 'J'.                   
007300     88  IDANSTNR-FEL                        VALUE 'N'.                   
007400                                                                          
007500 77  SW-IDPRT-RAETT-IFYLLD       PIC X       VALUE 'J'.                   
007600     88  IDPRT-RAETT                         VALUE 'J'.                   
007700     88  IDPRT-FEL                           VALUE 'N'.                   
007800                                                                          
007900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008000     88  EGEN-MID                            VALUE '4748'.                
008100     88  GODK-MID                            VALUE '4738' '4848'.         
008200     88  HELP-MID                            VALUE '0551'.                
008300     EJECT                                                                
008400*      --- VALID IDDC CODES                                               
008500*                                                                         
008600*01    -COPY WWDC99                                                       
008700     EJECT                                                                
008800*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008900 01  GENERELLA-SUBPROGRAM.                                                
009000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009400     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
009500     EJECT                                                                
009600*01  -COPY W006PRT                                                        
009700     EJECT                                                                
009800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009900*01 -COPY WMEDAREA                                                        
010000     SKIP3                                                                
010100 01  MESSAGE-CODES.                                                       
010200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010300     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
010400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010410     03  INF-DC-MISSING          PIC X(3)    VALUE '026'.                 
010500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010700     03  ERR-FLERA-FUNKTIONER    PIC X(3)    VALUE '097'.                 
010800     03  ERR-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
010900     03  ERR-UPPGIFTER-SAKNAS    PIC X(3)    VALUE '760'.                 
011000     03  INF-PRESS-PF4           PIC X(3)    VALUE '081'.                 
011100     03  INF-PRINT-BEG           PIC X(3)    VALUE '118'.                 
011200     03  ERR-NOTHING-PRINTED     PIC X(3)    VALUE '167'.                 
011300     03  ERR-NO-LINE-CHOSEN      PIC X(3)    VALUE '231'.                 
011400     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
011500     03  ERR-WRONG-COMMAND-CODE  PIC X(3)    VALUE '304'.                 
011600     EJECT                                                                
011700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011800*                                                                         
011900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012000     SKIP3                                                                
012100*01 -COPY WMSGINIT                                                        
012200     EJECT                                                                
012300*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
012400*                                                                         
012500 01  SPAR-AREA.                                                           
012600     03  SPAR-IDTRANS           PIC X(4)    VALUE '4748'.                 
012700     03  SPAR-MID-KDBEHX        PIC X       VALUE SPACE.                  
013000     03  SPAR-IDILIST-ENTER     PIC  9(5)   VALUE ZERO.                   
014100     03  SPAR-IDILIST-NEXT      PIC  9(5)   VALUE ZERO.                   
015000     EJECT                                                                
015100*                                                                         
015200 01  FILLER              PIC X(16)   VALUE 'P-TO-P-AREA2'.                
015300*                                                                         
015400 01  P-TO-P-T95.                                                          
015500*----TILL W40795                                                          
015600     03  P-TO-P2-LL              PIC S9(4)   COMP SYNC.                   
015700     03  P-TO-P2-Z1              PIC X(1)    VALUE LOW-VALUE.             
015800     03  P-TO-P2-Z2              PIC X(1)    VALUE LOW-VALUE.             
015900     03  P-TO-P2-TRANSKOD        PIC X(7)    VALUE 'W4T795X'.             
016000     03  FILLER                  PIC X(1)    VALUE SPACE.                 
016100     03  P-TO-P2-IDTRANS         PIC X(4)    VALUE '4748'.                
016200     03  P-TO-P2-KDMFSFOR        PIC X(1)    VALUE SPACE.                 
016300*    03  MID -COPY W4I79501 -PRE MOD4795-                                 
016400     EJECT                                                                
016500*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016600*                                                                         
016700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016800     SKIP3                                                                
016900*01  MID -COPY W4I74801                                                   
017000     EJECT                                                                
017100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
017200     SKIP3                                                                
017300*01  -COPY WMSGAREA                                                       
017400     EJECT                                                                
017500     03  MOD REDEFINES MSG-AREA.                                          
017600*      05  -COPY W4O74801                                                 
017700     EJECT                                                                
017800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017900     SKIP3                                                                
018000*01  -COPY WMFSAREA                                                       
018100     EJECT                                                                
018200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018300*                                                                         
018400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018500     SKIP3                                                                
018600 01  NYCKLAR-TILL-DLI.                                                    
018700*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
018800                                                                          
018900     03  W-WDGXKEY-X.                                                     
019100         05  W-IDHTYP            PIC  X(4)    VALUE '4703'.               
019101         05  W-IDDC-4704         PIC  X(2)    VALUE SPACE.                
019102         05  FILLER              PIC  X(24)   VALUE LOW-VALUE.            
019110                                                                          
019120     03  W-IDILIST-MIN-X.                                                 
019130         05  W-IDILIST-MIN       PIC  9(5)          VALUE ZERO.           
019200                                                                          
019300     03  W-IDILIST-MAX-X.                                                 
019400         05  W-IDILIST-MAX       PIC  9(5)          VALUE ZERO.           
020000                                                                          
021200                                                                          
021300     03  W-WDA2ESEQ-MIN-X.                                                
021400         05  W-IDDC-ESEQ-MIN     PIC  X(2)          VALUE SPACE.          
021500         05  W-IDILIST-ESEQ-MIN  PIC  9(5)          VALUE ZERO.           
021600         05  W-ADLAGOMR-ESEQ-MIN PIC S9(3)   COMP-3 VALUE ZERO.           
021700         05  W-ADGANG-ESEQ-MIN   PIC S9(3)   COMP-3 VALUE ZERO.           
021800         05  W-ADPLATS-ESEQ-MIN  PIC S9(5)   COMP-3 VALUE ZERO.           
021900                                                                          
022000     03  W-WDA2ESEQ-MAX-X.                                                
022100         05  W-IDDC-ESEQ-MAX     PIC  X(2)          VALUE SPACE.          
022200         05  W-IDILIST-ESEQ-MAX  PIC  9(5)          VALUE ZERO.           
022300         05  W-ADLAGOMR-ESEQ-MAX PIC S9(3)   COMP-3 VALUE ZERO.           
022400         05  W-ADGANG-ESEQ-MAX   PIC S9(3)   COMP-3 VALUE ZERO.           
022500         05  W-ADPLATS-ESEQ-MAX  PIC S9(5)   COMP-3 VALUE ZERO.           
022600                                                                          
022700     03  W-IDARTNR-X.                                                     
022800         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
022900                                                                          
023000     03  W-IDDC-X.                                                        
023100         05  W-IDDC              PIC  X(2)          VALUE SPACE.          
023200                                                                          
023210                                                                          
023300     SKIP2                                                                
023400*    --- STATUS-KOD FRÅN IMS                                              
023500 01  STATUS-WS                   PIC XX.                                  
023600     88  SEGMENT-FINNS                       VALUE '  '.                  
023700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
024000     SKIP2                                                                
024100 01  GODK-STATUSKODER.                                                    
024200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024300     SKIP3                                                                
024400 01  SSA1                        PIC X(128).                              
024500 01  SSA2                        PIC X(128).                              
024600     EJECT                                                                
024700*    --- IMS FUNKTIONSKODER                                               
024800*01  -COPY W0003                                                          
024900     EJECT                                                                
025000*    ---  DLI INPUT-OUTPUT AREA                                           
025100                                                                          
025200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
025300 01  DLI-IO-WDGX01.                                                       
025400*    03  -COPY WDGX01                                                     
025500     EJECT                                                                
025510                                                                          
025520 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4704'.                    
025530 01  DLI-IO-WDGX4704.                                                     
025540*    03  -COPY WDGX4704                                                   
025550     EJECT                                                                
025600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA211'.                      
025700 01  DLI-IO-WDA211.                                                       
025800*    03  -COPY WDA211                                                     
025900     EJECT                                                                
026000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
026100 01  DLI-IO-WDK611.                                                       
026200*    03  -COPY WDK611                                                     
026300     EJECT                                                                
026400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
026500 01  DLI-IO-WDK711.                                                       
026600*    03  -COPY WDK711                                                     
026700     EJECT                                                                
026800 LINKAGE SECTION.                                                         
026900*01  -COPY W0009   -PRE MSG-                                              
027000     EJECT                                                                
027100*01  -COPY W0009   -PRE W4795-                                            
027200     EJECT                                                                
027300*01  -COPY W0008   -PRE USEA-                                             
027400     05  FILLER                  PIC X.                                   
027500                                                                          
027600*01  -COPY W0008  -PRE WDR5-                                              
027700     05  FILLER                  PIC X.                                   
027800                                                                          
027900*01  -COPY W0008  -PRE WDK6-                                              
028000     05  FILLER                  PIC X.                                   
028100                                                                          
028200*01  -COPY W0008  -PRE WDK7-                                              
028300     05  FILLER                  PIC X.                                   
028400                                                                          
028500*01  -COPY W0008  -PRE WDA2-                                              
028600     05  FILLER                  PIC X.                                   
028700     EJECT                                                                
028800 PROCEDURE DIVISION  USING MSG-PCB W4795-PCB USEA-PCB WDR5-PCB            
028900     WDK6-PCB WDK7-PCB WDA2-PCB.                                          
029000 MAIN SECTION.                                                            
029100     ENTRY 'DLITCBL' USING MSG-PCB W4795-PCB USEA-PCB WDR5-PCB            
029200     WDK6-PCB WDK7-PCB WDA2-PCB.                                          
029300                                                                          
029400     PERFORM IMS-GET-MSG                                                  
029500     IF SEGMENT-FINNS                                                     
029600       PERFORM A-INIT                                                     
029700       PERFORM B-KOLLA-NYCKLAR                                            
029800       IF NYCKLAR-OK                                                      
029900         IF MFS-PRINT                                                     
030000           PERFORM G-KOLLA-INPUT                                          
030100           IF INDATA-OK                                                   
030200             PERFORM H-UPPDATERA-SKRIV-UT                                 
030300           END-IF                                                         
030400         ELSE                                                             
030500           IF MFS-FIRST                                                   
030600             PERFORM C-FOERSTA-SIDA                                       
030700           ELSE                                                           
030800             IF MFS-NEXT                                                  
030900               PERFORM D-NAESTA-SIDA                                      
031000             ELSE                                                         
031100               PERFORM E-SAMMA-SIDA                                       
031200             END-IF                                                       
031300           END-IF                                                         
031400         END-IF                                                           
031500                                                                          
031600         IF MFS-PRINT                                                     
031700           CONTINUE                                                       
031800         ELSE                                                             
031900           IF INDATA-OK                                                   
032000             PERFORM F-LAES-VISA-INFO                                     
032100           END-IF                                                         
032200         END-IF                                                           
032300       END-IF                                                             
032400                                                                          
032500       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O74801 + 4                      
032600       PERFORM IMS-INSERT-MSG                                             
032700     END-IF                                                               
032800                                                                          
032900     MOVE ZERO TO RETURN-CODE                                             
033000     GOBACK                                                               
033100     .                                                                    
033200     EJECT                                                                
033300 A-INIT SECTION.                                                          
033400                                                                          
033500     IF MSG-DUBBLA-TRANSKODER                                             
033600       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I74801                 
033700       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
033800       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
033900     ELSE                                                                 
034000       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I74801                  
034100       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
034200       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
034300     END-IF                                                               
034400                                                                          
034500     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
034600     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
034700     MOVE MFS-IDTRANS TO W-IDTRANS                                        
034800                                                                          
034900     MOVE LOW-VALUE TO MSG-AREA                                           
035000     MOVE 'W4O748N1' TO MFS-IDMOD                                         
035100     MOVE '4748' TO MOD-IDTRANS                                           
035200     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
035300                                                                          
035400     MOVE SPACE                       TO MED-IDMFSINF                     
035500     MOVE SPACE                       TO MED-IDMFSFEL                     
035600                                                                          
035700     IF EGEN-MID OR HELP-MID                                              
035800       CONTINUE                                                           
035900     ELSE                                                                 
036000       MOVE SPACE TO MFS-KDTRTYP                                          
036100       MOVE '7' TO MFS-IDPFK                                              
036200     END-IF                                                               
036300                                                                          
036400     MOVE ZERO                  TO W-IDILIST-MIN                          
036700     MOVE 99999                 TO W-IDILIST-MAX                          
036900                                                                          
037000     .                                                                    
037100     EJECT                                                                
037200 B-KOLLA-NYCKLAR SECTION.                                                 
037300                                                                          
037400     MOVE ALL '+'           TO MSGI-WMSGINIT                              
037500     MOVE '001'             TO MSGI-KDCALL                                
037600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
037700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
037800     MOVE '4748'            TO MSGI-IDTRANS                               
037900     IF EGEN-MID                                                          
038000        MOVE MID-KDBEHX-IN  TO WS-KDBEHX                                  
038100        MOVE MID-IDILIST-IN TO MSGI-IDILIST                               
038200     END-IF                                                               
038300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
038400     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
038500                                                                          
038600     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
038700     MOVE '2'               TO MFS-KDMFSFOR                               
038800                                                                          
038900     MOVE JA TO NYCKLAR-SW                                                
039000                                                                          
039100*    -- KONTROLL AV IDILIST                                               
039200     MOVE MFS-RENSA-FAELT TO MOD-IDILIST-IN                               
039300                                                                          
039400     IF MID-IDILIST-IN  NOT = ALL '+'                                     
039500       MOVE '7'         TO MFS-IDPFK                                      
039600       MOVE SPACE       TO MFS-KDTRTYP                                    
039700     END-IF                                                               
039800     INSPECT MSGI-IDILIST REPLACING LEADING SPACE BY ZERO                 
039900     IF MSGI-IDILIST NUMERIC                                              
040000       MOVE MSGI-IDILIST TO W-IDILIST-MIN                                 
040100                            W-IDILIST-MAX                                 
040200     ELSE                                                                 
040300       MOVE NEJ TO NYCKLAR-SW                                             
040400     END-IF                                                               
040500                                                                          
040600*    -- KONTROLL AV KDBEHX                                                
040700     MOVE MFS-RENSA-FAELT TO MOD-KDBEHX-IN                                
040800                                                                          
040900     IF MID-KDBEHX-IN  NOT = ALL '+'                                      
041000       MOVE '7'         TO MFS-IDPFK                                      
041100       MOVE SPACE       TO MFS-KDTRTYP                                    
041200       IF MID-KDBEHX-IN = 'C' OR 'S' OR 'P' OR 'T'                        
041300         CONTINUE                                                         
041400       ELSE                                                               
041500         MOVE NEJ TO NYCKLAR-SW                                           
041600       END-IF                                                             
041700     END-IF                                                               
041800                                                                          
042100     MOVE MSGI-IDDC               TO W-IDDC-ESEQ-MIN                      
042200                                     W-IDDC-ESEQ-MAX                      
042300                                     W-IDDC-4704                          
042310                                     W-IDDC                               
042400                                     WS-IDDC                              
042500                                                                          
042600     IF GODK-MID OR NYCKLAR-OK                                            
042700       MOVE MSGI-IDILIST        TO MOD-IDILIST-UT                         
042800       INSPECT MOD-IDILIST-UT REPLACING LEADING ZERO BY SPACE             
042900       IF MFS-FIRST                                                       
043000         IF EGEN-MID                                                      
043100           IF  MID-KDBEHX-IN = ALL '+'                                    
043200             MOVE SPACE             TO MOD-KDBEHX-UT                      
043300           ELSE                                                           
043400             MOVE WS-KDBEHX         TO MOD-KDBEHX-UT                      
043500           END-IF                                                         
043600           IF  MID-IDILIST-IN = ALL '+'                                   
043700             MOVE SPACE             TO MOD-IDILIST-UT                     
043800           END-IF                                                         
043900         ELSE                                                             
044000*-OM MAN HOPPAR FRÅN EN ANNAN BILD SÅ VISAS UNIK LISTA.                   
044100           MOVE SPACE             TO MOD-KDBEHX-UT                        
044200         END-IF                                                           
044300       ELSE                                                               
044400         IF SPAR-IDTRANS = '4748'                                         
044500           IF SPAR-MID-KDBEHX = ALL '+'                                   
044600             MOVE SPACE             TO MOD-KDBEHX-UT                      
044700           ELSE                                                           
044800             MOVE SPAR-MID-KDBEHX   TO MOD-KDBEHX-UT                      
044900             MOVE SPACE             TO MOD-IDILIST-UT                     
045000           END-IF                                                         
045100         ELSE                                                             
045200           MOVE MFS-RENSA-FAELT TO MOD-KDBEHX-UT                          
045300         END-IF                                                           
045400       END-IF                                                             
045500     ELSE                                                                 
045600       MOVE MFS-RENSA-FAELT TO MOD-KDBEHX-UT                              
045700                               MOD-IDILIST-UT                             
045800     END-IF                                                               
045900                                                                          
046000     IF ( MID-KDBEHX-IN NOT = ALL '+' ) AND                               
046100        ( MID-IDILIST-IN NOT = ALL '+')                                   
046200       MOVE ERR-FLERA-FUNKTIONER    TO MED-IDMFSFEL                       
046300       MOVE NEJ TO NYCKLAR-SW                                             
046400     END-IF                                                               
046500                                                                          
046600     IF NYCKLAR-FEL                                                       
046700       IF MED-IDMFSFEL  = SPACE                                           
046800         MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                               
047000       END-IF                                                             
047100       CALL WMEDKONV USING MED-WMEDAREA                                   
047200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
047300       PERFORM MFS-RENSA-FAELT-UT                                         
047400     END-IF                                                               
047500     .                                                                    
047600     EJECT                                                                
047700 C-FOERSTA-SIDA SECTION.                                                  
047800                                                                          
047900     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
048000     CALL WMEDKONV USING MED-WMEDAREA                                     
048100     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
048200     IF EGEN-MID                                                          
048300       IF MID-KDBEHX-IN = '+' AND MID-IDILIST-IN = ALL '+'  AND           
048400          SPAR-IDTRANS = '4748'                                           
048500         MOVE SPAR-MID-KDBEHX    TO WS-KDBEHX                             
048600       END-IF                                                             
048700     END-IF                                                               
048800                                                                          
048900     PERFORM MFS-RENSA-FAELT-IN                                           
049000     .                                                                    
049100     EJECT                                                                
049200 D-NAESTA-SIDA SECTION.                                                   
049300                                                                          
049400     IF SPAR-IDTRANS = '4748'                                             
049500       MOVE SPAR-IDILIST-NEXT  TO W-IDILIST-MIN-X                         
049600       MOVE SPAR-MID-KDBEHX    TO WS-KDBEHX                               
049700     ELSE                                                                 
049800       MOVE ZERO               TO W-IDILIST-MIN-X                         
050100       PERFORM MFS-RENSA-FAELT-IN                                         
050200     END-IF                                                               
050300     .                                                                    
050400     EJECT                                                                
050500 E-SAMMA-SIDA SECTION.                                                    
050600                                                                          
050700     IF EGEN-MID OR HELP-MID                                              
050800       IF SPAR-IDTRANS = '4748'                                           
050900         MOVE SPAR-IDILIST-ENTER  TO W-IDILIST-MIN-X                      
051000         MOVE SPAR-MID-KDBEHX     TO WS-KDBEHX                            
051100         IF MID-INPUT                =  ALL '+'                           
051200           PERFORM MFS-RENSA-FAELT-IN                                     
051300         ELSE                                                             
051400           MOVE +1                 TO INDX                                
051500           PERFORM UNTIL INDX       >  MAX-INDX                           
051600             IF MID-KDCMD(INDX) NOT = ALL '+'                             
051700               IF MID-KDCMD(INDX) = 'X'                                   
051800                 MOVE INF-PRESS-PF4         TO MED-IDMFSINF               
051900                 CALL WMEDKONV USING MED-WMEDAREA                         
052000                 MOVE MED-TEMFSINF          TO MOD-TEMFSFEL               
052100                 MOVE MAX-INDX TO INDX                                    
052200               ELSE                                                       
052300                 MOVE ERR-WRONG-COMMAND-CODE TO MED-IDMFSFEL              
052400                 CALL WMEDKONV USING MED-WMEDAREA                         
052500                 MOVE MED-MFSFEL TO MOD-TEMFSFEL                          
052600                 MOVE MAX-INDX TO INDX                                    
052700               END-IF                                                     
052800             END-IF                                                       
052900             ADD +1                TO INDX                                
053000           END-PERFORM                                                    
053100                                                                          
053200           IF MID-IDPRT NOT = ALL '+'                                     
053300             MOVE INF-PRESS-PF4         TO MED-IDMFSINF                   
053400             CALL WMEDKONV USING MED-WMEDAREA                             
053500             MOVE MED-TEMFSINF           TO MOD-TEMFSFEL                  
053600           END-IF                                                         
053700                                                                          
053800           PERFORM EA-MID-INDATA-TILL-MOD                                 
053900         END-IF                                                           
054000       END-IF                                                             
054100     END-IF                                                               
054200     .                                                                    
054300     EJECT                                                                
054400 EA-MID-INDATA-TILL-MOD SECTION.                                          
054500     SKIP2                                                                
054600                                                                          
054700     IF MID-IDPRT = ALL '+'                                               
054800       MOVE MFS-RENSA-FAELT TO MOD-IDPRT-UPD                              
054900     ELSE                                                                 
055000       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRT-UPD-ATTR                   
055100       MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRT-UPD                            
055200     END-IF                                                               
055300                                                                          
055400     IF MID-IDANSTNR = ALL '+'                                            
055500       MOVE MFS-RENSA-FAELT TO MOD-IDANSTNR-UPD                           
055600     ELSE                                                                 
055700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDANSTNR-UPD-ATTR                
055800       MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSTNR-UPD                         
055900     END-IF                                                               
056000                                                                          
056100     MOVE +1                          TO INDX                             
056200     PERFORM UNTIL INDX               >  MAX-INDX                         
056300       IF MID-KDCMD(INDX)            NOT = ALL '+'                        
056400          MOVE MID-KDCMD(INDX)       TO MOD-KDCMD(INDX)                   
056500          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR(INDX)              
056600       ELSE                                                               
056700          MOVE MFS-RENSA-FAELT       TO MOD-KDCMD(INDX)                   
056800       END-IF                                                             
056900       ADD +1                        TO INDX                              
057000     END-PERFORM                                                          
057100     .                                                                    
057200     EJECT                                                                
057300                                                                          
057400 F-LAES-VISA-INFO SECTION.                                                
057500                                                                          
057600     IF WS-KDBEHX = '+' OR ' '                                            
057700*- LÄS UNIK I-LISTA                                                       
057800       CONTINUE                                                           
057900     ELSE                                                                 
058000*- LÄS ALLA I-LISTOR                                                      
058100       IF MFS-FIRST                                                       
058200         MOVE ZERO           TO W-IDILIST-MIN                             
058300         MOVE 99999          TO W-IDILIST-MAX                             
058400       ELSE                                                               
058500         MOVE 99999          TO W-IDILIST-MAX                             
058600       END-IF                                                             
058700     END-IF                                                               
058800                                                                          
058900     PERFORM IMS-GU-WDGX01                                                
058901     IF SEGMENT-SAKNAS                                                    
058902        MOVE INF-DC-MISSING TO MED-IDMFSINF                               
058903        CALL WMEDKONV USING    MED-WMEDAREA                               
058904        MOVE MED-MFSINF     TO MOD-TEMFSINF                               
058905     ELSE                                                                 
058910        PERFORM IMS-GNP-WDGX4704                                          
059000                                                                          
059100        IF SEGMENT-SAKNAS                                                 
059200          MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                              
059300          CALL WMEDKONV USING   MED-WMEDAREA                              
059400          MOVE MED-MFSFEL    TO MOD-TEMFSFEL                              
059500          PERFORM MFS-RENSA-FAELT-UT                                      
059600                                                                          
059700          MOVE W-IDILIST-MIN          TO SPAR-IDILIST-ENTER               
059800          MOVE WS-KDBEHX              TO SPAR-MID-KDBEHX                  
059900        ELSE                                                              
060100          MOVE 4704-IDILIST           TO SPAR-IDILIST-ENTER               
061100          MOVE WS-KDBEHX              TO SPAR-MID-KDBEHX                  
061200                                                                          
061300          EVALUATE WS-KDBEHX                                              
061400            WHEN ' '                                                      
061500*-IFALL MAN KOMMER FRÅN ANNAN BILD VIA HÖRNET                             
061600              PERFORM FA-VISA-UNIK-ILISTA                                 
061700            WHEN '+'                                                      
061800              PERFORM FA-VISA-UNIK-ILISTA                                 
061900            WHEN 'T'                                                      
062000              PERFORM FB-VISA-ALLA-ILISTOR                                
062100            WHEN 'S'                                                      
062200              PERFORM FC-VISA-SKAPADE-ILISTOR                             
062300            WHEN 'C'                                                      
062400              PERFORM FC-VISA-SKAPADE-ILISTOR                             
062500            WHEN 'P'                                                      
062600              PERFORM FD-VISA-PRINTADE-ILISTOR                            
062700          END-EVALUATE                                                    
062800                                                                          
062900*--FLYTTA NEXTNYCKLAR FÖR BLÄDDRING.                                      
063000                                                                          
063100          IF SEGMENT-FINNS                                                
063300            MOVE 4704-IDILIST           TO SPAR-IDILIST-NEXT              
064300            MOVE WS-KDBEHX              TO SPAR-MID-KDBEHX                
064400            MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                     
064500            CALL WMEDKONV USING MED-WMEDAREA                              
064600            MOVE MED-TEMFSINF TO MOD-TEMFSINF                             
064700          ELSE                                                            
064900            MOVE 4704-IDILIST           TO SPAR-IDILIST-NEXT              
065900            MOVE WS-KDBEHX              TO SPAR-MID-KDBEHX                
066000            IF MED-IDMFSINF = '006'  OR '081'                             
066100              CONTINUE                                                    
066200            ELSE                                                          
066300              MOVE INF-LAST-PAGE  TO MED-IDMFSINF                         
066400              CALL WMEDKONV USING    MED-WMEDAREA                         
066500              MOVE MED-MFSINF     TO MOD-TEMFSINF                         
066510            END-IF                                                        
066600         END-IF                                                           
066700       END-IF                                                             
066800     END-IF                                                               
066900                                                                          
067000     MOVE '002'      TO MSGI-KDCALL                                       
067100     MOVE '4748'     TO SPAR-IDTRANS                                      
067200     MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                    
067300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
067400     .                                                                    
067500     EJECT                                                                
067600 FA-VISA-UNIK-ILISTA SECTION.                                             
067700                                                                          
067800     MOVE +1 TO INDX                                                      
067900                                                                          
067910     MOVE 4704-IDILIST            TO MOD-IDILIST  (INDX)                  
067920     MOVE 4704-TIREGDAT-ILI       TO MOD-TIREGDAT-ILI (INDX)              
067930     MOVE 4704-IDANSTNR-ILIR      TO MOD-IDANSTNR-ILIR (INDX)             
067940     MOVE 4704-TIUPPDAT-ILI       TO MOD-TIUPPDAT-ILI (INDX)              
067950     MOVE 4704-IDANSTNR-ILIU      TO MOD-IDANSTNR-ILIU (INDX)             
067960     MOVE 4704-TIUTSKR            TO MOD-TIUTSKR-ILI (INDX)               
067970                                                                          
067980     IF 4704-TIUTSKR > ZERO                                               
067990       MOVE 4704-IDANSTNR-ILIP    TO MOD-IDANSTNR-RET-PRT (INDX)          
067991     ELSE                                                                 
067992       MOVE MFS-RENSA-FAELT       TO MOD-IDANSTNR-RET-PRT (INDX)          
067993     END-IF                                                               
067994                                                                          
067995     ADD 1 TO INDX                                                        
067996                                                                          
068000     PERFORM UNTIL INDX > MAX-INDX                                        
070000        PERFORM MFS-RENSA-RAD-FAELT-UT                                    
070100        MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDCMD-ATTR (INDX)             
070300        ADD 1 TO INDX                                                     
070400     END-PERFORM                                                          
070500     .                                                                    
070600     EJECT                                                                
070700 FB-VISA-ALLA-ILISTOR SECTION.                                            
070800                                                                          
070900*- SKALL BARA VISA VARJE LISTNR EN GÅNG.                                  
071000                                                                          
071100     MOVE +1 TO INDX                                                      
071200                                                                          
071300     MOVE 4704-IDILIST        TO MOD-IDILIST  (INDX)                      
071500     MOVE 4704-TIREGDAT-ILI   TO MOD-TIREGDAT-ILI (INDX)                  
071600     MOVE 4704-IDANSTNR-ILIR  TO MOD-IDANSTNR-ILIR (INDX)                 
071700     MOVE 4704-TIUPPDAT-ILI   TO MOD-TIUPPDAT-ILI (INDX)                  
071800     MOVE 4704-IDANSTNR-ILIU  TO MOD-IDANSTNR-ILIU (INDX)                 
071900     MOVE 4704-TIUTSKR        TO MOD-TIUTSKR-ILI (INDX)                   
072000                                                                          
072100     IF 4704-TIUTSKR > ZERO                                               
072200       MOVE 4704-IDANSTNR-ILIP TO MOD-IDANSTNR-RET-PRT (INDX)             
072400     ELSE                                                                 
072500       MOVE MFS-RENSA-FAELT    TO MOD-IDANSTNR-RET-PRT (INDX)             
072600     END-IF                                                               
072700     ADD 1 TO INDX                                                        
072800     PERFORM IMS-GNP-WDGX4704                                             
072900                                                                          
073000     PERFORM UNTIL INDX > MAX-INDX                                        
073100        IF SEGMENT-FINNS                                                  
073500           MOVE 4704-IDILIST        TO MOD-IDILIST  (INDX)                
073700           MOVE 4704-TIREGDAT-ILI   TO MOD-TIREGDAT-ILI (INDX)            
073800           MOVE 4704-IDANSTNR-ILIR  TO MOD-IDANSTNR-ILIR (INDX)           
073900           MOVE 4704-TIUPPDAT-ILI   TO MOD-TIUPPDAT-ILI (INDX)            
074000           MOVE 4704-IDANSTNR-ILIU  TO MOD-IDANSTNR-ILIU (INDX)           
074100           MOVE 4704-TIUTSKR        TO MOD-TIUTSKR-ILI (INDX)             
074200                                                                          
074300           IF 4704-TIUTSKR > ZERO                                         
074400            MOVE 4704-IDANSTNR-ILIP TO MOD-IDANSTNR-RET-PRT(INDX)         
074600           ELSE                                                           
074700            MOVE MFS-RENSA-FAELT    TO MOD-IDANSTNR-RET-PRT(INDX)         
074800           END-IF                                                         
074900           ADD 1 TO INDX                                                  
075100         PERFORM IMS-GNP-WDGX4704                                         
075200       ELSE                                                               
075300         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
075400         MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDCMD-ATTR (INDX)            
075500         ADD 1 TO INDX                                                    
075600       END-IF                                                             
075700     END-PERFORM                                                          
075800     .                                                                    
075900     EJECT                                                                
076000 FC-VISA-SKAPADE-ILISTOR  SECTION.                                        
076100                                                                          
076200     MOVE +1 TO INDX                                                      
076300                                                                          
076400     IF 4704-TIUTSKR = ZERO                                               
076500       PERFORM S01-FLYTTA-TILL-MOD                                        
076600     ELSE                                                                 
076700       PERFORM UNTIL 4704-TIUTSKR = ZERO OR SEGMENT-SAKNAS                
076800       PERFORM IMS-GNP-WDGX4704                                           
076900         IF SEGMENT-FINNS AND 4704-TIUTSKR = ZERO                         
077000           PERFORM S01-FLYTTA-TILL-MOD                                    
077100         END-IF                                                           
077200       END-PERFORM                                                        
077300     END-IF                                                               
077400                                                                          
077500     IF SEGMENT-SAKNAS                                                    
077600       MOVE ERR-URVAL-SAKNAS TO MED-IDMFSFEL                              
077700       CALL WMEDKONV USING      MED-WMEDAREA                              
077800       MOVE MED-MFSFEL       TO MOD-TEMFSFEL                              
077900       PERFORM MFS-RENSA-FAELT-UT                                         
078000     ELSE                                                                 
078100       PERFORM IMS-GNP-WDGX4704                                           
078200       ADD +1 TO INDX                                                     
078300       PERFORM UNTIL INDX > MAX-INDX                                      
078400          IF SEGMENT-FINNS                                                
078500             IF 4704-TIUTSKR = ZERO                                       
079100                PERFORM S01-FLYTTA-TILL-MOD                               
079200                ADD 1 TO INDX                                             
079500             END-IF                                                       
079510             PERFORM IMS-GNP-WDGX4704                                     
079600          ELSE                                                            
079700             PERFORM MFS-RENSA-RAD-FAELT-UT                               
079800             MOVE MFS-STAENG-FAELT-NOMOD TO MOD-KDCMD-ATTR (INDX)         
079900             ADD +1 TO INDX                                               
080000          END-IF                                                          
080100       END-PERFORM                                                        
080200     END-IF                                                               
080300     .                                                                    
080400     EJECT                                                                
080500 FD-VISA-PRINTADE-ILISTOR  SECTION.                                       
080600                                                                          
080700     MOVE +1 TO INDX                                                      
080800                                                                          
080900     IF 4704-TIUTSKR > ZERO                                               
081000        PERFORM S01-FLYTTA-TILL-MOD                                       
081100     ELSE                                                                 
081200        PERFORM UNTIL 4704-TIUTSKR > ZERO OR SEGMENT-SAKNAS               
081300           PERFORM IMS-GNP-WDGX4704                                       
081400           IF SEGMENT-FINNS                                               
081500              PERFORM S01-FLYTTA-TILL-MOD                                 
081600           END-IF                                                         
081700        END-PERFORM                                                       
081800     END-IF                                                               
081900                                                                          
082000     IF SEGMENT-SAKNAS                                                    
082100       MOVE ERR-URVAL-SAKNAS TO MED-IDMFSFEL                              
082200       CALL WMEDKONV USING MED-WMEDAREA                                   
082300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
082400       PERFORM MFS-RENSA-FAELT-UT                                         
082500     ELSE                                                                 
082600       PERFORM IMS-GNP-WDGX4704                                           
082700       ADD +1 TO INDX                                                     
082800       PERFORM UNTIL INDX > MAX-INDX                                      
082900          IF SEGMENT-FINNS                                                
083000             IF 4704-TIUTSKR = ZERO                                       
083100                PERFORM IMS-GNP-WDGX4704                                  
083200             ELSE                                                         
083600                PERFORM S01-FLYTTA-TILL-MOD                               
083700                ADD +1 TO INDX                                            
083900                PERFORM IMS-GNP-WDGX4704                                  
084000             END-IF                                                       
084100          ELSE                                                            
084200             PERFORM MFS-RENSA-RAD-FAELT-UT                               
084300             MOVE MFS-STAENG-FAELT-NOMOD TO MOD-KDCMD-ATTR (INDX)         
084400             ADD +1 TO INDX                                               
084500          END-IF                                                          
084600       END-PERFORM                                                        
084700     END-IF                                                               
084800     .                                                                    
084900     EJECT                                                                
085000 G-KOLLA-INPUT SECTION.                                                   
085100                                                                          
085200     MOVE JA  TO INDATA-SW                                                
085300                 SW-IDANSTNR-RAETT-IFYLLD                                 
085400                 SW-IDPRT-RAETT-IFYLLD                                    
085500     MOVE NEJ TO SW-KDCMD                                                 
085600                 SW-KDCMD-RAETT-IFYLLD                                    
085700                                                                          
085800     MOVE ZERO                    TO MED-IDMFSFEL                         
085900                                                                          
086000     PERFORM GA-FORMELL-KONTROLL                                          
086100     IF INDATA-OK                                                         
086200        PERFORM GB-LOGISK-KONTROLL                                        
086300     END-IF                                                               
086400                                                                          
086500     IF INDATA-FEL                                                        
086600        IF MED-IDMFSFEL           = ZERO                                  
086700           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
086800        END-IF                                                            
086900        CALL WMEDKONV USING MED-WMEDAREA                                  
087000        MOVE MED-MFSFEL           TO MOD-TEMFSFEL                         
087100        PERFORM MFS-ROER-EJ-FAELT-UT                                      
087200        PERFORM MFS-ROER-EJ-FAELT-IN                                      
087300                                                                          
087400        IF KDCMD-RAETT                                                    
087500          PERFORM MFS-LAES-IN-IGEN                                        
087600        END-IF                                                            
087700        IF IDPRT-RAETT                                                    
087800          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDPRT-UPD-ATTR                
087900        END-IF                                                            
088000        IF IDANSTNR-RAETT                                                 
088100          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDANSTNR-UPD-ATTR             
088200        END-IF                                                            
088300     END-IF                                                               
088400                                                                          
088500     .                                                                    
088600     EJECT                                                                
088700 GA-FORMELL-KONTROLL SECTION.                                             
088800                                                                          
088900     IF MID-INPUT                = ALL '+'                                
089000       MOVE ERR-NOTHING-PRINTED  TO MED-IDMFSFEL                          
089100       MOVE NEJ                  TO INDATA-SW                             
089200     ELSE                                                                 
089300       PERFORM GAA-KOLLA-KDCMD                                            
089400       PERFORM GAB-KOLLA-IDPRT                                            
089500       PERFORM GAC-KOLLA-IDANSTNR                                         
089600     END-IF                                                               
089700                                                                          
089800     .                                                                    
089900     EJECT                                                                
090000 GAA-KOLLA-KDCMD      SECTION.                                            
090100                                                                          
090200     MOVE +1                           TO INDX                            
090300                                                                          
090400     MOVE NEJ                           TO SW-KDCMD                       
090500                                                                          
090600     PERFORM UNTIL INDX                 >  MAX-INDX                       
090700        IF MID-KDCMD(INDX)              NOT = ALL '+'                     
090800           MOVE JA                      TO SW-KDCMD                       
090900           IF MID-KDCMD(INDX)           = 'X'                             
091000              MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-ATTR(INDX)           
091100              MOVE JA                   TO SW-KDCMD-RAETT-IFYLLD          
091200           ELSE                                                           
091300              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDCMD-ATTR(INDX)           
091400              MOVE NEJ                  TO INDATA-SW                      
091500           END-IF                                                         
091600        END-IF                                                            
091700        ADD +1                          TO INDX                           
091800     END-PERFORM                                                          
091900                                                                          
092000     IF KDCMD-IFYLLT                                                      
092100        CONTINUE                                                          
092200     ELSE                                                                 
092300        MOVE NEJ                        TO INDATA-SW                      
092400        MOVE ERR-NO-LINE-CHOSEN         TO MED-IDMFSFEL                   
092500     END-IF                                                               
092600                                                                          
092700     .                                                                    
092800     EJECT                                                                
092900 GAB-KOLLA-IDPRT      SECTION.                                            
093000                                                                          
093100     IF MID-IDPRT                       NOT = ALL '+'                     
093200        MOVE MFS-ALFA-FAELT-RAETT       TO MOD-IDPRT-UPD-ATTR             
093300     ELSE                                                                 
093400        MOVE MFS-ALFA-FAELT-FEL         TO MOD-IDPRT-UPD-ATTR             
093500        MOVE ERR-WRONG-PRINTER          TO MED-IDMFSFEL                   
093600        MOVE NEJ                        TO INDATA-SW                      
093700        MOVE NEJ   TO SW-IDPRT-RAETT-IFYLLD                               
093800     END-IF                                                               
093900                                                                          
094000     .                                                                    
094100     EJECT                                                                
094200 GAC-KOLLA-IDANSTNR SECTION.                                              
094300                                                                          
094400     IF CDC-SE                                                            
094500       IF MID-IDANSTNR = ALL '+'                                          
094600         MOVE MFS-NUM-FAELT-FEL           TO MOD-IDANSTNR-UPD-ATTR        
094700         MOVE ERR-UPPGIFTER-SAKNAS        TO MED-IDMFSFEL                 
094800         MOVE NEJ                         TO INDATA-SW                    
094900         MOVE NEJ  TO SW-IDANSTNR-RAETT-IFYLLD                            
095000       ELSE                                                               
095100         IF MID-IDANSTNR NUMERIC                                          
095200           MOVE MFS-NUM-FAELT-RAETT       TO MOD-IDANSTNR-UPD-ATTR        
095300         ELSE                                                             
095400           MOVE MFS-NUM-FAELT-FEL         TO MOD-IDANSTNR-UPD-ATTR        
095500           MOVE NEJ                       TO INDATA-SW                    
095600           MOVE NEJ  TO SW-IDANSTNR-RAETT-IFYLLD                          
095700         END-IF                                                           
095800       END-IF                                                             
095900     ELSE                                                                 
096000       IF MID-IDANSTNR NOT = ALL '+'                                      
096100         IF MID-IDANSTNR NOT NUMERIC                                      
096200           MOVE MFS-NUM-FAELT-FEL         TO MOD-IDANSTNR-UPD-ATTR        
096300           MOVE NEJ                       TO INDATA-SW                    
096400           MOVE NEJ  TO SW-IDANSTNR-RAETT-IFYLLD                          
096500         ELSE                                                             
096600           MOVE MFS-NUM-FAELT-RAETT       TO MOD-IDANSTNR-UPD-ATTR        
096700         END-IF                                                           
096800       END-IF                                                             
096900     END-IF                                                               
097000     .                                                                    
097100     EJECT                                                                
097200 GB-LOGISK-KONTROLL SECTION.                                              
097300                                                                          
097400     PERFORM GBA-KOLLA-IDPRT                                              
097500     .                                                                    
097600     EJECT                                                                
097700                                                                          
097800 GBA-KOLLA-IDPRT                  SECTION.                                
097900                                                                          
098000     MOVE SPACE                TO PRT-IDPRTLST                            
098100     MOVE '4RT'                TO PRT-IDPRTLST(1:3)                       
098200                                                                          
098300     MOVE MID-IDPRT            TO PRT-IDPRTLST(4:3)                       
098400     MOVE 1                    TO PRT-KDCALL                              
098500     CALL W006PRT USING PRT-W006PRT                                       
098600                                                                          
098700     IF PRT-KDSVAR                     = 'F'                              
098800         MOVE ERR-WRONG-KEY            TO MED-IDMFSFEL                    
098900         MOVE PRT-IDPRTLST             TO MED-TEMFSINF                    
099000         MOVE NEJ                      TO INDATA-SW                       
099100         MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDPRT-UPD-ATTR              
099200         MOVE NEJ  TO SW-IDPRT-RAETT-IFYLLD                               
099300     END-IF                                                               
099400     .                                                                    
099500     EJECT                                                                
099600 H-UPPDATERA-SKRIV-UT SECTION.                                            
099700                                                                          
099800     PERFORM HA-BEHANDLA-VALDA-TILLSTAND                                  
099900                                                                          
100000     MOVE INF-PRINT-BEG          TO MED-IDMFSINF                          
100100     CALL WMEDKONV USING MED-WMEDAREA                                     
100200     MOVE MED-MFSINF             TO MOD-TEMFSINF                          
100300     PERFORM MFS-FORM-ATTR                                                
100400     PERFORM MFS-RENSA-FAELT-IN                                           
100500     PERFORM MFS-ROER-EJ-FAELT-UT                                         
100600     .                                                                    
100700     EJECT                                                                
100800 HA-BEHANDLA-VALDA-TILLSTAND SECTION.                                     
100900                                                                          
101000     MOVE +1                    TO INDX                                   
101100                                   4795-IX                                
101200     PERFORM UNTIL INDX > MAX-INDX                                        
101300                                                                          
101400       IF MID-KDCMD(INDX)         = W-PRINT                               
101500         MOVE MSGI-IDDC           TO MOD4795-MID-IDDC                     
101600                                                                          
101700         IF MID-IDILIST(INDX) NUMERIC AND                                 
101800            MID-IDILIST(INDX) > ZERO                                      
101900           MOVE MID-IDILIST(INDX)  TO MOD4795-MID-IDILIST(4795-IX)        
102000                                      W-IDILIST-ESEQ-MIN                  
102100                                      W-IDILIST-ESEQ-MAX                  
102200                                                                          
102300           ADD +1                 TO 4795-IX                              
102400           PERFORM IMS-GHU-SEQE-WDA211                                    
102500           PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                   
102600                                                                          
102700             PERFORM HAA-KOLLA-PLATS                                      
102800                                                                          
102900             IF MID-IDANSTNR NUMERIC                                      
103000               MOVE MID-IDANSTNR    TO LEV-IDANSTNR-RET                   
103100             END-IF                                                       
103200                                                                          
103300             ACCEPT LEV-TIUTSKR    FROM DATE                              
103400             PERFORM IMS-REPL-SEQE-WDA211                                 
103500                                                                          
103600             PERFORM IMS-GHN-SEQE-WDA211                                  
103700                                                                          
103800           END-PERFORM                                                    
103900         END-IF                                                           
104000       END-IF                                                             
104100       ADD +1                     TO INDX                                 
104200     END-PERFORM                                                          
104300                                                                          
104400     IF 4795-IX                > +1                                       
104500         PERFORM HAB-STARTA-4795                                          
104600     END-IF                                                               
104700     .                                                                    
104800     EJECT                                                                
104900 HAA-KOLLA-PLATS  SECTION.                                                
105000                                                                          
105100     MOVE LEV-IDARTNR          TO W-IDARTNR                               
105200                                                                          
105300     IF CDC-SE                                                            
105400       PERFORM IMS-GU-WDK611                                              
105500       IF SEGMENT-FINNS                                                   
105600         IF (CLAG-ADLAGOMR = LEV-ADLAGOMR    AND                          
105700            CLAG-ADGANG   = LEV-ADGANG       AND                          
105800            CLAG-ADPLATS  = LEV-ADPLATS)     OR                           
105900           (CLAG-ADLAGOMR = 22               AND                          
106000            LEV-ADLAGOMR  = 21               AND                          
106100            CLAG-ADGANG   = LEV-ADGANG       AND                          
106200            CLAG-ADPLATS  = LEV-ADPLATS)     OR                           
106300           (CLAG-ADLAGOMR = 31               AND                          
106400            LEV-ADLAGOMR  = 30               AND                          
106500            CLAG-ADGANG   = LEV-ADGANG       AND                          
106600            CLAG-ADPLATS  = LEV-ADPLATS)                                  
106700           CONTINUE                                                       
106800         ELSE                                                             
106900            MOVE CLAG-ADLAGOMR      TO LEV-ADLAGOMR                       
107000            IF LEV-ADLAGOMR = 22                                          
107100              MOVE 21               TO LEV-ADLAGOMR                       
107200            END-IF                                                        
107300            IF LEV-ADLAGOMR = 31                                          
107400              MOVE 30               TO LEV-ADLAGOMR                       
107500            END-IF                                                        
107600            MOVE CLAG-ADGANG        TO LEV-ADGANG                         
107700            MOVE CLAG-ADPLATS       TO LEV-ADPLATS                        
107800         END-IF                                                           
107900       END-IF                                                             
108000     ELSE                                                                 
108100       PERFORM IMS-GU-WDK711                                              
108200       IF SEGMENT-FINNS                                                   
108300         IF (SLAG-ADLAGOMR = LEV-ADLAGOMR    AND                          
108400            SLAG-ADGANG   = LEV-ADGANG       AND                          
108500            SLAG-ADPLATS  = LEV-ADPLATS)     OR                           
108600           (SLAG-ADLAGOMR = 22               AND                          
108700            LEV-ADLAGOMR  = 21               AND                          
108800            SLAG-ADGANG   = LEV-ADGANG       AND                          
108900            SLAG-ADPLATS  = LEV-ADPLATS)                                  
109000           CONTINUE                                                       
109100         ELSE                                                             
109200            MOVE SLAG-ADLAGOMR      TO LEV-ADLAGOMR                       
109300            IF LEV-ADLAGOMR = 22                                          
109400              MOVE 21               TO LEV-ADLAGOMR                       
109500            END-IF                                                        
109600            MOVE SLAG-ADGANG        TO LEV-ADGANG                         
109700            MOVE SLAG-ADPLATS       TO LEV-ADPLATS                        
109800         END-IF                                                           
109900       END-IF                                                             
110000     END-IF                                                               
110100     .                                                                    
110200     EJECT                                                                
110300 HAB-STARTA-4795  SECTION.                                                
110400                                                                          
110500     IF MSGI-IDLAND-SPR = 'SE'                                            
110600       MOVE '1'                 TO P-TO-P2-KDMFSFOR                       
110700     ELSE                                                                 
110800       MOVE MFS-KDMFSFOR        TO P-TO-P2-KDMFSFOR                       
110900     END-IF                                                               
111000                                                                          
111100     MOVE 'W40748'              TO MOD4795-MID-IDPGM                      
111200     MOVE PRT-IDPRTLST          TO MOD4795-MID-IDPRTLST                   
111300     MOVE MSGI-IDDC             TO MOD4795-MID-IDDC                       
111400     COMPUTE MOD4795-MID-KVPOST = 4795-IX - 1                             
111500                                                                          
111600     COMPUTE P-TO-P2-LL = LENGTH OF MOD4795-MID-W4I79501 + 17             
111700                                                                          
111800     PERFORM IMS-ISRT-MSG-ALT-4795                                        
111900     .                                                                    
112000     EJECT                                                                
112100 S01-FLYTTA-TILL-MOD SECTION.                                             
112200                                                                          
112300     MOVE 4704-IDILIST        TO MOD-IDILIST  (INDX)                      
112500     MOVE 4704-TIREGDAT-ILI   TO MOD-TIREGDAT-ILI (INDX)                  
112600     MOVE 4704-IDANSTNR-ILIR  TO MOD-IDANSTNR-ILIR (INDX)                 
112700     MOVE 4704-TIUPPDAT-ILI   TO MOD-TIUPPDAT-ILI (INDX)                  
112800     MOVE 4704-IDANSTNR-ILIU  TO MOD-IDANSTNR-ILIU (INDX)                 
112900     MOVE 4704-TIUTSKR        TO MOD-TIUTSKR-ILI (INDX)                   
113000                                                                          
113100     IF 4704-TIUTSKR > ZERO                                               
113200       MOVE 4704-IDANSTNR-ILIP TO MOD-IDANSTNR-RET-PRT (INDX)             
113400     ELSE                                                                 
113500       MOVE MFS-RENSA-FAELT    TO MOD-IDANSTNR-RET-PRT (INDX)             
113600     END-IF                                                               
113700     .                                                                    
113800     EJECT                                                                
113900 MFS-RENSA-FAELT-IN SECTION.                                              
114000                                                                          
114100*    --- ALLA INDATA-FÄLT                                                 
114200     MOVE MFS-RENSA-FAELT       TO MOD-IDPRT-UPD                          
114300                                   MOD-IDANSTNR-UPD                       
114400                                                                          
114500     MOVE +1 TO INDX                                                      
114600     PERFORM UNTIL INDX         >  MAX-INDX                               
114700       MOVE MFS-RENSA-FAELT     TO MOD-KDCMD(INDX)                        
114800       ADD +1                   TO INDX                                   
114900     END-PERFORM                                                          
115000     .                                                                    
115100     EJECT                                                                
115200 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
115300                                                                          
115400*    --- ALLA INDATA-FÄLT                                                 
115500     MOVE MFS-ROER-EJ-FAELT TO MOD-IDPRT-UPD                              
115600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDANSTNR-UPD                           
115700     MOVE +1 TO INDX                                                      
115800     PERFORM UNTIL INDX > MAX-INDX                                        
115900         MOVE MFS-ROER-EJ-FAELT   TO MOD-KDCMD (INDX)                     
116000         ADD +1 TO INDX                                                   
116100     END-PERFORM                                                          
116200     .                                                                    
116300     SKIP3                                                                
116400 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
116500                                                                          
116600*    --- ALLA UTDATA-FÄLT                                                 
116700                                                                          
116800     MOVE +1 TO INDX                                                      
116900     PERFORM UNTIL INDX > MAX-INDX                                        
117000         MOVE MFS-ROER-EJ-FAELT  TO MOD-IDILIST         (INDX)            
117100                                    MOD-TIREGDAT-ILI    (INDX)            
117200                                    MOD-IDANSTNR-ILIR   (INDX)            
117300                                    MOD-TIUPPDAT-ILI    (INDX)            
117400                                    MOD-IDANSTNR-ILIU   (INDX)            
117500                                    MOD-TIUTSKR-ILI     (INDX)            
117600                                    MOD-IDANSTNR-RET-PRT(INDX)            
117700         ADD +1 TO INDX                                                   
117800     END-PERFORM                                                          
117900     .                                                                    
118000     EJECT                                                                
118100 MFS-LAES-IN-IGEN SECTION.                                                
118200                                                                          
118300*    --- ALLA INDATA-FÄLT                                                 
118400                                                                          
118500     MOVE +1 TO INDX                                                      
118600     PERFORM UNTIL INDX > MAX-INDX                                        
118700       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-ATTR (INDX)                
118800       ADD +1 TO INDX                                                     
118900     END-PERFORM                                                          
119000     .                                                                    
119100     SKIP3                                                                
119200 MFS-RENSA-FAELT-UT  SECTION.                                             
119300                                                                          
119400*    --- ALLA UTDATA-FÄLT                                                 
119500*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
119600                                                                          
119700     MOVE +1 TO INDX                                                      
119800     PERFORM UNTIL INDX > MAX-INDX                                        
119900       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
120000       ADD +1 TO INDX                                                     
120100     END-PERFORM                                                          
120200     .                                                                    
120300     SKIP2                                                                
120400 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
120500                                                                          
120600*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
120700     MOVE MFS-RENSA-FAELT TO MOD-IDILIST          (INDX)                  
120800                             MOD-TIREGDAT-ILI     (INDX)                  
120900                             MOD-IDANSTNR-ILIR    (INDX)                  
121000                             MOD-TIUPPDAT-ILI     (INDX)                  
121100                             MOD-IDANSTNR-ILIU    (INDX)                  
121200                             MOD-TIUTSKR-ILI      (INDX)                  
121300                             MOD-IDANSTNR-RET-PRT (INDX)                  
121400     .                                                                    
121500     SKIP3                                                                
121600 MFS-FORM-ATTR SECTION.                                                   
121700                                                                          
121800*    --- ALLA INDATA-FÄLT                                                 
121900     MOVE MFS-FORMATETS-ATTR TO MOD-IDPRT-UPD-ATTR                        
122000     MOVE MFS-FORMATETS-ATTR TO MOD-IDANSTNR-UPD-ATTR                     
122100                                                                          
122200     MOVE +1 TO INDX                                                      
122300     PERFORM UNTIL INDX > MAX-INDX                                        
122400         MOVE MFS-FORMATETS-ATTR TO MOD-KDCMD-ATTR (INDX)                 
122500         ADD +1 TO INDX                                                   
122600     END-PERFORM                                                          
122700     .                                                                    
122800     EJECT                                                                
122900* --- IMS SEKTIONER ---                                                   
123000     SKIP3                                                                
123100 IMS-GET-MSG SECTION.                                                     
123200                                                                          
123300     MOVE '  QC' TO GODK-STATUSKODER                                      
123400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
123500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
123600     PERFORM IMS-STATUSKONTROLL                                           
123700     .                                                                    
123800     SKIP3                                                                
123900 IMS-INSERT-MSG SECTION.                                                  
124000                                                                          
124100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
124200     MOVE SPACE TO GODK-STATUSKODER                                       
124300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
124400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
124500     PERFORM IMS-STATUSKONTROLL                                           
124600     .                                                                    
124700     EJECT                                                                
124800 IMS-GU-WDGX01   SECTION.                                                 
124900                                                                          
125000     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-X ')'                         
125200          DELIMITED BY SIZE INTO SSA1                                     
125300     MOVE '  GE' TO GODK-STATUSKODER                                      
125400     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDGX01 SSA1                    
125500     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
125600     PERFORM IMS-STATUSKONTROLL                                           
125700     .                                                                    
125800     EJECT                                                                
125900 IMS-GNP-WDGX4704 SECTION.                                                
126000                                                                          
126030     STRING 'WDGX4704(IDILIST >=' W-IDILIST-MIN-X                         
126040                    '&IDILIST <=' W-IDILIST-MAX-X ')'                     
126050          DELIMITED BY SIZE INTO SSA1                                     
126400     MOVE '  GE'           TO GODK-STATUSKODER                            
126500     CALL CBLTDLI USING GNP WDR5-PCB DLI-IO-WDGX4704 SSA1                 
126600     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
126700     PERFORM IMS-STATUSKONTROLL                                           
126800     .                                                                    
126900     EJECT                                                                
127000 IMS-ISRT-MSG-ALT-4795 SECTION.                                           
127100                                                                          
127200     MOVE SPACE              TO GODK-STATUSKODER                          
127300     CALL CBLTDLI USING      ISRT W4795-PCB                               
127400                                  P-TO-P-T95                              
127500     MOVE W4795-STATUS-CODE   TO STATUS-WS                                
127600     PERFORM IMS-STATUSKONTROLL                                           
127700     .                                                                    
127800     SKIP3                                                                
127900 IMS-GU-WDK611 SECTION.                                                   
128000                                                                          
128100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
128200          DELIMITED BY SIZE INTO SSA1                                     
128300     MOVE 'WDK611   ' TO SSA2                                             
128400     MOVE '  ' TO GODK-STATUSKODER                                        
128500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
128600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
128700     PERFORM IMS-STATUSKONTROLL                                           
128800     .                                                                    
128900     SKIP3                                                                
129000 IMS-GU-WDK711 SECTION.                                                   
129100                                                                          
129200     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
129300          DELIMITED BY SIZE INTO SSA1                                     
129400     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
129500          DELIMITED BY SIZE INTO SSA2                                     
129600     MOVE '  GE' TO GODK-STATUSKODER                                      
129700     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
129800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
129900     PERFORM IMS-STATUSKONTROLL                                           
130000     .                                                                    
130100     SKIP3                                                                
130200 IMS-GHU-SEQE-WDA211    SECTION.                                          
130300                                                                          
130400     STRING 'WDA211  (WDA2ESEQ>=' W-WDA2ESEQ-MIN-X                        
130500                    '&WDA2ESEQ<=' W-WDA2ESEQ-MAX-X ')'                    
130600          DELIMITED BY SIZE INTO SSA1                                     
130700     MOVE '  GE'           TO GODK-STATUSKODER                            
130800     CALL CBLTDLI USING GHU WDA2-PCB DLI-IO-WDA211 SSA1                   
130900     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
131000     PERFORM IMS-STATUSKONTROLL                                           
131100     .                                                                    
131200                                                                          
131300 IMS-GHN-SEQE-WDA211    SECTION.                                          
131400                                                                          
131500     STRING 'WDA211  (WDA2ESEQ>=' W-WDA2ESEQ-MIN-X                        
131600                    '&WDA2ESEQ<=' W-WDA2ESEQ-MAX-X ')'                    
131700          DELIMITED BY SIZE INTO SSA1                                     
131800     MOVE '  GEGB'           TO GODK-STATUSKODER                          
131900     CALL CBLTDLI USING GHN WDA2-PCB DLI-IO-WDA211 SSA1                   
132000     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
132100     PERFORM IMS-STATUSKONTROLL                                           
132200     .                                                                    
132300                                                                          
132400 IMS-REPL-SEQE-WDA211   SECTION.                                          
132500                                                                          
132600     MOVE '    '           TO GODK-STATUSKODER                            
132700     CALL CBLTDLI USING REPL WDA2-PCB DLI-IO-WDA211                       
132800     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
132900     PERFORM IMS-STATUSKONTROLL                                           
133000     .                                                                    
133100     EJECT                                                                
133200 IMS-STATUSKONTROLL SECTION.                                              
133300                                                                          
133400     SET STATUS-IX TO 1                                                   
133500     SEARCH GODK-STATUS                                                   
133600       AT END                                                             
133700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
133800         DELIMITED BY SIZE INTO FELTEXT                                   
133900         CALL FELLOG                                                      
134000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
134100         CONTINUE                                                         
134200     END-SEARCH                                                           
134300     .                                                                    
