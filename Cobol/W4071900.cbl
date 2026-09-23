000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4071900.                                                
000400 AUTHOR.         OLSSON SUSANNE.                                          
000500 DATE-WRITTEN.   08/07/15.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET VISAR VILKA FAKTURARADER SOM FINNS I STATUS W         
001000*        FÖR HANTERINGSKOSTNAD AV RETURER.MAN FRÅGAR PÅ PARMA ID          
001100*        MEN KAN ANGE DISTR + KUND OCH DÅ HÄMTAR PGM PARMA ID FRÅN        
001200*        DL1-BAS. MAN KAN ÄVEN LÄGGA IN STYRDATA. OM INTE MIN-            
001300*        GRÄNSVÄRDET FÖR FAKTURERING SÄTTS, SÅ ANVÄNDS ETT DEFAULT        
001400*        MINIMIVÄRDE.PARMA ID = DEFAULT.MAN KAN ÄVEN TA BORT EN           
001500*        RAD MED KANTKOD=D.                                               
001600*        MAN KAN SKICKA ALLA RADER I STATUS W TILL FAKTURERING I          
001700*        FÖRTID,OM MAN INTE VILL VÄNTA TILLS MIN-VÄRDEGRÄNSEN ÄR          
001800*        NÅDD ELLER ÅRETS SLUT.TP8LRET UPPDATERAS MED 'M ' I              
001900*        KDRAPPSTA OCH MAN SKICKAR EN STARTTRANS TILL PGM                 
002000*SO??    W40789  WZ01-MODULEN (CARPARTS.PULS.INVKRE).                     
002100*                                                                         
002200*                                                                         
002300*        PROGRAMMET UPPDATERAR TABELL TP8LRET                             
002400*        PROGRAMMET UPPDATERAR TABELL TP8IRET                             
002500*        PROGRAMMET LÄSER      WDB2                                       
002600*        PROGRAMMET LÄSER      WDB1                                       
002800*                                                                         
002900*    INDATA.                                                              
003000*        TRANSAKTION: W4T719                                              
003100*                     W4T719U                                             
003200*        MID:         W4I71901                                            
003300*                                                                         
003400*    UTDATA.                                                              
003500*        MOD:         W4O71901                                            
003600*                                                                         
003700*    E'TRACKER 880053   DATED 2008-07                                     
003800*                                                                         
003900                                                                          
004000     SKIP3                                                                
004100 ENVIRONMENT DIVISION.                                                    
004200                                                                          
004300 DATA DIVISION.                                                           
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600 77  IDPGM                       PIC X(08)   VALUE 'W4071900'.            
004700                                                                          
004800*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004900 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005000                                                                          
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300 77  WS-IDPARTNR-IN              PIC X(9)    VALUE SPACE.                 
005400 77  WS-IDFTG-IN                 PIC X(2)    VALUE SPACE.                 
005500 77  WS-KDANMORS                 PIC X(2)    VALUE SPACE.                 
005600 77  WS-KDVALISO                 PIC X(3)    VALUE SPACE.                 
005700 77  WS-SUMMA                    PIC S9(7)V9(2) VALUE ZERO COMP-3.        
005800 77  WS-SUMMA-TOT                PIC S9(7)V9(2) VALUE ZERO COMP-3.        
005900 77  WS-SUARTBTO-JFR             PIC S9(7)V9(2) VALUE +0 COMP-3.          
006000 77  WS-IDEXCUST-1               PIC X(15)   VALUE SPACE.                 
006100 77  WS-IDEXCUST-2               PIC X(15)   VALUE SPACE.                 
006200 77  WS-MID-KDANMORS             PIC X(2)    VALUE SPACE.                 
006300 77  WS-MID-IDREF                PIC X(15)   VALUE SPACE.                 
006400 77  WS-MID-DAREGDAT             PIC X(8)    VALUE SPACE.                 
006500                                                                          
006600*    --- INDEX FÖR BLÄDDRINGSRADER                                        
006700 77  INDX                        PIC S9(3)  VALUE +0    COMP SYNC.        
006800 77  MAX-INDX                    PIC S9(3)  VALUE +8    COMP SYNC.        
006900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007000                                                                          
007100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
007200     88  NYCKLAR-OK                          VALUE 'J'.                   
007300     88  NYCKLAR-FEL                         VALUE 'N'.                   
007400                                                                          
007500 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007600     88  INDATA-OK                           VALUE 'J'.                   
007700     88  INDATA-FEL                          VALUE 'N'.                   
007800                                                                          
007900 77  KDCMD-SW                    PIC X       VALUE 'N'.                   
008000     88  KDCMD-ALL-PLUS                      VALUE 'J'.                   
008100     88  KDCMD-NOT-ALL-PLUS                  VALUE 'N'.                   
008200                                                                          
008300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008400     88  EGEN-MID                            VALUE '4719'.                
008500     88  GODK-MID                            VALUE '4707' '4709'          
008600                                                   '4719'.                
008700     88  HELP-MID                            VALUE '0551'.                
008800     EJECT                                                                
008900                                                                          
009000 01  KDRC-DISPLAY                PIC Z(5).                                
009100     EJECT                                                                
009200                                                                          
009300     EJECT                                                                
009400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009500 01  GENERELLA-SUBPROGRAM.                                                
009600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010000     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
010100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010200     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
010300     EJECT                                                                
010400                                                                          
010500 01  FILLER                      PIC X(16)   VALUE 'WDECEDIT'.            
010600     SKIP3                                                                
010700*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
010800*01 -COPY WDECAREA                                                        
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'WMEDKONV'.            
011100     SKIP3                                                                
011200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011300*01 -COPY WMEDAREA                                                        
011400     SKIP3                                                                
011500 01  MESSAGE-CODES.                                                       
011600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
012000     03  ERR-BETALARE-SAKNAS     PIC X(3)    VALUE '145'.                 
012100     03  ERR-DIST-KUND-SAKNAS    PIC X(3)    VALUE '412'.                 
012200     03  ERR-INFO-MISSING        PIC X(3)    VALUE '413'.                 
012300     03  ERR-VALUTAKOD-SAKNAS    PIC X(3)    VALUE '148'.                 
012400     03  ERR-PRICE-MISSING       PIC X(3)    VALUE '301'.                 
012500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
012600     03  ERR-RAD-FINNS-REDAN     PIC X(3)    VALUE '245'.                 
012700     03  ERR-FLERA-FUNKTIONER    PIC X(3)    VALUE '097'.                 
012800     03  ERR-UPDATE-NOT-OK       PIC X(3)    VALUE '007'.                 
012900     03  ERR-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
013000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
013100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
013200     EJECT                                                                
013300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013400*                                                                         
013500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013600     SKIP3                                                                
013700*01 -COPY WMSGINIT                                                        
013800     EJECT                                                                
013900                                                                          
014000*    --- PARAMETRAR TILL SUBPROGRAM ABEND                                 
014100*                                                                         
014200 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
014300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)  COMP VALUE +16.               
014400 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
014500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  VALUE +1000 COMP SYNC.        
014600                                                                          
014700*    --- AREOR FÖR KOMMUNIKATION                                          
014800 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
014900*01  -COPY WZ01SEND                                                       
015000     EJECT                                                                
015100 01  SEND-AREA.                                                           
015200*    03  -COPY WZ01REQU                                                   
015300     EJECT                                                                
015400                                                                          
015500*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
015600*                                                                         
015700 01  SPAR-AREA.                                                           
015800     03  SPAR-IDTRANS           PIC X(4)  VALUE '4719'.                   
015900     03  SPAR-IDPARTNR-ENTER    PIC X(9)  VALUE SPACE.                    
016000     03  SPAR-IDPARTNR-NEXT     PIC X(9)  VALUE SPACE.                    
016100     03  SPAR-IDFTG-ENTER       PIC X(2)  VALUE SPACE.                    
016200     03  SPAR-IDFTG-NEXT        PIC X(2)  VALUE SPACE.                    
016300     03  SPAR-KDANMORS-ENTER    PIC X(2)  VALUE SPACE.                    
016400     03  SPAR-KDANMORS-NEXT     PIC X(2)  VALUE SPACE.                    
016500     03  SPAR-DAREGDAT-ENTER    PIC X(8)  VALUE SPACE.                    
016600     03  SPAR-DAREGDAT-NEXT     PIC X(8)  VALUE SPACE.                    
016700     03  SPAR-IDREF-ENTER       PIC X(15) VALUE SPACE.                    
016800     03  SPAR-IDREF-NEXT        PIC X(15) VALUE SPACE.                    
016900     03  SPAR-IDEXCUST-1-ENTER  PIC X(15) VALUE SPACE.                    
017000     03  SPAR-IDEXCUST-1-NEXT   PIC X(15) VALUE SPACE.                    
017100     03  SPAR-IDEXCUST-2-ENTER  PIC X(15) VALUE SPACE.                    
017200     03  SPAR-IDEXCUST-2-NEXT   PIC X(15) VALUE SPACE.                    
017300     03  SPAR-MID-IDPARTNR      PIC X(9)  VALUE SPACE.                    
017400     03  SPAR-MID-IDFTG         PIC X(2)  VALUE SPACE.                    
017500     EJECT                                                                
017600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
017700*                                                                         
017800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
017900     SKIP3                                                                
018000*01  MID -COPY W4I71901                                                   
018100     EJECT                                                                
018200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
018300     SKIP3                                                                
018400*01  -COPY WMSGAREA                                                       
018500     EJECT                                                                
018600     03  MOD REDEFINES MSG-AREA.                                          
018700*      05  -COPY W4O71901                                                 
018800     EJECT                                                                
018900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
019000     SKIP3                                                                
019100*01  -COPY WMFSAREA                                                       
019200     EJECT                                                                
019300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
019400*                                                                         
019500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
019600     SKIP3                                                                
019700 01  NYCKLAR-TILL-DLI.                                                    
019800*    --- VÄRDE PÅ BLÄDRINGSNYCKEL FÖR FÖRSTA RADEN PÅ SKÄRMEN             
019900     03  W-WDB101KY-X.                                                    
020000         05  W-IDPARTNR         PIC X(9)  VALUE SPACE.                    
020100         05  W-IDFTG            PIC X(2)  VALUE SPACE.                    
020200                                                                          
020300     03  W-KDANMORS-X.                                                    
020400         05  W-KDANMORS         PIC X(2)  VALUE SPACE.                    
020500                                                                          
020600     03  W-DAREGDAT-X.                                                    
020700         05  W-DAREGDAT         PIC X(8)  VALUE SPACE.                    
020800                                                                          
020900     03  W-IDREF-X.                                                       
021000         05  W-IDREF            PIC X(15) VALUE SPACE.                    
021100                                                                          
021200     03  W-IDEXCUST-1-X.                                                  
021300         05  W-IDEXCUST-1       PIC X(15) VALUE SPACE.                    
021400                                                                          
021500     03  W-IDEXCUST-2-X.                                                  
021600         05  W-IDEXCUST-2       PIC X(15) VALUE SPACE.                    
021700                                                                          
021800     03  W-IDGMT-X.                                                       
021900         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
022000         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
022100                                                                          
022200     03  W-IDGMT-MIN-X.                                                   
022300         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
022400         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
022500                                                                          
022600     03  W-IDGMT-MAX-X.                                                   
022700         05  W-IDDISTR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
022800         05  W-IDKUNDNR-MAX      PIC S9(7)   VALUE ZERO COMP-3.           
022900                                                                          
024000     SKIP2                                                                
024100*    --- STATUS-KOD FRÅN IMS                                              
024200 01  STATUS-WS                   PIC XX.                                  
024300     88  SEGMENT-FINNS                       VALUE '  '.                  
024400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
024500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024600     SKIP2                                                                
024700 01  GODK-STATUSKODER.                                                    
024800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024900     SKIP3                                                                
025000 01  SSA1                        PIC X(64).                               
025100 01  SSA2                        PIC X(64).                               
025200     EJECT                                                                
025300*    --- IMS FUNKTIONSKODER                                               
025400*01  -COPY W0003                                                          
025500     EJECT                                                                
025600 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
025700       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
025800                                                                          
025900 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
026000 01  DB2-WS.                                                              
026100     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
026200         88  CURSOR-OK                       VALUE 000.                   
026300         88  RADER-FINNS                     VALUE 000.                   
026400         88  RADER-SAKNAS                    VALUE 100.                   
026500         88  ATKOMST-FEL                     VALUE 904.                   
026600         88  RAD-DUBLETT                     VALUE 803.                   
026700         88  RADER-SAKNAS-TOMT               VALUE 305.                   
026800                                                                          
026900     03  GODK-SQLCODEKODER.                                               
027000         05  GODK-SQLCODE OCCURS 5                                        
027100             INDEXED BY SQLCODE-IX PIC 9(3).                              
027200     EJECT                                                                
027300*    ---  DLI INPUT-OUTPUT AREA                                           
027400                                                                          
027500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB101'.                      
027600 01  DLI-IO-WDB101.                                                       
027700*    03  -COPY WDB101                                                     
027800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
027900 01  DLI-IO-WDB201.                                                       
028000*    03  -COPY WDB201                                                     
028600*                                                                         
028700*    ---  DB2 HOST-COPYTEXTER                                             
028800     EJECT                                                                
028900 01  FILLER                      PIC X(16)  VALUE 'TP8LRET-AREA'.         
029000*01  -COPY TP8LRET -PRE LRET-                                             
029100                                                                          
029200 01  FILLER                      PIC X(16)  VALUE 'TP8IRET-AREA'.         
029300*01  -COPY TP8IRET -PRE IRET-                                             
029400     EJECT                                                                
029500                                                                          
029600 01  FILLER                      PIC X(16)   VALUE 'TP8LRET-DCL '.        
029700     EXEC SQL INCLUDE TP8LRET END-EXEC.                                   
029800                                                                          
029900 01  FILLER                      PIC X(16)   VALUE 'TP8IRET-DCL '.        
030000     EXEC SQL INCLUDE TP8IRET END-EXEC.                                   
030100     EJECT                                                                
030200 LINKAGE SECTION.                                                         
030300*01  -COPY W0009   -PRE MSG-                                              
030400                                                                          
030500*01  -COPY W0009   -PRE INVKRE-                                           
030600                                                                          
030700*01  -COPY W0008   -PRE WDP7-                                             
030800     05  FILLER                  PIC X.                                   
030900                                                                          
031000*01  -COPY W0008  -PRE WDB1-                                              
031100     05  FILLER                  PIC X.                                   
031200                                                                          
031300*01  -COPY W0008  -PRE WDB2-                                              
031400     05  FILLER                  PIC X.                                   
031500                                                                          
031800     EJECT                                                                
031900 PROCEDURE DIVISION  USING MSG-PCB INVKRE-PCB WDP7-PCB WDB1-PCB           
032000     WDB2-PCB.                                                            
032100 MAIN SECTION.                                                            
032200     ENTRY 'DLITCBL' USING MSG-PCB INVKRE-PCB WDP7-PCB WDB1-PCB           
032300     WDB2-PCB.                                                            
032400                                                                          
032500     PERFORM IMS-GET-MSG                                                  
032600     IF SEGMENT-FINNS                                                     
032700       PERFORM A-INIT                                                     
032800       PERFORM B-KOLLA-NYCKLAR                                            
032900       IF NYCKLAR-OK                                                      
033000         IF MFS-UPDATE                                                    
033100           PERFORM G-KOLLA-INPUT                                          
033200           IF INDATA-OK                                                   
033300             PERFORM H-UPPDATERA                                          
033400           END-IF                                                         
033500         ELSE                                                             
033600           IF MFS-FIRST                                                   
033700             PERFORM C-FOERSTA-SIDA                                       
033800           ELSE                                                           
033900             IF MFS-NEXT                                                  
034000               PERFORM D-NAESTA-SIDA                                      
034100             ELSE                                                         
034200               PERFORM E-SAMMA-SIDA                                       
034300             END-IF                                                       
034400           END-IF                                                         
034500         END-IF                                                           
034600         IF INDATA-OK                                                     
034700           PERFORM F-LAES-VISA-INFO                                       
034800         END-IF                                                           
034900       END-IF                                                             
035000       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O71901 + 4                      
035100       PERFORM IMS-INSERT-MSG                                             
035200     END-IF                                                               
035300                                                                          
035400     MOVE ZERO TO RETURN-CODE                                             
035500     GOBACK                                                               
035600     .                                                                    
035700     EJECT                                                                
035800 A-INIT SECTION.                                                          
035900                                                                          
036000     IF MSG-DUBBLA-TRANSKODER                                             
036100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I71901                 
036200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
036300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
036400     ELSE                                                                 
036500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I71901                  
036600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
036700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
036800     END-IF                                                               
036900                                                                          
037000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
037100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
037200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
037300                                                                          
037400     MOVE LOW-VALUE TO MSG-AREA                                           
037500     MOVE 'W4O719N1' TO MFS-IDMOD                                         
037600     MOVE '4719' TO MOD-IDTRANS                                           
037700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
037800                                                                          
037900     MOVE SPACE                       TO MED-IDMFSINF                     
038000     MOVE SPACE                       TO MED-IDMFSFEL                     
038100                                                                          
038200     IF EGEN-MID OR HELP-MID                                              
038300       CONTINUE                                                           
038400     ELSE                                                                 
038500       MOVE SPACE TO MFS-KDTRTYP                                          
038600       MOVE '7' TO MFS-IDPFK                                              
038700     END-IF                                                               
038800                                                                          
038900     INITIALIZE GODK-SQLCODEKODER                                         
039000                                                                          
039100     MOVE LOW-VALUE         TO W-IDGMT-MIN-X                              
039200     MOVE HIGH-VALUE        TO W-IDGMT-MAX-X                              
039500     .                                                                    
039600     EJECT                                                                
039700 B-KOLLA-NYCKLAR SECTION.                                                 
039800                                                                          
039900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
040000     MOVE '001'             TO MSGI-KDCALL                                
040100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
040200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
040300     MOVE '4719'            TO MSGI-IDTRANS                               
040400     IF EGEN-MID                                                          
040500         MOVE MID-IDPARTNR-IN  TO MSGI-IDPARTNR                           
040600                                  WS-IDPARTNR-IN                          
040700         MOVE MID-IDFTG-IN     TO MSGI-IDFTG-KEY                          
040800                                  WS-IDFTG-IN                             
040900         MOVE MID-IDDISTR-IN   TO MSGI-IDDISTR                            
041000         MOVE MID-IDKUNDNR-IN  TO MSGI-IDKUNDNR                           
041100     END-IF                                                               
041200     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
041300     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
041400                                                                          
041500     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
041600     MOVE '2'             TO MFS-KDMFSFOR                                 
041700                                                                          
041800     MOVE JA TO NYCKLAR-SW                                                
041900                                                                          
042000                                                                          
042100*    -- KONTROLL AV IDPARTNR                                              
042200     MOVE MFS-RENSA-FAELT TO MOD-IDPARTNR-IN                              
042300                                                                          
042400     IF MID-IDPARTNR-IN NOT = ALL '+'                                     
042500       MOVE '7'         TO MFS-IDPFK                                      
042600       MOVE SPACE       TO MFS-KDTRTYP                                    
042700                                                                          
042800       MOVE MSGI-IDPARTNR TO W-IDPARTNR                                   
042900     END-IF                                                               
043000                                                                          
043100*    -- KONTROLL AV IDFTG                                                 
043200     MOVE MFS-RENSA-FAELT        TO MOD-IDFTG-IN                          
043300                                                                          
043400     IF MID-IDFTG-IN NOT = ALL '+'                                        
043500       MOVE '7'                  TO MFS-IDPFK                             
043600       MOVE SPACE                TO MFS-KDTRTYP                           
043700                                                                          
043800       INSPECT MSGI-IDFTG-KEY REPLACING LEADING SPACE BY ZERO             
043900       IF MSGI-IDFTG-KEY NUMERIC                                          
044000         MOVE MSGI-IDFTG-KEY     TO W-IDFTG                               
044100       ELSE                                                               
044200         MOVE NEJ                TO NYCKLAR-SW                            
044300       END-IF                                                             
044400     ELSE                                                                 
044500       MOVE MSGI-IDFTG           TO W-IDFTG                               
044600     END-IF                                                               
044700                                                                          
044800*    -- KONTROLL AV IDDISTR                                               
044900     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
045000                                                                          
045100     IF MID-IDDISTR-IN NOT = ALL '+'                                      
045200       MOVE '7'         TO MFS-IDPFK                                      
045300       MOVE SPACE       TO MFS-KDTRTYP                                    
045400                                                                          
045500       INSPECT MSGI-IDDISTR REPLACING LEADING SPACE BY ZERO               
045600       IF MSGI-IDDISTR NUMERIC                                            
045700         MOVE MSGI-IDDISTR TO W-IDDISTR                                   
045800                              W-IDDISTR-MIN                               
045900                              W-IDDISTR-MAX                               
046000       ELSE                                                               
046100         MOVE NEJ TO NYCKLAR-SW                                           
046200       END-IF                                                             
046300     END-IF                                                               
046400                                                                          
046500*    -- KONTROLL AV IDKUNDNR                                              
046600     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
046700                                                                          
046800     IF MID-IDKUNDNR-IN NOT = ALL '+'                                     
046900       MOVE '7'         TO MFS-IDPFK                                      
047000       MOVE SPACE       TO MFS-KDTRTYP                                    
047100                                                                          
047200       INSPECT MSGI-IDKUNDNR REPLACING LEADING SPACE BY ZERO              
047300       IF MSGI-IDKUNDNR NUMERIC                                           
047400         MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                 
047500                               W-IDKUNDNR-MIN                             
047600                               W-IDKUNDNR-MAX                             
047700       ELSE                                                               
047800         MOVE NEJ TO NYCKLAR-SW                                           
047900       END-IF                                                             
048000     END-IF                                                               
048100                                                                          
048200     IF EGEN-MID OR NYCKLAR-OK                                            
048300       IF MFS-FIRST                                                       
048400         IF  MID-IDPARTNR-IN = ALL '+'                                    
048500           MOVE SPACE              TO MOD-IDPARTNR-UT                     
048600         ELSE                                                             
048700           MOVE SPAR-IDPARTNR-ENTER  TO MOD-IDPARTNR-UT                   
048800         END-IF                                                           
048900         IF  MID-IDFTG-IN    = ALL '+'                                    
049000           MOVE SPACE              TO MOD-IDFTG-UT                        
049100         ELSE                                                             
049200           MOVE SPAR-IDFTG-ENTER   TO MOD-IDFTG-UT                        
049300         END-IF                                                           
049400       ELSE                                                               
049500         IF SPAR-IDTRANS = '4719'                                         
049600           IF SPAR-MID-IDPARTNR = SPACE                                   
049700             MOVE SPACE              TO MOD-IDPARTNR-UT                   
049800           ELSE                                                           
049900             MOVE SPAR-MID-IDPARTNR  TO MOD-IDPARTNR-UT                   
050000                                        W-IDPARTNR                        
050100                                        WS-IDPARTNR-IN                    
050200           END-IF                                                         
050300           IF SPAR-MID-IDFTG = SPACE                                      
050400             MOVE SPACE              TO MOD-IDFTG-UT                      
050500           ELSE                                                           
050600             MOVE SPAR-MID-IDFTG     TO MOD-IDFTG-UT                      
050700                                        W-IDFTG                           
050800                                        WS-IDFTG-IN                       
050900           END-IF                                                         
051000         ELSE                                                             
051100           MOVE SPAR-IDPARTNR-ENTER  TO MOD-IDPARTNR-UT                   
051200                                        W-IDPARTNR                        
051300           MOVE SPAR-IDFTG-ENTER     TO MOD-IDFTG-UT                      
051400                                        W-IDFTG                           
051500         END-IF                                                           
051600       END-IF                                                             
051700       IF  MID-IDDISTR-IN = ALL '+'                                       
051800         MOVE SPACE              TO MOD-IDDISTR-UT                        
051900       ELSE                                                               
052000         MOVE MSGI-IDDISTR       TO MOD-IDDISTR-UT                        
052100         INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE           
052200       END-IF                                                             
052300       IF  MID-IDKUNDNR-IN = ALL '+'                                      
052400         MOVE SPACE              TO MOD-IDKUNDNR-UT                       
052500       ELSE                                                               
052600         MOVE MSGI-IDKUNDNR      TO MOD-IDKUNDNR-UT                       
052700         INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE          
052800       END-IF                                                             
052900     ELSE                                                                 
053000       MOVE MFS-RENSA-FAELT TO MOD-IDPARTNR-UT                            
053100                               MOD-IDFTG-UT                               
053200                               MOD-IDDISTR-UT                             
053300                               MOD-IDKUNDNR-UT                            
053400     END-IF                                                               
053500                                                                          
053600     IF EGEN-MID  AND MFS-FIRST                                           
053700       IF MID-IDPARTNR-IN NOT = ALL '+' AND                               
053800          MID-IDFTG-IN NOT = ALL '+'                                      
053900         IF MID-IDDISTR-IN NOT = ALL '+'                                  
054000           MOVE NEJ TO NYCKLAR-SW                                         
054100         END-IF                                                           
054200         IF MID-IDKUNDNR-IN NOT = ALL '+'                                 
054300           MOVE NEJ TO NYCKLAR-SW                                         
054400         END-IF                                                           
054500       END-IF                                                             
054600     END-IF                                                               
054700                                                                          
054800                                                                          
054900     IF NYCKLAR-FEL                                                       
055000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
055100       CALL WMEDKONV USING MED-WMEDAREA                                   
055200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
055300       PERFORM MFS-RENSA-FAELT-IN                                         
055400       PERFORM MFS-RENSA-FAELT-UT                                         
055500     END-IF                                                               
055600     .                                                                    
055700     EJECT                                                                
055800 C-FOERSTA-SIDA SECTION.                                                  
055900                                                                          
056000     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
056100     CALL WMEDKONV USING MED-WMEDAREA                                     
056200     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
056300                                                                          
056400     PERFORM MFS-RENSA-FAELT-IN                                           
056500                                                                          
056600     IF (MID-IDPARTNR-IN = ALL '+') AND                                   
056700        (MID-IDFTG-IN = ALL '+') AND                                      
056800        (MID-IDDISTR-IN = ALL '+')  AND                                   
056900        (MID-IDKUNDNR-IN = ALL '+')                                       
057000                                                                          
057100       IF SPAR-IDTRANS = '4719'                                           
057200         MOVE SPAR-IDPARTNR-ENTER TO WS-IDPARTNR-IN                       
057300         MOVE SPAR-IDFTG-ENTER    TO WS-IDFTG-IN                          
057400         IF SPAR-IDPARTNR-ENTER NOT = ALL '+'                             
057500           MOVE SPAR-IDPARTNR-ENTER TO W-IDPARTNR                         
057600         END-IF                                                           
057700         IF SPAR-IDFTG-ENTER NOT = ALL '+'                                
057800           MOVE SPAR-IDFTG-ENTER TO W-IDFTG                               
057900         END-IF                                                           
058000       END-IF                                                             
058100     END-IF                                                               
058200     .                                                                    
058300     EJECT                                                                
058400 D-NAESTA-SIDA SECTION.                                                   
058500                                                                          
058600     IF SPAR-IDTRANS = '4719'                                             
058700       MOVE SPAR-IDPARTNR-NEXT TO W-IDPARTNR                              
058800       MOVE SPAR-IDFTG-NEXT    TO W-IDFTG                                 
058900       MOVE SPAR-KDANMORS-NEXT TO W-KDANMORS                              
059000       MOVE SPAR-DAREGDAT-NEXT TO W-DAREGDAT                              
059100       MOVE SPAR-IDREF-NEXT    TO W-IDREF                                 
059200       MOVE SPAR-IDEXCUST-1-NEXT TO W-IDEXCUST-1                          
059300       MOVE SPAR-IDEXCUST-2-NEXT TO W-IDEXCUST-2                          
059400                                                                          
059500       MOVE SPAR-MID-IDPARTNR    TO WS-IDPARTNR-IN                        
059600       MOVE SPAR-MID-IDFTG       TO WS-IDFTG-IN                           
059700     ELSE                                                                 
059800       PERFORM MFS-RENSA-FAELT-IN                                         
059900     END-IF                                                               
060000     .                                                                    
060100     EJECT                                                                
060200 E-SAMMA-SIDA SECTION.                                                    
060300                                                                          
060400     IF EGEN-MID OR HELP-MID                                              
060500       IF SPAR-IDTRANS = '4719' OR '0551'                                 
060600         MOVE SPAR-IDPARTNR-ENTER TO W-IDPARTNR                           
060700         MOVE SPAR-IDFTG-ENTER    TO W-IDFTG                              
060800         MOVE SPAR-KDANMORS-ENTER TO W-KDANMORS                           
060900         MOVE SPAR-DAREGDAT-ENTER TO W-DAREGDAT                           
061000         MOVE SPAR-IDREF-ENTER    TO W-IDREF                              
061100         MOVE SPAR-IDEXCUST-1-ENTER TO W-IDEXCUST-1                       
061200         MOVE SPAR-IDEXCUST-2-ENTER TO W-IDEXCUST-2                       
061300                                                                          
061400         MOVE SPAR-MID-IDPARTNR   TO WS-IDPARTNR-IN                       
061500         MOVE SPAR-MID-IDFTG      TO WS-IDFTG-IN                          
061600       END-IF                                                             
061700                                                                          
061800       MOVE JA TO KDCMD-SW                                                
061900                                                                          
062000       MOVE +1 TO INDX                                                    
062100       PERFORM UNTIL INDX > MAX-INDX                                      
062200         IF MID-KDBEHX (INDX) =  '+'                                      
062300           CONTINUE                                                       
062400         ELSE                                                             
062500           MOVE NEJ TO KDCMD-SW                                           
062600         END-IF                                                           
062700         ADD +1 TO INDX                                                   
062800       END-PERFORM                                                        
062900                                                                          
063000       IF MID-INPUT = ALL '+' AND KDCMD-ALL-PLUS                          
063100         PERFORM MFS-RENSA-FAELT-IN                                       
063200       ELSE                                                               
063300         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
063400         CALL WMEDKONV USING MED-WMEDAREA                                 
063500         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
063600         PERFORM EA-MID-INDATA-TILL-MOD                                   
063700       END-IF                                                             
063800     ELSE                                                                 
063900       PERFORM MFS-RENSA-FAELT-IN                                         
064000     END-IF                                                               
064100     .                                                                    
064200     EJECT                                                                
064300 EA-MID-INDATA-TILL-MOD SECTION.                                          
064400                                                                          
064500* * * * * FÖR VARJE MID-FÄLT                                              
064600* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
064700* * * * *        FLYTTA MFS-ADD-LAES-IN-FAELT TILL MOD-INDATA-ATTR        
064800* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
064900     IF MID-SUARTBTO-UPD = ALL '+'                                        
065000       MOVE MFS-RENSA-FAELT  TO MOD-SUARTBTO-UPD                          
065100     ELSE                                                                 
065200       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-SUARTBTO-ATTR                    
065300       MOVE MFS-ROER-EJ-FAELT     TO MOD-SUARTBTO-UPD                     
065400     END-IF                                                               
065500                                                                          
065600     IF MID-KDBEHX-UPD = ALL '+'                                          
065700       MOVE MFS-RENSA-FAELT  TO MOD-KDBEHX-UPD                            
065800     ELSE                                                                 
065900       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBEHX-UPD-ATTR                  
066000       MOVE MFS-ROER-EJ-FAELT     TO MOD-KDBEHX-UPD                       
066100     END-IF                                                               
066200                                                                          
066300*- KOLLA KANTKODEN PÅ ALLA UT-RADERNA.                                    
066400                                                                          
066500     MOVE +1 TO INDX                                                      
066600     PERFORM UNTIL INDX > MAX-INDX                                        
066700       IF MID-KDBEHX (INDX)  =  '+'                                       
066800         MOVE MFS-RENSA-FAELT  TO MOD-KDBEHX (INDX)                       
066900       ELSE                                                               
067000         MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBEHX-ATTR (INDX)             
067100         MOVE MFS-ROER-EJ-FAELT     TO MOD-KDBEHX (INDX)                  
067200       END-IF                                                             
067300       ADD +1 TO INDX                                                     
067400     END-PERFORM                                                          
067500     .                                                                    
067600     EJECT                                                                
067700 F-LAES-VISA-INFO SECTION.                                                
067800                                                                          
067900     IF MFS-FIRST                                                         
068000       IF WS-IDPARTNR-IN = ALL '+' AND                                    
068100          WS-IDFTG-IN = ALL '+'                                           
068200         PERFORM IMS-GU-WDB201                                            
068300         IF SEGMENT-FINNS                                                 
068400           MOVE GMT-IDPARTNR     TO W-IDPARTNR                            
068500                                    WS-IDPARTNR-IN                        
068600                                    MOD-IDPARTNR-UT                       
068700                                    SPAR-IDPARTNR-ENTER                   
068800                                    MSGI-IDPARTNR                         
068900           MOVE GMT-IDFTG        TO W-IDFTG                               
069000                                    WS-IDFTG-IN                           
069100                                    MOD-IDFTG-UT                          
069200                                    SPAR-IDFTG-ENTER                      
069300                                    MSGI-IDFTG                            
069400           PERFORM FB-LAES-STYRDATA-IRET                                  
069500           PERFORM FA-LAES-GRUNDDATA                                      
069600         ELSE                                                             
069700           MOVE ERR-DIST-KUND-SAKNAS  TO MED-IDMFSFEL                     
069800           CALL WMEDKONV USING MED-WMEDAREA                               
069900           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
070000           PERFORM MFS-RENSA-FAELT-UT                                     
070100         END-IF                                                           
070200       ELSE                                                               
070300         MOVE SPAR-IDPARTNR-ENTER TO WS-IDPARTNR-IN                       
070400         MOVE SPAR-IDFTG-ENTER    TO WS-IDFTG-IN                          
070500                                                                          
070600         PERFORM FB-LAES-STYRDATA-IRET                                    
070700         PERFORM FA-LAES-GRUNDDATA                                        
070800       END-IF                                                             
070900     ELSE                                                                 
071000       PERFORM FB-LAES-STYRDATA-IRET                                      
071100       PERFORM FA-LAES-GRUNDDATA                                          
071200     END-IF                                                               
071300                                                                          
071400                                                                          
071500     IF MED-IDMFSFEL = '413' OR '412'                                     
071600       CONTINUE                                                           
071700     ELSE                                                                 
071800       PERFORM FC-SUM-ALLA-KODER-RAD19                                    
071900       PERFORM FD-SUM-ALLA-KODER-TOT                                      
072000     END-IF                                                               
072100                                                                          
072200     MOVE '002'      TO MSGI-KDCALL                                       
072300     MOVE '4719'   TO SPAR-IDTRANS                                        
072400     MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                    
072500     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
072600     .                                                                    
072700     EJECT                                                                
072800 FA-LAES-GRUNDDATA SECTION.                                               
072900                                                                          
073000     PERFORM DB2-DCL-OPN-TP8LRET-CRS                                      
073100     PERFORM DB2-FETCH-TP8LRET-CRS                                        
073200                                                                          
073300     IF MFS-UPDATE                                                        
073400       CONTINUE                                                           
073500     ELSE                                                                 
073600       IF MFS-ENTER OR MFS-NEXT                                           
073700         PERFORM UNTIL RADER-SAKNAS OR                                    
073800                (LRET-IDPARTNR = W-IDPARTNR AND                           
073900                 LRET-IDFTG    = W-IDFTG    AND                           
074000                 LRET-KDANMORS = W-KDANMORS AND                           
074100                 LRET-DAREGDAT = W-DAREGDAT AND                           
074200                 LRET-IDREF    = W-IDREF    AND                           
074300                 LRET-IDEXCUST-1 = W-IDEXCUST-1 AND                       
074400                 LRET-IDEXCUST-2 = W-IDEXCUST-2)                          
074500                                                                          
074600           PERFORM DB2-FETCH-TP8LRET-CRS                                  
074700         END-PERFORM                                                      
074800       END-IF                                                             
074900     END-IF                                                               
075000                                                                          
075100     IF RADER-FINNS                                                       
075200       MOVE LRET-IDPARTNR   TO SPAR-IDPARTNR-ENTER                        
075300       MOVE LRET-IDFTG      TO SPAR-IDFTG-ENTER                           
075400       MOVE LRET-KDANMORS   TO SPAR-KDANMORS-ENTER                        
075500       MOVE LRET-DAREGDAT   TO SPAR-DAREGDAT-ENTER                        
075600       MOVE LRET-IDREF      TO SPAR-IDREF-ENTER                           
075700       MOVE LRET-IDEXCUST-1 TO SPAR-IDEXCUST-1-ENTER                      
075800       MOVE LRET-IDEXCUST-2 TO SPAR-IDEXCUST-2-ENTER                      
075900                                                                          
076000       MOVE WS-IDPARTNR-IN  TO SPAR-MID-IDPARTNR                          
076100       MOVE WS-IDFTG-IN     TO SPAR-MID-IDFTG                             
076200                                                                          
076300       MOVE +1  TO INDX                                                   
076400       PERFORM UNTIL INDX > MAX-INDX                                      
076500                                                                          
076600       IF RADER-FINNS                                                     
076700         MOVE LRET-KDANMORS        TO MOD-KDANMORS (INDX)                 
076800         MOVE LRET-IDEXCUST-1(1:4) TO MOD-IDDISTR  (INDX)                 
076900         MOVE LRET-IDEXCUST-2(1:6) TO MOD-IDKUNDNR (INDX)                 
077000         MOVE LRET-IDREF(1:10)     TO MOD-IDREF    (INDX)                 
077100                                                                          
077200         COMPUTE MOD-SUARTBTO (INDX) ROUNDED =                            
077300                 LRET-PRARTNTO * LRET-KVLEVART                            
077400         END-COMPUTE                                                      
077500                                                                          
077600**** WHEN INVOICED THE CURRENCY MAY NOT BE IN SEK                         
077700         IF LRET-KDRAPPSTA = 'MF'                                         
077800         OR LRET-KDRAPPSTA = 'SF'                                         
077900           MOVE LRET-KDVALISO      TO MOD-KDVALISO (INDX)                 
078000         ELSE                                                             
078100           MOVE 'SEK'              TO MOD-KDVALISO (INDX)                 
078200         END-IF                                                           
078300         MOVE LRET-DAREGDAT        TO MOD-DAREGDAT (INDX)                 
078400                                                                          
078500         PERFORM DB2-FETCH-TP8LRET-CRS                                    
078600       ELSE                                                               
078700         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
078800         MOVE MFS-STAENG-FAELT-NOMOD  TO MOD-KDBEHX-ATTR(INDX)            
078900       END-IF                                                             
079000       ADD +1  TO INDX                                                    
079100       END-PERFORM                                                        
079200     ELSE                                                                 
079300       CONTINUE                                                           
079400     END-IF                                                               
079500                                                                          
079600     PERFORM DB2-CLOSE-TP8LRET-CRS                                        
079700                                                                          
079800*SPARA BLÄDDRINGSNYCKLAR.                                                 
079900     IF RADER-FINNS                                                       
080000       MOVE LRET-IDPARTNR   TO SPAR-IDPARTNR-NEXT                         
080100       MOVE LRET-IDFTG      TO SPAR-IDFTG-NEXT                            
080200       MOVE LRET-KDANMORS   TO SPAR-KDANMORS-NEXT                         
080300       MOVE LRET-DAREGDAT   TO SPAR-DAREGDAT-NEXT                         
080400       MOVE LRET-IDREF      TO SPAR-IDREF-NEXT                            
080500       MOVE LRET-IDEXCUST-1 TO SPAR-IDEXCUST-1-NEXT                       
080600       MOVE LRET-IDEXCUST-2 TO SPAR-IDEXCUST-2-NEXT                       
080700       MOVE WS-IDPARTNR-IN  TO SPAR-MID-IDPARTNR                          
080800       MOVE WS-IDFTG-IN     TO SPAR-MID-IDFTG                             
080900                                                                          
081000       IF MED-IDMFSINF = SPACE OR '006'                                   
081100         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
081200         CALL WMEDKONV USING MED-WMEDAREA                                 
081300         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
081400       END-IF                                                             
081500     ELSE                                                                 
081600       MOVE LRET-IDPARTNR   TO SPAR-IDPARTNR-NEXT                         
081700       MOVE LRET-IDFTG      TO SPAR-IDFTG-NEXT                            
081800       MOVE LRET-KDANMORS   TO SPAR-KDANMORS-NEXT                         
081900       MOVE LRET-DAREGDAT   TO SPAR-DAREGDAT-NEXT                         
082000       MOVE LRET-IDREF      TO SPAR-IDREF-NEXT                            
082100       MOVE LRET-IDEXCUST-1 TO SPAR-IDEXCUST-1-NEXT                       
082200       MOVE LRET-IDEXCUST-2 TO SPAR-IDEXCUST-2-NEXT                       
082300       MOVE WS-IDPARTNR-IN  TO SPAR-MID-IDPARTNR                          
082400       MOVE WS-IDFTG-IN     TO SPAR-MID-IDFTG                             
082500     END-IF                                                               
082600     .                                                                    
082700     EJECT                                                                
082800 FB-LAES-STYRDATA-IRET  SECTION.                                          
082900                                                                          
083000     PERFORM DB2-SELECT-TP8IRET-TAB                                       
083100     IF RADER-FINNS                                                       
083200       MOVE IRET-SUNTO-TOT  TO MOD-SUARTBTO-MIN                           
083300     ELSE                                                                 
083400       PERFORM DB2-SELECT-TP8IRET-DEF                                     
083500       IF RADER-FINNS                                                     
083600         MOVE IRET-SUNTO-TOT  TO MOD-SUARTBTO-MIN                         
083700       ELSE                                                               
083800         MOVE MFS-RENSA-FAELT TO MOD-SUARTBTO-MIN                         
083900       END-IF                                                             
084000     END-IF                                                               
084100     .                                                                    
084200     EJECT                                                                
084300 FC-SUM-ALLA-KODER-RAD19  SECTION.                                        
084400                                                                          
084500     MOVE ZERO            TO WS-SUMMA                                     
084600     MOVE MFS-RENSA-FAELT TO MOD-SUARTBTO-72                              
084700                             MOD-KDVALISO-72                              
084800                             MOD-SUARTBTO-98                              
084900                             MOD-KDVALISO-98                              
085000                             MOD-SUARTBTO-RR                              
085100                             MOD-KDVALISO-RR                              
085200                                                                          
085300     PERFORM DB2-DCL-OPN-TP8LRET-CRS3                                     
085400     PERFORM DB2-FETCH-TP8LRET-CRS3                                       
085500     IF RADER-FINNS                                                       
085600       PERFORM UNTIL RADER-SAKNAS                                         
085700         PERFORM DB2-SELECT-TP8LRET-TAB                                   
085800         IF RADER-FINNS                                                   
085900           EVALUATE WS-KDANMORS                                           
086000             WHEN '72'                                                    
086100               MOVE 'SEK'       TO MOD-KDVALISO-72                        
086200               MOVE WS-SUMMA    TO MOD-SUARTBTO-72                        
086300             WHEN '98'                                                    
086400               MOVE 'SEK'       TO MOD-KDVALISO-98                        
086500               MOVE WS-SUMMA    TO MOD-SUARTBTO-98                        
086600             WHEN OTHER                                                   
086700               MOVE 'SEK'       TO MOD-KDVALISO-RR                        
086800               MOVE WS-SUMMA    TO MOD-SUARTBTO-RR                        
086900           END-EVALUATE                                                   
087000         END-IF                                                           
087100         PERFORM DB2-FETCH-TP8LRET-CRS3                                   
087200       END-PERFORM                                                        
087300     END-IF                                                               
087400                                                                          
087500     PERFORM DB2-CLOSE-TP8LRET-CRS3                                       
087600                                                                          
087700     .                                                                    
087800     EJECT                                                                
087900 FD-SUM-ALLA-KODER-TOT  SECTION.                                          
088000                                                                          
088100     MOVE MFS-RENSA-FAELT TO MOD-SUARTBTO-RET                             
088200     MOVE ZERO            TO WS-SUMMA-TOT                                 
088300                                                                          
088400     PERFORM DB2-SELECT-TP8LRET-TOT                                       
088500     IF RADER-FINNS                                                       
088600       MOVE WS-SUMMA-TOT    TO MOD-SUARTBTO-RET                           
088700     END-IF                                                               
088800                                                                          
088900     .                                                                    
089000     EJECT                                                                
089100 G-KOLLA-INPUT SECTION.                                                   
089200                                                                          
089300     MOVE JA   TO INDATA-SW                                               
089400                  KDCMD-SW                                                
089500                                                                          
089600     MOVE +1 TO INDX                                                      
089700     PERFORM UNTIL INDX > MAX-INDX                                        
089800       IF MID-KDBEHX (INDX) =  '+'                                        
089900         CONTINUE                                                         
090000       ELSE                                                               
090100         MOVE NEJ TO KDCMD-SW                                             
090200       END-IF                                                             
090300       ADD +1 TO INDX                                                     
090400     END-PERFORM                                                          
090500                                                                          
090600     IF MID-INPUT = ALL '+' AND KDCMD-ALL-PLUS                            
090700       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
090800       CALL WMEDKONV USING MED-WMEDAREA                                   
090900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
091000       PERFORM MFS-ROER-EJ-FAELT-IN                                       
091100       PERFORM MFS-ROER-EJ-FAELT-UT                                       
091200       MOVE NEJ TO INDATA-SW                                              
091300     ELSE                                                                 
091400       IF MID-INPUT NOT = ALL '+' AND KDCMD-NOT-ALL-PLUS                  
091500         MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                        
091600         CALL WMEDKONV USING MED-WMEDAREA                                 
091700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
091800         PERFORM MFS-ROER-EJ-FAELT-IN                                     
091900         PERFORM MFS-ROER-EJ-FAELT-UT                                     
092000         MOVE NEJ TO INDATA-SW                                            
092100       ELSE                                                               
092200         IF MID-INPUT NOT = ALL '+'                                       
092300           IF MID-KDBEHX-UPD   NOT = ALL '+'  AND                         
092400              MID-SUARTBTO-UPD NOT = ALL '+'                              
092500              MOVE ERR-FLERA-FUNKTIONER TO MED-IDMFSFEL                   
092600              CALL WMEDKONV USING MED-WMEDAREA                            
092700              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
092800              PERFORM MFS-ROER-EJ-FAELT-IN                                
092900              PERFORM MFS-ROER-EJ-FAELT-UT                                
093000              MOVE NEJ TO INDATA-SW                                       
093100           ELSE                                                           
093200             PERFORM GA-KOLLA-INPUT-RADER                                 
093300           END-IF                                                         
093400         ELSE                                                             
093500           PERFORM GB-KOLLA-INPUT-CMD                                     
093600         END-IF                                                           
093700       END-IF                                                             
093800     END-IF                                                               
093900     .                                                                    
094000     EJECT                                                                
094100 GA-KOLLA-INPUT-RADER SECTION.                                            
094200                                                                          
094300*- KOLLA INMATNINGFAELT *************************************             
094400                                                                          
094500     IF MID-SUARTBTO-UPD = ALL '+'                                        
094600       CONTINUE                                                           
094700     ELSE                                                                 
094800       MOVE ZERO                   TO WS-SUARTBTO-JFR                     
094900       MOVE MID-SUARTBTO-UPD       TO DEC-IDFRIDATA                       
095000       MOVE +7                     TO DEC-KVHELTAL                        
095100       MOVE +2                     TO DEC-KVDECIMAL                       
095200       CALL WDECEDIT USING DEC-WDECAREA                                   
095300                                                                          
095400       IF DEC-KDSVAR-FEL                                                  
095500         MOVE MFS-NUM-FAELT-FEL    TO MOD-SUARTBTO-ATTR                   
095600         MOVE NEJ                  TO INDATA-SW                           
095700       ELSE                                                               
095800         MOVE DEC-IDEDITDATA         TO WS-SUARTBTO-JFR                   
095900         IF WS-SUARTBTO-JFR > ZERO                                        
096000           MOVE MFS-NUM-FAELT-RAETT  TO MOD-SUARTBTO-ATTR                 
096100         ELSE                                                             
096200           MOVE MFS-NUM-FAELT-FEL    TO MOD-SUARTBTO-ATTR                 
096300           MOVE NEJ                  TO INDATA-SW                         
096400         END-IF                                                           
096500       END-IF                                                             
096600     END-IF                                                               
096700                                                                          
096800     IF MID-KDBEHX-UPD  NOT = ALL '+'                                     
096900       IF MID-KDBEHX-UPD = 'X'                                            
097000         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDBEHX-UPD-ATTR                 
097100       ELSE                                                               
097200         MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDBEHX-UPD-ATTR                 
097300         MOVE NEJ TO INDATA-SW                                            
097400       END-IF                                                             
097500     ELSE                                                                 
097600       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDBEHX-UPD-ATTR                   
097700     END-IF                                                               
097800                                                                          
097900* DET KAN FINNAS IDPARTNR 000000000 UPPLAGT PÅ WDB2 FÖR VISSA DIST        
098000* MAN MÅSTE DÅ KOLLA SÅ ATT MID-IDPARTNR FINNS INNAN UPPDATERING.         
098100     PERFORM IMS-GU-WDB101                                                
098200     IF SEGMENT-SAKNAS                                                    
098300       MOVE ERR-BETALARE-SAKNAS   TO MED-IDMFSFEL                         
098400       MOVE NEJ TO INDATA-SW                                              
098500     END-IF                                                               
098600                                                                          
098700     IF INDATA-FEL                                                        
098800       IF MED-IDMFSFEL  = SPACE                                           
098900         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
099000       END-IF                                                             
099100       CALL WMEDKONV USING MED-WMEDAREA                                   
099200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
099300       PERFORM MFS-ROER-EJ-FAELT-UT                                       
099400       PERFORM MFS-ROER-EJ-FAELT-IN                                       
099500     END-IF                                                               
099600     .                                                                    
099700     EJECT                                                                
099800 GB-KOLLA-INPUT-CMD SECTION.                                              
099900                                                                          
100000*- KOLLA KANTKODER *******************************************            
100100                                                                          
100200     MOVE +1 TO INDX                                                      
100300     PERFORM UNTIL INDX  >  MAX-INDX                                      
100400       IF MID-KDBEHX(INDX)    NOT = ALL '+'                               
100500         IF MID-KDBEHX(INDX)  = 'D'                                       
100600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDBEHX-ATTR (INDX)            
100700         ELSE                                                             
100800           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDBEHX-ATTR (INDX)            
100900           MOVE NEJ TO INDATA-SW                                          
101000         END-IF                                                           
101100       END-IF                                                             
101200                                                                          
101300       ADD +1     TO INDX                                                 
101400     END-PERFORM                                                          
101500                                                                          
101600     IF INDATA-FEL                                                        
101700       IF MED-IDMFSFEL  = SPACE                                           
101800         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
101900       END-IF                                                             
102000       CALL WMEDKONV USING MED-WMEDAREA                                   
102100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
102200       PERFORM MFS-ROER-EJ-FAELT-UT                                       
102300       PERFORM MFS-ROER-EJ-FAELT-IN                                       
102400     END-IF                                                               
102500     .                                                                    
102600     EJECT                                                                
102700 H-UPPDATERA SECTION.                                                     
102800                                                                          
102900     MOVE +1 TO INDX                                                      
103000     PERFORM UNTIL INDX            >  MAX-INDX                            
103100       IF MID-KDBEHX(INDX)      = ALL '+' OR SPACE                        
103200         CONTINUE                                                         
103300       ELSE                                                               
103400         IF MID-KDBEHX (INDX) = 'D'                                       
103500           MOVE MID-IDDISTR (INDX) TO WS-IDEXCUST-1                       
103600           MOVE MID-IDKUNDNR(INDX) TO WS-IDEXCUST-2                       
103700           MOVE MID-KDANMORS(INDX) TO WS-MID-KDANMORS                     
103800           MOVE MID-IDREF   (INDX) TO WS-MID-IDREF                        
103900           MOVE MID-DAREGDAT(INDX) TO WS-MID-DAREGDAT                     
104000                                                                          
104100           PERFORM DB2-SELECT-TP8LRET-KOD                                 
104200           IF RADER-FINNS                                                 
104300                                                                          
104400             MOVE MSG-SIGNON-USERID           TO LRET-IDUSER-2            
104500                                                                          
104600             MOVE FUNCTION CURRENT-DATE (1:8) TO LRET-DADELDAT            
104700                                                 LRET-DAFAKT              
104800                                                                          
104900**** FLYTTA TILL DAFAKT OCKSÅ FÖR RENSNINGSPGM TP8LRET ENL. ANDERS        
105000                                                                          
105100             MOVE 'D '                        TO LRET-KDRAPPSTA           
105200                                                                          
105300             PERFORM DB2-UPDATE-TP8LRET-TAB                               
105400                                                                          
105500             IF RADER-FINNS                                               
105600               MOVE INF-UPDATE-DONE TO MED-IDMFSINF                       
105700             END-IF                                                       
105800           ELSE                                                           
105900             MOVE ERR-URVAL-SAKNAS  TO MED-IDMFSFEL                       
106000             CALL WMEDKONV USING MED-WMEDAREA                             
106100             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
106200             PERFORM MFS-ROER-EJ-FAELT-UT                                 
106300             PERFORM MFS-ROER-EJ-FAELT-IN                                 
106400           END-IF                                                         
106500         END-IF                                                           
106600       END-IF                                                             
106700                                                                          
106800       ADD +1                         TO INDX                             
106900     END-PERFORM                                                          
107000     IF MED-IDMFSINF = '101'                                              
107100       CALL WMEDKONV USING MED-WMEDAREA                                   
107200       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
107300       PERFORM MFS-FORM-ATTR                                              
107400       PERFORM MFS-RENSA-FAELT-IN                                         
107500     END-IF                                                               
107600                                                                          
107700     IF MID-SUARTBTO-UPD NOT = ALL '+'                                    
107800       PERFORM DB2-SELECT-TP8IRET-TAB                                     
107900                                                                          
108000       MOVE W-IDPARTNR        TO IRET-IDPARTNR                            
108100       MOVE W-IDFTG           TO IRET-IDFTG                               
108200       MOVE DEC-IDEDITDATA    TO IRET-SUNTO-TOT                           
108300       MOVE 'SEK'             TO IRET-KDVALISO                            
108400       MOVE MSG-SIGNON-USERID TO IRET-IDUSER                              
108500       MOVE FUNCTION CURRENT-DATE (1:8) TO IRET-DAUPPDAT                  
108600                                                                          
108700       IF RADER-FINNS                                                     
108800         PERFORM DB2-UPDATE-TP8IRET-TAB                                   
108900       ELSE                                                               
109000         PERFORM DB2-INSERT-TP8IRET-TAB                                   
109100       END-IF                                                             
109200                                                                          
109300       IF CURSOR-OK                                                       
109400         MOVE INF-UPDATE-DONE TO MED-IDMFSINF                             
109500         CALL WMEDKONV USING MED-WMEDAREA                                 
109600         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
109700         PERFORM MFS-FORM-ATTR                                            
109800         PERFORM MFS-RENSA-FAELT-IN                                       
109900       ELSE                                                               
110000         IF RAD-DUBLETT                                                   
110100           MOVE ERR-RAD-FINNS-REDAN TO MED-IDMFSFEL                       
110200         ELSE                                                             
110300           MOVE ERR-UPDATE-NOT-OK   TO MED-IDMFSFEL                       
110400         END-IF                                                           
110500         CALL WMEDKONV USING MED-WMEDAREA                                 
110600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
110700         PERFORM MFS-ROER-EJ-FAELT-UT                                     
110800         PERFORM MFS-ROER-EJ-FAELT-IN                                     
110900       END-IF                                                             
111000     END-IF                                                               
111100                                                                          
111200*-MANNUELL START AV FAKTURERING BEGÄRD AV LINJEN,ANNARS SCHEMALAGD        
111300                                                                          
111400     IF MID-KDBEHX-UPD = 'X'                                              
111500                                                                          
111600       PERFORM DB2-DCL-OPN-TP8LRET-CRS2                                   
111700       PERFORM DB2-FETCH-TP8LRET-CRS2                                     
111800                                                                          
111900       IF RADER-FINNS                                                     
112000         PERFORM UNTIL RADER-SAKNAS                                       
112100                                                                          
112200           MOVE MSG-SIGNON-USERID           TO LRET-IDUSER-2              
112300                                                                          
112400           MOVE FUNCTION CURRENT-DATE (1:8) TO LRET-DAUPPDAT              
112500                                                                          
112600           PERFORM DB2-CRS-UPDATE-TP8LRET-TAB                             
112700                                                                          
112800           PERFORM DB2-FETCH-TP8LRET-CRS2                                 
112900         END-PERFORM                                                      
113000                                                                          
113100         PERFORM DB2-CLOSE-TP8LRET-CRS2                                   
113200                                                                          
113300         PERFORM I-START-PGM-W4078900                                     
113400                                                                          
113500         MOVE INF-UPDATE-DONE TO MED-IDMFSINF                             
113600         CALL WMEDKONV USING MED-WMEDAREA                                 
113700         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
113800         PERFORM MFS-FORM-ATTR                                            
113900         PERFORM MFS-RENSA-FAELT-IN                                       
114000       ELSE                                                               
114100         MOVE INF-UPDATE-DONE TO MED-IDMFSINF                             
114200         CALL WMEDKONV USING MED-WMEDAREA                                 
114300         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
114400         PERFORM MFS-FORM-ATTR                                            
114500         PERFORM MFS-RENSA-FAELT-IN                                       
114600       END-IF                                                             
114700     END-IF                                                               
114800     .                                                                    
114900     EJECT                                                                
115000 I-START-PGM-W4078900 SECTION.                                            
115100                                                                          
115200     PERFORM S01-SEND-TO-W40789-OPEN                                      
115300     PERFORM S02-SEND-TO-W40789-PUT                                       
115400     PERFORM S03-SEND-TO-W40789-CLOSE                                     
115500     .                                                                    
115600     EJECT                                                                
115700 S01-SEND-TO-W40789-OPEN SECTION.                                         
115800     MOVE 'OPEN' TO SEND-KDFUNC                                           
115900     MOVE 'CARPARTS.PULS.INVKRE'            TO SEND-ADDISPABS             
116000                                                                          
116100     CALL WZ01SEND USING           SEND-CONTROL-AREA                      
116200                                   SEND-OPEN-AREA                         
116300********              ...FELHANTERING...                                  
116400     IF SEND-KDRC > 0                                                     
116500      MOVE SEND-KDRC TO KDRC-DISPLAY                                      
116600      STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                      
116700      DELIMITED BY SIZE INTO FELTEXT                                      
116800      DISPLAY FELTEXT                                                     
116900      CALL ABEND USING RKOD-ABEND-MED-DUMP                                
117000     END-IF                                                               
117100     .                                                                    
117200     EJECT                                                                
117300 S02-SEND-TO-W40789-PUT SECTION.                                          
117400                                                                          
117500     MOVE '4719'  TO SEND-AREA(1:4)                                       
117600                                                                          
117700     MOVE 'PUT'                           TO SEND-KDFUNC                  
117800     COMPUTE SEND-KVDLEN = LENGTH OF SEND-AREA + 4                        
117900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
118000                         SEND-KVDLEN                                      
118100                         SEND-AREA                                        
118200**FELHANTERING...                                                         
118300     IF SEND-KDRC > 1                                                     
118400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
118500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
118600       DELIMITED BY SIZE INTO FELTEXT                                     
118700       DISPLAY FELTEXT                                                    
118800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
118900     END-IF                                                               
119000     .                                                                    
119100     EJECT                                                                
119200 S03-SEND-TO-W40789-CLOSE SECTION.                                        
119300                                                                          
119400     MOVE 'CLOSE' TO SEND-KDFUNC                                          
119500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
119600**FELHANTERING...                                                         
119700     IF SEND-KDRC > 0                                                     
119800      MOVE SEND-KDRC TO KDRC-DISPLAY                                      
119900      STRING 'WZ01SEND CLOSE ERROR RC= ' KDRC-DISPLAY                     
120000       DELIMITED BY SIZE INTO FELTEXT                                     
120100       DISPLAY FELTEXT                                                    
120200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
120300     END-IF                                                               
120400     .                                                                    
120500     EJECT                                                                
120600 MFS-RENSA-FAELT-UT SECTION.                                              
120700                                                                          
120800*    --- ALLA UTDATA-FÄLT                                                 
120900*    --- INKL. BLÄDDRINGSNYCKLAR                                          
121000     MOVE MFS-RENSA-FAELT TO MOD-SUARTBTO-MIN                             
121100                             MOD-KDVALISO-MIN                             
121200                             MOD-SUARTBTO-72                              
121300                             MOD-KDVALISO-72                              
121400                             MOD-SUARTBTO-98                              
121500                             MOD-KDVALISO-98                              
121600                             MOD-SUARTBTO-RR                              
121700                             MOD-KDVALISO-RR                              
121800                                                                          
121900     MOVE +1 TO INDX                                                      
122000     PERFORM UNTIL INDX > MAX-INDX                                        
122100       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
122200       ADD +1 TO INDX                                                     
122300     END-PERFORM                                                          
122400     .                                                                    
122500     SKIP3                                                                
122600 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
122700                                                                          
122800*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
122900     MOVE MFS-RENSA-FAELT TO MOD-KDANMORS (INDX)                          
123000                             MOD-IDDISTR  (INDX)                          
123100                             MOD-IDKUNDNR (INDX)                          
123200                             MOD-IDREF    (INDX)                          
123300                             MOD-SUARTBTO (INDX)                          
123400                             MOD-KDVALISO (INDX)                          
123500                             MOD-DAREGDAT (INDX)                          
123600     .                                                                    
123700     SKIP3                                                                
123800 MFS-RENSA-FAELT-IN SECTION.                                              
123900                                                                          
124000*    --- ALLA INDATA-FÄLT                                                 
124100     MOVE MFS-RENSA-FAELT TO MOD-SUARTBTO-UPD                             
124200                             MOD-KDVALISO-UPD                             
124300                             MOD-KDBEHX-UPD                               
124400                                                                          
124500     MOVE +1 TO INDX                                                      
124600     PERFORM UNTIL INDX > MAX-INDX                                        
124700       MOVE MFS-RENSA-FAELT TO MOD-KDBEHX (INDX)                          
124800       ADD +1 TO INDX                                                     
124900     END-PERFORM                                                          
125000     .                                                                    
125100     EJECT                                                                
125200 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
125300                                                                          
125400*    --- ALLA UTDATA-FÄLT                                                 
125500*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
125600     MOVE MFS-ROER-EJ-FAELT TO MOD-SUARTBTO-MIN                           
125700                               MOD-KDVALISO-MIN                           
125800                               MOD-SUARTBTO-72                            
125900                               MOD-KDVALISO-72                            
126000                               MOD-SUARTBTO-98                            
126100                               MOD-KDVALISO-98                            
126200                               MOD-SUARTBTO-RR                            
126300                               MOD-KDVALISO-RR                            
126400                                                                          
126500     MOVE +1 TO INDX                                                      
126600     PERFORM UNTIL INDX > MAX-INDX                                        
126700       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
126800       ADD +1 TO INDX                                                     
126900     END-PERFORM                                                          
127000     .                                                                    
127100     SKIP2                                                                
127200 MFS-ROER-EJ-RAD-FAELT-UT  SECTION.                                       
127300                                                                          
127400*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
127500     MOVE MFS-ROER-EJ-FAELT TO MOD-KDANMORS (INDX)                        
127600                               MOD-IDDISTR  (INDX)                        
127700                               MOD-IDKUNDNR (INDX)                        
127800                               MOD-IDREF    (INDX)                        
127900                               MOD-SUARTBTO (INDX)                        
128000                               MOD-KDVALISO (INDX)                        
128100                               MOD-DAREGDAT (INDX)                        
128200     .                                                                    
128300     SKIP3                                                                
128400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
128500                                                                          
128600*    --- ALLA INDATA-FÄLT                                                 
128700     MOVE MFS-ROER-EJ-FAELT TO MOD-SUARTBTO-UPD                           
128800                               MOD-KDVALISO-UPD                           
128900                               MOD-KDBEHX-UPD                             
129000                                                                          
129100     MOVE +1 TO INDX                                                      
129200     PERFORM UNTIL INDX > MAX-INDX                                        
129300       MOVE MFS-ROER-EJ-FAELT TO MOD-KDBEHX (INDX)                        
129400       ADD +1 TO INDX                                                     
129500     END-PERFORM                                                          
129600     .                                                                    
129700     EJECT                                                                
129800 MFS-FORM-ATTR SECTION.                                                   
129900                                                                          
130000*    --- ALLA INDATA-FÄLT                                                 
130100     MOVE MFS-FORMATETS-ATTR TO MOD-SUARTBTO-ATTR                         
130200                                MOD-KDVALISO-ATTR                         
130300                                MOD-KDBEHX-UPD-ATTR                       
130400                                                                          
130500     MOVE +1 TO INDX                                                      
130600     PERFORM UNTIL INDX > MAX-INDX                                        
130700       MOVE MFS-FORMATETS-ATTR  TO MOD-KDBEHX-ATTR (INDX)                 
130800       ADD +1 TO INDX                                                     
130900     END-PERFORM                                                          
131000     .                                                                    
131100     SKIP2                                                                
131200 IMS-GET-MSG SECTION.                                                     
131300                                                                          
131400     MOVE '  QC' TO GODK-STATUSKODER                                      
131500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
131600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
131700     PERFORM IMS-STATUSKONTROLL                                           
131800     .                                                                    
131900     SKIP3                                                                
132000 IMS-INSERT-MSG SECTION.                                                  
132100                                                                          
132200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
132300     MOVE SPACE TO GODK-STATUSKODER                                       
132400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
132500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
132600     PERFORM IMS-STATUSKONTROLL                                           
132700     .                                                                    
132800     EJECT                                                                
132900 IMS-GU-WDB101 SECTION.                                                   
133000                                                                          
133100     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
133200          DELIMITED BY SIZE INTO SSA1                                     
133300     MOVE '  GE' TO GODK-STATUSKODER                                      
133400     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-WDB101 SSA1                    
133500     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
133600     PERFORM IMS-STATUSKONTROLL                                           
133700     .                                                                    
133800     EJECT                                                                
133900 IMS-GU-WDB201 SECTION.                                                   
134000                                                                          
134100     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
134200          DELIMITED BY SIZE INTO SSA1                                     
134300     MOVE '  GE' TO GODK-STATUSKODER                                      
134400     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
134500     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
134600     PERFORM IMS-STATUSKONTROLL                                           
134700     .                                                                    
134800     EJECT                                                                
134900 IMS-GU-WDB201-MIN-MAX SECTION.                                           
135000                                                                          
135100     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
135200                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
135300          DELIMITED BY SIZE INTO SSA1                                     
135400     MOVE '  GE' TO GODK-STATUSKODER                                      
135500     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
135600     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
135700     PERFORM IMS-STATUSKONTROLL                                           
135800     .                                                                    
135900     EJECT                                                                
137200 IMS-STATUSKONTROLL SECTION.                                              
137300                                                                          
137400     SET STATUS-IX TO 1                                                   
137500     SEARCH GODK-STATUS                                                   
137600       AT END                                                             
137700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
137800         DELIMITED BY SIZE INTO FELTEXT                                   
137900         CALL FELLOG                                                      
138000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
138100         CONTINUE                                                         
138200     END-SEARCH                                                           
138300     .                                                                    
138400     EJECT                                                                
138500 DB2-SELECT-TP8LRET-KOD  SECTION.                                         
138600                                                                          
138700     EXEC SQL                                                             
138800         SELECT  IDUSER_2                                                 
138900                ,DADELDAT                                                 
139000                ,DAFAKT                                                   
139100                ,KDRAPPSTA                                                
139200                                                                          
139300         INTO   :LRET-IDUSER-2                                            
139400               ,:LRET-DADELDAT                                            
139500               ,:LRET-DAFAKT                                              
139600               ,:LRET-KDRAPPSTA                                           
139700                                                                          
139800         FROM    TP8LRET                                                  
139900                                                                          
140000         WHERE   IDPARTNR = :W-IDPARTNR                                   
140100           AND   IDFTG    = :W-IDFTG                                      
140200           AND   KDANMORS = :WS-MID-KDANMORS                              
140300           AND   DAREGDAT = :WS-MID-DAREGDAT                              
140400           AND   IDREF    = :WS-MID-IDREF                                 
140500           AND   IDEXCUST_1 = :WS-IDEXCUST-1                              
140600           AND   IDEXCUST_2 = :WS-IDEXCUST-2                              
140700           AND   KDRAPPSTA  ='W '                                         
140800                                                                          
140900     END-EXEC                                                             
141000                                                                          
141100     MOVE 000100305 TO GODK-SQLCODEKODER                                  
141200     MOVE SQLCODE TO SQLCODE-WS                                           
141300     PERFORM DB2-STATUS-KONTROLL                                          
141400     .                                                                    
141500     EJECT                                                                
141600 DB2-DCL-OPN-TP8LRET-CRS  SECTION.                                        
141700                                                                          
141800                                                                          
141900     EXEC SQL                                                             
142000         DECLARE TP8LRET-CRS CURSOR FOR                                   
142100                                                                          
142200           SELECT  IDPARTNR                                               
142300                  ,IDFTG                                                  
142400                  ,KDANMORS                                               
142500                  ,DAREGDAT                                               
142600                  ,IDREF                                                  
142700                  ,IDEXCUST_1                                             
142800                  ,IDEXCUST_2                                             
142900                  ,PRARTNTO                                               
143000                  ,KVLEVART                                               
143100                  ,KDVALISO                                               
143200                                                                          
143300           FROM    TP8LRET                                                
143400                                                                          
143500           WHERE   IDPARTNR   = :W-IDPARTNR                               
143600             AND   IDFTG      = :W-IDFTG                                  
143700             AND   KDRAPPSTA  ='W '                                       
143800                                                                          
143900           ORDER BY DAREGDAT DESC, KDANMORS, IDREF                        
144000     END-EXEC                                                             
144100                                                                          
144200     EXEC SQL OPEN TP8LRET-CRS END-EXEC                                   
144300                                                                          
144400     MOVE 000100305  TO GODK-SQLCODEKODER                                 
144500     MOVE SQLCODE TO SQLCODE-WS                                           
144600     PERFORM DB2-STATUS-KONTROLL                                          
144700     .                                                                    
144800     SKIP3                                                                
144900 DB2-FETCH-TP8LRET-CRS  SECTION.                                          
145000     SKIP2                                                                
145100     EXEC SQL                                                             
145200         FETCH TP8LRET-CRS                                                
145300       INTO                                                               
145400         :LRET-IDPARTNR                                                   
145500        ,:LRET-IDFTG                                                      
145600        ,:LRET-KDANMORS                                                   
145700        ,:LRET-DAREGDAT                                                   
145800        ,:LRET-IDREF                                                      
145900        ,:LRET-IDEXCUST-1                                                 
146000        ,:LRET-IDEXCUST-2                                                 
146100        ,:LRET-PRARTNTO                                                   
146200        ,:LRET-KVLEVART                                                   
146300        ,:LRET-KDVALISO                                                   
146400     END-EXEC                                                             
146500                                                                          
146600     MOVE 000100  TO GODK-SQLCODEKODER                                    
146700     MOVE SQLCODE TO SQLCODE-WS                                           
146800     PERFORM DB2-STATUS-KONTROLL                                          
146900     .                                                                    
147000     SKIP3                                                                
147100 DB2-CLOSE-TP8LRET-CRS  SECTION.                                          
147200                                                                          
147300     EXEC SQL CLOSE TP8LRET-CRS END-EXEC                                  
147400     .                                                                    
147500     EJECT                                                                
147600 DB2-DCL-OPN-TP8LRET-CRS2 SECTION.                                        
147700                                                                          
147800                                                                          
147900     EXEC SQL                                                             
148000         DECLARE TP8LRET-CRS2 CURSOR WITH HOLD FOR                        
148100                                                                          
148200           SELECT  IDPARTNR                                               
148300                  ,IDFTG                                                  
148400                  ,KDANMORS                                               
148500                  ,DAREGDAT                                               
148600                  ,IDREF                                                  
148700                  ,IDEXCUST_1                                             
148800                  ,IDEXCUST_2                                             
148900                  ,PRARTNTO                                               
149000                  ,KVLEVART                                               
149100                  ,KDVALISO                                               
149200                  ,IDUSER_2                                               
149300                  ,DAUPPDAT                                               
149400                  ,KDRAPPSTA                                              
149500                                                                          
149600           FROM    TP8LRET                                                
149700                                                                          
149800           WHERE   IDPARTNR   = :W-IDPARTNR                               
149900             AND   IDFTG      = :W-IDFTG                                  
150000             AND   KDRAPPSTA  ='W '                                       
150100                                                                          
150200           FOR UPDATE OF                                                  
150300                         IDUSER_2                                         
150400                        ,DAUPPDAT                                         
150500                        ,KDRAPPSTA                                        
150600                                                                          
150700     END-EXEC                                                             
150800                                                                          
150900     EXEC SQL OPEN TP8LRET-CRS2 END-EXEC                                  
151000                                                                          
151100     MOVE 000100305 TO GODK-SQLCODEKODER                                  
151200     MOVE SQLCODE TO SQLCODE-WS                                           
151300     PERFORM DB2-STATUS-KONTROLL                                          
151400     .                                                                    
151500     SKIP3                                                                
151600 DB2-FETCH-TP8LRET-CRS2  SECTION.                                         
151700     SKIP2                                                                
151800     EXEC SQL                                                             
151900         FETCH TP8LRET-CRS2                                               
152000       INTO                                                               
152100         :LRET-IDPARTNR                                                   
152200        ,:LRET-IDFTG                                                      
152300        ,:LRET-KDANMORS                                                   
152400        ,:LRET-DAREGDAT                                                   
152500        ,:LRET-IDREF                                                      
152600        ,:LRET-IDEXCUST-1                                                 
152700        ,:LRET-IDEXCUST-2                                                 
152800        ,:LRET-PRARTNTO                                                   
152900        ,:LRET-KVLEVART                                                   
153000        ,:LRET-KDVALISO                                                   
153100        ,:LRET-IDUSER-2                                                   
153200        ,:LRET-DAUPPDAT                                                   
153300        ,:LRET-KDRAPPSTA                                                  
153400     END-EXEC                                                             
153500                                                                          
153600     MOVE 000100  TO GODK-SQLCODEKODER                                    
153700     MOVE SQLCODE TO SQLCODE-WS                                           
153800     PERFORM DB2-STATUS-KONTROLL                                          
153900     .                                                                    
154000     SKIP3                                                                
154100 DB2-CLOSE-TP8LRET-CRS2  SECTION.                                         
154200                                                                          
154300     EXEC SQL CLOSE TP8LRET-CRS2 END-EXEC                                 
154400     .                                                                    
154500     EJECT                                                                
154600 DB2-DCL-OPN-TP8LRET-CRS3  SECTION.                                       
154700                                                                          
154800                                                                          
154900     EXEC SQL                                                             
155000         DECLARE TP8LRET-CRS3 CURSOR FOR                                  
155100                                                                          
155200           SELECT DISTINCT (KDANMORS), KDVALISO                           
155300                                                                          
155400           FROM    TP8LRET                                                
155500                                                                          
155600           WHERE   IDPARTNR = :W-IDPARTNR                                 
155700             AND   IDFTG    = :W-IDFTG                                    
155800             AND   KDRAPPSTA ='W '                                        
155900                                                                          
156000     END-EXEC                                                             
156100                                                                          
156200     EXEC SQL OPEN TP8LRET-CRS3 END-EXEC                                  
156300                                                                          
156400     MOVE 000100305  TO GODK-SQLCODEKODER                                 
156500     MOVE SQLCODE TO SQLCODE-WS                                           
156600     PERFORM DB2-STATUS-KONTROLL                                          
156700     .                                                                    
156800     SKIP3                                                                
156900 DB2-FETCH-TP8LRET-CRS3  SECTION.                                         
157000     SKIP2                                                                
157100     EXEC SQL                                                             
157200         FETCH TP8LRET-CRS3                                               
157300       INTO                                                               
157400         :WS-KDANMORS                                                     
157500        ,:WS-KDVALISO                                                     
157600     END-EXEC                                                             
157700                                                                          
157800     MOVE 000100  TO GODK-SQLCODEKODER                                    
157900     MOVE SQLCODE TO SQLCODE-WS                                           
158000     PERFORM DB2-STATUS-KONTROLL                                          
158100     .                                                                    
158200     SKIP3                                                                
158300 DB2-CLOSE-TP8LRET-CRS3 SECTION.                                          
158400                                                                          
158500     EXEC SQL CLOSE TP8LRET-CRS3 END-EXEC                                 
158600     .                                                                    
158700     EJECT                                                                
158800 DB2-SELECT-TP8LRET-TAB  SECTION.                                         
158900                                                                          
159000     EXEC SQL                                                             
159100         SELECT  SUM(PRARTNTO * KVLEVART)                                 
159200                                                                          
159300                                                                          
159400         INTO   :WS-SUMMA                                                 
159500                                                                          
159600                                                                          
159700         FROM    TP8LRET                                                  
159800                                                                          
159900         WHERE   IDPARTNR = :W-IDPARTNR                                   
160000           AND   IDFTG    = :W-IDFTG                                      
160100           AND   KDANMORS = :WS-KDANMORS                                  
160200           AND   KDRAPPSTA = 'W '                                         
160300                                                                          
160400     END-EXEC                                                             
160500                                                                          
160600     MOVE 000100305  TO GODK-SQLCODEKODER                                 
160700     MOVE SQLCODE TO SQLCODE-WS                                           
160800     PERFORM DB2-STATUS-KONTROLL                                          
160900     .                                                                    
161000     EJECT                                                                
161100 DB2-SELECT-TP8LRET-TOT  SECTION.                                         
161200                                                                          
161300     MOVE 000100305  TO GODK-SQLCODEKODER                                 
161400     EXEC SQL                                                             
161500         SELECT  SUM(PRARTNTO * KVLEVART)                                 
161600                                                                          
161700                                                                          
161800         INTO   :WS-SUMMA-TOT                                             
161900                                                                          
162000                                                                          
162100         FROM    TP8LRET                                                  
162200                                                                          
162300         WHERE   IDPARTNR = :W-IDPARTNR                                   
162400           AND   IDFTG    = :W-IDFTG                                      
162500           AND   KDRAPPSTA = 'W '                                         
162600                                                                          
162700     END-EXEC                                                             
162800                                                                          
162900     MOVE SQLCODE TO SQLCODE-WS                                           
163000     PERFORM DB2-STATUS-KONTROLL                                          
163100     .                                                                    
163200     EJECT                                                                
163300 DB2-UPDATE-TP8LRET-TAB  SECTION.                                         
163400                                                                          
163500     EXEC SQL                                                             
163600         UPDATE TP8LRET                                                   
163700                                                                          
163800         SET IDUSER_2  = :LRET-IDUSER-2                                   
163900            ,DADELDAT  = :LRET-DADELDAT                                   
164000            ,DAFAKT    = :LRET-DAFAKT                                     
164100            ,KDRAPPSTA = :LRET-KDRAPPSTA                                  
164200                                                                          
164300         WHERE   IDPARTNR = :W-IDPARTNR                                   
164400           AND   IDFTG    = :W-IDFTG                                      
164500           AND   KDANMORS = :WS-MID-KDANMORS                              
164600           AND   DAREGDAT = :WS-MID-DAREGDAT                              
164700           AND   IDREF    = :WS-MID-IDREF                                 
164800           AND   IDEXCUST_1 = :WS-IDEXCUST-1                              
164900           AND   IDEXCUST_2 = :WS-IDEXCUST-2                              
165000                                                                          
165100     END-EXEC                                                             
165200                                                                          
165300     MOVE 000     TO GODK-SQLCODEKODER                                    
165400     MOVE SQLCODE TO SQLCODE-WS                                           
165500     PERFORM DB2-STATUS-KONTROLL                                          
165600     .                                                                    
165700     EJECT                                                                
165800 DB2-CRS-UPDATE-TP8LRET-TAB  SECTION.                                     
165900                                                                          
166000                                                                          
166100     EXEC SQL                                                             
166200         UPDATE TP8LRET                                                   
166300         SET IDUSER_2  = :LRET-IDUSER-2                                   
166400            ,DAUPPDAT  = :LRET-DAUPPDAT                                   
166500            ,KDRAPPSTA = 'M '                                             
166600                                                                          
166700         WHERE  CURRENT OF TP8LRET-CRS2                                   
166800                                                                          
166900     END-EXEC                                                             
167000                                                                          
167100     MOVE 000    TO GODK-SQLCODEKODER                                     
167200     MOVE SQLCODE TO SQLCODE-WS                                           
167300     PERFORM DB2-STATUS-KONTROLL                                          
167400     .                                                                    
167500     EJECT                                                                
167600 DB2-SELECT-TP8IRET-TAB  SECTION.                                         
167700                                                                          
167800     EXEC SQL                                                             
167900         SELECT  SUNTO_TOT                                                
168000                ,KDVALISO                                                 
168100                ,IDUSER                                                   
168200                ,DAUPPDAT                                                 
168300                                                                          
168400                                                                          
168500         INTO   :IRET-SUNTO-TOT                                           
168600               ,:IRET-KDVALISO                                            
168700               ,:IRET-IDUSER                                              
168800               ,:IRET-DAUPPDAT                                            
168900                                                                          
169000                                                                          
169100         FROM    TP8IRET                                                  
169200                                                                          
169300         WHERE   IDPARTNR  = :W-IDPARTNR                                  
169400           AND   IDFTG     = :W-IDFTG                                     
169500                                                                          
169600     END-EXEC                                                             
169700                                                                          
169800     MOVE 000100305  TO GODK-SQLCODEKODER                                 
169900     MOVE SQLCODE TO SQLCODE-WS                                           
170000     PERFORM DB2-STATUS-KONTROLL                                          
170100     .                                                                    
170200     EJECT                                                                
170300 DB2-SELECT-TP8IRET-DEF  SECTION.                                         
170400                                                                          
170500     EXEC SQL                                                             
170600         SELECT  SUNTO_TOT                                                
170700                ,KDVALISO                                                 
170800                                                                          
170900                                                                          
171000         INTO   :IRET-SUNTO-TOT                                           
171100               ,:IRET-KDVALISO                                            
171200                                                                          
171300                                                                          
171400         FROM    TP8IRET                                                  
171500                                                                          
171600         WHERE   IDPARTNR  = 'DEFAULT'                                    
171700                                                                          
171800     END-EXEC                                                             
171900                                                                          
172000     MOVE 000100  TO GODK-SQLCODEKODER                                    
172100     MOVE SQLCODE TO SQLCODE-WS                                           
172200     PERFORM DB2-STATUS-KONTROLL                                          
172300     .                                                                    
172400     EJECT                                                                
172500 DB2-UPDATE-TP8IRET-TAB  SECTION.                                         
172600                                                                          
172700     EXEC SQL                                                             
172800         UPDATE TP8IRET                                                   
172900                                                                          
173000         SET SUNTO_TOT = :IRET-SUNTO-TOT                                  
173100            ,KDVALISO  = :IRET-KDVALISO                                   
173200            ,IDUSER    = :IRET-IDUSER                                     
173300            ,DAUPPDAT  = :IRET-DAUPPDAT                                   
173400                                                                          
173500         WHERE   IDPARTNR = :W-IDPARTNR                                   
173600           AND   IDFTG    = :W-IDFTG                                      
173700                                                                          
173800     END-EXEC                                                             
173900                                                                          
174000     MOVE 000     TO GODK-SQLCODEKODER                                    
174100     MOVE SQLCODE TO SQLCODE-WS                                           
174200     PERFORM DB2-STATUS-KONTROLL                                          
174300     .                                                                    
174400     EJECT                                                                
174500 DB2-INSERT-TP8IRET-TAB  SECTION.                                         
174600     SKIP2                                                                
174700     EXEC SQL                                                             
174800         INSERT INTO TP8IRET                                              
174900          (                                                               
175000           IDPARTNR                                                       
175100          ,IDFTG                                                          
175200          ,SUNTO_TOT                                                      
175300          ,KDVALISO                                                       
175400          ,IDUSER                                                         
175500          ,DAUPPDAT                                                       
175600          )                                                               
175700         VALUES                                                           
175800          (                                                               
175900           :IRET-IDPARTNR                                                 
176000          ,:IRET-IDFTG                                                    
176100          ,:IRET-SUNTO-TOT                                                
176200          ,:IRET-KDVALISO                                                 
176300          ,:IRET-IDUSER                                                   
176400          ,:IRET-DAUPPDAT                                                 
176500          )                                                               
176600     END-EXEC                                                             
176700                                                                          
176800     MOVE 000803  TO GODK-SQLCODEKODER                                    
176900     MOVE SQLCODE TO SQLCODE-WS                                           
177000     PERFORM DB2-STATUS-KONTROLL                                          
177100     .                                                                    
177200     EJECT                                                                
177300 DB2-STATUS-KONTROLL  SECTION.                                            
177400                                                                          
177500     SET SQLCODE-IX TO 1                                                  
177600     SEARCH GODK-SQLCODE                                                  
177700       AT END                                                             
177800          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
177900          DELIMITED BY SIZE INTO FELTEXT                                  
178000          CALL ABEND USING RKOD-ABEND-DB2                                 
178100       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
178200     END-SEARCH                                                           
178300     .                                                                    
