000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3017600.                                                
000400*AUTHOR.         YLVA BOLINDER.                                           
000500*DATE-WRITTEN.   94/02/18.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800**                                                                        
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        REGISTRERING AV BYTESRETURER FÖR DISTRIKT SOM EJ ÄR              
001200*        VIPS-4 ANVÄNDARE.                                                
001300*                                                                         
001400*        PROGRAMMET LÄSER      WDK6                                       
001500*        PROGRAMMET LÄSER      WDB2                                       
001600*        PROGRAMMET UPPDATERAR WDM6                                       
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W3T176                                              
002000*        MID:         W3I17601                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W3O17601                                            
002400*                                                                         
002500*    CHANGE LOG:                                                          
002600*                                                                         
002700*    DIGAMBAR/20020715                                                    
002800*    PUT IDBYTREP-9KOMPL IN WDM611                                        
002900*                                                                         
003000*    ELEONOR Ö/20070403                                                   
003100*    RÄTTNING UPPDATERING ETRACKER 4811130                                
003200*                                                                         
003300*    ELEONOR Ö/20080814                                                   
003400*    ÄNDRA SÅ ATT MAN EJ KAN LÄGGA UPP MED DISTRIKT 7574                  
003500*    ELLER 7674.ETRACKER 7229898.                                         
003600*                                                                         
003700*    ETRACKER 10251639 OCT 2015/RAHUL REDDY                               
003800*    ALLOW RETURN QUATITY UPTO 999                                        
003900*                                                                         
004000                                                                          
004100     SKIP3                                                                
004200 ENVIRONMENT DIVISION.                                                    
004300     EJECT                                                                
004400 DATA DIVISION.                                                           
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700*    -- CHECKED BY WY2000                                                 
004800 77  IDPGM                       PIC X(08) VALUE 'W3017600'.              
004900                                                                          
005000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
005100 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005200                                                                          
005300 77  JA                          PIC X     VALUE 'J'.                     
005400 77  NEJ                         PIC X     VALUE 'N'.                     
005500                                                                          
005600*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005700 77  INDX                        PIC S9(4) VALUE +0    COMP SYNC.         
005800 77  MAX-INDX                    PIC S9(4) VALUE +27   COMP SYNC.         
005900 77  MAX-ANTAL                   PIC S9(4) VALUE +5000 COMP SYNC.         
006000 77  ANTAL                       PIC S9(6) VALUE +0    COMP SYNC.         
006100 77  SPRAK-IX                    PIC S9(9) VALUE +0    COMP SYNC.         
006200*   OM SVAR TILL SKÄRM: MAX-MOD-LAENGD = MOD-LÄNGD + 4                    
006300*   OM PROGRAM-TILL-PROGRAM-SWITCH:    = MOD-LÄNGD + 17                   
006400 77  MAX-MOD-LAENGD              PIC S9(4) VALUE +794 COMP SYNC.          
006500 77  DAGENS-TID                  PIC  9(9).                               
006600     EJECT                                                                
006700 77  ARTIKEL-SW                  PIC  X    VALUE 'J'.                     
006800     88  ARTIKEL-LIKA                      VALUE 'J'.                     
006900     88  ARTIKEL-OLIKA                     VALUE 'N'.                     
007000                                                                          
007100 77  WS-KUND-SW                  PIC X     VALUE 'N'.                     
007200     88  KUND-INGANG                       VALUE 'J'.                     
007300                                                                          
007400 77  WS-IDBYTRAD                 PIC S9(5) VALUE ZERO.                    
007500 77  KUND-REG                    PIC X     VALUE SPACE.                   
007600 77  WDM6-FINNS                  PIC X     VALUE 'N'.                     
007700 77  SPEC-MEDD                   PIC X     VALUE 'N'.                     
007800 77  UPD-KLAR                    PIC X     VALUE 'N'.                     
007900 77  W-9KOMPL                    PIC 9(07) VALUE 9999999.                 
008000 77  W-RAPP-IDBYTRAP             PIC 9(07) VALUE ZERO.                    
008100                                                                          
008200                                                                          
008300 01  DAGENS-DATUM                PIC 9(8)  VALUE ZERO.                    
008400 01  FILLER REDEFINES DAGENS-DATUM.                                       
008500     03  DAGENS-DATUM-AAAA       PIC 9(4).                                
008600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008800                                                                          
008900 01  WS-BERADREF.                                                         
009000   03  FILLER                    PIC X(3)  VALUE SPACE.                   
009100   03  WS-IDFAKT                 PIC X(7)  VALUE SPACE.                   
009200                                                                          
009300 01  WS-LOPNUMMER                PIC 9(5).                                
009400 01  XX-LOPNUMMER REDEFINES WS-LOPNUMMER.                                 
009500    03  WS-LOPNUMMER1            PIC 9(3).                                
009600    03  WS-LOPNUMMER2            PIC 9(2).                                
009700                                                                          
009800                                                                          
009900 01  TEST-IDDISTR                PIC 9(5)  COMP-3.                        
010000*01  FILLER -COPY WWDIST33       -RED TEST-IDDISTR.                       
010100 01  TEST-IDARTNR                PIC 9(9)  COMP-3.                        
010200*01  FILLER -COPY WWBYT03        -RED TEST-IDARTNR.                       
010300 01  TEST-KDBYTREF               PIC X(3).                                
010400*01  FILLER -COPY WWBYT15        -RED TEST-KDBYTREF.                      
010500                                                                          
010600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
010700                                                                          
010800 77  INDATA-SW                   PIC X     VALUE 'J'.                     
010900     88  INDATA-OK                         VALUE 'J'.                     
011000     88  INDATA-FEL                        VALUE 'N'.                     
011100                                                                          
011200 77  NYCKLAR-SW                  PIC X     VALUE 'J'.                     
011300     88  NYCKLAR-OK                        VALUE 'J'.                     
011400     88  NYCKLAR-FEL                       VALUE 'N'.                     
011500                                                                          
011600 77  W-IDTRANS                   PIC X(4)  VALUE SPACE.                   
011700     88  EGEN-MID                          VALUE '3176'.                  
011800     88  HELP-MID                          VALUE '0551'.                  
011900     EJECT                                                                
012000*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
012100 01  GENERELLA-SUBPROGRAM.                                                
012200     03  WMEDKONV                PIC X(8)  VALUE 'WMEDKONV'.              
012300     03  CBLTDLI                 PIC X(8)  VALUE 'CBLTDLI '.              
012400     03  FELLOG                  PIC X(8)  VALUE 'FELLOG  '.              
012500     03  W005INIT                PIC X(8)  VALUE 'W005INIT'.              
012600     EJECT                                                                
012700*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012800 01  FILLER                      PIC X(16) VALUE 'WMEDKONV'.              
012900*01 -COPY WMEDAREA                                                        
013000     SKIP3                                                                
013100 01  FILLER                      PIC X(16) VALUE 'WMSGINIT'.              
013200     SKIP3                                                                
013300*01 -COPY WMSGINIT                                                        
013400     SKIP3                                                                
013500*    --- VALID IDDC CODES                                                 
013600*                                                                         
013700 01  FILLER                      PIC X(16) VALUE 'IDDC CODES'.            
013800     SKIP3                                                                
013900*01 -COPY WWDC99                                                          
014000     SKIP3                                                                
014100 01  MESSAGE-CODES.                                                       
014200     03  WRONG-STATUS            PIC X(3)  VALUE '079'.                   
014300     03  REPORT-HAS-WRONG-STATUS PIC X(3)  VALUE '212'.                   
014400     03  ERR-LINE-ALREADY-EXIST  PIC X(3)  VALUE '245'.                   
014500     03  ERR-CORR-HILITE-FLDS    PIC X(3)  VALUE '001'.                   
014600     03  ERR-CUST-MISSING        PIC X(3)  VALUE '137'.                   
014700     03  ERR-DIST-CUST-MISSING   PIC X(3)  VALUE '040'.                   
014800     03  INF-PRESS-PF11          PIC X(3)  VALUE '003'.                   
014900     03  ERR-PF11-AND-NO-DATA    PIC X(3)  VALUE '011'.                   
015000     03  INF-UPDATE-DONE         PIC X(3)  VALUE '101'.                   
015100     03  ERR-WRONG-KEY           PIC X(3)  VALUE '401'.                   
015200     03  ERR-UPDATING-FORBIDDEN  PIC X(3)  VALUE '777'.                   
015300     03  USER-NOT-ALLOWED        PIC X(3)  VALUE '405'.                   
015400     03  ERR-WRONG-DISTRICT      PIC X(3)  VALUE '747'.                   
015500     EJECT                                                                
015600*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015700*                                                                         
015800 01  FILLER                      PIC X(16)  VALUE 'MID-AREA'.             
015900     SKIP3                                                                
016000*01  MID -COPY W3I17601                                                   
016100     EJECT                                                                
016200 01  FILLER                      PIC X(16) VALUE 'MSG/MOD-AREA'.          
016300     SKIP3                                                                
016400*01  -COPY WMSGAREA                                                       
016500     EJECT                                                                
016600     03  MOD REDEFINES MSG-AREA.                                          
016700*      05  -COPY W3O17601                                                 
016800     EJECT                                                                
016900 01  FILLER                      PIC X(16) VALUE 'MFS-AREA'.              
017000     SKIP3                                                                
017100*01  -COPY WMFSAREA                                                       
017200     EJECT                                                                
017300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017400*                                                                         
017500     EJECT                                                                
017600 01  FILLER                      PIC X(16) VALUE 'IMS-WS'.                
017700     SKIP3                                                                
017800 01  NYCKLAR-TILL-DLI.                                                    
017900     03  W-IDARTNR-X.                                                     
018000         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
018100     03  W-KDSEGKEY-X.                                                    
018200         05  FILLER              PIC X(1)  VALUE '1'  .                   
018300                                                                          
018400     03  W-WDM601KY-X.                                                    
018500         05  W-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
018600         05  W-IDBYTRAP-X.                                                
018700           07  W-IDBYTRAP        PIC S9(7) VALUE ZERO COMP-3.             
018800     03  W-WDM611KY-X.                                                    
018900         05  W-IDARTNR-OBJ       PIC S9(9) VALUE ZERO COMP-3.             
019000         05  W-IDTABNR           PIC S9(3) VALUE ZERO COMP-3.             
019100                                                                          
019200     03  W-IDBYTRAD-X.                                                    
019300         05  W-IDBYTRAD          PIC S9(5) VALUE ZERO COMP-3.             
019400                                                                          
019500     03  W-IDGMT-MIN-X.                                                   
019600         05  W-IDDISTR-B2-MIN    PIC S9(5)    VALUE ZERO COMP-3.          
019700         05  W-IDKUNDNR-B2-MIN   PIC S9(7)    VALUE ZERO COMP-3.          
019800                                                                          
019900     03  W-IDGMT-MAX-X.                                                   
020000         05  W-IDDISTR-B2-MAX    PIC S9(5)    VALUE ZERO COMP-3.          
020100         05  W-IDKUNDNR-B2-MAX   PIC S9(7)    VALUE 9999999               
020200                                                         COMP-3.          
020300                                                                          
020400     03  W-IDKUNDNR-X.                                                    
020500         05 W-IDKUNDNR           PIC S9(7) VALUE ZERO COMP-3.             
020600                                                                          
020700     SKIP2                                                                
020800*    --- STATUS-KOD FRÅN IMS                                              
020900 01  STATUS-WS                   PIC XX.                                  
021000     88  SEGMENT-FINNS                     VALUE '  '.                    
021100     88  SEGMENT-FINNS-REDAN               VALUE 'II'.                    
021200     88  SEGMENT-SAKNAS                    VALUE 'GE'.                    
021300     SKIP2                                                                
021400 01  GODK-STATUSKODER.                                                    
021500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021600     SKIP3                                                                
021700 01  SSA1                        PIC X(128).                              
021800 01  SSA2                        PIC X(64).                               
021900     EJECT                                                                
022000*    --- IMS FUNKTIONSKODER                                               
022100*01  -COPY W0003                                                          
022200     EJECT                                                                
022300*    ---  DLI INPUT-OUTPUT AREA                                           
022400 01  FILLER                      PIC X(16) VALUE                          
022500                                               'DLI-IO-WDK601'.           
022600 01  DLI-IO-WDK601.                                                       
022700*    03  -COPY WDK601                                                     
022800     EJECT                                                                
022900 01  FILLER                      PIC X(16) VALUE                          
023000                                               'DLI-IO-WDK611'.           
023100 01  DLI-IO-WDK611.                                                       
023200*    03  -COPY WDK611                                                     
023300*                                                                         
023400 01  FILLER                      PIC X(16) VALUE                          
023500                                               'DLI-IO-WDM601'.           
023600 01  DLI-IO-WDM601.                                                       
023700*    03  -COPY WDM601 -PRE BYT01-                                         
023800*                                                                         
023900 01  FILLER                      PIC X(16) VALUE                          
024000                                               'DLI-IO-WDM611'.           
024100 01  DLI-IO-WDM611.                                                       
024200*    03  -COPY WDM611 -PRE BYT11-                                         
024300*                                                                         
024400 01  FILLER                      PIC X(16) VALUE                          
024500                                               'DLI-IO-WDB201'.           
024600 01  DLI-IO-WDB201.                                                       
024700*    03  -COPY WDB201                                                     
024800     EJECT                                                                
024900 LINKAGE SECTION.                                                         
025000                                                                          
025100*01  -COPY W0009  -PRE MSG-                                               
025200     EJECT                                                                
025300*01  -COPY W0008  -PRE WDP7-                                              
025400     05  FILLER                  PIC X.                                   
025500     EJECT                                                                
025600*01  -COPY W0008  -PRE WDK6-                                              
025700     05  FILLER                  PIC X.                                   
025800     EJECT                                                                
025900*01  -COPY W0008  -PRE WDM6-                                              
026000     05  FILLER                  PIC X.                                   
026100     EJECT                                                                
026200*01  -COPY W0008  -PRE WDB2-                                              
026300     05  FILLER                  PIC X.                                   
026400     EJECT                                                                
026500 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDK6-PCB                      
026600               WDM6-PCB WDB2-PCB.                                         
026700     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDK6-PCB                      
026800               WDM6-PCB WDB2-PCB.                                         
026900                                                                          
027000     PERFORM IMS-GET-MSG                                                  
027100     IF SEGMENT-FINNS                                                     
027200       PERFORM A-INIT                                                     
027300       PERFORM B-KOLLA-NYCKLAR                                            
027400       IF NYCKLAR-OK                                                      
027500         IF MFS-UPDATE                                                    
027600           PERFORM G-KOLLA-INPUT                                          
027700           IF INDATA-OK                                                   
027800             PERFORM H-UPPDATERA                                          
027900           END-IF                                                         
028000         ELSE                                                             
028100           PERFORM E-SAMMA-SIDA                                           
028200         END-IF                                                           
028300       END-IF                                                             
028400       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
028500       PERFORM IMS-ISRT-MSG                                               
028600     END-IF                                                               
028700                                                                          
028800     MOVE ZERO TO RETURN-CODE                                             
028900     GOBACK                                                               
029000     .                                                                    
029100     EJECT                                                                
029200 A-INIT SECTION.                                                          
029300     IF MSG-DUBBLA-TRANSKODER                                             
029400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I17601                 
029500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
029600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
029700     ELSE                                                                 
029800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I17601                  
029900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
030000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
030100     END-IF                                                               
030200                                                                          
030300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
030400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
030500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
030600                                                                          
030700     MOVE LOW-VALUE TO MSG-AREA                                           
030800     MOVE 'W3O176N1' TO MFS-IDMOD                                         
030900     MOVE '3176' TO MOD-IDTRANS                                           
031000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
031100     MOVE SPACE           TO MED-IDMFSFEL                                 
031200                                                                          
031300     IF EGEN-MID OR HELP-MID                                              
031400       CONTINUE                                                           
031500     ELSE                                                                 
031600       MOVE SPACE TO MFS-KDTRTYP                                          
031700     END-IF                                                               
031800                                                                          
031900     IF ENGLISH-TEXT                                                      
032000       MOVE +2 TO SPRAK-IX                                                
032100       MOVE 'GB ' TO MED-IDSKYLT                                          
032200     ELSE                                                                 
032300       MOVE +1 TO SPRAK-IX                                                
032400       MOVE 'S  ' TO MED-IDSKYLT                                          
032500     END-IF                                                               
032600                                                                          
032700     ACCEPT DAGENS-TID   FROM TIME                                        
032800     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
032900     MOVE ZERO      TO WS-IDBYTRAD                                        
033000     .                                                                    
033100     EJECT                                                                
033200 B-KOLLA-NYCKLAR SECTION.                                                 
033300                                                                          
033400     MOVE JA TO NYCKLAR-SW                                                
033500     MOVE ALL '+' TO MSGI-WMSGINIT                                        
033600     MOVE '001' TO MSGI-KDCALL                                            
033700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
033800     MOVE '3176' TO MSGI-IDTRANS                                          
033900     MOVE MSG-LTERM-NAME TO MSGI-IDLTERM-USER                             
034000     IF NOT EGEN-MID                                                      
034100       PERFORM MFS-RENSA-FAELT-IN                                         
034200       MOVE ZERO TO MSGI-IDDISTR                                          
034300       MOVE SPACE TO MSGI-IDKUNDNR                                        
034400       MOVE ZERO TO MSGI-IDFAKT                                           
034500       MOVE ZERO TO MSGI-IDBYTRAP                                         
034600       MOVE NEJ TO NYCKLAR-SW                                             
034700       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
034800     ELSE                                                                 
034900       MOVE MID-IDDISTR-IN TO MSGI-IDDISTR                                
035000       MOVE MID-IDKUNDNR-IN TO MSGI-IDKUNDNR                              
035100       MOVE MID-IDFAKT-IN TO MSGI-IDFAKT                                  
035200       MOVE MID-IDBYTRAP-IN TO MSGI-IDBYTRAP                              
035300       CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                         
035400                                                                          
035500*      -- KONTROLL AV DC                                                  
035600       MOVE MSGI-IDDC TO WS-IDDC                                          
035700                                                                          
035800       IF GOOD-DC                                                         
035810         PERFORM BA-KOLLA-DISTR                                           
035820         PERFORM BB-KOLLA-KUNDNR-OCH-WDB2                                 
036400       ELSE                                                               
036500         MOVE USER-NOT-ALLOWED TO MED-IDMFSINF                            
036600         CALL WMEDKONV USING MED-WMEDAREA                                 
036610         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
036620         PERFORM MFS-RENSA-FAELT-IN                                       
036630         MOVE NEJ TO INDATA-SW                                            
036700       END-IF                                                             
036800                                                                          
036900       MOVE MFS-RENSA-FAELT TO MOD-IDFAKT-IN                              
037000       IF MSGI-IDFAKT NUMERIC AND MSGI-IDFAKT > ZERO                      
037100         CONTINUE                                                         
037200       ELSE                                                               
037300         MOVE NEJ TO NYCKLAR-SW                                           
037400       MOVE NEJ TO KUND-REG                                               
037500       END-IF                                                             
037600                                                                          
037700       MOVE MFS-RENSA-FAELT TO MOD-IDBYTRAP-IN                            
037800       IF MSGI-IDBYTRAP NUMERIC AND MSGI-IDBYTRAP > ZERO                  
037900         MOVE MSGI-IDBYTRAP TO W-IDBYTRAP                                 
038000       ELSE                                                               
038100         MOVE NEJ TO NYCKLAR-SW                                           
038200       END-IF                                                             
038300                                                                          
038400       IF EGEN-MID OR NYCKLAR-OK                                          
038500         MOVE MSGI-IDDISTR TO MOD-IDDISTR-UT                              
038600         INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE           
038700         MOVE MSGI-IDKUNDNR TO MOD-IDKUNDNR-UT                            
038800         INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE          
038900         MOVE MSGI-IDFAKT TO MOD-IDFAKT-UT                                
039000                             WS-IDFAKT                                    
039100         INSPECT MOD-IDFAKT-UT REPLACING LEADING ZERO BY SPACE            
039200         INSPECT WS-IDFAKT REPLACING LEADING ZERO BY SPACE                
039300         MOVE MSGI-IDBYTRAP TO MOD-IDBYTRAP-UT                            
039400         INSPECT MOD-IDBYTRAP-UT REPLACING LEADING ZERO BY SPACE          
039500       ELSE                                                               
039600         MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                           
039700                                 MOD-IDKUNDNR-UT                          
039800                                 MOD-IDFAKT-UT                            
039900                                 MOD-IDBYTRAP-UT                          
040000       END-IF                                                             
040100                                                                          
040200       IF NYCKLAR-FEL                                                     
040300         IF MED-IDMFSFEL = SPACE                                          
040400           MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                             
040500         END-IF                                                           
040600         CALL WMEDKONV USING MED-WMEDAREA                                 
040700         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
040800         PERFORM MFS-RENSA-FAELT-IN                                       
040900       END-IF                                                             
041000     END-IF                                                               
041100     .                                                                    
041200     EJECT                                                                
041300 BA-KOLLA-DISTR SECTION.                                                  
041400                                                                          
041500     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
041600     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
042100       MOVE MSGI-IDDISTR TO W-IDDISTR                                     
042200       MOVE MSGI-IDDISTR TO W-IDDISTR-B2-MIN                              
042300       MOVE MSGI-IDDISTR TO W-IDDISTR-B2-MAX                              
042500     ELSE                                                                 
042600       MOVE NEJ TO NYCKLAR-SW                                             
042700     END-IF                                                               
042800     .                                                                    
042900     EJECT                                                                
043000 BB-KOLLA-KUNDNR-OCH-WDB2 SECTION.                                        
043100                                                                          
043200     MOVE MFS-RENSA-FAELT TO MOD-IDKUNDNR-IN                              
043300     IF NYCKLAR-OK                                                        
043400       IF MID-IDKUNDNR-IN NOT = ALL '+'                                   
043500         IF MSGI-IDKUNDNR = SPACE                                         
043600           MOVE ZERO        TO W-IDKUNDNR                                 
043700         ELSE                                                             
043800           IF MSGI-IDKUNDNR NOT NUMERIC                                   
043900             MOVE NEJ TO NYCKLAR-SW                                       
044000           ELSE                                                           
044100             MOVE JA TO WS-KUND-SW                                        
044200             MOVE MSGI-IDKUNDNR TO W-IDKUNDNR-B2-MIN                      
044300                                   W-IDKUNDNR-B2-MAX                      
044400                                   W-IDKUNDNR                             
044500           END-IF                                                         
044600         END-IF                                                           
044700       ELSE                                                               
044800         IF MSGI-IDKUNDNR = SPACE                                         
044900           MOVE ZERO        TO W-IDKUNDNR                                 
045000         ELSE                                                             
045100           MOVE JA TO WS-KUND-SW                                          
045200           MOVE MSGI-IDKUNDNR TO W-IDKUNDNR-B2-MIN                        
045300                                 W-IDKUNDNR-B2-MAX                        
045400                                 W-IDKUNDNR                               
045500         END-IF                                                           
045600       END-IF                                                             
045700                                                                          
045800       PERFORM IMS-GU-WDB2                                                
045900                                                                          
046000       IF SEGMENT-SAKNAS                                                  
046100         MOVE NEJ TO NYCKLAR-SW                                           
046200         MOVE ERR-DIST-CUST-MISSING TO MED-IDMFSFEL                       
046300         CALL WMEDKONV USING MED-WMEDAREA                                 
046400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
046500         PERFORM MFS-RENSA-FAELT-IN                                       
046600       END-IF                                                             
046700     END-IF                                                               
046800     .                                                                    
046900     EJECT                                                                
047000 E-SAMMA-SIDA SECTION.                                                    
047100                                                                          
047200     IF EGEN-MID OR HELP-MID                                              
047300       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
047400       CALL WMEDKONV USING MED-WMEDAREA                                   
047500       MOVE MED-MFSINF TO MOD-TEMFSFEL                                    
047600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
047700       PERFORM MFS-LAES-IN-IGEN                                           
047800     ELSE                                                                 
047900       PERFORM MFS-RENSA-FAELT-IN                                         
048000     END-IF                                                               
048100     .                                                                    
048200     EJECT                                                                
048300 G-KOLLA-INPUT SECTION.                                                   
048400                                                                          
048500     MOVE JA   TO INDATA-SW                                               
048600     PERFORM IMS-GHU-WDM601                                               
048700     IF SEGMENT-FINNS AND (BYT01-RAPP-IDKUNDNR NOT = W-IDKUNDNR)          
048800       MOVE ERR-LINE-ALREADY-EXIST TO MED-IDMFSFEL                        
048900       CALL WMEDKONV USING MED-WMEDAREA                                   
049000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
049100       PERFORM MFS-RENSA-FAELT-IN                                         
049200       MOVE NEJ TO INDATA-SW                                              
049300     ELSE                                                                 
049400       IF BYT01-RAPP-KDBYTSTA-RAPP < 4                                    
049500         IF SEGMENT-FINNS                                                 
049600           MOVE JA TO WDM6-FINNS                                          
049700           MOVE BYT01-RAPP-KVRETUR-TOT TO ANTAL                           
049800         ELSE                                                             
049900           MOVE NEJ TO WDM6-FINNS                                         
050000           MOVE ZERO TO ANTAL                                             
050100         END-IF                                                           
050200                                                                          
050300         MOVE +1 TO INDX                                                  
050400         PERFORM UNTIL INDX > MAX-INDX OR MID-INPUT (INDX)                
050500           NOT = ALL '+'                                                  
050600           ADD +1 TO INDX                                                 
050700         END-PERFORM                                                      
050800         IF INDX = 28                                                     
050900*       FLAGGA-KLAR                                                       
051000           IF MID-FLKLAR NOT = ALL '+'                                    
051100             IF MID-FLKLAR = 'Y' OR 'J'                                   
051200               MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKLAR-ATTR               
051300               MOVE JA TO UPD-KLAR                                        
051400             ELSE                                                         
051500               MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKLAR-ATTR                 
051600               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
051700               CALL WMEDKONV USING MED-WMEDAREA                           
051800               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
051900               PERFORM MFS-ROER-EJ-FAELT-IN                               
052000               MOVE NEJ TO INDATA-SW                                      
052100             END-IF                                                       
052200           ELSE                                                           
052300             MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                    
052400             CALL WMEDKONV USING MED-WMEDAREA                             
052500             MOVE MED-MFSFEL TO MOD-TEMFSFEL                              
052600             PERFORM MFS-ROER-EJ-FAELT-IN                                 
052700             MOVE NEJ TO INDATA-SW                                        
052800             MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDARTNR-RAD-ATTR (1)        
052900           END-IF                                                         
053000         ELSE                                                             
053100           MOVE +1 TO INDX                                                
053200           PERFORM UNTIL INDX > MAX-INDX                                  
053300             IF MID-INPUT (INDX) NOT = ALL '+'                            
053400               IF MID-IDARTNR-RAD (INDX) NOT = ALL '+'                    
053500                 IF MID-IDARTNR-RAD (INDX) NUMERIC                        
053600                   MOVE MID-IDARTNR-RAD (INDX) TO TEST-IDARTNR            
053700                   IF BYT03-OBJEKT                                        
053800                     MOVE MID-IDARTNR-RAD (INDX) TO W-IDARTNR             
053900                     PERFORM IMS-GU-WDK601                                
054000                     IF SEGMENT-FINNS                                     
054100                       IF CLAG-PRARTSJK > ZERO                            
054200                         MOVE MFS-NUM-FAELT-RAETT TO                      
054300                              MOD-IDARTNR-RAD-ATTR (INDX)                 
054400                       ELSE                                               
054500                         MOVE MFS-NUM-FAELT-FEL TO                        
054600                              MOD-IDARTNR-RAD-ATTR (INDX)                 
054700                         MOVE NEJ TO INDATA-SW                            
054800                       END-IF                                             
054900                     ELSE                                                 
055000                       MOVE MFS-NUM-FAELT-FEL TO                          
055100                         MOD-IDARTNR-RAD-ATTR (INDX)                      
055200                       MOVE NEJ TO INDATA-SW                              
055300                     END-IF                                               
055400                   ELSE                                                   
055500                     MOVE MFS-NUM-FAELT-FEL TO                            
055600                          MOD-IDARTNR-RAD-ATTR (INDX)                     
055700                     MOVE NEJ TO INDATA-SW                                
055800                   END-IF                                                 
055900                 ELSE                                                     
056000                   MOVE MFS-NUM-FAELT-FEL TO                              
056100                        MOD-IDARTNR-RAD-ATTR (INDX)                       
056200                   MOVE NEJ TO INDATA-SW                                  
056300                 END-IF                                                   
056400               ELSE                                                       
056500                 MOVE MFS-NUM-FAELT-FEL TO                                
056600                      MOD-IDARTNR-RAD-ATTR (INDX)                         
056700                 MOVE NEJ TO INDATA-SW                                    
056800               END-IF                                                     
056900                                                                          
057000               IF MID-KVRETUR-RAD (INDX) NOT = ALL '+'                    
057100                 IF MID-KVRETUR-RAD (INDX) NUMERIC AND                    
057200                    MID-KVRETUR-RAD (INDX) > ZERO                         
057300*       ANTAL FÅR EJ ÖVERSKRIDA 999 PÅ RADEN                              
057400                                                                          
057500                   IF WDM6-FINNS = JA                                     
057600                     MOVE MID-IDARTNR-RAD (INDX) TO W-IDARTNR-OBJ         
057700                     MOVE ZERO                 TO W-IDTABNR               
057800                     MOVE ZERO                 TO W-IDBYTRAD              
057900                     MOVE NEJ                  TO ARTIKEL-SW              
058000                     PERFORM IMS-GHNP-WDM611                              
058100                     PERFORM UNTIL SEGMENT-SAKNAS OR ARTIKEL-LIKA         
058200                       IF SEGMENT-FINNS                                   
058300                         IF W-IDARTNR-OBJ = BYT11-OBJ-IDARTNR-OBJ         
058400                           MOVE JA             TO ARTIKEL-SW              
058500                         END-IF                                           
058600                       END-IF                                             
058700                       PERFORM IMS-GHNP-WDM611                            
058800                     END-PERFORM                                          
058900                     IF ARTIKEL-LIKA                                      
059000                       ADD MID-KVRETUR-RAD (INDX) TO                      
059100                           BYT11-OBJ-KVRETUR-URSP                         
059200                       IF BYT11-OBJ-KVRETUR-URSP NOT > +999               
059300                         CONTINUE                                         
059400                       ELSE                                               
059500                         MOVE MFS-NUM-FAELT-FEL TO                        
059600                              MOD-KVRETUR-RAD-ATTR (INDX)                 
059700                         MOVE JA TO SPEC-MEDD                             
059800                         MOVE NEJ TO INDATA-SW                            
059900                       END-IF                                             
060000                     ELSE                                                 
060100                       IF MID-KVRETUR-RAD (INDX) NOT > +999               
060200                         CONTINUE                                         
060300                       ELSE                                               
060400                         MOVE MFS-NUM-FAELT-FEL TO                        
060500                              MOD-KVRETUR-RAD-ATTR (INDX)                 
060600                         MOVE JA TO SPEC-MEDD                             
060700                         MOVE NEJ TO INDATA-SW                            
060800                       END-IF                                             
060900                     END-IF                                               
060910                   ELSE                                                   
060920                     IF MID-KVRETUR-RAD (INDX) NOT > +999                 
060930                        CONTINUE                                          
060940                     ELSE                                                 
060950                        MOVE MFS-NUM-FAELT-FEL TO                         
060960                             MOD-KVRETUR-RAD-ATTR (INDX)                  
060970                        MOVE JA TO SPEC-MEDD                              
060980                        MOVE NEJ TO INDATA-SW                             
060990                     END-IF                                               
061000                   END-IF                                                 
061100                   IF INDATA-OK                                           
061200                     IF MID-KVRETUR-RAD (INDX) NOT > MAX-ANTAL            
061300                      COMPUTE ANTAL = ANTAL +                             
061400                                     MID-KVRETUR-RAD (INDX)               
061500                       IF ANTAL < +5001                                   
061600                         MOVE MFS-ALFA-FAELT-RAETT TO                     
061700                              MOD-KVRETUR-RAD-ATTR (INDX)                 
061800                       ELSE                                               
061900                         MOVE MFS-NUM-FAELT-FEL TO                        
062000                              MOD-KVRETUR-RAD-ATTR (INDX)                 
062100                         MOVE NEJ TO INDATA-SW                            
062200                       END-IF                                             
062300                     ELSE                                                 
062400                       MOVE MFS-NUM-FAELT-FEL TO                          
062500                            MOD-KVRETUR-RAD-ATTR (INDX)                   
062600                       MOVE NEJ TO INDATA-SW                              
062700                     END-IF                                               
062800                   END-IF                                                 
062900                 ELSE                                                     
063000                   MOVE MFS-NUM-FAELT-FEL TO                              
063100                        MOD-KVRETUR-RAD-ATTR (INDX)                       
063200                   MOVE NEJ TO INDATA-SW                                  
063300                 END-IF                                                   
063400               ELSE                                                       
063500                 MOVE MFS-NUM-FAELT-FEL TO                                
063600                      MOD-KVRETUR-RAD-ATTR (INDX)                         
063700                 MOVE NEJ TO INDATA-SW                                    
063800               END-IF                                                     
063900*       FLAGGA-KLAR                                                       
064000               IF MID-FLKLAR NOT = ALL '+'                                
064100                 IF MID-FLKLAR = 'Y'                                      
064200                   MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKLAR-ATTR           
064300                   MOVE JA TO UPD-KLAR                                    
064400                 ELSE                                                     
064500                   IF MID-FLKLAR = 'N'                                    
064600                     MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKLAR-ATTR         
064700                     MOVE NEJ TO UPD-KLAR                                 
064800                   ELSE                                                   
064900                     MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKLAR-ATTR           
065000                     MOVE NEJ TO UPD-KLAR                                 
065100                     MOVE NEJ TO INDATA-SW                                
065200                   END-IF                                                 
065300                 END-IF                                                   
065400               ELSE                                                       
065500                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKLAR-ATTR             
065600                 MOVE NEJ TO UPD-KLAR                                     
065700               END-IF                                                     
065800             ELSE                                                         
065900               CONTINUE                                                   
066000             END-IF                                                       
066100             ADD +1 TO INDX                                               
066200           END-PERFORM                                                    
066300           IF INDATA-FEL                                                  
066400             IF SPEC-MEDD = NEJ                                           
066500               MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                  
066600               CALL WMEDKONV USING MED-WMEDAREA                           
066700               MOVE MED-MFSFEL TO MOD-TEMFSFEL                            
066800               PERFORM MFS-ROER-EJ-FAELT-IN                               
066900             ELSE                                                         
067000               IF ENGLISH-TEXT                                            
067100                 MOVE 'LINE QUANTITY MORE THAN 999'                       
067200                                     TO MOD-TEMFSFEL                      
067300               ELSE                                                       
067400                MOVE 'RAD KVANTITET STÖRRE ÄN 999'                        
067500                                     TO MOD-TEMFSFEL                      
067600               END-IF                                                     
067700               PERFORM MFS-ROER-EJ-FAELT-IN                               
067800             END-IF                                                       
067900           END-IF                                                         
068000         END-IF                                                           
068100       ELSE                                                               
068200         MOVE WRONG-STATUS TO MED-IDMFSFEL                                
068300         CALL WMEDKONV USING MED-WMEDAREA                                 
068400         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
068500         PERFORM MFS-RENSA-FAELT-IN                                       
068600         MOVE NEJ TO INDATA-SW                                            
068700       END-IF                                                             
068800     END-IF                                                               
068900     .                                                                    
069000     EJECT                                                                
069100 H-UPPDATERA SECTION.                                                     
069200                                                                          
069300     PERFORM IMS-GHU-WDM601                                               
069400     MOVE W-IDDISTR           TO BYT01-RAPP-IDDISTR                       
069500     MOVE W-IDKUNDNR          TO BYT01-RAPP-IDKUNDNR                      
069600     MOVE MSGI-IDBYTRAP       TO BYT01-RAPP-IDBYTRAP                      
069700     MOVE MSGI-IDFAKT         TO BYT01-RAPP-IDFAKT                        
069800     MOVE MSG-SIGNON-USERID   TO BYT01-RAPP-IDUSER                        
069900     MOVE ANTAL               TO BYT01-RAPP-KVRETUR-TOT                   
070000     MOVE FUNCTION CURRENT-DATE (1:8) TO BYT01-RAPP-DAREGDAT              
070500     MOVE ZERO                TO BYT01-RAPP-DAANKDAG                      
070100     MOVE 'N'                 TO BYT01-RAPP-FLBYTGAR                      
070200     MOVE 'N'                 TO BYT01-RAPP-FLBYGODK                      
070300     MOVE WS-IDDC             TO BYT01-RAPP-IDDC                          
070400     IF UPD-KLAR = JA                                                     
070600       MOVE '2'               TO BYT01-RAPP-KDBYTSTA-RAPP                 
070700     ELSE                                                                 
070900       MOVE SPACE             TO BYT01-RAPP-KDBYTSTA-RAPP                 
071000     END-IF                                                               
071100     IF WDM6-FINNS = JA                                                   
071200       PERFORM IMS-REPL-WDM601                                            
071300     ELSE                                                                 
071400       MOVE SPACE               TO BYT01-RAPP-ADBYTANK                    
071500       MOVE ZERO                TO BYT01-RAPP-DAREGDAT-GODK               
071600       MOVE SPACE               TO BYT01-RAPP-KDBYTSTA-AVL                
071700       MOVE SPACE               TO BYT01-RAPP-KDBYTBEK                    
071800       PERFORM IMS-ISRT-WDM601                                            
071900       PERFORM IMS-GU-WDM601                                              
072000     END-IF                                                               
072100                                                                          
072200     MOVE 1 TO INDX                                                       
072300     PERFORM UNTIL INDX > MAX-INDX                                        
072400       IF MID-INPUT (INDX) NOT = ALL '+'                                  
072500         PERFORM IMS-GHU-WDM601                                           
072600         MOVE BYT01-RAPP-IDBYTRAP    TO W-RAPP-IDBYTRAP                   
072700         MOVE MID-IDARTNR-RAD (INDX) TO W-IDARTNR-OBJ                     
072800         MOVE ZERO                   TO W-IDTABNR                         
072900         MOVE ZERO                   TO WS-IDBYTRAD                       
073000         MOVE ZERO                   TO WS-LOPNUMMER                      
073100         MOVE ZERO                   TO W-IDBYTRAD                        
073200         MOVE NEJ                    TO ARTIKEL-SW                        
073300         PERFORM UNTIL SEGMENT-SAKNAS                                     
073400         OR ARTIKEL-LIKA                                                  
073500            PERFORM IMS-GHNP-WDM611                                       
073600            IF SEGMENT-FINNS                                              
073700               IF W-IDARTNR-OBJ = BYT11-OBJ-IDARTNR-OBJ                   
073800                  MOVE JA              TO ARTIKEL-SW                      
073900               END-IF                                                     
074000               MOVE BYT11-OBJ-IDBYTRAD TO WS-IDBYTRAD                     
074100            END-IF                                                        
074200         END-PERFORM                                                      
074300         MOVE ZERO                     TO BYT11-OBJ-IDORDER               
074400         MOVE WS-BERADREF              TO BYT11-OBJ-BERADREF              
074500         IF ARTIKEL-LIKA                                                  
074600           ADD MID-KVRETUR-RAD (INDX)  TO BYT11-OBJ-KVRETUR-URSP          
074700           PERFORM IMS-REPL-WDM611                                        
074800         ELSE                                                             
074900           MOVE MID-IDARTNR-RAD (INDX) TO BYT11-OBJ-IDARTNR-OBJ           
075000           MOVE ZERO                   TO BYT11-OBJ-IDTABNR               
075100           MOVE SPACE                  TO BYT11-OBJ-KDBYTREF              
075200           MOVE ZERO                   TO BYT11-OBJ-KVRETUR-GODK          
075300           MOVE SPACE                  TO BYT11-OBJ-KDBYTSTA-OBJ          
075400           MOVE MID-KVRETUR-RAD (INDX) TO BYT11-OBJ-KVRETUR-URSP          
075500           MOVE SPACE                  TO BYT11-OBJ-KDBYTSTA-AVL          
075600           MOVE SPACE                  TO BYT11-OBJ-FLSKROT               
075700           IF WS-IDBYTRAD = ZERO                                          
075800              MOVE 00100               TO WS-IDBYTRAD                     
075900           ELSE                                                           
076000              MOVE WS-IDBYTRAD         TO WS-LOPNUMMER                    
076100              IF WS-LOPNUMMER2 = ZERO                                     
076200                 ADD +100              TO WS-IDBYTRAD                     
076300              ELSE                                                        
076400                 ADD +1                TO WS-IDBYTRAD                     
076500              END-IF                                                      
076600           END-IF                                                         
076700           MOVE WS-IDBYTRAD         TO BYT11-OBJ-IDBYTRAD                 
076800           COMPUTE  BYT11-OBJ-IDBYTRAP-9KOMPL =                           
076900                             W-9KOMPL - W-RAPP-IDBYTRAP                   
077000                                                                          
077100           PERFORM IMS-ISRT-WDM611                                        
077200         END-IF                                                           
077300       END-IF                                                             
077400       ADD +1 TO INDX                                                     
077500     END-PERFORM                                                          
077600                                                                          
077700     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
077800     CALL WMEDKONV USING MED-WMEDAREA                                     
077900     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
078000     PERFORM MFS-FORM-ATTR                                                
078100     PERFORM MFS-RENSA-FAELT-IN                                           
078200     IF UPD-KLAR = NEJ                                                    
078300       MOVE MFS-ADD-SAETT-CURSOR TO MOD-IDARTNR-RAD-ATTR (1)              
078400     END-IF                                                               
078500     .                                                                    
078600     EJECT                                                                
078700 MFS-RENSA-FAELT-IN SECTION.                                              
078800                                                                          
078900*    --- ALLA INDATA-FÄLT                                                 
079000     MOVE +1 TO INDX                                                      
079100     PERFORM UNTIL INDX > MAX-INDX                                        
079200       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-RAD (INDX)                     
079300                               MOD-KVRETUR-RAD (INDX)                     
079400                               MOD-IDTABNR-RAD (INDX)                     
079500       ADD +1 TO INDX                                                     
079600     END-PERFORM                                                          
079700     MOVE MFS-RENSA-FAELT   TO MOD-FLKLAR                                 
079800     .                                                                    
079900     EJECT                                                                
080000 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
080100                                                                          
080200*    --- ALLA INDATA-FÄLT                                                 
080300     MOVE +1 TO INDX                                                      
080400     PERFORM UNTIL INDX > MAX-INDX                                        
080500       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-RAD (INDX)                   
080600                                 MOD-KVRETUR-RAD (INDX)                   
080700                                 MOD-IDTABNR-RAD (INDX)                   
080800       ADD +1 TO INDX                                                     
080900     END-PERFORM                                                          
081000     MOVE MFS-ROER-EJ-FAELT   TO MOD-FLKLAR                               
081100     .                                                                    
081200     EJECT                                                                
081300 MFS-FORM-ATTR SECTION.                                                   
081400                                                                          
081500*    --- ALLA INDATA-FÄLT                                                 
081600     MOVE +1 TO INDX                                                      
081700     PERFORM UNTIL INDX > MAX-INDX                                        
081800       MOVE MFS-FORMATETS-ATTR TO MOD-IDARTNR-RAD-ATTR (INDX)             
081900                                  MOD-KVRETUR-RAD-ATTR (INDX)             
082000                                  MOD-IDTABNR-RAD-ATTR (INDX)             
082100       ADD +1 TO INDX                                                     
082200     END-PERFORM                                                          
082300     MOVE MFS-FORMATETS-ATTR   TO MOD-FLKLAR-ATTR                         
082400     .                                                                    
082500     SKIP2                                                                
082600 MFS-LAES-IN-IGEN SECTION.                                                
082700                                                                          
082800*    --- ALLA INDATA-FÄLT                                                 
082900     MOVE +1 TO INDX                                                      
083000     PERFORM UNTIL INDX > MAX-INDX                                        
083100       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-RAD-ATTR (INDX)          
083200                                     MOD-KVRETUR-RAD-ATTR (INDX)          
083300                                     MOD-IDTABNR-RAD-ATTR (INDX)          
083400       ADD +1 TO INDX                                                     
083500     END-PERFORM                                                          
083600     MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-FLKLAR-ATTR                      
083700     .                                                                    
083800     EJECT                                                                
083900* --- IMS SEKTIONER ---                                                   
084000     SKIP3                                                                
084100 IMS-GET-MSG SECTION.                                                     
084200                                                                          
084300     MOVE '  QC' TO GODK-STATUSKODER                                      
084400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
084500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
084600     PERFORM IMS-STATUSKONTROLL                                           
084700     .                                                                    
084800     SKIP3                                                                
084900 IMS-ISRT-MSG SECTION.                                                    
085000                                                                          
085100     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
085200       MOVE '0' TO MFS-KDHUVOMR                                           
085300     END-IF                                                               
085400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
085500     MOVE SPACE TO GODK-STATUSKODER                                       
085600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
085700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
085800     PERFORM IMS-STATUSKONTROLL                                           
085900     .                                                                    
086000     SKIP3                                                                
086100 IMS-GU-WDK601 SECTION.                                                   
086200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
086300          DELIMITED BY SIZE INTO SSA1                                     
086400     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
086500          DELIMITED BY SIZE INTO SSA2                                     
086600     MOVE '  GE' TO GODK-STATUSKODER                                      
086700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
086800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
086900     PERFORM IMS-STATUSKONTROLL                                           
087000     .                                                                    
087100     SKIP3                                                                
087200 IMS-GHU-WDM601-KUND SECTION.                                             
087300     STRING 'WDM601  (WDM601KY =' W-WDM601KY-X                            
087400                    '&IDKUNDNR =' W-IDKUNDNR-X ')'                        
087500          DELIMITED BY SIZE INTO SSA1                                     
087600     MOVE '  GE' TO GODK-STATUSKODER                                      
087700     CALL CBLTDLI USING GHU WDM6-PCB DLI-IO-WDM601 SSA1                   
087800     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
087900     PERFORM IMS-STATUSKONTROLL                                           
088000     .                                                                    
088100     SKIP3                                                                
088200 IMS-GHU-WDM601 SECTION.                                                  
088300     STRING 'WDM601  (WDM601KY =' W-WDM601KY-X ')'                        
088400          DELIMITED BY SIZE INTO SSA1                                     
088500     MOVE '  GE' TO GODK-STATUSKODER                                      
088600     CALL CBLTDLI USING GHU WDM6-PCB DLI-IO-WDM601 SSA1                   
088700     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
088800     PERFORM IMS-STATUSKONTROLL                                           
088900     .                                                                    
089000     SKIP3                                                                
089100 IMS-GU-WDM601 SECTION.                                                   
089200     STRING 'WDM601  (WDM601KY =' W-WDM601KY-X ')'                        
089300          DELIMITED BY SIZE INTO SSA1                                     
089400     MOVE '  GE' TO GODK-STATUSKODER                                      
089500     CALL CBLTDLI USING GU WDM6-PCB DLI-IO-WDM601 SSA1                    
089600     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
089700     PERFORM IMS-STATUSKONTROLL                                           
089800     .                                                                    
089900     SKIP3                                                                
090000 IMS-ISRT-WDM601 SECTION.                                                 
090100                                                                          
090200     MOVE 'WDM601  ' TO SSA1                                              
090300     MOVE '  ' TO GODK-STATUSKODER                                        
090400     CALL CBLTDLI USING ISRT WDM6-PCB DLI-IO-WDM601 SSA1                  
090500     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
090600     PERFORM IMS-STATUSKONTROLL                                           
090700     .                                                                    
090800     SKIP3                                                                
090900 IMS-REPL-WDM601 SECTION.                                                 
091000                                                                          
091100     MOVE '  ' TO GODK-STATUSKODER                                        
091200     CALL CBLTDLI USING REPL WDM6-PCB DLI-IO-WDM601                       
091300     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
091400     PERFORM IMS-STATUSKONTROLL                                           
091500     .                                                                    
091600     SKIP3                                                                
091700 IMS-GHNP-WDM611 SECTION.                                                 
091800     STRING 'WDM611  (IDBYTRAD>=' W-IDBYTRAD-X ')'                        
091900          DELIMITED BY SIZE INTO SSA1                                     
092000     MOVE '  GEII' TO GODK-STATUSKODER                                    
092100     CALL CBLTDLI USING GHNP WDM6-PCB DLI-IO-WDM611 SSA1                  
092200     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
092300     PERFORM IMS-STATUSKONTROLL                                           
092400     .                                                                    
092500     SKIP3                                                                
092600 IMS-ISRT-WDM611 SECTION.                                                 
092700                                                                          
092800     MOVE 'WDM611  ' TO SSA1                                              
092900     MOVE '  ' TO GODK-STATUSKODER                                        
093000     CALL CBLTDLI USING ISRT WDM6-PCB DLI-IO-WDM611 SSA1                  
093100     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
093200     PERFORM IMS-STATUSKONTROLL                                           
093300     .                                                                    
093400     SKIP3                                                                
093500 IMS-REPL-WDM611 SECTION.                                                 
093600                                                                          
093700     MOVE '  ' TO GODK-STATUSKODER                                        
093800     CALL CBLTDLI USING REPL WDM6-PCB DLI-IO-WDM611                       
093900     MOVE WDM6-STATUS-CODE TO STATUS-WS                                   
094000     PERFORM IMS-STATUSKONTROLL                                           
094100     .                                                                    
094200     SKIP3                                                                
094300 IMS-GU-WDB2 SECTION.                                                     
094400                                                                          
094500     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
094600                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
094700          DELIMITED BY SIZE INTO SSA1                                     
094800     MOVE '  GE' TO GODK-STATUSKODER                                      
094900     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
095000     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
095100     PERFORM IMS-STATUSKONTROLL                                           
095200     .                                                                    
095300     SKIP3                                                                
095400 IMS-STATUSKONTROLL SECTION.                                              
095500                                                                          
095600     SET STATUS-IX TO 1                                                   
095700     SEARCH GODK-STATUS                                                   
095800       AT END                                                             
095900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
096000         DELIMITED BY SIZE INTO FELTEXT                                   
096100         CALL FELLOG                                                      
096200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
096300         CONTINUE                                                         
096400     END-SEARCH                                                           
096500     .                                                                    
