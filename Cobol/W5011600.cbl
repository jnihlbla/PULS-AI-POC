000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5011600.                                                
000300 AUTHOR.         GUN LÖFGREN.                                             
000400 DATE-WRITTEN.   96/07/01.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        FRÅGA/ÄNDRA STANDARPRIS - INKÖPSPRIS                             
000900*                                                                         
001000*    INDATA.                                                              
001100*        TRANSAKTION: W5T116                                              
001200*        MID:         W5I11601                                            
001300*                                                                         
001400*    UTDATA.                                                              
001500*        MOD:         W5O11601                                            
001600                                                                          
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP3                                                                
002000 DATA DIVISION.                                                           
002100 WORKING-STORAGE SECTION.                                                 
002200*    -COPY WY2000W1                                                       
002300     SKIP3                                                                
002400 77  IDPGM                       PIC X(08)   VALUE 'W5011600'.            
002500                                                                          
002600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
002700 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
002800                                                                          
002900 77  JA                          PIC X       VALUE 'J'.                   
003000 77  NEJ                         PIC X       VALUE 'N'.                   
003100 77  FLFEL-FAELT                 PIC X       VALUE 'N'.                   
003200                                                                          
003300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003400                                                                          
003500                                                                          
003600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
003700     88  NYCKLAR-OK                          VALUE 'J'.                   
003800     88  NYCKLAR-FEL                         VALUE 'N'.                   
003900                                                                          
004000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004100     88  EGEN-MID                            VALUE '5116'.                
004200     88  GODK-MID                            VALUE '5111' '5112'          
004300                                                   '5113' '5114'          
004400                                                   '5115' '5116'          
004500                                                   '5117' '5118'          
004600                                                   '5119'.                
004700     88  HELP-MID                            VALUE '0551'.                
004800     EJECT                                                                
004900 01  DIVERSE.                                                             
005000     03  DAGENS-AAAAMMDD         PIC 9(8)    VALUE ZERO.                  
005010     03  WS-PRARTBES             PIC X       VALUE 'N'.                   
005020     03  DAGENS-DATUM            PIC 9(6)    VALUE ZERO.                  
005100     03  WS-KVDISP               PIC S9(6)   VALUE ZERO.                  
005200     03  W-KDPRODSL              PIC S9(3)   VALUE ZERO COMP-3.           
005300     03  WS-PROCENT              PIC S9(4)V9 VALUE ZERO.                  
005400     03  WS-IDARTNR              PIC X(9)    VALUE SPACE.                 
005500     03  WS-PRI-IDARTNR          PIC X(9)    VALUE SPACE.                 
005600     03  LAES-IND                PIC 9(4)    VALUE ZERO.                  
005700     03  WS-LAESIND              PIC 9(4)    VALUE ZERO.                  
005800     03  WS-SEKTION              PIC X(40)   VALUE SPACE.                 
005900     03  WS-KDSORT               PIC X(2)    VALUE SPACE.                 
006000     03  W-KDVALISO              PIC X(3)    VALUE SPACE.                 
006100     03  W-RETULF                PIC S9(3)V9(4)                           
006200                                             VALUE +0.                    
006300     03  W-PRKURS                PIC S9(5)V9(2)                           
006400                                             VALUE +0  COMP-3.            
006500     03  W-NYTT-PRARTSTD     PIC S9(7)V99   VALUE ZERO COMP-3.            
006600     03  W-DIFF-PRARTSTD     PIC S9(7)V99   VALUE ZERO COMP-3.            
006700     03  W-GAM-PRARTSTD      PIC S9(7)V99   VALUE ZERO COMP-3.            
006800     03  W-DIFF-LAGERVARDE   PIC S9(7)V99   VALUE ZERO COMP-3.            
006900 01  WS-ARBAREA.                                                          
007000     03  WS-HHMMSSTH             PIC 9(8)    VALUE ZERO.                  
007100                                                                          
007200 01  DAGENS-TIAAMMDD             PIC 9(6)    VALUE ZERO.                  
007300                                                                          
007400 01  DAGENS-TIAAAAMMDD           PIC 9(8).                                
007500 01  DAGENS-DATUM2               PIC 9(8)    VALUE ZERO.                  
007600                                                                          
007700 01  SPAR-IDDC                   PIC 9(2).                                
007800 01  W-IDSEKVNR                  PIC S9(3)   VALUE ZERO COMP-3.           
007900 01  W-KDKOSTTYP                 PIC X       VALUE ' '.                   
007910 01  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
008000                                                                          
008100                                                                          
008200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008300 01  GENERELLA-SUBPROGRAM.                                                
008400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008800     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
008810     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
008900     EJECT                                                                
009000                                                                          
009100**   --- VALID IDDC CODES                                                 
009200 01  -COPY WWDC99                                                         
009300 01  -COPY WWDCKONS                                                       
009301     SKIP3                                                                
009310 01  -COPY W510CURR                                                       
009400     EJECT                                                                
009500*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
009600*01 -COPY WMEDAREA                                                        
009700     SKIP3                                                                
009800 01  MESSAGE-CODES.                                                       
009900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
010000     03  ERR-INFO-MISSING        PIC X(3)    VALUE '005'.                 
010100     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
010200     EJECT                                                                
010300*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010400*                                                                         
010500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010600     SKIP3                                                                
010700*01 -COPY WMSGINIT                                                        
010800     EJECT                                                                
010900*    --- PARAMETRAR TILL SUBPROGRAM W009CIA                               
011000*01 -COPY W009CIA                                                         
011100                                                                          
011200     EJECT                                                                
011300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
011400*                                                                         
011500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011600     SKIP3                                                                
011700*01  MID -COPY W5I11601                                                   
011800     EJECT                                                                
011900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012000     SKIP3                                                                
012100*01  -COPY WMSGAREA                                                       
012200     EJECT                                                                
012300     03  MOD REDEFINES MSG-AREA.                                          
012400*      05  -COPY W5O11601                                                 
012500     EJECT                                                                
012600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012700     SKIP3                                                                
012800*01  -COPY WMFSAREA                                                       
012900     EJECT                                                                
013000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013100*                                                                         
013200*                                                                         
013300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013400     SKIP3                                                                
013500 01  NYCKLAR-TILL-DLI.                                                    
013600     03  W-IDARTNR-X.                                                     
013700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013710     03  W-DAPRLIST-X.                                                    
013720         05  W-DAPRLIST          PIC 9(8)  VALUE ZERO.                    
013800     03  W-KDSEGKEY-X.                                                    
013900         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
014000     03  W-IDARTNR-WDD3-X.                                                
014100         05  W-IDARTNR-WDD3      PIC S9(9)   VALUE ZERO COMP-3.           
014200     03  W-IDSKYLT-X.                                                     
014300         05  W-IDSKYLT           PIC X(3)    VALUE 'S  '.                 
015100     03  W-WDH801KY-X.                                                    
015200         05  W-IDARTNR-H8        PIC S9(9)   VALUE ZERO COMP-3.           
015300         05  W-DAREGDAT          PIC 9(8)    VALUE ZERO.                  
015400         05  W-TIREGTID          PIC S9(7)   VALUE ZERO COMP-3.           
015500     03  W-WDH801KY-MIN.                                                  
015600         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
015700         05  FILLER              PIC X(12)   VALUE LOW-VALUE.             
015800     03  W-WDH801KY-MAX.                                                  
015900         05  W-IDARTNR-MAX       PIC S9(9)   VALUE ZERO COMP-3.           
016000         05  FILLER              PIC X(12)   VALUE HIGH-VALUE.            
016100     03  W-IDLEVNR-X.                                                     
016200         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
016210     03  W-IDLAND-X.                                                      
016220         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
016300     03  W-IDDC-B6-X.                                                     
016400         05 W-IDDC-B6            PIC X(2)    VALUE SPACE.                 
016500*                                                                         
016600     EJECT                                                                
016700*    --- MEDDELANDEN                                                      
016800 01  MEDDELANDE.                                                          
016900     03  W-FEL-1                 PIC X(40)   VALUE                        
017000       'FYLL I INMATNINGSFÄLT VID UPPDATERING'.                           
017100     03  W-FEL-2                 PIC X(26)   VALUE                        
017200       'UPPLYSTA FÄLT FEL'.                                               
017300     03  W-FEL-3                 PIC X(40)   VALUE                        
017400       'TRYCK PF11 FÖR UPPDATERING'.                                      
017500     03  W-FEL-4                 PIC X(40)   VALUE                        
017600       'DET FINNS INGEN RAD ATT UPPDATERA'.                               
017700     03  W-FEL-5                 PIC X(40)   VALUE                        
017800       'UPPDAT. AV BEHANDLAD RAD EJ TILLÅTEN'.                            
017900     03  W-FEL-6                 PIC X(40)   VALUE                        
018000       'UPPDATERING EJ GJORD'.                                            
018100     03  W-MED-1                 PIC X(26)   VALUE                        
018200       'UPPDATERING UTFÖRD'.                                              
018300*                                                                         
018400     SKIP2                                                                
018500*    --- STATUS-KOD FRÅN IMS                                              
018600 01  STATUS-WS                   PIC XX.                                  
018700     88  SEGMENT-FINNS                       VALUE '  '.                  
018800     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018900     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019000     SKIP2                                                                
019100 01  GODK-STATUSKODER.                                                    
019200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019300     SKIP3                                                                
019400 01  SSA1                        PIC X(75).                               
019500 01  SSA2                        PIC X(75).                               
019600     EJECT                                                                
019700*    --- IMS FUNKTIONSKODER                                               
019800*01  -COPY W0003                                                          
019900     EJECT                                                                
020000 01  FILLER                      PIC X(16)   VALUE 'WLARTC01   '.         
020100     SKIP3                                                                
020200 01  WLARTC01   -COPY WDK601.                                             
020300     EJECT                                                                
020400 01  FILLER                      PIC X(16)   VALUE 'WLARTC11   '.         
020500     SKIP3                                                                
020600 01  WLARTC11   -COPY WDK611.                                             
020700     EJECT                                                                
020800 01  FILLER                      PIC X(16)   VALUE 'WLARTC21   '.         
020900     SKIP3                                                                
020910 01  WLARTC21   -COPY WDK621.                                             
020920     EJECT                                                                
020930 01  FILLER                      PIC X(16)   VALUE 'WLBENA11   '.         
020940     SKIP3                                                                
021000 01  WLBENA11   -COPY WDD311  -PRE BENA-.                                 
021100     EJECT                                                                
021600 01  FILLER                      PIC X(16)   VALUE 'WLARTS01   '.         
021700     SKIP3                                                                
021800 01  WLARTS01   -COPY WDK701.                                             
021900     SKIP3                                                                
022000 01  FILLER                      PIC X(16)   VALUE 'WLARTS11   '.         
022100     SKIP3                                                                
022200 01  WLARTS11   -COPY WDK711.                                             
022300     EJECT                                                                
022400 01  FILLER                      PIC X(16)   VALUE 'WLPRIG01   '.         
022500     SKIP3                                                                
022600 01  WLPRIG01   -COPY WDH801.                                             
022700     EJECT                                                                
022800 01  FILLER                      PIC X(16)   VALUE 'WLLEVA01   '.         
022900     SKIP3                                                                
023000 01  WLLEVA01   -COPY WDF101.                                             
023100     EJECT                                                                
023200 01  FILLER                      PIC X(16)   VALUE 'WLLEVA11   '.         
023300     SKIP3                                                                
023400 01  WLLEVA11   -COPY WDF102  -PRE LEV-.                                  
023500     EJECT                                                                
023600 01  FILLER                      PIC X(16)   VALUE 'WLSAPA01   '.         
023700 01  WLSAPA01   -COPY WDR901.                                             
023800     05  -COPY W510EKHA -RED FIL-WDR901-DATA                              
023900     EJECT                                                                
024000 01  FILLER                      PIC X(16)   VALUE 'WDB601 AREA'.         
024100 01   DLI-IO-AREA-B601.                                                   
024200*     03  -COPY WDB601                                                    
024300                                                                          
024400     EJECT                                                                
024500 LINKAGE SECTION.                                                         
024600                                                                          
024700*01  -COPY W0009   -PRE MSG-                                              
024800*01  -COPY W0008   -PRE USEA-                                             
024900     05  FILLER                  PIC X.                                   
025000     EJECT                                                                
025100*01  -COPY W0008  -PRE ARTC-                                              
025200     05  FILLER                  PIC X.                                   
025300                                                                          
025400*01  -COPY W0008  -PRE BENA-                                              
025500     05  FILLER                  PIC X.                                   
025600     EJECT                                                                
025700*01  -COPY W0008  -PRE 9305-                                              
025800     05  FILLER                  PIC X.                                   
025900                                                                          
026000*01  -COPY W0008  -PRE ARTS-                                              
026100     05  FILLER                  PIC X.                                   
026200     EJECT                                                                
026300*01  -COPY W0008  -PRE PRIG-                                              
026400     05  FILLER                  PIC X.                                   
026500                                                                          
026600*01  -COPY W0008  -PRE PRIG2-                                             
026700     05  FILLER                  PIC X.                                   
026800     EJECT                                                                
026900*01  -COPY W0008  -PRE LEV-                                               
027000     05  FILLER                  PIC X.                                   
027100                                                                          
027200*01  -COPY W0008  -PRE ARTS2-                                             
027300     05  FILLER                  PIC X.                                   
027400*01  -COPY W0008  -PRE SAPA-                                              
027500     05  FILLER                  PIC X.                                   
027600*01  -COPY W0008  -PRE WDB6-                                              
027700     05  FILLER                  PIC X.                                   
027800                                                                          
027900     EJECT                                                                
028000 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ARTC-PCB BENA-PCB             
028100                           9305-PCB ARTS-PCB PRIG-PCB PRIG2-PCB           
028200                           LEV-PCB ARTS2-PCB SAPA-PCB WDB6-PCB.           
028300 MAIN SECTION.                                                            
028400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ARTC-PCB BENA-PCB             
028500                           9305-PCB ARTS-PCB PRIG-PCB PRIG2-PCB           
028600                           LEV-PCB ARTS2-PCB SAPA-PCB WDB6-PCB.           
028700     PERFORM IMS-GET-MSG                                                  
028800     IF SEGMENT-FINNS                                                     
028900       PERFORM A-INIT                                                     
029000       PERFORM B-KOLLA-NYCKLAR                                            
029100       IF NYCKLAR-OK                                                      
029200         IF MFS-UPDATE                                                    
029300           PERFORM C-KONTROLLERA-INDATA                                   
029400           IF FLFEL-FAELT = NEJ                                           
029500              PERFORM G-UPPDATERA-DATA                                    
029600              PERFORM F-LAES-VISA-INFO                                    
029700           END-IF                                                         
029800         ELSE                                                             
029900           IF MID-KDPRIBEH-IN NOT = '+'                                   
030000             MOVE W-FEL-3  TO MOD-TEMFSFEL                                
030100             MOVE JA       TO FLFEL-FAELT                                 
030200             PERFORM MFS-ROER-EJ-FAELT-UT                                 
030300             PERFORM MFS-ROER-EJ-FAELT-IN                                 
030400             PERFORM MFS-LAES-IN-IGEN                                     
030500           ELSE                                                           
030600             PERFORM F-LAES-VISA-INFO                                     
030700           END-IF                                                         
030800         END-IF                                                           
030900       END-IF                                                             
031000       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O11601 + 4                      
031100       PERFORM IMS-INSERT-MSG                                             
031200     END-IF                                                               
031300                                                                          
031400     MOVE ZERO TO RETURN-CODE                                             
031500     GOBACK                                                               
031600     .                                                                    
031700     EJECT                                                                
031800 A-INIT SECTION.                                                          
031900                                                                          
032000     MOVE 'A-INIT'         TO WS-SEKTION                                  
032100                                                                          
032200     ACCEPT DAGENS-DATUM  FROM DATE                                       
032300                                                                          
032400     IF MSG-DUBBLA-TRANSKODER                                             
032500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I11601                 
032600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
032700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
032800     ELSE                                                                 
032900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I11601                  
033000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
033100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
033200     END-IF                                                               
033300                                                                          
033400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
033500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
033600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
033700                                                                          
033800     MOVE LOW-VALUE TO MSG-AREA                                           
033900     MOVE 'W5O11601' TO MFS-IDMOD                                         
034000     MOVE '5116' TO MOD-IDTRANS                                           
034100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
034200                                                                          
034300     IF EGEN-MID OR HELP-MID                                              
034400       CONTINUE                                                           
034500     ELSE                                                                 
034600       MOVE SPACE TO MFS-KDTRTYP                                          
034700       MOVE '7' TO MFS-IDPFK                                              
034800     END-IF                                                               
034900     .                                                                    
035000     EJECT                                                                
035100 B-KOLLA-NYCKLAR SECTION.                                                 
035200                                                                          
035300     MOVE 'B-KOLLA-NYCKLAR' TO WS-SEKTION                                 
035400                                                                          
035500     MOVE ALL '+'           TO MSGI-WMSGINIT                              
035600     MOVE '001'             TO MSGI-KDCALL                                
035700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
035800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
035900     MOVE '5116'            TO MSGI-IDTRANS                               
036000     IF GODK-MID                                                          
036100         MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                          
036200     END-IF                                                               
036300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
036400                                                                          
036500     MOVE MSGI-IDARTNR      TO WS-IDARTNR                                 
036600     INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO                   
036700     MOVE JA TO NYCKLAR-SW                                                
036800     MOVE MSGI-IDRADNR      TO WS-LAESIND                                 
036900                                                                          
037000*    -- KONTROLL AV IDARTNR                                               
037100     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
037200                                                                          
037300     IF MID-IDARTNR-IN NOT = ALL '+'                                      
037400       MOVE '7'         TO MFS-IDPFK                                      
037500       MOVE SPACE       TO MFS-KDTRTYP                                    
037600     END-IF                                                               
037700                                                                          
037800     IF WS-IDARTNR NUMERIC                                                
037900       MOVE WS-IDARTNR TO W-IDARTNR                                       
038000                          W-IDARTNR-WDD3                                  
038100     ELSE                                                                 
038200       MOVE NEJ TO NYCKLAR-SW                                             
038300     END-IF                                                               
038400                                                                          
038500     IF GODK-MID OR NYCKLAR-OK                                            
038600       MOVE WS-IDARTNR        TO MOD-IDARTNR-UT                           
038700       INSPECT MOD-IDARTNR-UT REPLACING                                   
038800                              LEADING ZERO BY SPACE                       
038900     ELSE                                                                 
039000       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
039100     END-IF                                                               
039200                                                                          
039300     IF NYCKLAR-FEL                                                       
039400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
039500       CALL WMEDKONV USING MED-WMEDAREA                                   
039600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
039700       PERFORM MFS-RENSA-FAELT-IN                                         
039800     END-IF                                                               
039900     .                                                                    
040000     EJECT                                                                
040100                                                                          
040200 C-KONTROLLERA-INDATA  SECTION.                                           
040300                                                                          
040400     MOVE 'C-KONTROLLERA-INDATA' TO WS-SEKTION                            
040500                                                                          
040600     IF MID-KDPRIBEH-IN = '+'                                             
040700        MOVE W-FEL-1       TO MOD-TEMFSFEL                                
040800        MOVE JA            TO FLFEL-FAELT                                 
040900     ELSE                                                                 
041000        IF MID-KDPRIBEH-IN = 'N' OR 'B' OR 'J' OR 'V'                     
041100           MOVE MFS-ALFA-FAELT-RAETT                                      
041200                           TO MOD-KDPRIBEH-IN-ATTR                        
041300        ELSE                                                              
041400           MOVE MFS-ALFA-FAELT-FEL                                        
041500                           TO MOD-KDPRIBEH-IN-ATTR                        
041600           MOVE W-FEL-2    TO MOD-TEMFSFEL                                
041700           MOVE JA         TO FLFEL-FAELT                                 
041800        END-IF                                                            
041900     END-IF                                                               
042000     .                                                                    
042100     EJECT                                                                
042200                                                                          
042300 F-LAES-VISA-INFO SECTION.                                                
042400                                                                          
042500     MOVE 'F-LAES-VISA-INFO'  TO WS-SEKTION                               
042600                                                                          
042700     PERFORM IMS-GU-WLARTC01                                              
042800                                                                          
042900     IF SEGMENT-SAKNAS                                                    
043000        MOVE ERR-INFO-MISSING TO MED-IDMFSFEL                             
043100        CALL WMEDKONV USING MED-WMEDAREA                                  
043200        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
043300        PERFORM MFS-RENSA-FAELT-UT                                        
043400     ELSE                                                                 
043500       MOVE ART-IDLEVNR    TO MOD-IDLEVNR-HUV                             
043600                              W-IDLEVNR                                   
043700       MOVE ART-KDPRODSL   TO MOD-KDPRODSL                                
043800       MOVE ART-FLIART     TO MOD-FLIART                                  
043900       MOVE ART-KDSORT     TO WS-KDSORT                                   
044000       PERFORM IMS-GU-WDD3                                                
044100                                                                          
044200       IF SEGMENT-FINNS                                                   
044300         MOVE BENA-TEXT-BEART      TO MOD-BEART-SVE                       
044400       ELSE                                                               
044500         MOVE SPACE                TO MOD-BEART-SVE                       
044600       END-IF                                                             
044700                                                                          
044800       PERFORM IMS-GNP-WLARTC11                                           
044900       IF SEGMENT-SAKNAS                                                  
045000          MOVE ERR-PART-MISSING TO MED-IDMFSFEL                           
045100          CALL WMEDKONV USING MED-WMEDAREA                                
045200          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
045300          PERFORM MFS-RENSA-FAELT-UT                                      
045400       ELSE                                                               
045500         ADD  CLAG-KVAKS-CDC     TO WS-KVDISP                             
045600         ADD  CLAG-KVAKS-PAV     TO WS-KVDISP                             
045700         ADD  CLAG-KVAKS-T       TO WS-KVDISP                             
045800         ADD  CLAG-KVEFRS        TO WS-KVDISP                             
045900         ADD  CLAG-KVLS          TO WS-KVDISP                             
046000         MOVE CLAG-IDINK         TO MOD-IDINK                             
046010                                                                          
046020         MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD               
046030         COMPUTE W-DAPRLIST = 99999999 - DAGENS-AAAAMMDD                  
046040         PERFORM IMS-GNP-WDK621                                           
046050         IF SEGMENT-SAKNAS                                                
046060           MOVE CLAG-PRARTSTD       TO MOD-PRARTBES                       
046070         ELSE                                                             
046080           MOVE NEJ                 TO WS-PRARTBES                        
046090           PERFORM UNTIL  SEGMENT-SAKNAS                                  
046091             IF PRL-SUINLEV-PR > ZERO                                     
046092               MOVE PRL-PRARTBES-PR  TO MOD-PRARTBES                      
046093               SET SEGMENT-SAKNAS TO TRUE                                 
046094             ELSE                                                         
046095               IF WS-PRARTBES = NEJ                                       
046096                 MOVE PRL-PRARTBES-PR TO MOD-PRARTBES                     
046097                 MOVE JA              TO WS-PRARTBES                      
046098               END-IF                                                     
046099               PERFORM IMS-GNP-WDK621                                     
046100             END-IF                                                       
046101           END-PERFORM                                                    
046102         END-IF                                                           
046103                                                                          
046200         MOVE CLAG-PRARTSJK      TO MOD-PRARTSJK                          
046300         MOVE CLAG-PRARTSTD      TO MOD-PRARTSTD-AKT                      
046400         MOVE CLAG-PRDIRLON      TO MOD-PRDIRLON-AKT                      
046500         MOVE CLAG-PRDMTRL       TO MOD-PRDMTRL-AKT                       
046600         MOVE CLAG-PRINK         TO MOD-PRINK-AKT                         
046700         MOVE CLAG-PROVRPAL      TO MOD-PROVRPAL-AKT                      
046800         MOVE CLAG-REDIRLEV      TO MOD-REDIRLEV                          
046900         MOVE CLAG-RETULF        TO MOD-RETULF                            
047000                                    W-RETULF                              
047100       END-IF                                                             
047200       PERFORM IMS-GU-WLARTS01                                            
047300       IF STATUS-WS = '  '                                                
047400          PERFORM IMS-GNP-WLARTS11                                        
047500          PERFORM UNTIL SEGMENT-SAKNAS                                    
047600           MOVE SLAG-IDDC       TO WS-IDDC                                
047700                                   W-IDDC-B6                              
047800           PERFORM IMS-GU-WDB601                                          
047900                                                                          
048000           IF DCS-CDC OR DCS-CDC-TR OR DCS-SDC OR DCS-NDC-PF              
048011             IF DCS-KDTRADP NOT = 'SEPV'                                  
048020               CONTINUE                                                   
048030             ELSE                                                         
048100               ADD SLAG-KVEFRS  TO WS-KVDISP                              
048200               ADD SLAG-KVLS    TO WS-KVDISP                              
048300               ADD SLAG-KVAKS-SDC TO WS-KVDISP                            
048400               ADD SLAG-KVAKS-PAV TO WS-KVDISP                            
048500             END-IF                                                       
048510           END-IF                                                         
048600           PERFORM IMS-GNP-WLARTS11                                       
048700          END-PERFORM                                                     
048800       END-IF                                                             
048900                                                                          
049000       MOVE WS-KVDISP            TO MOD-KVDISP-SPIS                       
049100       IF W-RETULF = ZERO                                                 
049200         PERFORM FA-LAES-LEV                                              
049300       END-IF                                                             
049400                                                                          
049500       MOVE W-IDARTNR              TO W-IDARTNR-H8                        
049600                                      W-IDARTNR-MIN                       
049700                                      W-IDARTNR-MAX                       
049800       IF MID-DAREGDAT NUMERIC                                            
049900         MOVE MID-DAREGDAT         TO W-DAREGDAT                          
050000         MOVE MID-TIREGTID         TO W-TIREGTID                          
050100         PERFORM IMS-GU-WDH801                                            
050200       ELSE                                                               
050300         PERFORM IMS-GN-WDH801                                            
050400         PERFORM UNTIL (PRI-FLKLAR NOT = JA                               
050500                       AND (PRI-KDPRIBEH NOT = 'B' AND 'J')               
050600                       OR  SEGMENT-SAKNAS)                                
050700           PERFORM IMS-GN-WDH801                                          
050800         END-PERFORM                                                      
050900       END-IF                                                             
051000       IF SEGMENT-FINNS                                                   
051100         MOVE PRI-DAREGDAT       TO MOD-DAREGDAT                          
051200         MOVE PRI-TIREGTID       TO MOD-TIREGTID                          
051300         MOVE PRI-N-PRINK        TO MOD-PRINK-KOM                         
051400         MOVE PRI-N-PRARTSTD     TO MOD-PRARTSTD-KOM                      
051500         MOVE PRI-N-PRDIRLON     TO MOD-PRDIRLON-KOM                      
051600         MOVE PRI-N-PRDMTRL      TO MOD-PRDMTRL-KOM                       
051700         MOVE PRI-N-PROVRPAL     TO MOD-PROVRPAL-KOM                      
051800         MOVE PRI-KDPRIBEH       TO MOD-KDPRIBEH                          
051900         MOVE PRI-FLPRFIL        TO MOD-FLPRFIL                           
052000         MOVE PRI-N-TIPRLIST     TO MOD-TIPRLIST-UTM                      
052100         IF PRI-N-IDLEVNR-PR NOT = SPACE                                  
052200           MOVE PRI-N-IDLEVNR-PR TO MOD-IDLEVNR-UTM                       
052300         ELSE                                                             
052400           MOVE PRI-O-IDLEVNR-PR TO MOD-IDLEVNR-UTM                       
052500         END-IF                                                           
052600         IF PRI-N-KDSTATUS-PR NOT = ZERO                                  
052700           MOVE PRI-N-KDSTATUS-PR TO MOD-KDSTASPIS                        
052800         ELSE                                                             
052900           MOVE PRI-O-KDSTATUS-PR TO MOD-KDSTASPIS                        
053000         END-IF                                                           
053100         IF PRI-N-KDVALISO NOT = SPACE                                    
053200           MOVE PRI-N-KDVALISO    TO W-KDVALISO                           
053300                                     MOD-KDVALISO-UTM                     
053400         ELSE                                                             
053500           MOVE PRI-O-KDVALISO    TO W-KDVALISO                           
053600                                     MOD-KDVALISO-UTM                     
053700         END-IF                                                           
053800         IF PRI-N-PRARTBEL-PR NOT = ZERO                                  
053900           MOVE PRI-N-PRARTBEL-PR TO MOD-PRARTBEL-PR-UTM                  
054000         ELSE                                                             
054100           MOVE PRI-O-PRARTBEL-PR TO MOD-PRARTBEL-PR-UTM                  
054200         END-IF                                                           
054300         IF PRI-N-PRINK NOT = ZERO                                        
054400           COMPUTE WS-PROCENT = ((PRI-N-PRINK -                           
054500                               PRI-O-PRINK) * 100) /                      
054600                               PRI-O-PRINK                                
054700           MOVE WS-PROCENT      TO MOD-REAENDR-INK                        
054800         ELSE                                                             
054900           MOVE ZERO            TO MOD-REAENDR-INK                        
055000         END-IF                                                           
055100         IF PRI-N-PRARTSTD NOT = ZERO                                     
055200           COMPUTE WS-PROCENT = ((PRI-N-PRARTSTD  -                       
055300                               PRI-O-PRARTSTD) * 100) /                   
055400                               PRI-O-PRARTSTD                             
055500           MOVE WS-PROCENT      TO MOD-REAENDR-STD                        
055600         ELSE                                                             
055700           MOVE ZERO            TO MOD-REAENDR-STD                        
055800         END-IF                                                           
055900         IF PRI-N-PRDIRLON NOT = ZERO                                     
056000           COMPUTE WS-PROCENT = ((PRI-N-PRDIRLON  -                       
056100                               PRI-O-PRDIRLON) * 100) /                   
056200                               PRI-O-PRDIRLON                             
056300           MOVE WS-PROCENT      TO MOD-REAENDR-DL                         
056400         ELSE                                                             
056500           MOVE ZERO            TO MOD-REAENDR-DL                         
056600         END-IF                                                           
056700         IF PRI-N-PRDMTRL NOT = ZERO                                      
056800           COMPUTE WS-PROCENT = ((PRI-N-PRDMTRL  -                        
056900                                PRI-O-PRDMTRL) * 100) /                   
057000                                PRI-O-PRDMTRL                             
057100           MOVE WS-PROCENT      TO MOD-REAENDR-MTRL                       
057200         ELSE                                                             
057300           MOVE ZERO            TO MOD-REAENDR-MTRL                       
057400         END-IF                                                           
057500         IF PRI-N-PROVRPAL NOT = ZERO                                     
057600           COMPUTE WS-PROCENT = ((PRI-N-PROVRPAL  -                       
057700                                PRI-O-PROVRPAL) * 100) /                  
057800                                PRI-O-PROVRPAL                            
057900           MOVE WS-PROCENT      TO MOD-REAENDR-OVR                        
058000         ELSE                                                             
058100           MOVE ZERO            TO MOD-REAENDR-OVR                        
058200         END-IF                                                           
058300       END-IF                                                             
058400                                                                          
058500       MOVE DAGENS-DATUM(1:2)   TO W-DATE-AAMM(1:2)                       
058510       MOVE 01                  TO W-DATE-AAMM(3:2)                       
058600       MOVE W-KDVALISO          TO CURR-KDVALISO-ROW                      
058610       MOVE 'SEK'               TO CURR-KDVALISO-HUV                      
058620       MOVE W-DATE-AAMM         TO CURR-TIAAMM                            
058630       MOVE 'A'                 TO CURR-KDVALTYP                          
058640       CALL W510CURR USING CURR-W510CURR 9305-PCB                         
058650       IF CURR-KDSVAR = ' '                                               
058900          COMPUTE W-PRKURS = CURR-PRKURS-NEW                              
059000                             / CURR-REVALUTA-TO                           
059100          MOVE W-PRKURS         TO MOD-PRKURS                             
059200       ELSE                                                               
059300          MOVE ZERO             TO MOD-PRKURS                             
059400       END-IF                                                             
059500       MOVE MFS-FORMATETS-ATTR  TO MOD-KDPRIBEH-IN-ATTR                   
059600     END-IF                                                               
059700     .                                                                    
059800     EJECT                                                                
059900 FA-LAES-LEV  SECTION.                                                    
060000                                                                          
060100     MOVE 'FA-LAES-LEV'      TO WS-SEKTION                                
060200                                                                          
060300     PERFORM IMS-GU-WLLEVA01                                              
060400                                                                          
060500     IF SEGMENT-FINNS                                                     
060510       MOVE 'SE' TO W-IDLAND                                              
060600       PERFORM IMS-GNP-WLLEVA11                                           
060700       IF SEGMENT-FINNS                                                   
060800         MOVE LEV-TULL-TITULF TO TMP1-YYMMDD                              
060900         MOVE DAGENS-DATUM    TO TMP2-YYMMDD                              
061000         PERFORM WY2000P1                                                 
061100         IF TMP1-YYMMDD < TMP2-YYMMDD                                     
061200           MOVE LEV-TULL-RETULF-1 TO MOD-RETULF                           
061300                                W-RETULF                                  
061400         ELSE                                                             
061500           MOVE LEV-TULL-RETULF-2 TO MOD-RETULF                           
061600                                W-RETULF                                  
061700         END-IF                                                           
061800       END-IF                                                             
061900     END-IF                                                               
062000                                                                          
062100     .                                                                    
062200     EJECT                                                                
062300 G-UPPDATERA-DATA  SECTION.                                               
062400                                                                          
062500     MOVE 'G-UPPDATERA-DATA' TO WS-SEKTION                                
062600                                                                          
062700     MOVE W-IDARTNR              TO W-IDARTNR-H8                          
062800     MOVE MID-DAREGDAT           TO W-DAREGDAT                            
062900     MOVE MID-TIREGTID           TO W-TIREGTID                            
063000     PERFORM IMS-GHU-WDH801                                               
063100     IF SEGMENT-FINNS                                                     
063200       IF PRI-KDPRIBEH NOT = JA                                           
063300         IF MID-KDPRIBEH-IN NOT = JA                                      
063400           MOVE MID-KDPRIBEH-IN TO PRI-KDPRIBEH                           
063500           PERFORM IMS-REPL-WDH801                                        
063600           MOVE W-MED-1        TO MOD-TEMFSINF                            
063700         ELSE                                                             
063800           IF PRI-FLKLAR = JA                                             
063900             MOVE W-FEL-6      TO MOD-TEMFSFEL                            
064000           ELSE                                                           
064100             PERFORM GA-UPPDAT-PRIS-STD                                   
064200             MOVE JA           TO PRI-FLKLAR                              
064300             MOVE JA           TO PRI-KDPRIBEH                            
064400             PERFORM IMS-REPL-WDH801                                      
064500             MOVE W-MED-1      TO MOD-TEMFSINF                            
064600           END-IF                                                         
064700         END-IF                                                           
064800       ELSE                                                               
064900         MOVE W-FEL-5          TO MOD-TEMFSFEL                            
065000         MOVE JA               TO FLFEL-FAELT                             
065100       END-IF                                                             
065200     ELSE                                                                 
065300       MOVE W-FEL-4          TO MOD-TEMFSFEL                              
065400       MOVE JA               TO FLFEL-FAELT                               
065500     END-IF                                                               
065600                                                                          
065700     .                                                                    
065800     EJECT                                                                
065900 GA-UPPDAT-PRIS-STD SECTION.                                              
066000                                                                          
066100     MOVE PRI-IDARTNR             TO W-IDARTNR                            
066200     MOVE ZERO                    TO W-GAM-PRARTSTD                       
066300                                     W-DIFF-PRARTSTD                      
066400                                     W-DIFF-LAGERVARDE                    
066500     PERFORM IMS-GU-WLARTC01                                              
066600     IF SEGMENT-FINNS                                                     
066700        MOVE ART-KDPRODSL         TO W-KDPRODSL                           
066800        MOVE ART-KDSORT           TO WS-KDSORT                            
066900        PERFORM IMS-GNP-WLARTC11                                          
067000        IF SEGMENT-FINNS                                                  
067100           MOVE CLAG-PRARTSTD     TO W-GAM-PRARTSTD                       
067200                                                                          
067300           COMPUTE CLAG-PRARTSTD ROUNDED =                                
067400             PRI-N-PRINK + CLAG-PRDIRLON + CLAG-PRDMTRL                   
067500                                         + CLAG-PROVRPAL                  
067600           MOVE CLAG-PRARTSTD     TO W-NYTT-PRARTSTD                      
067700*** UPPDATERA INKÖPSPRIS                                                  
067800           MOVE PRI-N-PRINK       TO CLAG-PRINK                           
067900           PERFORM IMS-REPL-ARTC                                          
068000           PERFORM GB-SKAPA-BOKFORING-TRANS                               
068100        ELSE                                                              
068200           MOVE ERR-PART-MISSING  TO MED-IDMFSFEL                         
068300           CALL WMEDKONV USING MED-WMEDAREA                               
068400           MOVE MED-MFSFEL        TO MOD-TEMFSFEL                         
068500           PERFORM MFS-RENSA-FAELT-UT                                     
068600        END-IF                                                            
068700     ELSE                                                                 
068800       MOVE ERR-INFO-MISSING      TO MED-IDMFSFEL                         
068900       CALL WMEDKONV USING MED-WMEDAREA                                   
069000       MOVE MED-MFSFEL            TO MOD-TEMFSFEL                         
069100       PERFORM MFS-RENSA-FAELT-UT                                         
069200     END-IF                                                               
069300     .                                                                    
069400     EJECT                                                                
069500 GB-SKAPA-BOKFORING-TRANS SECTION.                                        
069600                                                                          
069700     COMPUTE W-DIFF-PRARTSTD =                                            
069800             W-NYTT-PRARTSTD - W-GAM-PRARTSTD                             
069900                                                                          
070000*****  LAGERVÄRDESFÖRÄNDRING CDC (IDDC = 11)                              
070100       MOVE WC-CDC-SE           TO SPAR-IDDC                              
070200       COMPUTE W-DIFF-LAGERVARDE = W-DIFF-PRARTSTD *                      
070300         (CLAG-KVLS + CLAG-KVEFRS +                                       
070400          CLAG-KVAKS-CDC + CLAG-KVAKS-PAV)                                
070500       COMPUTE EKH-KVANTAL = CLAG-KVLS + CLAG-KVEFRS +                    
070600                        CLAG-KVAKS-CDC + CLAG-KVAKS-PAV                   
070700       IF W-DIFF-LAGERVARDE NOT = 0                                       
070800         PERFORM GD-UPPDATERA-WDR9                                        
070900       END-IF                                                             
071000                                                                          
071100*****  LAGERVÄRDESFÖRÄNDRING TERMINAL (IDDC = 12)                         
071200       MOVE WC-CDC-TR           TO SPAR-IDDC                              
071300       COMPUTE W-DIFF-LAGERVARDE = W-DIFF-PRARTSTD *                      
071400                              CLAG-KVAKS-T                                
071500       COMPUTE EKH-KVANTAL = CLAG-KVAKS-T                                 
071600       IF W-DIFF-LAGERVARDE NOT = 0                                       
071700         PERFORM GD-UPPDATERA-WDR9                                        
071800       END-IF                                                             
071900                                                                          
072000*****  LAGERVÄRDESFÖRÄNDRING SDC (IDDC = 21 - 26/61 - 62)                 
072100       PERFORM IMS-GU-WLARTS01                                            
072200       IF SEGMENT-FINNS                                                   
072300         PERFORM IMS-GNP-WLARTS11                                         
072400         PERFORM UNTIL NOT SEGMENT-FINNS                                  
072500                                                                          
072600           IF SEGMENT-FINNS                                               
072700             MOVE SLAG-IDDC             TO SPAR-IDDC                      
072800             COMPUTE W-DIFF-LAGERVARDE = W-DIFF-PRARTSTD *                
072900               (SLAG-KVLS + SLAG-KVEFRS +                                 
073000                SLAG-KVAKS-SDC + SLAG-KVAKS-PAV)                          
073100                COMPUTE EKH-KVANTAL = SLAG-KVLS +                         
073200                                       SLAG-KVEFRS +                      
073300                                       SLAG-KVAKS-SDC +                   
073400                                       SLAG-KVAKS-PAV                     
073500               IF W-DIFF-LAGERVARDE NOT = 0                               
073600                 MOVE SPAR-IDDC       TO WS-IDDC                          
073700                                         W-IDDC-B6                        
073800                 PERFORM IMS-GU-WDB601                                    
073900                                                                          
074000                IF DCS-CDC OR DCS-CDC-TR OR DCS-SDC OR DCS-NDC-PF         
074010                  IF DCS-KDTRADP NOT = 'SEPV'                             
074020                    CONTINUE                                              
074030                  ELSE                                                    
074100                    PERFORM GD-UPPDATERA-WDR9                             
074110                  END-IF                                                  
074200                END-IF                                                    
074300               END-IF                                                     
074400           END-IF                                                         
074500           PERFORM IMS-GNP-WLARTS11                                       
074600         END-PERFORM                                                      
074700       END-IF                                                             
074800     .                                                                    
074900     EJECT                                                                
075000                                                                          
075100 GD-UPPDATERA-WDR9   SECTION.                                             
075200                                                                          
075300     MOVE 'W5011600'       TO FIL-IDPGM IN FIL-WDR901                     
075400     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM2                    
075500     MOVE DAGENS-DATUM2    TO FIL-DAREGDAT                                
075600     ACCEPT FIL-TIKLOCK IN FIL-WDR901 FROM TIME                           
075700     MOVE 1                TO FIL-IDSEKVNR IN FIL-WDR901                  
075800     MOVE 'W510EKHA'       TO FIL-IDCPYTXT IN FIL-WDR901                  
075900     MOVE MSG-SIGNON-USERID TO FIL-IDUSER                                 
076000     MOVE PRI-IDARTNR      TO EKH-IDARTNR                                 
076100     MOVE '401'            TO EKH-KDEKHHT                                 
076200     MOVE '401'            TO EKH-KDEKSHT                                 
076300     MOVE 'DET'            TO EKH-KDEKNIVA                                
076400     MOVE SPAR-IDDC        TO EKH-IDDC-SEND                               
076500     MOVE SPACE            TO EKH-IDDC-REC                                
076600     MOVE +0               TO EKH-IDDISTR                                 
076700     MOVE +0               TO EKH-IDKUNDNR                                
076800                                                                          
076900     MOVE 'VO'             TO CIA-IDARTPRE-IN                             
077000     MOVE PRI-IDARTNR      TO CIA-IDARTBET-IN                             
077100     CALL W009CIA USING       CIA-W009CIA                                 
077200     MOVE CIA-IDARTBET-UT TO EKH-IDVERGL                                  
077300                                                                          
077400     MOVE DAGENS-DATUM2    TO EKH-DAVERDAT                                
077500     MOVE W-KDPRODSL       TO EKH-KDPRODSL                                
077600     MOVE ZERO             TO EKH-KDPSLLOC                                
077700     MOVE SPACE            TO EKH-FLLSBOK                                 
077800     MOVE 'SEK'            TO EKH-KDVALISO                                
077900     MOVE 1.00             TO EKH-PRKURS                                  
078000     MOVE ZERO             TO EKH-PRARTNTO                                
078100     MOVE ZERO             TO EKH-PRARTSJK                                
078200     MOVE ZERO             TO EKH-PRHEMTAG                                
078300     MOVE W-DIFF-PRARTSTD  TO EKH-PRARTSTD                                
078400     MOVE ZERO             TO EKH-PRLANDCO                                
078500     MOVE ZERO             TO EKH-PRINK                                   
078600     MOVE ZERO             TO EKH-PRDIRLON                                
078700     MOVE ZERO             TO EKH-PRDMTRL                                 
078800     MOVE ZERO             TO EKH-PROVRPAL                                
078900     MOVE ZERO             TO EKH-SUBEL                                   
079000     MOVE '5116'           TO EKH-IDTRANS                                 
079100     MOVE ZERO                   TO EKH-BEVAT                             
079200                                    EKH-IDANALYS                          
079300                                    EKH-IDKONTO                           
079500                                    EKH-KDANMORS                          
079600                                    EKH-KDFRAKT                           
079700                                    EKH-SUVAT                             
079800     MOVE ZERO                   TO EKH-DAAVIDAT                          
079900                                    EKH-IDAVINR                           
080000                                    EKH-KDAVVTYP                          
080100                                    EKH-KDRT                              
080200                                    EKH-KVANTMOT                          
080300                                    EKH-KVAVIS                            
080400     MOVE WS-KDSORT              TO EKH-KDSORT                            
080500     MOVE SPACE                  TO EKH-KDTRADP                           
080600                                    EKH-IDLEVNR                           
080601                                    EKH-IDKST                             
080602                                    EKH-FLDCET                            
080610                                    EKH-IDKUNDRF                          
080620                                    EKH-IDFAKT-EXP                        
080700                                                                          
080800     PERFORM IMS-ISRT-WDR901                                              
080900     PERFORM UNTIL SEGMENT-FINNS                                          
081000       ADD +1  TO FIL-IDSEKVNR IN FIL-WDR901                              
081100       PERFORM IMS-ISRT-WDR901                                            
081200     END-PERFORM                                                          
081300     .                                                                    
081400     EJECT                                                                
081500 MFS-RENSA-FAELT-UT SECTION.                                              
081600                                                                          
081700*    --- ALLA UTDATA-FÄLT                                                 
081800     MOVE MFS-RENSA-FAELT TO MOD-BEART-SVE                                
081900                             MOD-PRINK-AKT                                
082000                             MOD-PRINK-KOM                                
082100                             MOD-REAENDR-INK                              
082200                             MOD-KVDISP-SPIS                              
082300                             MOD-PRARTSTD-AKT                             
082400                             MOD-PRARTSTD-KOM                             
082500                             MOD-REAENDR-STD                              
082600                             MOD-RETULF                                   
082700                             MOD-PRARTBES                                 
082800                             MOD-PRKURS                                   
082900                             MOD-PRARTSJK                                 
083000                             MOD-FLIART                                   
083100                             MOD-PRDIRLON-AKT                             
083200                             MOD-PRDIRLON-KOM                             
083300                             MOD-REAENDR-DL                               
083400                             MOD-IDLEVNR-HUV                              
083500                             MOD-PRDMTRL-AKT                              
083600                             MOD-PRDMTRL-KOM                              
083700                             MOD-REAENDR-MTRL                             
083800                             MOD-KDPRODSL                                 
083900                             MOD-PROVRPAL-AKT                             
084000                             MOD-PROVRPAL-KOM                             
084100                             MOD-REAENDR-OVR                              
084200                             MOD-TIPRLIST-UTM                             
084300                             MOD-IDLEVNR-UTM                              
084400                             MOD-PRARTBEL-PR-UTM                          
084500                             MOD-KDVALISO-UTM                             
084600                             MOD-KDSTASPIS                                
084700                             MOD-KDPRIBEH                                 
084800                             MOD-REDIRLEV                                 
084900                             MOD-FLPRFIL                                  
085000     .                                                                    
085100     EJECT                                                                
085200 MFS-RENSA-FAELT-IN SECTION.                                              
085300*    --- ALLA INDATA-FÄLT                                                 
085400                                                                          
085500     MOVE MFS-RENSA-FAELT   TO MOD-KDPRIBEH-IN                            
085600     .                                                                    
085700     EJECT                                                                
085800 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
085900                                                                          
086000*    --- ALLA UTDATA-FÄLT                                                 
086100     MOVE MFS-ROER-EJ-FAELT TO MOD-BEART-SVE                              
086200                               MOD-PRINK-AKT                              
086300                               MOD-PRINK-KOM                              
086400                               MOD-REAENDR-INK                            
086500                               MOD-KVDISP-SPIS                            
086600                               MOD-PRARTSTD-AKT                           
086700                               MOD-PRARTSTD-KOM                           
086800                               MOD-REAENDR-STD                            
086900                               MOD-RETULF                                 
087000                               MOD-PRARTBES                               
087100                               MOD-PRKURS                                 
087200                               MOD-PRARTSJK                               
087300                               MOD-FLIART                                 
087400                               MOD-PRDIRLON-AKT                           
087500                               MOD-PRDIRLON-KOM                           
087600                               MOD-REAENDR-DL                             
087700                               MOD-IDLEVNR-HUV                            
087800                               MOD-PRDMTRL-AKT                            
087900                               MOD-PRDMTRL-KOM                            
088000                               MOD-REAENDR-MTRL                           
088100                               MOD-KDPRODSL                               
088200                               MOD-PROVRPAL-AKT                           
088300                               MOD-PROVRPAL-KOM                           
088400                               MOD-REAENDR-OVR                            
088500                               MOD-TIPRLIST-UTM                           
088600                               MOD-IDLEVNR-UTM                            
088700                               MOD-PRARTBEL-PR-UTM                        
088800                               MOD-KDVALISO-UTM                           
088900                               MOD-KDSTASPIS                              
089000                               MOD-KDPRIBEH                               
089100                               MOD-REDIRLEV                               
089200                               MOD-FLPRFIL                                
089300     .                                                                    
089400     EJECT                                                                
089500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
089600                                                                          
089700*    --- ALLA INDATA-FÄLT                                                 
089800     MOVE MFS-ROER-EJ-FAELT TO  MOD-KDPRIBEH-IN                           
089900     .                                                                    
090000     SKIP2                                                                
090100 MFS-LAES-IN-IGEN SECTION.                                                
090200                                                                          
090300*    --- ALLA INDATA-FÄLT                                                 
090400     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDPRIBEH-IN-ATTR                   
090500     .                                                                    
090600     EJECT                                                                
090700* --- IMS SEKTIONER ---                                                   
090800     SKIP3                                                                
090900 IMS-GET-MSG SECTION.                                                     
091000                                                                          
091100     MOVE '  QC' TO GODK-STATUSKODER                                      
091200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
091300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
091400     PERFORM IMS-STATUSKONTROLL                                           
091500     .                                                                    
091600     SKIP3                                                                
091700 IMS-INSERT-MSG SECTION.                                                  
091800                                                                          
091900     IF ENGLISH-TEXT                                                      
092000       MOVE '0' TO MFS-KDHUVOMR                                           
092100     END-IF                                                               
092200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
092300     MOVE SPACE TO GODK-STATUSKODER                                       
092400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
092500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
092600     PERFORM IMS-STATUSKONTROLL                                           
092700     .                                                                    
092800     EJECT                                                                
092900 IMS-GN-WDH801  SECTION.                                                  
093000                                                                          
093100     STRING 'WLPRIG01(WDH801KY>=' W-WDH801KY-MIN                          
093200            '&WDH801KY<=' W-WDH801KY-MAX ')'                              
093300          DELIMITED BY SIZE INTO SSA1                                     
093400     MOVE '  GE' TO GODK-STATUSKODER                                      
093500     CALL CBLTDLI USING GN PRIG-PCB WLPRIG01 SSA1                         
093600     MOVE PRIG-STATUS-CODE TO STATUS-WS                                   
093700     PERFORM IMS-STATUSKONTROLL                                           
093800     .                                                                    
093900     EJECT                                                                
094000 IMS-GU-WDH801  SECTION.                                                  
094100                                                                          
094200     STRING 'WLPRIG01(WDH801KY =' W-WDH801KY-X ')'                        
094300          DELIMITED BY SIZE INTO SSA1                                     
094400     MOVE '  GE' TO GODK-STATUSKODER                                      
094500     CALL CBLTDLI USING GU PRIG-PCB WLPRIG01 SSA1                         
094600     MOVE PRIG-STATUS-CODE TO STATUS-WS                                   
094700     PERFORM IMS-STATUSKONTROLL                                           
094800     .                                                                    
094900     EJECT                                                                
095000 IMS-GHU-WDH801  SECTION.                                                 
095100                                                                          
095200     STRING 'WLPRIG01(WDH801KY =' W-WDH801KY-X ')'                        
095300          DELIMITED BY SIZE INTO SSA1                                     
095400     MOVE '  GE' TO GODK-STATUSKODER                                      
095500     CALL CBLTDLI USING GHU PRIG-PCB WLPRIG01 SSA1                        
095600     MOVE PRIG-STATUS-CODE TO STATUS-WS                                   
095700     PERFORM IMS-STATUSKONTROLL                                           
095800     .                                                                    
095900     EJECT                                                                
096000 IMS-REPL-WDH801    SECTION.                                              
096100                                                                          
096200     MOVE '  ' TO GODK-STATUSKODER                                        
096300     CALL CBLTDLI USING REPL PRIG-PCB WLPRIG01                            
096400     MOVE PRIG-STATUS-CODE TO STATUS-WS                                   
096500     PERFORM IMS-STATUSKONTROLL                                           
096600     .                                                                    
096700     EJECT                                                                
096800 IMS-GU-WLARTC01 SECTION.                                                 
096900                                                                          
097000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
097100          DELIMITED BY SIZE INTO SSA1                                     
097200     MOVE '  GE' TO GODK-STATUSKODER                                      
097300     CALL CBLTDLI USING GU ARTC-PCB WLARTC01 SSA1                         
097400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
097500     PERFORM IMS-STATUSKONTROLL                                           
097600     .                                                                    
097700     SKIP3                                                                
097800 IMS-GNP-WLARTC11 SECTION.                                                
097900                                                                          
098000     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
098100          DELIMITED BY SIZE INTO SSA1                                     
098200     MOVE '  GE' TO GODK-STATUSKODER                                      
098300     CALL CBLTDLI USING GHNP ARTC-PCB WLARTC11 SSA1                       
098400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
098500     PERFORM IMS-STATUSKONTROLL                                           
098600     .                                                                    
098700     SKIP3                                                                
098710 IMS-GNP-WDK621 SECTION.                                                  
098720                                                                          
098730     STRING 'WLARTC21(DAPRLIST=>' W-DAPRLIST-X ')'                        
098740          DELIMITED BY SIZE INTO SSA1                                     
098750     MOVE '  GE' TO GODK-STATUSKODER                                      
098760     CALL CBLTDLI USING GNP ARTC-PCB WLARTC21 SSA1                        
098770     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
098780     PERFORM IMS-STATUSKONTROLL                                           
098790     .                                                                    
098791     SKIP3                                                                
098800 IMS-REPL-ARTC SECTION.                                                   
098900                                                                          
099000     MOVE '  ' TO GODK-STATUSKODER                                        
099100     CALL CBLTDLI USING REPL ARTC-PCB WLARTC11                            
099200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
099300     PERFORM IMS-STATUSKONTROLL                                           
099400     .                                                                    
099500     EJECT                                                                
099600 IMS-GU-WLARTS01 SECTION.                                                 
099700                                                                          
099800     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
099900          DELIMITED BY SIZE INTO SSA1                                     
100000     MOVE '  GE' TO GODK-STATUSKODER                                      
100100     CALL CBLTDLI USING GU ARTS-PCB WLARTS01 SSA1                         
100200     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
100300     PERFORM IMS-STATUSKONTROLL                                           
100400     .                                                                    
100500     SKIP3                                                                
100600 IMS-GNP-WLARTS11 SECTION.                                                
100700                                                                          
100800     MOVE 'WLARTS11 ' TO SSA1                                             
100900     MOVE '  GE' TO GODK-STATUSKODER                                      
101000     CALL CBLTDLI USING GHNP ARTS-PCB WLARTS11 SSA1                       
101100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
101200     PERFORM IMS-STATUSKONTROLL                                           
101300     .                                                                    
101400     EJECT                                                                
102800 IMS-GU-WLLEVA01 SECTION.                                                 
102900                                                                          
103000     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
103100          DELIMITED BY SIZE INTO SSA1                                     
103200     MOVE '  GE' TO GODK-STATUSKODER                                      
103300     CALL CBLTDLI USING GU LEV-PCB WLLEVA01 SSA1                          
103400     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
103500     PERFORM IMS-STATUSKONTROLL                                           
103600     .                                                                    
103700     EJECT                                                                
103800 IMS-GNP-WLLEVA11  SECTION.                                               
103900                                                                          
104010     STRING 'WLLEVA11(IDLAND   =' W-IDLAND-X ')'                          
104020          DELIMITED BY SIZE INTO SSA1                                     
104100     MOVE '  GE' TO GODK-STATUSKODER                                      
104200     CALL CBLTDLI USING GNP LEV-PCB LEV-WLLEVA11 SSA1                     
104300     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
104400     PERFORM IMS-STATUSKONTROLL                                           
104500     .                                                                    
104600     EJECT                                                                
104700 IMS-GU-WDD3 SECTION.                                                     
104800                                                                          
104900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
105000          DELIMITED BY SIZE INTO SSA1                                     
105100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
105200          DELIMITED BY SIZE INTO SSA2                                     
105300     MOVE '  GE' TO GODK-STATUSKODER                                      
105400     CALL CBLTDLI USING GU BENA-PCB BENA-WLBENA11 SSA1 SSA2               
105500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
105600     PERFORM IMS-STATUSKONTROLL                                           
105700     .                                                                    
105800     EJECT                                                                
105900 IMS-ISRT-WDR901 SECTION.                                                 
106000     SKIP2                                                                
106100     MOVE 'WLSAPA01 ' TO SSA1                                             
106200     MOVE '  II' TO GODK-STATUSKODER                                      
106300     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
106400     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
106500     PERFORM IMS-STATUSKONTROLL                                           
106600     .                                                                    
106700     EJECT                                                                
106800 IMS-GU-WDB601    SECTION.                                                
106900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
107000          DELIMITED BY SIZE INTO SSA1                                     
107100     MOVE '  ' TO GODK-STATUSKODER                                        
107200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
107300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
107400     PERFORM IMS-STATUSKONTROLL                                           
107500     .                                                                    
107600     EJECT                                                                
107700 IMS-STATUSKONTROLL SECTION.                                              
107800                                                                          
107900     SET STATUS-IX TO 1                                                   
108000     SEARCH GODK-STATUS                                                   
108100       AT END                                                             
108200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
108300         DELIMITED BY SIZE INTO FELTEXT                                   
108400         CALL FELLOG                                                      
108500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
108600         CONTINUE                                                         
108700     END-SEARCH                                                           
108800     .                                                                    
108900     EJECT                                                                
109000*    -COPY WY2000P1                                                       
