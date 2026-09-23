000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3031300.                                                
000300 AUTHOR.         THOMAS LARSSON.                                          
000400 DATE-WRITTEN.   93/12/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        VISAR PRIS INFORMATION. VAD ARTIKELN KOSTAR OCH VILKEN           
000900*        RABATT DEN HAR.                                                  
001000**                                                                        
001100*        PROGRAMMET LÄSER WDC1                                            
001200*        PROGRAMMET LÄSER WDD3                                            
001300*        PROGRAMMET LÄSER WDC2                                            
001400*        PROGRAMMET LÄSER WDK6                                            
001500*        PROGRAMMET LÄSER WDB1 OCH WDB2                                   
001600*        PROGRAMMET LÄSER WDR2                                            
001700*                                                                         
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W3T313                                              
002100*        MID:         W3I31301                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W3O31301                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200*    -- CHECKED BY WY2000                                                 
003300 77  IDPGM                       PIC X(08)   VALUE 'W3031300'.            
003400                                                                          
003500*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003700                                                                          
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000 77  YA                          PIC X       VALUE 'Y'.                   
004100                                                                          
004200 77  PRISOMR-SW                  PIC X       VALUE 'J'.                   
004300     88  PRISOMR-FINNS                       VALUE 'J'.                   
004400     88  PRISOMR-SAKNAS                      VALUE 'N'.                   
004500                                                                          
004600 77  MARKNAD-SW                  PIC X       VALUE 'J'.                   
004700     88  MARKNAD-ENDAST                      VALUE 'J'.                   
004800     88  MARKNAD-PRISOMR                     VALUE 'N'.                   
004900                                                                          
005000 77  SPAR-KDARTRAB               PIC 9(2)    VALUE ZERO.                  
005100 77  RAB-INDX                    PIC 9(2)    VALUE ZERO.                  
005200                                                                          
005300 77  WS-RETAILPRIS               PIC S9(9)V9(2) VALUE ZERO COMP-3.        
005400 77  WS-RETAILPRIS-DO            PIC S9(9)V9(2) VALUE ZERO COMP-3.        
005500 77  WS-RETAILPRIS-MO            PIC S9(9)V9(2) VALUE ZERO COMP-3.        
005600 77  WS-RETAILPRIS-NOR-DO        PIC S9(9)V9(2) VALUE ZERO COMP-3.        
005700 77  WS-RETAILPRIS-NOR-MO        PIC S9(9)V9(2) VALUE ZERO COMP-3.        
005800                                                                          
005900 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
006000                                                                          
006100*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006200 77  WS-IDMARKBO                 PIC X(1)    VALUE SPACE.                 
006300 77  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
006400 77  WS-FLMRKVAL                 PIC X      VALUE SPACE.                  
006500                                                                          
006800 77  WS-KDVALISO-MC              PIC X(3)   VALUE 'SEK'.                  
007000                                                                          
007500 01  WS-BEL                     PIC 9(10)V9(5) VALUE ZERO.                
007600 01  WS-BEL2               REDEFINES WS-BEL.                              
007700     03  WS-BELL                PIC 9(15).                                
007800                                                                          
007900 01  WS-IDDISTR                  PIC X(5)    VALUE SPACE.                 
008000 01  FILLER REDEFINES WS-IDDISTR.                                         
008100     03 FILLER                   PIC X(1).                                
008200     03 WS-IDDISTR-IN            PIC X(4).                                
008300                                                                          
008400 01  WS-IDPROMR                  PIC X(3)    VALUE SPACE.                 
008500 01  FILLER REDEFINES WS-IDPROMR.                                         
008600     03  WS-MARKBOLAG            PIC X(1).                                
008700     03  WS-IDPROMRN             PIC X(2).                                
008800                                                                          
008900 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
009000                                                                          
009100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009200     88  NYCKLAR-OK                          VALUE 'J'.                   
009300     88  NYCKLAR-FEL                         VALUE 'N'.                   
009400                                                                          
009500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009600     88  EGEN-MID                            VALUE '3313'.                
009700     88  GODK-MID                            VALUE '3313'                 
009800                                                   '3317'.                
009900     88  HELP-MID                            VALUE '0551'.                
010000     SKIP2                                                                
010100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010200 01  GENERELLA-SUBPROGRAM.                                                
010300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010800     EJECT                                                                
010900*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011000*01 -COPY WMSGINIT                                                        
011100     EJECT                                                                
011200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011300*01 -COPY WMEDAREA                                                        
011400     SKIP3                                                                
011500 01  MESSAGE-CODES.                                                       
011600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011700     03  ERR-ART-MISSING         PIC X(3)    VALUE '017'.                 
011800     EJECT                                                                
011900 01  TEST-IDDISTR                PIC  9(5)   COMP-3.                      
012000*                                                                         
012400*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012500*                                                                         
012600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012700     SKIP3                                                                
012800*01  MID -COPY W3I31301                                                   
012900     EJECT                                                                
013000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
013100     SKIP3                                                                
013200*01  -COPY WMSGAREA                                                       
013300     EJECT                                                                
013400     03  MOD REDEFINES MSG-AREA.                                          
013500*      05  -COPY W3O31301                                                 
013600     EJECT                                                                
013700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013800     SKIP3                                                                
013900*01  -COPY WMFSAREA                                                       
014000     EJECT                                                                
014400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014500*                                                                         
014600     SKIP2                                                                
014700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014800     SKIP3                                                                
014900 01  NYCKLAR-TILL-DLI.                                                    
015000     03  W-IDARTNR-X.                                                     
015100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
015200                                                                          
015300***** NYCKLAR TILL WDC2 *******************************                   
015400                                                                          
015500     03  W-IDPROMR-X.                                                     
015600         05  W-IDMARKBO          PIC X       VALUE SPACE.                 
015700         05  W-IDPROMRN          PIC X(2)    VALUE SPACE.                 
015800                                                                          
015900     03  W-WDC211KY-MIN-X.                                                
016000         05  W-IDARTNR-211-MIN   PIC S9(9)   VALUE ZERO COMP-3.           
016100         05  W-DASTADAT-211-MIN  PIC 9(8)    VALUE ZERO.                  
016200                                                                          
016300     03  W-WDC211KY-MAX-X.                                                
016400         05  W-IDARTNR-211-MAX   PIC S9(9)   VALUE ZERO COMP-3.           
016500         05  W-DASTADAT-211-MAX  PIC 9(8)    VALUE ZERO.                  
016600                                                                          
016700     03  W-WDC212KY-MIN-X.                                                
016800         05  W-KDARTKAM-212-MIN  PIC 9(5)    VALUE ZERO.                  
016900         05  W-DASTADAT-212-MIN  PIC 9(8)    VALUE ZERO.                  
017000                                                                          
017100     03  W-WDC212KY-MAX-X.                                                
017200         05  W-KDARTKAM-212-MAX  PIC 9(5)    VALUE ZERO.                  
017300         05  W-DASTADAT-212-MAX  PIC 9(8)    VALUE ZERO.                  
017400                                                                          
017500     03  W-DASTADAT-X.                                                    
017600         05  W-DASTADAT          PIC 9(8)    VALUE ZERO.                  
017700                                                                          
017800******************************************************************        
017900                                                                          
018000******* NYCKLAR TILL WDC1   *******************                           
018100                                                                          
018200     03  W-WDC101KY-X.                                                    
018300         05  W-IDARTNR-111       PIC S9(9)   VALUE ZERO COMP-3.           
018400         05  W-IDMARKBO-111      PIC X       VALUE SPACE.                 
018500***************************                                               
018600*** NYCKLAR TILL WDD3 *******                                             
018700     03  W-IDARTNR-WDD3-X.                                                
018800         05  W-IDARTNR-WDD3      PIC S9(9)   VALUE ZERO COMP-3.           
018900                                                                          
019000     03  W-IDSKYLT-X.                                                     
019100         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
019200                                                                          
019300*   NYCKLAR TILL KUNDREG             ***********                          
019400                                                                          
019500     03  W-IDGMT-MIN-X.                                                   
019600         05  W-IDDISTR-B1        PIC S9(5)   VALUE ZERO COMP-3.           
019700         05  W-IDKUNDNR-B1       PIC S9(7)   VALUE ZERO COMP-3.           
019800                                                                          
019900     03  W-IDGMT-MAX-X.                                                   
020000         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO COMP-3.           
020100         05  W-IDKUNDNR-B2       PIC S9(7)   VALUE 9999999                
020200                                                        COMP-3.           
020300                                                                          
020400     03  W-IDDISTR-X.                                                     
020500         05  WA-IDDISTR  PIC S9(5)           COMP-3.                      
020600*   NYCKLAR TILL BETALNINGSREGISTRET ***********                          
020700     03  W-WDB101KY-X.                                                    
020800         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
020900         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
021000                                                                          
023200     SKIP2                                                                
023300*    --- STATUS-KOD FRÅN IMS                                              
023400 01  STATUS-WS                   PIC XX.                                  
023500     88  SEGMENT-FINNS                       VALUE '  '.                  
023600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
023700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
023800     SKIP2                                                                
023900 01  GODK-STATUSKODER.                                                    
024000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
024100     SKIP3                                                                
024200 01  SSA1                        PIC X(64).                               
024300 01  SSA2                        PIC X(64).                               
024400     EJECT                                                                
024500*    --- IMS FUNKTIONSKODER                                               
024600*01  -COPY W0003                                                          
024700     EJECT                                                                
024800*    ---  DLI INPUT-OUTPUT AREA                                           
024900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
025000     SKIP3                                                                
025100 01  DLI-IO-AREA.                                                         
025200     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
025300     SKIP3                                                                
025400     03  WDC101 REDEFINES IO-AREA.                                        
025500*        05  -COPY WDC101  -PRE WDC1-                                     
025600     EJECT                                                                
025700     03  WLBENA11 REDEFINES IO-AREA.                                      
025800*        05  -COPY WDD311  -PRE BENA-                                     
025900     EJECT                                                                
026000     SKIP3                                                                
026100     03  WDK601 REDEFINES IO-AREA.                                        
026200*        05  -COPY WDK601  -PRE WDK601-                                   
026300     EJECT                                                                
026400     SKIP3                                                                
026500     03  WDK611 REDEFINES IO-AREA.                                        
026600*        05  -COPY WDK611  -PRE WDK611-                                   
026700     EJECT                                                                
026800 01  DLI-IO-AREA2.                                                        
026900     03  IO-AREA2                PIC X(850)  VALUE SPACE.                 
027000     03  WDC201 REDEFINES IO-AREA2.                                       
027100*        05  -COPY WDC201  -PRE WDC2-                                     
027200     SKIP3                                                                
027300     03  WDC211 REDEFINES IO-AREA2.                                       
027400*        05  -COPY WDC211  -PRE WDC2-                                     
027500     EJECT                                                                
027600     03  WDC212 REDEFINES IO-AREA2.                                       
027700*        05  -COPY WDC212  -PRE WDC2-                                     
027800     EJECT                                                                
027900     03  WDC213 REDEFINES IO-AREA2.                                       
028000*        05  -COPY WDC213  -PRE WDC2-                                     
028100     EJECT                                                                
028200**   KUNDREGISTER                                                         
028300 01  DLI-IO-AREA3.                                                        
028400*    03  WDB201    -COPY WDB201 -PRE WDB2-                                
028500     EJECT                                                                
028600**   BETALNINGSREGISTER                                                   
028700 01  DLI-IO-AREA4.                                                        
028800     03  WLBETA01.                                                        
028900*        05  -COPY WDB101  -PRE WDB1-                                     
029000     EJECT                                                                
030000 LINKAGE SECTION.                                                         
030100                                                                          
030200*01  -COPY W0009   -PRE MSG-                                              
030300     EJECT                                                                
030400*01  -COPY W0008  -PRE USEA-                                              
030500     05  FILLER                  PIC X.                                   
030600     EJECT                                                                
030700*01  -COPY W0008  -PRE WDC1-                                              
030800     05  FILLER                  PIC X.                                   
030900     EJECT                                                                
031000*01  -COPY W0008  -PRE WDD3-                                              
031100     05  FILLER                  PIC X.                                   
031200     EJECT                                                                
031300*01  -COPY W0008  -PRE WDC2-                                              
031400     05  FILLER                  PIC X.                                   
031500     EJECT                                                                
031600*01  -COPY W0008  -PRE WDK6-                                              
031700     05  FILLER                  PIC X.                                   
031800     EJECT                                                                
031900*01  -COPY W0008  -PRE WDB2-                                              
032000     05  FILLER                  PIC X.                                   
032100     EJECT                                                                
032200*01  -COPY W0008  -PRE WDB1-                                              
032300     05  FILLER                  PIC X.                                   
032400     EJECT                                                                
033100 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
033200                                   WDC1-PCB WDD3-PCB                      
033300                                   WDC2-PCB WDK6-PCB                      
033400                                   WDB2-PCB WDB1-PCB.                     
033600 MAIN SECTION.                                                            
033700     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
033800                                   WDC1-PCB WDD3-PCB                      
033900                                   WDC2-PCB WDK6-PCB                      
034000                                   WDB2-PCB WDB1-PCB.                     
034200                                                                          
034300     PERFORM IMS-GET-MSG                                                  
034400     IF SEGMENT-FINNS                                                     
034500       PERFORM A-INIT                                                     
034600       PERFORM B-KOLLA-NYCKLAR                                            
034700       IF NYCKLAR-OK                                                      
034800         PERFORM F-LAES-VISA-INFO                                         
034900       END-IF                                                             
035000       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O31301 + 4                      
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
036100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I31301                 
036200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
036300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
036400     ELSE                                                                 
036500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I31301                  
036600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
036700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
036800     END-IF                                                               
036900                                                                          
037000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
037100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
037200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
037300                                                                          
037400     MOVE LOW-VALUE TO MSG-AREA                                           
037500     MOVE 'W3O313N1' TO MFS-IDMOD                                         
037600     MOVE '3313' TO MOD-IDTRANS                                           
037700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
037800                                                                          
037900     IF EGEN-MID OR HELP-MID                                              
038000       CONTINUE                                                           
038100     ELSE                                                                 
038200       MOVE SPACE TO MFS-KDTRTYP                                          
038300       MOVE '7' TO MFS-IDPFK                                              
038400     END-IF                                                               
038500                                                                          
038600     IF ENGLISH-TEXT                                                      
038700       MOVE +2 TO SPRAK-IX                                                
038800       MOVE 'GB ' TO MED-IDSKYLT                                          
038900       MOVE 'GB ' TO W-IDSKYLT                                            
039000     ELSE                                                                 
039100       MOVE +1 TO SPRAK-IX                                                
039200       MOVE 'S  ' TO MED-IDSKYLT                                          
039300       MOVE 'S  ' TO W-IDSKYLT                                            
039400     END-IF                                                               
039500     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
039600     MOVE DAGENS-DATUM TO W-DASTADAT-211-MAX                              
039700                          W-DASTADAT-212-MAX                              
039800                          W-DASTADAT                                      
040000     .                                                                    
040100     EJECT                                                                
040200 B-KOLLA-NYCKLAR SECTION.                                                 
040300                                                                          
040400     MOVE JA TO NYCKLAR-SW                                                
040500     MOVE JA TO PRISOMR-SW                                                
040600                                                                          
040700     MOVE ALL '+' TO MSGI-WMSGINIT                                        
040800     MOVE '001'             TO MSGI-KDCALL                                
040900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
041000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
041100     MOVE '3313'            TO MSGI-IDTRANS                               
041200                                                                          
041300     IF MFS-IDTRANS = '3313'                                              
041400       MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                                
041500       MOVE MID-IDDISTR-IN TO MSGI-IDDISTR                                
041600     ELSE                                                                 
041700       MOVE SPACE          TO MID-IDPROMR-IN                              
041800                              MID-IDDISTR-IN                              
041900     END-IF                                                               
042000                                                                          
042100     IF  MID-FLMRKVAL-IN  = '+'                                           
042200       CONTINUE                                                           
042300     ELSE                                                                 
042400       MOVE MID-FLMRKVAL-IN    TO MSGI-FLMRKVAL                           
042500     END-IF                                                               
042600                                                                          
042700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
042800                                                                          
042900*    -- KONTROLL AV IDPROMR                                               
043000     MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-IN                               
043100                                                                          
043200     IF MID-IDPROMR-IN = ALL '+'                                          
043300       MOVE MID-IDPROMR-UT TO WS-IDPROMR                                  
043400     ELSE                                                                 
043500       MOVE MID-IDPROMR-IN TO WS-IDPROMR                                  
043600       MOVE SPACE       TO MID-IDDISTR-UT                                 
043700       MOVE '7'         TO MFS-IDPFK                                      
043800       MOVE SPACE       TO MFS-KDTRTYP                                    
043900     END-IF                                                               
044000                                                                          
044100     IF WS-IDPROMR NOT = SPACE                                            
044200       MOVE WS-MARKBOLAG TO W-IDMARKBO                                    
044300                            W-IDMARKBO-111                                
044400                            WS-IDMARKBO                                   
044500       IF WS-IDPROMRN = SPACE                                             
044600         MOVE JA TO MARKNAD-SW                                            
044700       ELSE                                                               
044800         MOVE NEJ TO MARKNAD-SW                                           
044900       END-IF                                                             
045000     END-IF                                                               
045100                                                                          
045200     MOVE WS-IDPROMRN TO W-IDPROMRN                                       
045300                                                                          
045400                                                                          
045500*    -- KONTROLL AV IDARTNR OBLIGATORISK NYCKEL                           
045600     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
045700                                                                          
045800     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
045900     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
046000     MOVE WS-IDARTNR          TO MOD-IDARTNR-UT                           
046100     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
046200                                                                          
046300     IF MID-IDARTNR-IN = ALL '+'                                          
046400       CONTINUE                                                           
046500     ELSE                                                                 
046600       MOVE '7'         TO MFS-IDPFK                                      
046700       MOVE SPACE       TO MFS-KDTRTYP                                    
046800     END-IF                                                               
046900                                                                          
047000     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
047100       MOVE WS-IDARTNR TO W-IDARTNR                                       
047200                          W-IDARTNR-211-MIN                               
047300                          W-IDARTNR-211-MAX                               
047400                          W-IDARTNR-111                                   
047500     END-IF                                                               
047600                                                                          
047700                                                                          
047800*    -- KONTROLL AV IDDISTR                                               
047900     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
048000                                                                          
048100     IF MID-IDDISTR-IN = ALL '+'                                          
048200       MOVE MID-IDDISTR-UT TO WS-IDDISTR-IN                               
048300     ELSE                                                                 
048400       MOVE MID-IDDISTR-IN TO WS-IDDISTR-IN                               
048500       MOVE '7'         TO MFS-IDPFK                                      
048600       MOVE SPACE       TO MFS-KDTRTYP                                    
048700     END-IF                                                               
048800                                                                          
048900     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
049000                                                                          
049100     IF WS-IDDISTR NUMERIC                                                
049200       IF WS-IDDISTR > ZERO                                               
049300         MOVE WS-IDDISTR TO WA-IDDISTR                                    
049400                                                                          
049500                            W-IDDISTR-B2                                  
049600                            W-IDDISTR-B1                                  
049700        END-IF                                                            
049800     ELSE                                                                 
049900       MOVE NEJ TO NYCKLAR-SW                                             
050000     END-IF                                                               
050100                                                                          
050200     IF WS-IDARTNR NUMERIC AND WS-IDARTNR > ZERO                          
050300       IF WS-IDPROMR = SPACE                                              
050400          IF WS-IDDISTR NUMERIC                                           
050500             IF WS-IDDISTR > ZERO                                         
050600                MOVE NEJ TO PRISOMR-SW                                    
050700             ELSE                                                         
050800                MOVE NEJ TO NYCKLAR-SW                                    
050900             END-IF                                                       
051000          END-IF                                                          
051100       ELSE                                                               
051200          IF WS-IDDISTR NUMERIC                                           
051300             IF WS-IDDISTR > ZERO                                         
051400                MOVE NEJ TO PRISOMR-SW                                    
051500             END-IF                                                       
051600          END-IF                                                          
051700       END-IF                                                             
051800     ELSE                                                                 
051900        MOVE NEJ TO NYCKLAR-SW                                            
052000     END-IF                                                               
052100                                                                          
052200     MOVE MFS-RENSA-FAELT        TO MOD-FLMRKVAL-IN                       
052300     MOVE MSGI-FLMRKVAL          TO WS-FLMRKVAL                           
052400                                                                          
052500     IF EGEN-MID OR GODK-MID                                              
052600        CONTINUE                                                          
052700     ELSE                                                                 
052800        MOVE NEJ TO NYCKLAR-SW                                            
052900     END-IF                                                               
053000                                                                          
053100     IF NYCKLAR-OK                                                        
053200       IF EGEN-MID OR GODK-MID                                            
053300         IF PRISOMR-SAKNAS                                                
053400            MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-UT                        
053500            MOVE SPACE       TO W-IDPROMRN                                
053600                                W-IDMARKBO                                
053700                                W-IDMARKBO-111                            
053800         ELSE                                                             
053900            MOVE WS-IDPROMR      TO MOD-IDPROMR-UT                        
054000         END-IF                                                           
054100                                                                          
054200         MOVE WS-IDDISTR-IN TO MOD-IDDISTR-UT                             
054300         INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE           
054400       ELSE                                                               
054500         MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-UT                           
054600         MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                           
054700       END-IF                                                             
054800       MOVE  WS-FLMRKVAL          TO MOD-FLMRKVAL-UT                      
054900     ELSE                                                                 
055000       IF EGEN-MID OR GODK-MID                                            
055100          MOVE WS-IDPROMR         TO MOD-IDPROMR-UT                       
055200          MOVE WS-IDDISTR-IN TO MOD-IDDISTR-UT                            
055300          INSPECT MOD-IDDISTR-UT                                          
055400                REPLACING LEADING ZERO BY SPACE                           
055500       ELSE                                                               
055600          MOVE MFS-RENSA-FAELT TO MOD-IDPROMR-UT                          
055700          MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-UT                          
055800       END-IF                                                             
055900     END-IF                                                               
056000                                                                          
056100                                                                          
056200     IF NYCKLAR-FEL                                                       
056300       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
056400       CALL WMEDKONV USING MED-WMEDAREA                                   
056500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
056600       PERFORM MFS-RENSA-FAELT-UT                                         
056700     END-IF                                                               
056800     .                                                                    
056900     EJECT                                                                
057000 F-LAES-VISA-INFO SECTION.                                                
057100                                                                          
057200                                                                          
057300     IF PRISOMR-SAKNAS                                                    
057400        PERFORM FE-HT-PROM-VIA-DISTRIKT                                   
057500     END-IF                                                               
057600                                                                          
057700     PERFORM FA-LAES-GRUNDDATA                                            
057800     IF SEGMENT-SAKNAS                                                    
057900        MOVE ERR-ART-MISSING TO MED-IDMFSFEL                              
058000        CALL WMEDKONV USING MED-WMEDAREA                                  
058100        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
058200        PERFORM MFS-RENSA-FAELT-UT                                        
058300     ELSE                                                                 
058400       IF MARKNAD-PRISOMR                                                 
058500         PERFORM IMS-GU-WDC201                                            
058600         IF SEGMENT-FINNS                                                 
058700           PERFORM IMS-GNP-WDC211                                         
058800           IF SEGMENT-FINNS                                               
058900             PERFORM FB-FLYTTA-ARTRAB                                     
059000           ELSE                                                           
059100             PERFORM IMS-GNP-WDC212                                       
059200             IF SEGMENT-FINNS                                             
059300               PERFORM FC-FLYTTA-KAMPANJRAB                               
059400             END-IF                                                       
059500           END-IF                                                         
059600           PERFORM IMS-GNP-WDC213                                         
059700           IF SEGMENT-FINNS                                               
059800             PERFORM FD-FLYTTA-NORMALRAB                                  
059900           END-IF                                                         
060000         END-IF                                                           
060100       END-IF                                                             
060200     END-IF                                                               
060300     .                                                                    
060400     EJECT                                                                
060500 FA-LAES-GRUNDDATA SECTION.                                               
060600                                                                          
060700     MOVE WS-IDDISTR             TO TEST-IDDISTR                          
060800     PERFORM IMS-GU-WDC101                                                
060900     IF SEGMENT-FINNS                                                     
061000       MOVE WDC1-ART-PRARTBTO-MARK TO MOD-RETAIL-PRIS                     
061100                                      WS-RETAILPRIS                       
061200       MOVE WDC1-ART-IDMARKBO      TO WS-IDMARKBO                         
061300       MOVE WS-KDVALISO-MC         TO MOD-KDVALISO                        
061310       MOVE WDC1-ART-PRARTBTO-MARK TO MOD-RETAIL-PRIS                     
062600       MOVE WDC1-ART-KDARTKAM      TO W-KDARTKAM-212-MIN                  
062700                                      W-KDARTKAM-212-MAX                  
             IF WDB1-BET-FLARTRAB = 'J'                                         
               MOVE WDC1-ART-KDARTRAB-ALT  TO SPAR-KDARTRAB                     
             ELSE                                                               
               MOVE WDC1-ART-KDARTRAB      TO SPAR-KDARTRAB                     
             END-IF                                                             
062900       PERFORM IMS-GET-BENA-GU                                            
063000       IF SEGMENT-FINNS                                                   
063100         MOVE BENA-TEXT-BEART      TO MOD-BEART                           
063200       END-IF                                                             
063300       PERFORM IMS-GU-WDK601                                              
063400       IF SEGMENT-FINNS                                                   
063500         MOVE WDK601-ART-IDFKNGRP  TO MOD-IDFKNGRP                        
063600         MOVE WDK601-ART-KDPRODSL  TO MOD-KDPRODSL                        
063700         PERFORM IMS-GNP-WDK611                                           
063800         IF SEGMENT-FINNS                                                 
063900           MOVE WDK611-CLAG-PRARTSTD TO MOD-PRARTSTD                      
064800         ELSE                                                             
064900           MOVE ZERO                 TO MOD-PRARTSTD                      
065000         END-IF                                                           
065100       END-IF                                                             
065200     END-IF                                                               
065300     .                                                                    
065400     EJECT                                                                
065500 FB-FLYTTA-ARTRAB SECTION.                                                
065600     SKIP2                                                                
065700     MOVE WDC2-ART-REARTRAB-DO   TO MOD-DO-RAB                            
065800     MOVE WDC2-ART-REARTRAB-BULK TO MOD-MO-RAB                            
065900     IF WS-RETAILPRIS NOT = ZERO                                          
066000       COMPUTE WS-RETAILPRIS-DO ROUNDED =                                 
066100              ((100 - WDC2-ART-REARTRAB-DO )                              
066200              * WS-RETAILPRIS) / 100                                      
066300       COMPUTE WS-RETAILPRIS-MO ROUNDED =                                 
066400              ((100 - WDC2-ART-REARTRAB-BULK )                            
066500              * WS-RETAILPRIS) / 100                                      
066600       MOVE WS-RETAILPRIS-DO     TO MOD-DO-RAB-PRIS                       
066700       MOVE WS-RETAILPRIS-MO     TO MOD-MO-RAB-PRIS                       
067600       MOVE MFS-RENSA-FAELT      TO MOD-KDARTKAM                          
067700       MOVE ' X '                TO MOD-ARTIKEL-RAB-FINNS                 
067800     ELSE                                                                 
067900       MOVE MFS-RENSA-FAELT      TO MOD-DO-RAB                            
068000       MOVE MFS-RENSA-FAELT      TO MOD-MO-RAB                            
068100       MOVE MFS-RENSA-FAELT      TO MOD-DO-RAB-PRIS                       
068200       MOVE MFS-RENSA-FAELT      TO MOD-MO-RAB-PRIS                       
068300       MOVE MFS-RENSA-FAELT      TO MOD-ARTIKEL-RAB-FINNS                 
068400       MOVE ZERO                 TO WS-RETAILPRIS-DO                      
068500       MOVE ZERO                 TO WS-RETAILPRIS-MO                      
068600     END-IF                                                               
068700     .                                                                    
068800     EJECT                                                                
068900 FC-FLYTTA-KAMPANJRAB SECTION.                                            
069000     SKIP2                                                                
069100     MOVE WDC2-KAM-KDARTKAM      TO MOD-KDARTKAM                          
069200     MOVE WDC2-KAM-REARTRAB-DO   TO MOD-DO-RAB                            
069300     MOVE WDC2-KAM-REARTRAB-BULK TO MOD-MO-RAB                            
069400     IF WS-RETAILPRIS NOT = ZERO                                          
069500       COMPUTE WS-RETAILPRIS-DO ROUNDED =                                 
069600               ((100 - WDC2-KAM-REARTRAB-DO )                             
069700               * WS-RETAILPRIS) / 100                                     
069800       COMPUTE WS-RETAILPRIS-MO ROUNDED =                                 
069900               ((100 - WDC2-KAM-REARTRAB-BULK)                            
070000               * WS-RETAILPRIS ) / 100                                    
070100       MOVE WS-RETAILPRIS-DO     TO MOD-DO-RAB-PRIS                       
070200       MOVE WS-RETAILPRIS-MO     TO MOD-MO-RAB-PRIS                       
070300       MOVE MFS-RENSA-FAELT      TO MOD-ARTIKEL-RAB-FINNS                 
070400     ELSE                                                                 
070500       MOVE MFS-RENSA-FAELT      TO MOD-DO-RAB                            
070600       MOVE MFS-RENSA-FAELT      TO MOD-MO-RAB                            
070700       MOVE MFS-RENSA-FAELT      TO MOD-DO-RAB-PRIS                       
070800       MOVE MFS-RENSA-FAELT      TO MOD-MO-RAB-PRIS                       
070900       MOVE MFS-RENSA-FAELT      TO MOD-KDARTKAM                          
071000       MOVE ZERO                 TO WS-RETAILPRIS-DO                      
071100       MOVE ZERO                 TO WS-RETAILPRIS-MO                      
071200     END-IF                                                               
071300     .                                                                    
071400     EJECT                                                                
071500 FD-FLYTTA-NORMALRAB SECTION.                                             
071600     SKIP2                                                                
071700******************************************************************        
071800**************** OM KDARTRAB ÄR NOLL SÅ HAR ARTIKELN INGEN  ******        
071900**************** RABATT UTAN DET ÄR RETAILPRISET SOM GÄLLER ******        
072000******************************************************************        
072100     IF SPAR-KDARTRAB > ZERO                                              
072200       MOVE SPAR-KDARTRAB TO RAB-INDX                                     
072300                             MOD-NORMAL-RABATT                            
072400       MOVE WDC2-RAB-REARTRAB-DO(RAB-INDX) TO MOD-DO-NOR-RAB              
072500       MOVE WDC2-RAB-REARTRAB-BULK(RAB-INDX) TO MOD-MO-NOR-RAB            
072600       IF WS-RETAILPRIS NOT = ZERO                                        
072700         COMPUTE WS-RETAILPRIS-NOR-DO ROUNDED =                           
072800                 ((100 - WDC2-RAB-REARTRAB-DO(RAB-INDX))                  
072900                 * WS-RETAILPRIS ) / 100                                  
073000         COMPUTE WS-RETAILPRIS-NOR-MO ROUNDED =                           
073100                 ((100 - WDC2-RAB-REARTRAB-BULK(RAB-INDX))                
073200                 * WS-RETAILPRIS ) / 100                                  
073300       ELSE                                                               
073400         MOVE ZERO             TO WS-RETAILPRIS-NOR-DO                    
073500         MOVE ZERO             TO WS-RETAILPRIS-NOR-MO                    
073600       END-IF                                                             
073700       MOVE WS-RETAILPRIS-NOR-DO     TO MOD-DO-NOR-PRIS                   
073800       MOVE WS-RETAILPRIS-NOR-MO     TO MOD-MO-NOR-PRIS                   
074700     ELSE                                                                 
074800       MOVE ZERO                 TO MOD-NORMAL-RABATT                     
074900       MOVE ZERO                 TO MOD-DO-NOR-RAB                        
075000       MOVE ZERO                 TO MOD-MO-NOR-RAB                        
075100       MOVE WS-RETAILPRIS        TO MOD-DO-NOR-PRIS                       
075200       MOVE WS-RETAILPRIS        TO MOD-MO-NOR-PRIS                       
075300     END-IF                                                               
075400     .                                                                    
075500     EJECT                                                                
075600 FE-HT-PROM-VIA-DISTRIKT SECTION.                                         
075700                                                                          
075800     PERFORM IMS-GET-WDB201                                               
075900     IF SEGMENT-FINNS                                                     
076000        MOVE WDB2-GMT-IDPARTNR   TO W-WDB1-IDPARTNR                       
076100        MOVE WDB2-GMT-IDFTG      TO W-WDB1-IDFTG                          
076200        PERFORM IMS-GU-WDB101                                             
076300        IF SEGMENT-FINNS                                                  
076400           MOVE WDB1-BET-IDPROMR TO WS-IDPROMR                            
076500           MOVE WS-IDPROMRN      TO W-IDPROMRN                            
076600           MOVE WS-MARKBOLAG     TO W-IDMARKBO                            
076700                                   W-IDMARKBO-111                         
076800                                   WS-IDMARKBO                            
076900           MOVE WS-KDVALISO-MC   TO MOD-KDVALISO                          
077000           MOVE NEJ TO MARKNAD-SW                                         
077100        END-IF                                                            
077200     END-IF                                                               
077300     .                                                                    
077400     EJECT                                                                
087300 MFS-RENSA-FAELT-UT SECTION.                                              
087400                                                                          
087500*    --- ALLA UTDATA-FÄLT                                                 
087600     MOVE MFS-RENSA-FAELT TO MOD-BEART                                    
087700                             MOD-IDFKNGRP                                 
087800                             MOD-KDPRODSL                                 
087900                             MOD-RETAIL-PRIS                              
088000                             MOD-PRARTSTD                                 
088100                             MOD-KDVALISO                                 
088200                             MOD-KDARTKAM                                 
088300                             MOD-ARTIKEL-RAB-FINNS                        
088400                             MOD-DO-RAB                                   
088500                             MOD-DO-RAB-PRIS                              
088600                             MOD-MO-RAB                                   
088700                             MOD-MO-RAB-PRIS                              
088800                             MOD-NORMAL-RABATT                            
088900                             MOD-DO-NOR-RAB                               
089000                             MOD-DO-NOR-PRIS                              
089100                             MOD-MO-NOR-RAB                               
089200                             MOD-MO-NOR-PRIS                              
089300     .                                                                    
089400     EJECT                                                                
089500* --- IMS SEKTIONER ---                                                   
089600     SKIP3                                                                
089700 IMS-GET-MSG SECTION.                                                     
089800                                                                          
089900     MOVE '  QC' TO GODK-STATUSKODER                                      
090000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
090100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
090200     PERFORM IMS-STATUSKONTROLL                                           
090300     .                                                                    
090400     SKIP3                                                                
090500 IMS-INSERT-MSG SECTION.                                                  
090600                                                                          
090700     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
090800       MOVE '0' TO MFS-KDHUVOMR                                           
090900     END-IF                                                               
091000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
091100     MOVE SPACE TO GODK-STATUSKODER                                       
091200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
091300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
091400     PERFORM IMS-STATUSKONTROLL                                           
091500     .                                                                    
091600     EJECT                                                                
091700 IMS-GU-WDC101 SECTION.                                                   
091800     STRING 'WDC101  (WDC101KY =' W-WDC101KY-X ')'                        
091900          DELIMITED BY SIZE INTO SSA1                                     
092000     MOVE '  GE' TO GODK-STATUSKODER                                      
092100     CALL CBLTDLI USING GU WDC1-PCB DLI-IO-AREA SSA1                      
092200     MOVE WDC1-STATUS-CODE TO STATUS-WS                                   
092300     PERFORM IMS-STATUSKONTROLL                                           
092400     .                                                                    
092500     SKIP3                                                                
092600 IMS-GU-WDK601 SECTION.                                                   
092700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
092800          DELIMITED BY SIZE INTO SSA1                                     
092900     MOVE '  GE' TO GODK-STATUSKODER                                      
093000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA SSA1                      
093100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
093200     PERFORM IMS-STATUSKONTROLL                                           
093300     .                                                                    
093400     SKIP3                                                                
093500 IMS-GNP-WDK611 SECTION.                                                  
093600     MOVE 'WDK611   ' TO SSA1                                             
093700     MOVE '  GE' TO GODK-STATUSKODER                                      
093800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA SSA1                      
093900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
094000     PERFORM IMS-STATUSKONTROLL                                           
094100     .                                                                    
094200     EJECT                                                                
094300 IMS-GET-BENA-GU SECTION.                                                 
094400     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
094500          DELIMITED BY SIZE INTO SSA1                                     
094600     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
094700          DELIMITED BY SIZE INTO SSA2                                     
094800     MOVE '  GE' TO GODK-STATUSKODER                                      
094900     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-AREA SSA1 SSA2                 
095000     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
095100     PERFORM IMS-STATUSKONTROLL                                           
095200     .                                                                    
095300     EJECT                                                                
095400 IMS-GU-WDC201 SECTION.                                                   
095500     STRING 'WDC201  (IDPROMR  =' W-IDPROMR-X ')'                         
095600          DELIMITED BY SIZE INTO SSA1                                     
095700     MOVE '  GE' TO GODK-STATUSKODER                                      
095800     CALL CBLTDLI USING GU WDC2-PCB DLI-IO-AREA2 SSA1                     
095900     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
096000     PERFORM IMS-STATUSKONTROLL                                           
096100     .                                                                    
096200     SKIP3                                                                
096300 IMS-GNP-WDC211 SECTION.                                                  
096400     STRING 'WDC211  (WDC211KY>=' W-WDC211KY-MIN-X                        
096500                    '&WDC211KY<=' W-WDC211KY-MAX-X ')'                    
096600          DELIMITED BY SIZE INTO SSA1                                     
096700     MOVE '  GE' TO GODK-STATUSKODER                                      
096800     CALL CBLTDLI USING GNP WDC2-PCB DLI-IO-AREA2 SSA1                    
096900     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
097000     PERFORM IMS-STATUSKONTROLL                                           
097100     .                                                                    
097200     EJECT                                                                
097300 IMS-GNP-WDC212 SECTION.                                                  
097400     STRING 'WDC212  (WDC212KY>=' W-WDC212KY-MIN-X                        
097500                    '&WDC212KY<=' W-WDC212KY-MAX-X ')'                    
097600          DELIMITED BY SIZE INTO SSA1                                     
097700     MOVE '  GE' TO GODK-STATUSKODER                                      
097800     CALL CBLTDLI USING GNP WDC2-PCB DLI-IO-AREA2 SSA1                    
097900     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
098000     PERFORM IMS-STATUSKONTROLL                                           
098100     .                                                                    
098200     SKIP3                                                                
098300 IMS-GNP-WDC213 SECTION.                                                  
098400     STRING 'WDC201  (IDPROMR  =' W-IDPROMR-X ')'                         
098500          DELIMITED BY SIZE INTO SSA1                                     
098600     STRING 'WDC213  (DASTADAT<=' W-DASTADAT-X ')'                        
098700          DELIMITED BY SIZE INTO SSA2                                     
098800     MOVE '  GE' TO GODK-STATUSKODER                                      
098900     CALL CBLTDLI USING GNP WDC2-PCB DLI-IO-AREA2 SSA1 SSA2               
099000     MOVE WDC2-STATUS-CODE TO STATUS-WS                                   
099100     PERFORM IMS-STATUSKONTROLL                                           
099200     .                                                                    
099300     EJECT                                                                
099400 IMS-GET-WDB201    SECTION.                                               
099500                                                                          
099600     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
099700                   '&IDGMT   <=' W-IDGMT-MAX-X ')'                        
099800             DELIMITED BY SIZE INTO SSA1                                  
099900     MOVE '  GE' TO GODK-STATUSKODER                                      
100000     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA3 SSA1                     
100100     MOVE WDB2-STATUS-CODE  TO STATUS-WS                                  
100200     PERFORM IMS-STATUSKONTROLL                                           
100300     .                                                                    
100400     SKIP2                                                                
100500 IMS-GU-WDB101 SECTION.                                                   
100600                                                                          
100700     STRING 'WDB101  (WDB101KY =' W-WDB101KY-X ')'                        
100800          DELIMITED BY SIZE INTO SSA1                                     
100900     MOVE '  GE' TO GODK-STATUSKODER                                      
101000     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA4 SSA1                     
101100     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
101200     PERFORM IMS-STATUSKONTROLL                                           
101300     .                                                                    
101400     SKIP3                                                                
104000 IMS-STATUSKONTROLL SECTION.                                              
104100                                                                          
104200     SET STATUS-IX TO 1                                                   
104300     SEARCH GODK-STATUS                                                   
104400       AT END                                                             
104500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
104600         DELIMITED BY SIZE INTO FELTEXT                                   
104700         CALL FELLOG                                                      
104800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
104900         CONTINUE                                                         
105000     END-SEARCH                                                           
105100     .                                                                    
