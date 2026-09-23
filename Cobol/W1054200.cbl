000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1054200.                                                
000400 AUTHOR.         ODD OLSEN.                                               
000500 DATE-WRITTEN.   APRIL 85.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*                                                                         
001100*        FRÅGEPROGRAM PÅ MASTERREGISTRET.                                 
001200*                                                                         
001300*        PROGRAMMET LÄSER MASTERREGISTRET (WDN6) MED ART.NR.              
001400*        SOM NYCKEL OCH PLOCKAR UT DE KATALOGER SOM LIGGER                
001500*        UNDER DET ARTIKELNUMRET.                                         
001600*                                                                         
001700*    ÄNDRAT APRIL-89 C.EGHOLT:                                            
001800*        TILLAGT SAMMA KONTROLL PÅ KDMASTAT SOM GÖRS VID                  
001900*        PRODUKTION AV MASTERKATALOG. (W15405)                            
002000*        SAMT KONTROLL AV VCBV-ARTIKLAR UTGÅR.                            
002100*                                                                         
002200*    ÄNDRAT MARS-91 C.EGHOLT:                                             
002300*        TILLAGT NYCKLAR FÖR HOPP TILL OCH FRÅN 1541-BILDEN.              
002400*                                                                         
002500*    ÄNDRAT SEP-99 C.EGHOLT:                                              
002600*        TILLAGT ANROP FÖR WLUSEA (WMSGI)                                 
002700*                                                                         
002800*    INDATA.                                                              
002900*        TRANSAKTION: W1T542                                              
003000*        MID:         W1I54201                                            
003100*                                                                         
003200*    UTDATA.                                                              
003300*        MOD:         W1O54201                                            
003400*    SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600     SKIP3                                                                
003700 DATA DIVISION.                                                           
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100*    -- CHECKED BY WY2000                                                 
004200 77   PROGRAM-NAMN           VALUE 'W1054200'                             
004300                                 PIC X(8).                                
004310 01   FILLER                     PIC X(16)  VALUE 'ABEND-HELP'.           
004320 01   ABEND-HELP                 PIC X(80)  VALUE SPACE.                  
004321 01   FILLER                     PIC X(16)  VALUE 'IMS-CALL'.             
004322 01   IMS-CALL                   PIC X(80)  VALUE SPACE.                  
004330*                                                                         
004400 01    JA                        PIC X       VALUE 'J'.                   
004500 77    NEJ                       PIC X       VALUE 'N'.                   
004600 77    TOMMA-RADER-FINNS         PIC X       VALUE 'J'.                   
004700 77    MAX-LINE                  PIC S9(9)   VALUE +0   COMP SYNC.        
004710 77   FILLER                     PIC X(9)    VALUE ' LINE-IX='.           
004800 77    MOD-IX-LINE               PIC S9(9)   VALUE +0   COMP SYNC.        
004900 77    MAX-COL                   PIC S9(9)   VALUE +0   COMP SYNC.        
004910 77   FILLER                     PIC X(8)    VALUE ' COL-IX='.            
005000 77    MOD-IX-COL                PIC S9(9)   VALUE +0   COMP SYNC.        
005100 77    INDX                      PIC S9(9)   VALUE +0   COMP SYNC.        
005110 77   FILLER                     PIC X(8)    VALUE ' ITERIX='.            
005120 77    ITERIX                    PIC 9(3)    VALUE ZERO.                  
005200     EJECT                                                                
005300* - - - - - - - - - - - - - - -  ARBETSAREA                               
005400 01  IDARTNR-WS                  PIC X(9)    VALUE SPACE.                 
005500 01  KDFORDON-WS                 PIC X(2)    VALUE SPACE.                 
005600 01  IDKATNR-WS                  PIC X(5)    VALUE SPACE.                 
005700 01  W-KDERS                     PIC S9(3)   VALUE ZERO COMP-3.           
005800 01  W-FLLSRDEL                  PIC X       VALUE SPACE.                 
005900*                                                                         
006000 77  W-IDTRANS               PIC X(4)  VALUE SPACE.                       
006100   88  EGEN-MID                        VALUE '1542'.                      
006200   88  HELP-MID                        VALUE '0551'.                      
006300   88  GODK-MID                        VALUE '1101' '1102'                
006400                                             '1103' '1107'                
006500                                             '1108' '1115'                
006600                                             '1116' '1117'                
006700                                             '1541' '1542'                
006800                                             '1543'                       
006810                                             '2101' '2102'                
006900                                             '2103' '2104'                
007000                                             .                            
007100*                                                                         
007200     SKIP2                                                                
007300 01  W-NY-NYCKEL                 PIC X(1).                                
007400     SKIP2                                                                
007500 01  NYCKLAR-TILL-DLI.                                                    
007600   03  W-IDARTNR-X.                                                       
007700     05  W-IDARTNR               PIC S9(9)   VALUE ZERO  COMP-3.          
007800     SKIP2                                                                
007900   03  W-WDN611KY-X.                                                      
008000     05  W-IDFORDON              PIC S9(2)   VALUE ZERO  COMP-3.          
008100     05  W-TIOMBRYT-9KOMPL       PIC S9(7)   VALUE ZERO  COMP-3.          
008200     SKIP2                                                                
008300   03  W-IDSKYLT-X.                                                       
008400     05  W-IDSKYLT               PIC X(3)    VALUE 'S  '.                 
008500     SKIP2                                                                
008600   03  W-BEART-X.                                                         
008700     05  W-BEART                 PIC X(25)   VALUE SPACE.                 
008800     EJECT                                                                
008900* - - - - - - - - - - -  S U B P R O G R A M                              
009000 01  FILLER                      PIC X(16)   VALUE 'SUBPROGRAM'.          
009100 01  DYNAMISKA-SUBPROGRAM.                                                
009200     03 CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.            
009300     03 FELLOG                   PIC X(8)    VALUE 'FELLOG  '.            
009400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009500                                                                          
009600 01  MEDDELANDE.                                                          
009700     03  W-FEL-1.                                                         
009800         05   FILLER                   PIC X(40)   VALUE                  
009900            'ARTIKELNUMMER EJ NUMERISKT             '.                    
010000         05   FILLER                   PIC X(40)   VALUE                  
010100            'PART NUMBER NOT NUMERIC                '.                    
010200     03 FILLER REDEFINES W-FEL-1.                                         
010300         05  FEL-1 OCCURS 2            PIC X(40).                         
010400                                                                          
010500     03  W-FEL-2.                                                         
010600         05    FILLER                  PIC X(40)   VALUE                  
010700             'ARTIKELNUMMER SAKNAS PÅ MASTERREGISTER'.                    
010800         05    FILLER                  PIC X(40)   VALUE                  
010900             'THIS PART IS NOT IN THE MASTER        '.                    
011000     03 FILLER REDEFINES W-FEL-2.                                         
011100         05  FEL-2 OCCURS 2            PIC X(40).                         
011200                                                                          
011300     03  W-FEL-3.                                                         
011400         05    FILLER                  PIC X(40)   VALUE                  
011500             'BENÄMNING SAKNAS                      '.                    
011600         05    FILLER                  PIC X(40)   VALUE                  
011700             'DESCRIPTION IS MISSING                '.                    
011800     03 FILLER REDEFINES W-FEL-3.                                         
011900         05  FEL-3 OCCURS 2            PIC X(40).                         
012000                                                                          
012100     03  W-MED-1.                                                         
012200         05    FILLER                  PIC X(40)   VALUE                  
012300             'FLER RADER FINNS                      '.                    
012400         05    FILLER                  PIC X(40)   VALUE                  
012500             'FOR MORE INFORMATION, PRESS  ENTER    '.                    
012600     03 FILLER REDEFINES W-MED-1.                                         
012700         05  MED-1 OCCURS 2            PIC X(40).                         
012800     EJECT                                                                
012900*01  -COPY WWBYT02                                                        
013000                                                                          
013100     EJECT                                                                
013200*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
013300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
013400     SKIP3                                                                
013500*01 -COPY WMSGINIT                                                        
013600     EJECT                                                                
013700                                                                          
013800*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
013900*                                                                         
014000 01  SPAR-AREA.                                                           
014100     03  SPAR-IDTRANS           PIC X(4)    VALUE '1542'.                 
014200     03  SPAR-IDKATNR-ENTER       PIC 9(5).                               
014300     03  SPAR-IDKATNR-NEXT        PIC 9(5).                               
014400     EJECT                                                                
014500******************************************************************        
014600*                                                                         
014700*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
014800*                                                                         
014900 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
015000     SKIP3                                                                
015100*01    MID -COPY W1I54201                                                 
015200     EJECT                                                                
015210*01  MID -COPY W1I54301 -PRE 1543-.                                       
015220     EJECT                                                                
015300*01    -COPY WMSGAREA                                                     
015400     EJECT                                                                
015500*  03    MOD -COPY W1O54201  -RED MSG-AREA.                               
015600     EJECT                                                                
015700*01    -COPY WMFSAREA                                                     
015800     EJECT                                                                
015900******************************************************************        
016000*                                                                         
016100*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016200*                                                                         
016300 01    IMS-WS.                                                            
016400   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
016500     SKIP3                                                                
016600*                        **** STATUS-KOD FRÅN IMS                         
016700   03    STATUS-WS               PIC XX.                                  
016800     88    SEGMENT-FINNS                     VALUE '  '.                  
016900     88    SEGMENT-SAKNAS                    VALUE 'GE' 'GB'.             
017100     SKIP3                                                                
017200   03    GODK-STATUSKODER.                                                
017300     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
017400     SKIP3                                                                
017500 01    SSA1                      PIC X(64).                               
017600 01    SSA2                      PIC X(64).                               
017700 01    SSA3                      PIC X(64).                               
017800     EJECT                                                                
017900*                            IMS FUNKTIONSKODER                           
018000*01    -COPY W0003                                                        
018100     EJECT                                                                
018200*                            DLI INPUT-OUTPUT AREA                        
018300 01    DLI-IO-AREA.                                                       
018400   03    FILLER                  PIC X(16)   VALUE 'IO-AREA    '.         
018500   03    IO-AREA                 PIC X(900)  VALUE SPACE.                 
018600     SKIP3                                                                
018700*  03    WDN601 -COPY WDN601 -PRE WDN6-  -RED IO-AREA.                    
018800     EJECT                                                                
018900*  03    WDN611 -COPY WDN611 -PRE WDN6-  -RED IO-AREA.                    
019000     EJECT                                                                
019100*  03    WDN612 -COPY WDN612 -PRE WDN6-  -RED IO-AREA.                    
019200     EJECT                                                                
019300*  03    WLBENA11 -COPY WDD311 -PRE BENC-  -RED IO-AREA.                  
019400     EJECT                                                                
019500*  03    WLARTC01 -COPY WDK601 -PRE ARTC01-  -RED IO-AREA.                
019600     EJECT                                                                
019700*  03    WLARTC11 -COPY WDK611 -PRE ARTC11-  -RED IO-AREA.                
019800     EJECT                                                                
019900 LINKAGE SECTION.                                                         
020000*01    -COPY W0009     -PRE MSG-                                          
020010                                                                          
020100*01  -COPY W0008   -PRE USEA-                                             
020200     05  FILLER                  PIC X.                                   
020300     EJECT                                                                
020400*01    -COPY W0008     -PRE WDN6-                                         
020500     05  FILLER                  PIC X.                                   
020600     EJECT                                                                
020700*01    -COPY W0008     -PRE ARTC-                                         
020800     05  FILLER                  PIC X.                                   
020900     EJECT                                                                
021000*01    -COPY W0008     -PRE BENB-                                         
021100     05  FILLER                  PIC X.                                   
021200     EJECT                                                                
021300*01    -COPY W0008     -PRE BENC-                                         
021400     05  FILLER                  PIC X.                                   
021500     EJECT                                                                
021600 PROCEDURE DIVISION USING MSG-PCB USEA-PCB WDN6-PCB ARTC-PCB              
021700                          BENC-PCB BENB-PCB.                              
021800     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDN6-PCB ARTC-PCB             
021900                          BENC-PCB BENB-PCB.                              
022000     SKIP2                                                                
022100 STYR SECTION.                                                            
022200     SKIP2                                                                
022300     PERFORM IMS-GET-MSG                                                  
022400     IF SEGMENT-FINNS                                                     
022500       PERFORM A-INIT-SPARA-INPUT                                         
022600       IF IDARTNR-WS NOT NUMERIC                                          
022700         MOVE FEL-1(INDX) TO MOD-TEMFSFEL                                 
022800       ELSE                                                               
022900         PERFORM B-REDIGERA-BILD                                          
023000       END-IF                                                             
023100       COMPUTE MSG-KVLL = LENGTH OF MOD-W1O54201 + 4                      
023200       PERFORM IMS-INSERT-MSG                                             
023300     END-IF                                                               
023400     MOVE ZERO TO RETURN-CODE                                             
023500     GOBACK                                                               
023600     .                                                                    
023700     EJECT                                                                
023800 A-INIT-SPARA-INPUT SECTION.                                              
023900     IF MSG-DUBBLA-TRANSKODER                                             
024000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I54201                 
024100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
024200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
024300     ELSE                                                                 
024400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I54201                  
024500       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
024600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
024700     END-IF                                                               
024800                                                                          
024900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
025000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
025100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
025200                                                                          
025300*    --  RENSA MOD-AREAN --                                               
025400     MOVE LOW-VALUE TO MSG-AREA                                           
025500     MOVE 'W1O542N1' TO MFS-IDMOD                                         
025600     MOVE '1542'     TO MOD-IDTRANS                                       
025700                                                                          
025800     IF EGEN-MID AND MFS-IDPFK = '7'                                      
025900         MOVE SPACE TO MID-SEGM-NYCKEL                                    
026000     ELSE                                                                 
026100       IF NOT EGEN-MID                                                    
026200         MOVE SPACE TO MID-SEGM-NYCKEL                                    
026300       END-IF                                                             
026400     END-IF                                                               
026500                                                                          
026600     IF EGEN-MID OR HELP-MID                                              
026700       CONTINUE                                                           
026800     ELSE                                                                 
026900       MOVE SPACE TO MFS-KDTRTYP                                          
027000       MOVE '7' TO MFS-IDPFK                                              
027100     END-IF                                                               
027200*---                                                                      
027300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
027400     MOVE '001'             TO MSGI-KDCALL                                
027500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
027600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
027700     MOVE '1542'            TO MSGI-IDTRANS                               
027800     IF GODK-MID                                                          
027900        MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                               
027910        IF W-IDTRANS = '1543'                                             
027911            MOVE SPACE TO MID-KDFORDON-IN                                 
027920        END-IF                                                            
028000     END-IF                                                               
028100     IF EGEN-MID                                                          
028200       MOVE MID-IDKATNR-IN TO MSGI-IDCATNR                                
028300     END-IF                                                               
028400                                                                          
028500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
028600                                                                          
028700     IF MSGI-IDLAND-SPR  = 'SE'                                           
028800        MOVE +1 TO INDX                                                   
028900     ELSE                                                                 
029000        MOVE +2 TO INDX                                                   
029100     END-IF                                                               
029200                                                                          
029300*    -- KONTROLL AV IDARTNR                                               
029400     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
029500                                                                          
029600     IF MID-IDARTNR-IN NOT = ALL '+'                                      
029700       MOVE '7'         TO MFS-IDPFK                                      
029800       MOVE SPACE       TO MFS-KDTRTYP                                    
029900     END-IF                                                               
030000                                                                          
030100     MOVE MSGI-IDARTNR  TO  IDARTNR-WS                                    
030200     MOVE MSGI-IDCATNR  TO  IDKATNR-WS                                    
030300                                                                          
030400     INSPECT IDKATNR-WS REPLACING ALL SPACE BY ZERO                       
030500                                  ALL '+'   BY ZERO                       
030600                                                                          
030700     IF MID-KDFORDON-IN = ALL '+'                                         
030800       MOVE MID-KDFORDON-UT TO KDFORDON-WS                                
030900     ELSE                                                                 
031000       MOVE MID-KDFORDON-IN TO KDFORDON-WS                                
031100     END-IF                                                               
031200                                                                          
031300     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
031400     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
031500                                                                          
031600     MOVE KDFORDON-WS  TO MOD-KDFORDON-UT                                 
031700                                                                          
031800     MOVE IDKATNR-WS TO MOD-IDKATNR-UT                                    
031900     INSPECT MOD-IDKATNR-UT REPLACING LEADING ZERO BY SPACE               
032000                                                                          
032100     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
032200                             MOD-KDFORDON-IN                              
032300                             MOD-IDKATNR-IN                               
032400                             MOD-TEMFSFEL                                 
032500                             MOD-TEMFSINF                                 
032600                             MOD-SEGM-NYCKEL                              
032700     IF MID-IDARTNR-IN = ALL '+'                                          
032800       MOVE NEJ TO W-NY-NYCKEL                                            
032900     ELSE                                                                 
033000       MOVE JA TO W-NY-NYCKEL                                             
033100     END-IF                                                               
033200     .                                                                    
033300     EJECT                                                                
033400 B-REDIGERA-BILD SECTION.                                                 
033500     SKIP3                                                                
033600     MOVE IDARTNR-WS TO W-IDARTNR                                         
033700     IF MID-IDFORDON NUMERIC AND MID-TIOMBRYT-9KOMPL NUMERIC AND          
033800        W-NY-NYCKEL = NEJ                                                 
033900        PERFORM IMS-GU-MAST                                               
034000        IF SEGMENT-FINNS                                                  
034100           MOVE MID-IDFORDON TO W-IDFORDON                                
034200           MOVE MID-TIOMBRYT-9KOMPL TO  W-TIOMBRYT-9KOMPL                 
034300           PERFORM IMS-GNP-WDN6-KAT-NEXT                                  
034400           PERFORM BA-LAS-KATALOGIDENTITET                                
034500           MOVE MFS-ROER-EJ-FAELT TO MOD-BEART                            
034600           MOVE MFS-ROER-EJ-FAELT TO MOD-KDMASTAT                         
034700        END-IF                                                            
034800     ELSE                                                                 
034900        PERFORM IMS-GU-MAST                                               
035000        IF SEGMENT-FINNS                                                  
035100           MOVE WDN6-MAST-KDMASTAT TO MOD-KDMASTAT                        
035110           MOVE 0 TO ITERIX                                               
035200           PERFORM IMS-GNP-WDN6-KAT                                       
035201           IF SEGMENT-FINNS                                               
035210             MOVE 1 TO ITERIX                                             
035220           END-IF                                                         
035300           PERFORM BA-LAS-KATALOGIDENTITET                                
035400           PERFORM BB-LAS-BENAMNING                                       
035500           IF MOD-KDMASTAT = SPACE                                        
035600             PERFORM BC-KONTROLERA-STATUS-PA-ARTC                         
035700           END-IF                                                         
035800        ELSE                                                              
035900           MOVE FEL-2(INDX) TO MOD-TEMFSFEL                               
036000        END-IF                                                            
036100     END-IF                                                               
036200     .                                                                    
036300     EJECT                                                                
036400 BA-LAS-KATALOGIDENTITET SECTION.                                         
036500     SKIP3                                                                
036600     MOVE JA TO TOMMA-RADER-FINNS                                         
036700     MOVE +13 TO MAX-LINE                                                 
036800     MOVE +1 TO MOD-IX-LINE                                               
036900     MOVE +3 TO MAX-COL                                                   
037000     MOVE +1 TO MOD-IX-COL                                                
037100*                                                                         
037200     PERFORM UNTIL SEGMENT-SAKNAS OR TOMMA-RADER-FINNS = NEJ              
037300        IF MOD-IX-COL <= MAX-COL                                          
037400           MOVE WDN6-KAT-BEMASTER TO                                      
037500                        MOD-BEMASTER (MOD-IX-LINE, MOD-IX-COL)            
037600           MOVE WDN6-KAT-IDKATNR TO                                       
037700                         MOD-IDKATNR (MOD-IX-LINE, MOD-IX-COL)            
037701                                                                          
037702           STRING 'HAR PRECIS LÄST '                                      
037703                   MOD-IDKATNR (MOD-IX-LINE, MOD-IX-COL) ','              
037704                   MOD-BEMASTER (MOD-IX-LINE, MOD-IX-COL) '. '            
037705                   '(SEGMENT NR.' ITERIX ')'                              
037710                   DELIMITED BY SIZE INTO ABEND-HELP                      
037720                                                                          
037800           IF MOD-IX-LINE = MAX-LINE                                      
037900              MOVE +1 TO MOD-IX-LINE                                      
038000              ADD +1 TO MOD-IX-COL                                        
038100           ELSE                                                           
038200              ADD +1 TO MOD-IX-LINE                                       
038300           END-IF                                                         
038400           PERFORM IMS-GNP-WDN6-KAT                                       
038401           IF SEGMENT-FINNS                                               
038410             ADD 1 TO ITERIX                                              
038420           END-IF                                                         
038500        ELSE                                                              
038600           MOVE NEJ TO TOMMA-RADER-FINNS                                  
038700           MOVE MED-1(INDX) TO MOD-TEMFSINF                               
038800           MOVE WDN6-KAT-IDFORDON TO MOD-IDFORDON                         
038900           MOVE WDN6-KAT-TIOMBRYT-9KOMPL TO MOD-TIOMBRYT-9KOMPL           
039000        END-IF                                                            
039100     END-PERFORM                                                          
039200*                                                                         
039300     PERFORM BAA-RENSA-RESTERANDE-FALT                                    
039400     .                                                                    
039500     EJECT                                                                
039600 BAA-RENSA-RESTERANDE-FALT SECTION.                                       
039700     SKIP3                                                                
039800     PERFORM UNTIL MOD-IX-COL > MAX-COL                                   
039900        MOVE MFS-RENSA-FAELT TO                                           
040000               MOD-BEMASTER (MOD-IX-LINE, MOD-IX-COL)                     
040100        MOVE MFS-RENSA-FAELT TO                                           
040200               MOD-IDKATNR (MOD-IX-LINE, MOD-IX-COL)                      
040300        IF MOD-IX-LINE = MAX-LINE                                         
040400           MOVE +1 TO MOD-IX-LINE                                         
040500           ADD +1 TO MOD-IX-COL                                           
040600        ELSE                                                              
040700           ADD +1 TO MOD-IX-LINE                                          
040800        END-IF                                                            
040900     END-PERFORM                                                          
041000     .                                                                    
041100     EJECT                                                                
041200 BB-LAS-BENAMNING SECTION.                                                
041300     SKIP3                                                                
041400     IF INDX = 1                                                          
041500       IF MOD-KDMASTAT = SPACE OR MOD-KDMASTAT = 'IK'                     
041600         PERFORM IMS-GU-BENC-TEXT                                         
041700         IF SEGMENT-FINNS                                                 
041800           MOVE BENC-TEXT-BEART TO MOD-BEART                              
041900         ELSE                                                             
042000           PERFORM IMS-GU-MAST                                            
042100           PERFORM IMS-GNP-WDN6-BEN                                       
042200           IF SEGMENT-FINNS                                               
042300             MOVE WDN6-BEN-BEART TO MOD-BEART                             
042400           ELSE                                                           
042500             MOVE FEL-3(INDX) TO MOD-TEMFSFEL                             
042600             MOVE MFS-RENSA-FAELT TO MOD-BEART                            
042700           END-IF                                                         
042800         END-IF                                                           
042900       ELSE                                                               
043000         PERFORM IMS-GNP-WDN6-BEN                                         
043100         IF SEGMENT-FINNS                                                 
043200           MOVE WDN6-BEN-BEART TO MOD-BEART                               
043300         ELSE                                                             
043400           MOVE FEL-3(INDX) TO MOD-TEMFSFEL                               
043500           MOVE MFS-RENSA-FAELT TO MOD-BEART                              
043600         END-IF                                                           
043700       END-IF                                                             
043800     ELSE                                                                 
043900       IF MOD-KDMASTAT = SPACE OR MOD-KDMASTAT = 'IK'                     
044000         MOVE 'GB ' TO W-IDSKYLT                                          
044100         PERFORM IMS-GU-BENC-TEXT                                         
044200         IF SEGMENT-FINNS                                                 
044300           IF BENC-TEXT-BEART = SPACE                                     
044400             MOVE FEL-3(INDX) TO MOD-TEMFSFEL                             
044500             MOVE MFS-RENSA-FAELT TO MOD-BEART                            
044600           ELSE                                                           
044700             MOVE BENC-TEXT-BEART TO MOD-BEART                            
044800           END-IF                                                         
044900         ELSE                                                             
045100           PERFORM IMS-GU-MAST                                            
045200           PERFORM IMS-GNP-WDN6-BEN                                       
045300           IF SEGMENT-FINNS                                               
045400             MOVE WDN6-BEN-BEART TO W-BEART                               
045500             MOVE 'S  '                 TO W-IDSKYLT                      
045600             PERFORM IMS-GU-BENB-TEXT                                     
045700             IF SEGMENT-FINNS                                             
045800               MOVE 'GB '                  TO W-IDSKYLT                   
045900               PERFORM IMS-GNP-BENB-TEXT                                  
046000               IF SEGMENT-FINNS                                           
046100                 IF BENC-TEXT-BEART = SPACE                               
046200                   MOVE FEL-3(INDX) TO MOD-TEMFSFEL                       
046300                   MOVE MFS-RENSA-FAELT TO MOD-BEART                      
046400                 ELSE                                                     
046500                   MOVE BENC-TEXT-BEART TO MOD-BEART                      
046600                 END-IF                                                   
046700               ELSE                                                       
046800                 MOVE FEL-3(INDX) TO MOD-TEMFSFEL                         
046900                 MOVE MFS-RENSA-FAELT TO MOD-BEART                        
047000               END-IF                                                     
047100             ELSE                                                         
047200               MOVE FEL-3(INDX) TO MOD-TEMFSFEL                           
047300               MOVE MFS-RENSA-FAELT TO MOD-BEART                          
047400             END-IF                                                       
047500           ELSE                                                           
047600             MOVE FEL-3(INDX) TO MOD-TEMFSFEL                             
047700             MOVE MFS-RENSA-FAELT TO MOD-BEART                            
047800           END-IF                                                         
047900         END-IF                                                           
048000       ELSE                                                               
048100         PERFORM IMS-GNP-WDN6-BEN                                         
048200         IF SEGMENT-FINNS                                                 
048300           MOVE WDN6-BEN-BEART TO W-BEART                                 
048400           MOVE 'S  '              TO W-IDSKYLT                           
048500           PERFORM IMS-GU-BENB-TEXT                                       
048600           IF SEGMENT-FINNS                                               
048700             MOVE 'GB '              TO W-IDSKYLT                         
048800             PERFORM IMS-GNP-BENB-TEXT                                    
048900             IF SEGMENT-FINNS                                             
049000               IF BENC-TEXT-BEART = SPACE                                 
049100                 MOVE FEL-3(INDX) TO MOD-TEMFSFEL                         
049200                 MOVE MFS-RENSA-FAELT TO MOD-BEART                        
049300               ELSE                                                       
049400                 MOVE BENC-TEXT-BEART TO MOD-BEART                        
049500               END-IF                                                     
049600             ELSE                                                         
049700               MOVE FEL-3(INDX) TO MOD-TEMFSFEL                           
049800               MOVE MFS-RENSA-FAELT TO MOD-BEART                          
049900             END-IF                                                       
050000           ELSE                                                           
050100             MOVE FEL-3(INDX) TO MOD-TEMFSFEL                             
050200             MOVE MFS-RENSA-FAELT TO MOD-BEART                            
050300           END-IF                                                         
050400         ELSE                                                             
050500           MOVE FEL-3(INDX) TO MOD-TEMFSFEL                               
050600           MOVE MFS-RENSA-FAELT TO MOD-BEART                              
050700         END-IF                                                           
050800       END-IF                                                             
050900     END-IF                                                               
051000     .                                                                    
051100     EJECT                                                                
051200 BC-KONTROLERA-STATUS-PA-ARTC SECTION.                                    
051300     SKIP3                                                                
051400     MOVE ZERO TO W-KDERS                                                 
051500                                                                          
051600     IF MOD-KDMASTAT = SPACE OR 'IK'                                      
051700       MOVE IDARTNR-WS TO BYT02-IDARTNR                                   
051800       IF BYT02-RENOV                                                     
051900          MOVE 'EU' TO MOD-KDMASTAT                                       
052000       END-IF                                                             
052100        PERFORM IMS-GU-ARTC01                                             
052200        IF SEGMENT-FINNS                                                  
052300           IF ARTC01-ART-KDERS-UTG > ZERO                                 
052400              MOVE ARTC01-ART-KDERS-UTG TO W-KDERS                        
052500           ELSE                                                           
052600              PERFORM IMS-GNP-ARTC11                                      
052700              IF SEGMENT-FINNS                                            
052800                 MOVE ARTC11-CLAG-FLLSRDEL TO W-FLLSRDEL                  
052900                 MOVE ARTC11-CLAG-KDERS    TO W-KDERS                     
053000              ELSE                                                        
053100                 MOVE SPACE TO W-FLLSRDEL                                 
053200              END-IF                                                      
053300           END-IF                                                         
053400                                                                          
053500           IF W-FLLSRDEL = NEJ                                            
053600              MOVE 'NS' TO MOD-KDMASTAT                                   
053700           END-IF                                                         
053800                                                                          
053900           IF W-KDERS > ZERO                                              
054000              IF W-KDERS > +20 AND < +27                                  
054100                 MOVE 'SP' TO MOD-KDMASTAT                                
054200              ELSE                                                        
054300                 IF W-KDERS = +29                                         
054400                    MOVE 'OP' TO MOD-KDMASTAT                             
054500                 ELSE                                                     
054600                    IF W-KDERS > +29                                      
054700                       MOVE 'NS' TO MOD-KDMASTAT                          
054800                    END-IF                                                
054900                 END-IF                                                   
055000              END-IF                                                      
055100           END-IF                                                         
055200        END-IF                                                            
055300     END-IF                                                               
055400     .                                                                    
055500     EJECT                                                                
055600* IMS SEKTIONER                                                           
055700     SKIP3                                                                
055800 IMS-GET-MSG SECTION.                                                     
055810     MOVE 'IMS-GET-MSG            ' TO IMS-CALL                           
055900     MOVE '  QC' TO GODK-STATUSKODER                                      
056000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
056100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
056200     PERFORM IMS-STATUSKONTROLL                                           
056300     SKIP3                                                                
056400     .                                                                    
056500 IMS-INSERT-MSG SECTION.                                                  
056510     MOVE 'IMS-INSERT-MSG         ' TO IMS-CALL                           
056600     IF MSGI-IDLAND-SPR = 'SE'                                            
056700       MOVE '0' TO MFS-KDHUVOMR                                           
056800     END-IF                                                               
056900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
057000     MOVE SPACE TO GODK-STATUSKODER                                       
057100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
057200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
057300     PERFORM IMS-STATUSKONTROLL                                           
057400     .                                                                    
057500     EJECT                                                                
057600 IMS-GU-MAST SECTION.                                                     
057610     MOVE 'IMS-GU-MAST            ' TO IMS-CALL                           
057700     STRING 'WDN601  (IDARTNR  =' W-IDARTNR-X ')'                         
057800            DELIMITED BY SIZE INTO SSA1                                   
057900     MOVE '  GE' TO GODK-STATUSKODER                                      
058000     CALL CBLTDLI USING GU WDN6-PCB IO-AREA SSA1                          
058100     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
058200     PERFORM IMS-STATUSKONTROLL                                           
058300     SKIP3                                                                
058400     .                                                                    
058500 IMS-GNP-WDN6-KAT SECTION.                                                
058510     MOVE 'IMS-GNP-WDN6-KAT       ' TO IMS-CALL                           
058600     MOVE 'WDN611  ' TO SSA1                                              
058700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
058800     CALL CBLTDLI USING GNP WDN6-PCB IO-AREA SSA1                         
058900     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
059000     PERFORM IMS-STATUSKONTROLL                                           
059100     SKIP3                                                                
059200     .                                                                    
059300 IMS-GNP-WDN6-KAT-NEXT SECTION.                                           
059310     MOVE 'IMS-GNP-WDN6-KAT-NEXT  ' TO IMS-CALL                           
059400     STRING 'WDN611  (WDN611KY =' W-WDN611KY-X ')'                        
059500            DELIMITED BY SIZE INTO SSA1                                   
059600     MOVE '  GE' TO GODK-STATUSKODER                                      
059700     CALL CBLTDLI USING GNP WDN6-PCB IO-AREA SSA1                         
059800     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
059900     PERFORM IMS-STATUSKONTROLL                                           
060000     .                                                                    
060100     EJECT                                                                
060200 IMS-GNP-WDN6-BEN SECTION.                                                
060210     MOVE 'IMS-GNP-WDN6-BEN       ' TO IMS-CALL                           
060300     MOVE 'WDN612  ' TO SSA1                                              
060400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
060500     CALL CBLTDLI USING GNP WDN6-PCB IO-AREA SSA1                         
060600     MOVE WDN6-STATUS-CODE TO STATUS-WS                                   
060700     PERFORM IMS-STATUSKONTROLL                                           
060800     .                                                                    
060900     EJECT                                                                
061000 IMS-GU-BENC-TEXT SECTION.                                                
061010     MOVE 'IMS-GU-BENC-TEXT       ' TO IMS-CALL                           
061100     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
061200            DELIMITED BY SIZE INTO SSA1                                   
061300     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
061400            DELIMITED BY SIZE INTO SSA2                                   
061500     MOVE '  GE' TO GODK-STATUSKODER                                      
061600     CALL CBLTDLI USING GU BENC-PCB IO-AREA SSA1 SSA2                     
061700     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
061800     PERFORM IMS-STATUSKONTROLL                                           
061900     SKIP3                                                                
062000     .                                                                    
062100 IMS-GU-BENB-TEXT SECTION.                                                
062110     MOVE 'IMS-GU-BENB-TEXT       ' TO IMS-CALL                           
062200     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
062300            W-BEART-X ')'                                                 
062400            DELIMITED BY SIZE INTO SSA1                                   
062500     MOVE '  GE' TO GODK-STATUSKODER                                      
062600     CALL CBLTDLI USING GU BENB-PCB IO-AREA SSA1                          
062700     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
062800     PERFORM IMS-STATUSKONTROLL                                           
062900     SKIP3                                                                
063000     .                                                                    
063100 IMS-GNP-BENB-TEXT SECTION.                                               
063110     MOVE 'IMS-GNP-BENB-TEXT      ' TO IMS-CALL                           
063200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
063300            DELIMITED BY SIZE INTO SSA1                                   
063400     MOVE '  GE' TO GODK-STATUSKODER                                      
063500     CALL CBLTDLI USING GNP BENB-PCB IO-AREA SSA1                         
063600     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
063700     PERFORM IMS-STATUSKONTROLL                                           
063800     .                                                                    
063900     EJECT                                                                
064000 IMS-GU-ARTC01 SECTION.                                                   
064010     MOVE 'IMS-GU-ARTC01          ' TO IMS-CALL                           
064100     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
064200            DELIMITED BY SIZE INTO SSA1                                   
064300     MOVE '  GE' TO GODK-STATUSKODER                                      
064400     CALL CBLTDLI USING GU ARTC-PCB IO-AREA SSA1                          
064500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
064600     PERFORM IMS-STATUSKONTROLL                                           
064700     .                                                                    
064800     EJECT                                                                
064900 IMS-GNP-ARTC11 SECTION.                                                  
064910     MOVE 'IMS-GNP-ARTC11         ' TO IMS-CALL                           
065000     MOVE 'WLARTC11 ' TO SSA1                                             
065100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
065200     CALL CBLTDLI USING GNP ARTC-PCB IO-AREA SSA1                         
065300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
065400     PERFORM IMS-STATUSKONTROLL                                           
065500     .                                                                    
065600     EJECT                                                                
065700 IMS-STATUSKONTROLL SECTION.                                              
065800     SET STATUS-IX TO 1                                                   
065900     SEARCH GODK-STATUS AT END CALL FELLOG                                
066000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
066100         CONTINUE                                                         
066200     END-SEARCH                                                           
066300     .                                                                    
