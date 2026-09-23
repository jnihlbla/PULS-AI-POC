000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4050700.                                                
000400 AUTHOR.         FRANK THORBURN.                                          
000500     DATE-WRITTEN. SEP.1987.                                              
000600                                                                          
000700     REMARKS.                                                             
000800*   PROGRAMMET LÄSER SAMTLIGA SEGMENT UNDER WDL1 (LOGISKT WLORDM)         
000900*   OCH LÄGGER UT ARKIVERINGSDATUM FÖR PACKNINGS-UNDERLAG PÅ              
001000*   BILDEN (I FORM AV ÅR MÅNAD OCH DAG).                                  
001100*                                                                         
001200*    FUNKTION.                                                            
001300*                                                                         
001400*                                                                         
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W4T507                                              
001800*        MID:         W4I50701                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        MOD:         W4O50701                                            
002200*    SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP3                                                                
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002701                                                                          
002710*    -- CHECKED BY WY2000                                                 
002800 77   PROGRAM-NAMN           VALUE 'W4050700'                             
002900                                 PIC X(8).                                
003000 77  JA                        PIC X       VALUE 'J'.                     
003100 77  NEJ                       PIC X       VALUE 'N'.                     
003200 77  SPRAK-IX                  PIC S9(9)   VALUE +0    COMP SYNC.         
003300 77  RAD-IX                    PIC S9(9)   VALUE +0    COMP SYNC.         
003400 77  KOL-IX                    PIC S9(9)   VALUE +0    COMP SYNC.         
003500 77  RAD-MAX-PLUS-1            PIC S9(9)   VALUE +7    COMP SYNC.         
003600 77  KOL-MAX-PLUS-1            PIC S9(9)   VALUE +8    COMP SYNC.         
003700 77  SW-NYCKLAR-OK             PIC X       VALUE 'J'.                     
003800 77  MAX-MOD-LAENGD            PIC S9(4)   VALUE +535  COMP SYNC.         
003900 77  EGEN-BILD                 PIC X(4)    VALUE '4507'.                  
004200 77  WS-IDARTNR                PIC X(9)    VALUE SPACE.                   
004300 77  WS-IDKOLLI                PIC X(5)    VALUE SPACE.                   
004400 77  WS-IDPRODNR               PIC X(7)    VALUE SPACE.                   
004700                                                                          
004710 01  W-SPAR-IDKUNDRF.                                                     
004720     03  W-SPAR-IDORDNR7         PIC X(7)    VALUE '+++++++'.             
004730     03  FILLER                  PIC X(3)    VALUE '+++'.                 
004740                                                                          
004800 77  WS-IDTRANS                  PIC X(4).                                
004900     88  EGEN-MID                            VALUE '4507'.                
004910     88  WS-GODKAEND-BILD                    VALUE '4501' '4502'          
005000              '4503' '4504' '4505' '4506' '4507' '4508' '4509'.           
005100                                                                          
005200 01    NYCKLAR-TILL-DLI.                                                  
005300   03  W-01-WDL1KEY-X.                                                    
005400       05 W-01-IDDISTR         PIC S9(5)   VALUE ZERO  COMP-3.            
005500       05 W-01-IDKUNDNR        PIC S9(7)   VALUE ZERO  COMP-3.            
005600       05 W-01-IDKUNDRF.                                                  
005700          07 W-01-IDORDNR      PIC 9(5)    VALUE ZERO.                    
005800          07 FILLER            PIC X(5)    VALUE SPACE.                   
005900       05 W-01-IDDC            PIC  X(2)   VALUE SPACE.                   
006000     SKIP3                                                                
006100   03  W-11-WDL1KEY-MIN-X.                                                
006200       05 W-11-DAFAKT-MIN      PIC 9(8)  VALUE ZERO.                      
006300                                                                          
006400     EJECT                                                                
006500 01    MEDDELANDE.                                                        
006600   03    FEL901.                                                          
006700     05  FILLER                  PIC X(40)                                
006800         VALUE '901 FEL NYCKEL'.                                          
006900     05  FILLER                  PIC X(40)                                
007000         VALUE '901 WRONG KEY'.                                           
007100   03    FILLER REDEFINES FEL901.                                         
007200     05  FEL-901 OCCURS 2        PIC X(40).                               
007300     SKIP3                                                                
007400   03    FEL910.                                                          
007500     05  FILLER                  PIC X(40)                                
007600         VALUE '910 ARKIVERINGSDATUM SAKNAS'.                             
007700     05  FILLER                  PIC X(40)                                
007800         VALUE '910 FILING-DATE MISSING'.                                 
007900   03    FILLER REDEFINES FEL910.                                         
008000     05  FEL-910 OCCURS 2        PIC X(40).                               
008100     SKIP3                                                                
008200   03    FEL911.                                                          
008300     05  FILLER                  PIC X(40)                                
008400         VALUE '911 ORDERN SAKNAS'.                                       
008500     05  FILLER                  PIC X(40)                                
008600         VALUE '911 ORDER MISSING'.                                       
008700   03    FILLER REDEFINES FEL911.                                         
008800     05  FEL-911 OCCURS 2        PIC X(40).                               
008900     SKIP3                                                                
009000   03    MED1.                                                            
009100     05  FILLER                  PIC X(79)                                
009200         VALUE '    FLER PU FINNS                '.                       
009300     05  FILLER                  PIC X(79)                                
009400         VALUE '  MORE PACKING-LISTS FOLLOWS     '.                       
009500   03    FILLER REDEFINES MED1.                                           
009600     05  MED-1 OCCURS 2          PIC X(79).                               
009700     EJECT                                                                
009800******************************************************************        
009900*                                                                         
010000*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
010100*                                                                         
010200 01    FILLER                    PIC X(16)   VALUE 'MFS-WS'.              
010300     SKIP3                                                                
010400*01    MID -COPY W4I50701                                                 
010600     EJECT                                                                
010700*01    -COPY WMSGAREA                                                     
010900     EJECT                                                                
011000*  03    MOD -COPY W4O50701 -RED MSG-AREA -PRE MOD-.                      
011200     EJECT                                                                
011300*01    -COPY WMFSAREA                                                     
011500     EJECT                                                                
011600 01  FILLER                      PIC X(8)    VALUE 'DATUM   '.            
011700                                                                          
011800     EJECT                                                                
011801*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011802 01  GENERELLA-SUBPROGRAM.                                                
011803     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011805     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011806     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011807     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011808     EJECT                                                                
011809*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
011810*01 -COPY WMSGINIT                                                        
011820     EJECT                                                                
011900******************************************************************        
012000*                                                                         
012100*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012200*                                                                         
012300 01    IMS-WS.                                                            
012400   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
012500     SKIP3                                                                
012510 01  NYCKLAR-TILL-DLI.                                                    
012520   03  W-IDDC-B6-X.                                                       
012530       05 W-IDDC-B6                  PIC X(2).                            
012540                                                                          
012600*                        **** STATUS-KOD FRÅN IMS                         
012700   03    STATUS-WS               PIC XX.                                  
012800     88    SEGMENT-FINNS                     VALUE '  '.                  
012900     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
013000     SKIP3                                                                
013100   03    GODK-STATUSKODER.                                                
013200     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
013300     SKIP3                                                                
013400 01    SSA1                      PIC X(96).                               
013500     EJECT                                                                
013600*                            IMS FUNKTIONSKODER                           
013700*01    -COPY W0003                                                        
013900                                                                          
014000     EJECT                                                                
014100*                            DLI INPUT-OUTPUT AREA                        
014200 01    DLI-IO-AREA.                                                       
014300   03    IO-AREA                 PIC X(20)  VALUE SPACE.                  
014400     SKIP3                                                                
014500*  03    WLORDM01 -COPY WDL101 -PRE ORDM01- -RED IO-AREA.                 
014700     EJECT                                                                
014800*  03    WLORDM11 -COPY WDL111 -PRE OWDM11- -RED IO-AREA.                 
014900                                                                          
014910 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
014920 01   DLI-IO-AREA-B601.                                                   
014930*     03  -COPY WDB601                                                    
014940                                                                          
015000     EJECT                                                                
015100 LINKAGE SECTION.                                                         
015200*01    -COPY W0009     -PRE MSG-                                          
015400     EJECT                                                                
015500*01    -COPY W0008     -PRE USEA-                                         
015700     05  FILLER                  PIC X.                                   
015800                                                                          
015810*01    -COPY W0008     -PRE ORDM-                                         
015820     05  FILLER                  PIC X.                                   
015821                                                                          
015822*01    -COPY W0008     -PRE WDB6-                                         
015823     05  FILLER                  PIC X.                                   
015830     EJECT                                                                
015900 PROCEDURE DIVISION USING MSG-PCB USEA-PCB ORDM-PCB WDB6-PCB.             
016000     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ORDM-PCB WDB6-PCB.            
016100     PERFORM IMS-GET-MSG                                                  
016200                                                                          
016300     IF SEGMENT-FINNS                                                     
016400        PERFORM A-INIT                                                    
016410        PERFORM B-KOLLA-NYCKLAR                                           
016800        IF SW-NYCKLAR-OK = JA                                             
016900            IF MFS-IDPFK = '7'                                            
017000                PERFORM C-LAS-FORSTA                                      
017100            ELSE                                                          
017200                IF MFS-IDPFK = '8'                                        
017300                    PERFORM D-LAS-NASTA                                   
017400                ELSE                                                      
017500                    PERFORM E-LAS-SAMMA                                   
017600                END-IF                                                    
017700            END-IF                                                        
017800            PERFORM F-VISA-ARKIVERINGSDATUM                               
017900        ELSE                                                              
018000            MOVE FEL-901 (SPRAK-IX) TO MOD-TEMFSFEL                       
018100            PERFORM MFS-RENSA                                             
018200        END-IF                                                            
019300        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
019400        PERFORM IMS-INSERT-MSG                                            
019500     END-IF                                                               
019600                                                                          
019700     MOVE ZERO TO RETURN-CODE                                             
019800     GOBACK                                                               
019900     .                                                                    
020000     EJECT                                                                
020100 A-INIT SECTION.                                                          
020200                                                                          
020300     IF MSG-DUBBLA-TRANSKODER                                             
020400         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I50701               
020500         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                
020600         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
020700         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
020800         MOVE MSG-IDPFK TO MFS-IDPFK                                      
020900     ELSE                                                                 
021000         MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I50701                 
021100         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                
021200         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
021300         MOVE ' ' TO MFS-KDTRTYP        MFS-IDPFK                         
021400     END-IF                                                               
021500     MOVE MFS-IDTRANS TO WS-IDTRANS                                       
021700*----FÖRBEREDELSE FÖR BLÄDDRING MED PF-8                                  
021800     MOVE '8' TO MFS-IDPFK                                                
021900                                                                          
022000     MOVE LOW-VALUE          TO MSG-AREA                                  
022100     MOVE 'W4O507N1'         TO MFS-IDMOD                                 
022200     MOVE EGEN-BILD          TO MOD-IDTRANS                               
022300     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-IN                               
022400                             MOD-IDKUNDNR-IN                              
022500                             MOD-IDORDNR7-IN                              
022600                             MOD-IDARTNR-IN                               
022700                             MOD-IDKOLLI-IN                               
022800                             MOD-IDPRODNR-IN                              
022900                             MOD-IDDC-IN                                  
023000                             MOD-TEMFSFEL                                 
023100                             MOD-TEMFSINF                                 
023200                                                                          
023300                                                                          
023400     IF MFS-IDTRANS NOT = EGEN-BILD                                       
023500         MOVE SPACE TO MFS-KDTRTYP                                        
023600         MOVE '7' TO MFS-IDPFK                                            
023700     END-IF                                                               
023800                                                                          
023900     IF ENGLISH-TEXT                                                      
024000       MOVE +2 TO SPRAK-IX                                                
024100     ELSE                                                                 
024200       MOVE +1 TO SPRAK-IX                                                
024300     END-IF                                                               
024500     .                                                                    
024600     EJECT                                                                
024700 B-KOLLA-NYCKLAR SECTION.                                                 
024800                                                                          
024801     MOVE ALL '+'           TO MSGI-WMSGINIT                              
024802     MOVE '001'             TO MSGI-KDCALL                                
024803     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024804     MOVE '4507'            TO MSGI-IDTRANS                               
024805     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
024806     IF EGEN-MID                                                          
024807        MOVE MID-IDPRODNR-IN    TO MSGI-IDPRODNR                          
024808        MOVE MID-IDDISTR-IN     TO MSGI-IDDISTR                           
024809        MOVE MID-IDKUNDNR-IN    TO MSGI-IDKUNDNR                          
024810        IF MID-IDORDNR7-IN      NOT = ALL '+'                             
024811           MOVE MID-IDORDNR7-IN TO W-SPAR-IDORDNR7                        
024812           MOVE W-SPAR-IDKUNDRF TO MSGI-IDKUNDRF                          
024813        END-IF                                                            
024814        MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                           
024815        MOVE MID-IDKOLLI-IN     TO MSGI-IDKOLLI                           
024816     END-IF                                                               
024817     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
024818     MOVE JA                          TO SW-NYCKLAR-OK                    
024819                                                                          
025000     IF MID-IDDISTR-IN                NOT = ALL '+'                       
025500        MOVE '7'                      TO MFS-IDPFK                        
025600     END-IF                                                               
025610                                                                          
025700     IF MSGI-IDDISTR NUMERIC                                              
025800       MOVE MSGI-IDDISTR TO W-01-IDDISTR                                  
025900     ELSE                                                                 
026000       MOVE NEJ           TO SW-NYCKLAR-OK                                
026100     END-IF                                                               
026200     MOVE MSGI-IDDISTR      TO MOD-IDDISTR-UT                             
026300     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
026400                                                                          
026500     IF MID-IDKUNDNR-IN               NOT = ALL '+'                       
027000        MOVE '7'                      TO MFS-IDPFK                        
027100     END-IF                                                               
027200     IF MSGI-IDKUNDNR NUMERIC                                             
027300       MOVE MSGI-IDKUNDNR TO W-01-IDKUNDNR                                
027400     ELSE                                                                 
027500       MOVE NEJ           TO SW-NYCKLAR-OK                                
027600     END-IF                                                               
027700     MOVE MSGI-IDKUNDNR     TO MOD-IDKUNDNR-UT                            
027800     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
027900     IF MOD-IDKUNDNR-UT = SPACE                                           
028000       MOVE '     0'      TO MOD-IDKUNDNR-UT                              
028100     END-IF                                                               
028200                                                                          
028300     IF MID-IDORDNR7-IN               NOT = '+++++++'                     
028800        MOVE '7'                      TO MFS-IDPFK                        
028900     END-IF                                                               
029000     IF MSGI-IDKUNDRF (1:7) NUMERIC                                       
029100       MOVE MSGI-IDKUNDRF(1:7)           TO W-01-IDORDNR                  
029200     ELSE                                                                 
029300       MOVE NEJ           TO SW-NYCKLAR-OK                                
029400     END-IF                                                               
029500     MOVE MSGI-IDKUNDRF(1:7) TO MOD-IDORDNR7-UT                           
029600     INSPECT MOD-IDORDNR7-UT REPLACING LEADING ZERO BY SPACE              
029700                                                                          
029710     IF WS-GODKAEND-BILD                                                  
029800        IF MID-IDARTNR-IN      =   ALL '+'                                
029900            MOVE MID-IDARTNR-UT TO WS-IDARTNR                             
030000            INSPECT WS-IDARTNR REPLACING LEADING SPACE BY ZERO            
030100         ELSE                                                             
030200            MOVE MID-IDARTNR-IN TO WS-IDARTNR                             
030300        END-IF                                                            
030400        MOVE WS-IDARTNR        TO  MOD-IDARTNR-UT                         
030500        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
030600                                                                          
030700        IF MID-IDKOLLI-IN      =   ALL '+'                                
030800            MOVE MID-IDKOLLI-UT TO WS-IDKOLLI                             
030900            INSPECT WS-IDKOLLI REPLACING LEADING SPACE BY ZERO            
031000         ELSE                                                             
031100            MOVE MID-IDKOLLI-IN TO WS-IDKOLLI                             
031200        END-IF                                                            
031300        MOVE WS-IDKOLLI        TO  MOD-IDKOLLI-UT                         
031400        INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE            
031500                                                                          
031600        IF MID-IDPRODNR-IN     =   ALL '+'                                
031700            MOVE MID-IDPRODNR-UT TO WS-IDPRODNR                           
031800            INSPECT WS-IDPRODNR REPLACING LEADING SPACE BY ZERO           
031900         ELSE                                                             
032000            MOVE MID-IDPRODNR-IN TO WS-IDPRODNR                           
032100        END-IF                                                            
032200        MOVE WS-IDPRODNR       TO  MOD-IDPRODNR-UT                        
032300        INSPECT MOD-IDPRODNR-UT REPLACING LEADING ZERO BY SPACE           
032310     END-IF                                                               
032311                                                                          
032312     IF EGEN-MID                                                          
032313        IF MID-IDDC-IN     = ALL '+'                                      
032314          MOVE MID-IDDC-UT TO W-IDDC-B6                                   
032315        ELSE                                                              
032316          MOVE MID-IDDC-IN TO W-IDDC-B6                                   
032317          MOVE '7'         TO MFS-IDPFK                                   
032318          MOVE SPACE       TO MFS-KDTRTYP                                 
032319        END-IF                                                            
032320     ELSE                                                                 
032321        MOVE MSGI-IDDC     TO W-IDDC-B6                                   
032322     END-IF                                                               
032323     PERFORM IMS-GU-WDB601                                                
032324                                                                          
033100     MOVE W-IDDC-B6        TO  MOD-IDDC-UT                                
033210     IF DCS-KDDC = SPACE                                                  
033211       MOVE NEJ            TO SW-NYCKLAR-OK                               
033212     ELSE                                                                 
033220       MOVE DCS-IDDC       TO W-01-IDDC                                   
033250     END-IF                                                               
033300                                                                          
033400     .                                                                    
033500     EJECT                                                                
033600 C-LAS-FORSTA SECTION.                                                    
033700                                                                          
033800*--- NÄR MAN KOMER HIT, LÄSER MAN ALLTID FRÅN BÖRJAN.                     
033900     CONTINUE                                                             
034000     .                                                                    
034100     EJECT                                                                
034200 D-LAS-NASTA SECTION.                                                     
034300                                                                          
034400     IF MID-TIFAKT-NEXT NUMERIC AND                                       
034500        MID-TIFAKT-NEXT > ZERO                                            
034600       MOVE MID-TIFAKT-NEXT   TO W-11-DAFAKT-MIN                          
034610        IF MID-TIFAKT-NEXT < 500000                                       
034620           MOVE 20 TO W-11-DAFAKT-MIN (1:2)                               
034630        ELSE                                                              
034640           IF MID-TIFAKT-NEXT < 999999                                    
034650              MOVE 19 TO W-11-DAFAKT-MIN (1:2)                            
034660           ELSE                                                           
034670              MOVE 99999999 TO W-11-DAFAKT-MIN                            
034680           END-IF                                                         
034690        END-IF                                                            
034700     ELSE                                                                 
034800       MOVE ZERO              TO MID-TIFAKT-NEXT                          
034900     END-IF                                                               
035000                                                                          
035100     .                                                                    
035200     EJECT                                                                
035300 E-LAS-SAMMA SECTION.                                                     
035400                                                                          
035500     IF MID-TIFAKT-FIRST NUMERIC AND                                      
035600        MID-TIFAKT-FIRST > ZERO                                           
035700        MOVE MID-TIFAKT-FIRST  TO W-11-DAFAKT-MIN                         
035710        IF MID-TIFAKT-FIRST < 500000                                      
035720           MOVE 20 TO W-11-DAFAKT-MIN (1:2)                               
035730        ELSE                                                              
035740           IF MID-TIFAKT-FIRST < 999999                                   
035750              MOVE 19 TO W-11-DAFAKT-MIN (1:2)                            
035760           ELSE                                                           
035761              MOVE 99999999 TO W-11-DAFAKT-MIN                            
035770           END-IF                                                         
035780        END-IF                                                            
035800     ELSE                                                                 
035900       MOVE ZERO              TO MID-TIFAKT-FIRST                         
036000     END-IF                                                               
036100                                                                          
036200     .                                                                    
036300     EJECT                                                                
036400 F-VISA-ARKIVERINGSDATUM SECTION.                                         
036500                                                                          
036600     PERFORM IMS-LAS-GU-WDL101                                            
036700     IF SEGMENT-FINNS                                                     
036800       PERFORM IMS-LAS-GNP-WDL111                                         
036900       IF SEGMENT-FINNS                                                   
037000         MOVE OWDM11-DAT-DAFAKT (3:6) TO MOD-TIFAKT-FIRST                 
037100                                                                          
037200         MOVE +1                    TO RAD-IX                             
037300                                                                          
037400         PERFORM UNTIL RAD-IX NOT < RAD-MAX-PLUS-1                        
037500           IF SEGMENT-FINNS                                               
037600             MOVE +1                  TO KOL-IX                           
037700             PERFORM UNTIL SEGMENT-SAKNAS OR                              
037800              KOL-IX NOT < KOL-MAX-PLUS-1                                 
037900               MOVE OWDM11-DAT-DAFAKT (3:6) TO                            
038000                             MOD-TIAAMMDD (RAD-IX, KOL-IX)                
038100               ADD +1 TO KOL-IX                                           
038200               PERFORM IMS-LAS-GNP-WDL111                                 
038300             END-PERFORM                                                  
038400           ELSE                                                           
038500             MOVE MFS-RENSA-FAELT TO MOD-RAD (RAD-IX)                     
038600           END-IF                                                         
038700           ADD +1 TO RAD-IX                                               
038800         END-PERFORM                                                      
038900         IF SEGMENT-FINNS                                                 
039000            MOVE OWDM11-DAT-DAFAKT (3:6)  TO MOD-TIFAKT-NEXT              
039100            MOVE MED-1 (SPRAK-IX)         TO MOD-TEMFSINF                 
039200         ELSE                                                             
039300            MOVE ZERO                      TO MOD-TIFAKT-NEXT             
039400         END-IF                                                           
039500       ELSE                                                               
039600           MOVE FEL-910 (SPRAK-IX)         TO MOD-TEMFSFEL                
039700           PERFORM MFS-RENSA                                              
039800       END-IF                                                             
039900     ELSE                                                                 
040000         MOVE FEL-911 (SPRAK-IX)           TO MOD-TEMFSFEL                
040100         PERFORM MFS-RENSA                                                
040200     END-IF                                                               
040300                                                                          
040400     .                                                                    
040500     EJECT                                                                
040600 MFS-RENSA SECTION.                                                       
040700                                                                          
040800     MOVE MFS-RENSA-FAELT            TO MOD-TIFAKT-FIRST                  
040900                                        MOD-TIFAKT-NEXT                   
041000     MOVE +1                         TO RAD-IX                            
041100     PERFORM UNTIL RAD-IX NOT < RAD-MAX-PLUS-1                            
041200       MOVE MFS-RENSA-FAELT          TO MOD-RAD (RAD-IX)                  
041300       ADD +1                        TO RAD-IX                            
041400     END-PERFORM                                                          
041500                                                                          
041600     EJECT                                                                
041700* IMS SEKTIONER                                                           
041800     SKIP3                                                                
041900     .                                                                    
042000 IMS-GET-MSG SECTION.                                                     
042100     SKIP2                                                                
042200     MOVE '  QC' TO GODK-STATUSKODER                                      
042300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
042400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
042500     PERFORM IMS-STATUSKONTROLL                                           
042600     SKIP3                                                                
042700     .                                                                    
042800 IMS-INSERT-MSG SECTION.                                                  
042900     SKIP2                                                                
043000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
043100        MOVE '0' TO MFS-KDHUVOMR                                          
043200     END-IF                                                               
043300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
043400     MOVE SPACE TO GODK-STATUSKODER                                       
043500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
043600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043700     PERFORM IMS-STATUSKONTROLL                                           
043800     .                                                                    
043900     EJECT                                                                
044000 IMS-LAS-GU-WDL101 SECTION.                                               
044100     SKIP2                                                                
044200     STRING 'WLORDM01(WDL101KY =' W-01-WDL1KEY-X ')'                      
044300            DELIMITED BY SIZE INTO SSA1                                   
044400     MOVE '  GE' TO GODK-STATUSKODER                                      
044500     CALL CBLTDLI USING GU ORDM-PCB DLI-IO-AREA SSA1                      
044600     MOVE ORDM-STATUS-CODE TO STATUS-WS                                   
044700     PERFORM IMS-STATUSKONTROLL                                           
044800     SKIP3                                                                
044900     .                                                                    
045000 IMS-LAS-GNP-WDL111 SECTION.                                              
045100                                                                          
045200     STRING 'WLORDM11(DAFAKT  =>' W-11-WDL1KEY-MIN-X ')'                  
045300            DELIMITED BY SIZE INTO SSA1                                   
045400     MOVE '  GE' TO GODK-STATUSKODER                                      
045500     CALL CBLTDLI USING GNP ORDM-PCB DLI-IO-AREA SSA1                     
045600     MOVE ORDM-STATUS-CODE TO STATUS-WS                                   
045700     PERFORM IMS-STATUSKONTROLL                                           
045800     .                                                                    
045900     EJECT                                                                
045910 IMS-GU-WDB601    SECTION.                                                
045920     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
045930          DELIMITED BY SIZE INTO SSA1                                     
045940     MOVE '  GE' TO GODK-STATUSKODER                                      
045950     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
045960     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
045970     PERFORM IMS-STATUSKONTROLL                                           
045980     IF SEGMENT-SAKNAS                                                    
045990         MOVE SPACE TO DCS-KDDC                                           
045991     END-IF                                                               
045992     .                                                                    
046000 IMS-STATUSKONTROLL SECTION.                                              
046100     SKIP2                                                                
046200     SET STATUS-IX TO 1                                                   
046300     SEARCH GODK-STATUS AT END CALL FELLOG                                
046400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
046500     END-SEARCH                                                           
046600     .                                                                    
