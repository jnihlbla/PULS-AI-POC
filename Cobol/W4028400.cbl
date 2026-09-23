000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4028400.                                                
000400 AUTHOR.         GÖRAN KJELLSON   GUIDE DATAKONSULT AB                    
000500 DATE-WRITTEN.   NOV -90.                                                 
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        BILD 4284 - FRÅGA  ORDERBEKRÄFTELSE PÅ FICHE                     
001100*        PROGRAMMET VISAR ALLA DATUM ORDERBEKRÄFTELSER FÖR                
001200*        GIVEN ORDER HAR FLYTTATS TILL FICHE                              
001300*                                                                         
001400*        PROGRAMMET ÄR EN FRÅGE-MPP                                       
001600*        PROGRAMMET LÄSER      WLORDM (WDL1)                              
001610*        PROGRAMMET LÄSER      WDB6                                       
001700*                                                                         
001800*                                                                         
001900*    INDATA.                                                              
002000*        TRANSAKTION: W4T284                                              
002100*        MID:         W4I28401                                            
002200*                                                                         
002300*    UTDATA.                                                              
002400*        MOD:         W4O28401                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003001                                                                          
003010*    -- CHECKED BY WY2000                                                 
003100 77  IDPGM                       PIC X(08)   VALUE 'W4028200'.            
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500                                                                          
003600*    --- INDEX FÖR BLÄDDRINGSRADER                                        
003700 77  KOL-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
003800 77  KOL-IX-MAX                  PIC S9(4)  VALUE +7    COMP SYNC.        
003900 77  RAD-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
004000 77  RAD-IX-MAX                  PIC S9(4)  VALUE +6    COMP SYNC.        
004100                                                                          
004200 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004400                                                                          
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004610 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
004700 77  WS-IDKUNDNR                 PIC X(6)    VALUE SPACE.                 
004800 77  WS-IDORDNR                  PIC X(7)    VALUE SPACE.                 
005100 77  WS-IDDISTR-NUM              PIC 9(4)    VALUE ZERO.                  
005200 77  WS-IDKUNDNR-NUM             PIC 9(6)    VALUE ZERO.                  
005201 01  W-SPAR-IDKUNDRF.                                                     
005202     03  W-SPAR-IDORDNR7         PIC X(7)    VALUE '+++++++'.             
005210     03  FILLER                  PIC X(3)    VALUE '+++'.                 
005300                                                                          
005400                                                                          
005500********************************************************                  
005600                                                                          
005700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005800     88  NYCKLAR-OK                          VALUE 'J'.                   
005900     88  NYCKLAR-FEL                         VALUE 'N'.                   
006000                                                                          
006400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006500     88  EGEN-MID                            VALUE '4284'.                
006600     88  GODK-MID                            VALUE '4281' '4282'          
006700                                                   '4283' '4284'.         
006710*    --- VALID IDDC CODES                                                 
006720*                                                                         
006730*01  -COPY WWDCKONS                                                       
006800                                                                          
006900     EJECT                                                                
007000                                                                          
007600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007700 01  GENERELLA-SUBPROGRAM.                                                
007800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008001     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008002     EJECT                                                                
008003*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008010*01 -COPY WMSGINIT                                                        
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008300*   -COPY WMEDAREA                                                        
008500     SKIP3                                                                
008600 01  MESSAGE-CODES.                                                       
008700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008900     03  INF-INFO-SAKNAS         PIC X(3)    VALUE '413'.                 
009000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009100     03  ERR-SISTA-RADEN-SKRIVEN PIC X(3)    VALUE '115'.                 
009200     EJECT                                                                
009300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
009400*                                                                         
009500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009600     SKIP3                                                                
009700*01  MID -COPY W4I28401    -PRE MID-                                      
009900     EJECT                                                                
010000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010100     SKIP3                                                                
010200*01  -COPY WMSGAREA                                                       
010400     EJECT                                                                
010500     03  MOD REDEFINES MSG-AREA.                                          
010600*      05  -COPY W4O28401    -PRE MOD-                                    
010800     EJECT                                                                
010900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011000     SKIP3                                                                
011100*01  -COPY WMFSAREA                                                       
011300     EJECT                                                                
011400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011500*                                                                         
011600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011700     SKIP3                                                                
011800 01  NYCKLAR-TILL-DLI.                                                    
011900                                                                          
013100     03  W-WDL101KY-X.                                                    
013200         05  W-IDDISTR           PIC S9(5)    VALUE ZERO COMP-3.          
013300         05  W-IDKUNDNR          PIC S9(7)    VALUE ZERO COMP-3.          
013400         05  W-IDKUNDRF          PIC X(10)    VALUE SPACE.                
013500         05  W-IDDC              PIC  X(2)    VALUE SPACE.                
013600                                                                          
014100     03  W-DAHISTOB-X.                                                    
014200         05  W-DAHISTOB          PIC  9(8)    VALUE ZERO.                 
014300                                                                          
014310     03  W-IDDC-B6-X.                                                     
014311         05 W-IDDC-B6                  PIC X(2).                          
014320                                                                          
014400*    --- STATUS-KOD FRÅN IMS                                              
014500 01  STATUS-WS                   PIC XX.                                  
014600     88  SEGMENT-FINNS                       VALUE '  '.                  
014800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014900     88  BASEN-SLUT                          VALUE 'GB'.                  
015000     SKIP2                                                                
015100 01  GODK-STATUSKODER.                                                    
015200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015300     SKIP3                                                                
015400 01  SSA1                        PIC X(96).                               
015600     EJECT                                                                
015700*    --- IMS FUNKTIONSKODER                                               
015800*01  -COPY W0003                                                          
016000     EJECT                                                                
016100*    ---  DLI INPUT-OUTPUT AREA                                           
016200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016300     SKIP3                                                                
016400 01  DLI-IO-AREA.                                                         
016600     SKIP3                                                                
017900*    03  WLORDM01  -COPY WDL101                                           
018200     EJECT                                                                
018210*    03  WLORDM12  -COPY WDL112                                           
018230                                                                          
018240 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
018250 01   DLI-IO-AREA-B601.                                                   
018260*     03  -COPY WDB601                                                    
018270     EJECT                                                                
018300 LINKAGE SECTION.                                                         
018400                                                                          
018500*01  -COPY W0009      -PRE MSG-                                           
018700     EJECT                                                                
019200*01  -COPY W0008      -PRE USEA-                                          
019400     05  FILLER                  PIC X.                                   
019500     EJECT                                                                
019510*01  -COPY W0008      -PRE ORDM-                                          
019520     05  FILLER                  PIC X.                                   
019530     EJECT                                                                
019540*01  -COPY W0008      -PRE WDB6-                                          
019550     05  FILLER                  PIC X.                                   
019560     EJECT                                                                
019600 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB ORDM-PCB WDB6-PCB.            
019700     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB ORDM-PCB WDB6-PCB.            
019800                                                                          
019900     PERFORM IMS-GET-MSG                                                  
020000     IF SEGMENT-FINNS                                                     
020100       PERFORM A-INIT                                                     
020200       PERFORM B-KOLLA-NYCKLAR                                            
020300       IF NYCKLAR-OK                                                      
020400         IF MFS-FIRST                                                     
020500           PERFORM C-FOERSTA-SIDA                                         
020600         ELSE                                                             
020700           IF MFS-NEXT                                                    
020800             PERFORM D-NAESTA-SIDA                                        
020900           ELSE                                                           
021000             PERFORM E-SAMMA-SIDA                                         
021100           END-IF                                                         
021200         END-IF                                                           
021700         PERFORM G-LAES-VISA-INFO                                         
022600       END-IF                                                             
022610       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O28401 + 4                      
022800       PERFORM IMS-INSERT-MSG                                             
022900     END-IF                                                               
023000                                                                          
023100     MOVE ZERO TO RETURN-CODE                                             
023200     GOBACK                                                               
023300     .                                                                    
023400     EJECT                                                                
023500 A-INIT SECTION.                                                          
023600                                                                          
023700     IF MSG-DUBBLA-TRANSKODER                                             
023800       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I28401                 
023900       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
024000       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
024100     ELSE                                                                 
024200       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I28401                  
024300       MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                   
024400       MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                  
024500     END-IF                                                               
024600                                                                          
024700     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
024800     MOVE MSG-IDPFK            TO MFS-IDPFK                               
024900     MOVE MFS-IDTRANS          TO W-IDTRANS                               
025000                                                                          
025100     MOVE LOW-VALUE       TO MSG-AREA                                     
025200     MOVE 'W4O28401'      TO MFS-IDMOD                                    
025300     MOVE '4284'          TO MOD-IDTRANS                                  
025400     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
025500                                                                          
025600     IF NOT EGEN-MID                                                      
025700       MOVE SPACE TO MFS-KDTRTYP                                          
025800       MOVE '7'   TO MFS-IDPFK                                            
025900     END-IF                                                               
026000                                                                          
026100     IF ENGLISH-TEXT                                                      
026200       MOVE +2    TO SPRAK-IX                                             
026400       MOVE 'GB ' TO MED-IDSKYLT                                          
026500     ELSE                                                                 
026600       MOVE +1    TO SPRAK-IX                                             
026800       MOVE 'S  ' TO MED-IDSKYLT                                          
026900     END-IF                                                               
027000     .                                                                    
027100     EJECT                                                                
027200                                                                          
027300 B-KOLLA-NYCKLAR SECTION.                                                 
027400                                                                          
027401                                                                          
027402     MOVE ALL '+'           TO MSGI-WMSGINIT                              
027403     MOVE '001'             TO MSGI-KDCALL                                
027404     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
027405     MOVE '4284'            TO MSGI-IDTRANS                               
027406     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
027407     IF EGEN-MID                                                          
027408        MOVE MID-IDDISTR-IN    TO MSGI-IDDISTR                            
027409        MOVE MID-IDKUNDNR-IN   TO MSGI-IDKUNDNR                           
027410        MOVE MID-IDORDNR-IN    TO W-SPAR-IDORDNR7                         
027411        MOVE W-SPAR-IDKUNDRF   TO MSGI-IDKUNDRF                           
027412        MOVE MID-IDARTNR-IN    TO MSGI-IDARTNR                            
027413        MOVE MID-KDFRAKT-IN    TO MSGI-KDFRAKT                            
027414     END-IF                                                               
027415     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
027416     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
027420                                                                          
027600     MOVE JA TO NYCKLAR-SW                                                
027700                                                                          
028100     PERFORM BA-KOLLA-DISTRIKT                                            
028200     PERFORM BB-KOLLA-KUNDNR                                              
028300     PERFORM BC-KOLLA-KUNDRF                                              
028310     PERFORM BD-KOLLA-IDDC                                                
028400                                                                          
028500     PERFORM BE-KOLLA-OVRIGA-NYCKLAR                                      
028600                                                                          
028700     IF NYCKLAR-FEL                                                       
028800       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
028900       CALL WMEDKONV USING MED-WMEDAREA                                   
029000       MOVE MED-MFSFEL      TO MOD-TEMFSFEL                               
029100       PERFORM MFS-RENSA-FAELT-UT                                         
029200     END-IF                                                               
029700     .                                                                    
029800     EJECT                                                                
029900                                                                          
030000 BA-KOLLA-DISTRIKT SECTION.                                               
030100                                                                          
030200     MOVE +0                   TO WS-IDDISTR-NUM                          
030300                                                                          
030400     MOVE MFS-RENSA-FAELT  TO MOD-IDDISTR-IN                              
030500     IF MID-IDDISTR-IN     NOT = ALL '+'                                  
031100       MOVE '7'            TO MFS-IDPFK                                   
031200       MOVE SPACE          TO MFS-KDTRTYP                                 
031300     END-IF                                                               
031400                                                                          
031500     IF MSGI-IDDISTR NUMERIC AND MSGI-IDDISTR > ZERO                      
031600       MOVE MSGI-IDDISTR     TO WS-IDDISTR-NUM                            
031700       MOVE WS-IDDISTR-NUM   TO W-IDDISTR                                 
031800     ELSE                                                                 
031900       MOVE NEJ              TO NYCKLAR-SW                                
032000     END-IF                                                               
032100                                                                          
032300     IF MSGI-IDDISTR     = ZERO                                           
032400       MOVE '   0'       TO MOD-IDDISTR-UT                                
032500     ELSE                                                                 
032600       MOVE MSGI-IDDISTR TO MOD-IDDISTR-UT                                
032700       INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE             
032800     END-IF                                                               
033200     .                                                                    
033300     EJECT                                                                
033400                                                                          
033500 BB-KOLLA-KUNDNR SECTION.                                                 
033600                                                                          
033700     MOVE MFS-RENSA-FAELT   TO MOD-IDKUNDNR-IN                            
033800     IF MID-IDKUNDNR-IN     NOT = ALL '+'                                 
034400       MOVE '7'             TO MFS-IDPFK                                  
034500       MOVE SPACE           TO MFS-KDTRTYP                                
034600     END-IF                                                               
034700                                                                          
034800     IF MSGI-IDKUNDNR NUMERIC                                             
034900       MOVE WS-IDKUNDNR     TO WS-IDKUNDNR-NUM                            
035000       MOVE WS-IDKUNDNR-NUM TO W-IDKUNDNR                                 
035100     ELSE                                                                 
035200       MOVE NEJ             TO NYCKLAR-SW                                 
035300     END-IF                                                               
035400                                                                          
035500     MOVE MSGI-IDKUNDNR     TO MOD-IDKUNDNR-UT                            
035600     INSPECT MOD-IDKUNDNR-UT REPLACING LEADING ZERO BY SPACE              
035700     IF MOD-IDKUNDNR-UT     = SPACE                                       
035800       MOVE '     0'        TO MOD-IDKUNDNR-UT                            
035900     END-IF                                                               
036300     .                                                                    
036400     EJECT                                                                
036500                                                                          
036600 BC-KOLLA-KUNDRF SECTION.                                                 
036700                                                                          
036800     MOVE MFS-RENSA-FAELT  TO MOD-IDORDNR-IN                              
036900     IF MID-IDORDNR-IN     NOT = ALL '+'                                  
037500       MOVE '7'            TO MFS-IDPFK                                   
037600       MOVE SPACE          TO MFS-KDTRTYP                                 
037700     END-IF                                                               
037800                                                                          
037900     IF MSGI-IDKUNDRF (1:7) NUMERIC                                       
038000       MOVE MSGI-IDKUNDRF(1:7) TO W-IDKUNDRF                              
038100     ELSE                                                                 
038200       MOVE NEJ            TO NYCKLAR-SW                                  
038300     END-IF                                                               
038400                                                                          
038600     MOVE MSGI-IDKUNDRF(1:7) TO MOD-IDORDNR-UT                            
038700     INSPECT MOD-IDORDNR-UT REPLACING LEADING ZERO BY SPACE               
038800     IF MOD-IDORDNR-UT     = SPACE                                        
038900       MOVE '      0'      TO MOD-IDORDNR-UT                              
039000     END-IF                                                               
039400     .                                                                    
039500     EJECT                                                                
039510 BD-KOLLA-IDDC   SECTION.                                                 
039520                                                                          
039530     MOVE MFS-RENSA-FAELT  TO MOD-IDDC-IN                                 
039540     IF MID-IDDC-IN        =  ALL '+'                                     
039550       MOVE MID-IDDC-UT    TO W-IDDC-B6                                   
039570     ELSE                                                                 
039580       MOVE MID-IDDC-IN    TO W-IDDC-B6                                   
039591       MOVE '7'            TO MFS-IDPFK                                   
039592       MOVE SPACE          TO MFS-KDTRTYP                                 
039593     END-IF                                                               
039594     PERFORM IMS-03-GU-WDB601                                             
039595                                                                          
039596     IF DCS-KDDC = SPACE OR DCS-DDC                                       
039597       MOVE MSGI-IDDC  TO W-IDDC                                          
039599     ELSE                                                                 
039600       MOVE DCS-IDDC   TO W-IDDC                                          
039601     END-IF                                                               
039602                                                                          
039603     MOVE W-IDDC-B6     TO MOD-IDDC-UT                                    
039604     INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                  
039611     .                                                                    
039612     EJECT                                                                
039620                                                                          
039700 BE-KOLLA-OVRIGA-NYCKLAR SECTION.                                         
039800                                                                          
040000     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
040100                             MOD-KDORDBEK-IN                              
040200                             MOD-KDFRAKT-IN                               
040300                             MOD-KDORDKL-IN                               
040400                                                                          
041100                                                                          
041500     MOVE MSGI-IDARTNR     TO MOD-IDARTNR-UT                              
041700     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
041800                                                                          
041900     IF MID-KDORDBEK-IN = ALL '+'                                         
042000       MOVE MID-KDORDBEK-UT TO MOD-KDORDBEK-UT                            
042100     ELSE                                                                 
042200       MOVE MID-KDORDBEK-IN TO MOD-KDORDBEK-UT                            
042300     END-IF                                                               
042400     INSPECT MOD-KDORDBEK-UT REPLACING LEADING ZERO BY SPACE              
042500                                                                          
042900     MOVE MSGI-KDFRAKT     TO MOD-KDFRAKT-UT                              
043100     INSPECT MOD-KDFRAKT-UT REPLACING LEADING ZERO BY SPACE               
043200                                                                          
043300     IF MID-KDORDKL-IN = ALL '+'                                          
043400       MOVE MID-KDORDKL-UT  TO MOD-KDORDKL-UT                             
043500     ELSE                                                                 
043600       MOVE MID-KDORDKL-IN  TO MOD-KDORDKL-UT                             
043700     END-IF                                                               
043800     INSPECT MOD-KDORDKL-UT REPLACING LEADING ZERO BY SPACE               
043900     .                                                                    
044000     EJECT                                                                
044100                                                                          
044200 C-FOERSTA-SIDA SECTION.                                                  
044300                                                                          
044400     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
044500     CALL WMEDKONV USING MED-WMEDAREA                                     
044600     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
044610                                                                          
044620     MOVE ZERO                TO W-DAHISTOB                               
044630     MOVE WC-CDC-SE           TO W-IDDC                                   
044700     .                                                                    
044800     SKIP3                                                                
044900 D-NAESTA-SIDA SECTION.                                                   
045000                                                                          
045100     MOVE MID-TIHIST-NEXT     TO W-DAHISTOB                               
045101     IF MID-TIHIST-NEXT < 500000                                          
045102        MOVE 20 TO W-DAHISTOB (1:2)                                       
045103     ELSE                                                                 
045104        IF MID-TIHIST-NEXT < 999999                                       
045105           MOVE 19 TO W-DAHISTOB (1:2)                                    
045106        ELSE                                                              
045107           MOVE 99999999 TO W-DAHISTOB                                    
045108        END-IF                                                            
045109     END-IF                                                               
045110     MOVE MID-IDDC-NEXT       TO W-IDDC                                   
045200     .                                                                    
045300     SKIP3                                                                
045400 E-SAMMA-SIDA SECTION.                                                    
045500                                                                          
045600     MOVE MID-TIHIST-ENTER    TO W-DAHISTOB                               
045601     IF MID-TIHIST-ENTER < 500000                                         
045602        MOVE 20 TO W-DAHISTOB (1:2)                                       
045603     ELSE                                                                 
045604        IF MID-TIHIST-ENTER < 999999                                      
045605           MOVE 19 TO W-DAHISTOB (1:2)                                    
045606        ELSE                                                              
045607           MOVE 99999999 TO W-DAHISTOB                                    
045608        END-IF                                                            
045609     END-IF                                                               
045610     MOVE MID-IDDC-ENTER      TO W-IDDC                                   
045700     .                                                                    
045800     EJECT                                                                
048000 G-LAES-VISA-INFO SECTION.                                                
048100                                                                          
048200     MOVE +1                   TO KOL-IX                                  
048300                                  RAD-IX                                  
048910                                                                          
049000     PERFORM IMS-01-GU-ORDM01                                             
049110     PERFORM UNTIL (SEGMENT-SAKNAS   OR                                   
049111                   RAD-IX > RAD-IX-MAX)                                   
049113       IF SEGMENT-FINNS                                                   
049114         PERFORM GA-LAES-TIHIST-OCH-SKRIV-MOD                             
049115       END-IF                                                             
049116                                                                          
050800     END-PERFORM                                                          
050801                                                                          
050810     PERFORM GB-SPARA-NEXT-NYCKLAR                                        
050820                                                                          
051000     IF RAD-IX = +1 AND KOL-IX = +1                                       
051010        PERFORM GC-SKRIV-FELTEXT                                          
052400     END-IF                                                               
052500     .                                                                    
052600     EJECT                                                                
052610 GA-LAES-TIHIST-OCH-SKRIV-MOD SECTION.                                    
052611                                                                          
052620     PERFORM IMS-02-GNP-OWDM12                                            
052630     PERFORM UNTIL SEGMENT-SAKNAS OR                                      
052640                    RAD-IX > RAD-IX-MAX                                   
052650        PERFORM UNTIL SEGMENT-SAKNAS OR KOL-IX > KOL-IX-MAX               
052660           MOVE DAT-DAHISTOB (3:6)                                        
052670                            TO MOD-TIHIST(RAD-IX, KOL-IX)                 
052680           ADD +1           TO KOL-IX                                     
052690           PERFORM IMS-02-GNP-OWDM12                                      
052691        END-PERFORM                                                       
052692                                                                          
052693        IF KOL-IX > KOL-IX-MAX                                            
052694           ADD +1           TO RAD-IX                                     
052695           MOVE +1          TO KOL-IX                                     
052696        END-IF                                                            
052697     END-PERFORM                                                          
052698     .                                                                    
052699     EJECT                                                                
052700                                                                          
052800 GB-SPARA-NEXT-NYCKLAR SECTION.                                           
052900                                                                          
053000     IF SEGMENT-FINNS                                                     
053020                                                                          
053100       MOVE ORD-IDDC          TO MOD-IDDC-NEXT                            
053110       MOVE DAT-DAHISTOB (3:6) TO MOD-TIHIST-NEXT                         
053120                                                                          
053200       MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                          
053300       CALL WMEDKONV USING MED-WMEDAREA                                   
053400       MOVE MED-MFSINF        TO MOD-TEMFSINF                             
053410     ELSE                                                                 
053420       IF MFS-NEXT                                                        
053430          MOVE ERR-SISTA-RADEN-SKRIVEN                                    
053440                              TO MED-IDMFSFEL                             
053450          CALL WMEDKONV USING MED-WMEDAREA                                
053460          MOVE MED-MFSFEL     TO MOD-TEMFSFEL                             
053500       END-IF                                                             
053600     END-IF                                                               
053800     .                                                                    
054000     SKIP3                                                                
054010 GC-SKRIV-FELTEXT SECTION.                                                
054011                                                                          
054020     IF MFS-NEXT                                                          
054030        MOVE ERR-SISTA-RADEN-SKRIVEN                                      
054040                            TO MED-IDMFSFEL                               
054050        CALL WMEDKONV USING MED-WMEDAREA                                  
054060        MOVE MED-MFSFEL      TO MOD-TEMFSFEL                              
054070     ELSE                                                                 
054080        MOVE INF-INFO-SAKNAS TO MED-IDMFSINF                              
054090        CALL WMEDKONV USING MED-WMEDAREA                                  
054091        MOVE MED-MFSINF      TO MOD-TEMFSINF                              
054092        MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                              
054093     END-IF                                                               
054094     .                                                                    
054095     EJECT                                                                
056600 MFS-RENSA-FAELT-UT SECTION.                                              
056700                                                                          
056800*    --- ALLA UTDATA-FÄLT                                                 
056810     MOVE +1                    TO RAD-IX                                 
056830     PERFORM UNTIL RAD-IX > RAD-IX-MAX                                    
056831                                                                          
056840        MOVE +1                 TO KOL-IX                                 
056860        PERFORM UNTIL KOL-IX > KOL-IX-MAX                                 
056861                                                                          
056862           MOVE MFS-RENSA-FAELT TO MOD-TIHIST(RAD-IX, KOL-IX)             
056870           ADD +1               TO KOL-IX                                 
056871        END-PERFORM                                                       
056872        ADD +1                  TO RAD-IX                                 
056880     END-PERFORM                                                          
057600     .                                                                    
058400     EJECT                                                                
058500 IMS-GET-MSG SECTION.                                                     
058600                                                                          
058700     MOVE '  QC'          TO GODK-STATUSKODER                             
058800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
058900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
059000     PERFORM IMS-STATUSKONTROLL                                           
059100     .                                                                    
059200     SKIP3                                                                
059300 IMS-INSERT-MSG SECTION.                                                  
059400                                                                          
059500     IF ENGLISH-TEXT                                                      
059600       MOVE 'N'           TO MFS-KDHUVOMR                                 
059700     END-IF                                                               
059800     MOVE LOW-VALUE       TO MSG-KDZ1 MSG-KDZ2                            
059900     MOVE SPACE           TO GODK-STATUSKODER                             
060000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
060100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
060200     PERFORM IMS-STATUSKONTROLL                                           
060300     .                                                                    
060310     EJECT                                                                
061600 IMS-01-GU-ORDM01 SECTION.                                                
061700                                                                          
061800     STRING 'WLORDM01(WDL101KY =' W-WDL101KY-X ')'                        
061900          DELIMITED BY SIZE INTO SSA1                                     
062000     MOVE '  GE'              TO GODK-STATUSKODER                         
062100     CALL CBLTDLI USING GU ORDM-PCB WLORDM01 SSA1                         
062200     MOVE ORDM-STATUS-CODE    TO STATUS-WS                                
062300     PERFORM IMS-STATUSKONTROLL                                           
062400     .                                                                    
062410     SKIP3                                                                
062700 IMS-02-GNP-OWDM12 SECTION.                                               
062800                                                                          
062900     STRING 'WLORDM12(DAHISTOB=>' W-DAHISTOB-X ')'                        
063000          DELIMITED BY SIZE INTO SSA1                                     
063100     MOVE '  GE'              TO GODK-STATUSKODER                         
063200     CALL CBLTDLI USING GNP ORDM-PCB WLORDM12 SSA1                        
063300     MOVE ORDM-STATUS-CODE    TO STATUS-WS                                
063400     PERFORM IMS-STATUSKONTROLL                                           
063500     .                                                                    
063510     SKIP3                                                                
063520 IMS-03-GU-WDB601    SECTION.                                             
063530     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
063540          DELIMITED BY SIZE INTO SSA1                                     
063550     MOVE '  GE' TO GODK-STATUSKODER                                      
063560     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
063570     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
063580     PERFORM IMS-STATUSKONTROLL                                           
063590     IF SEGMENT-SAKNAS                                                    
063600         MOVE SPACE TO DCS-KDDC                                           
063700     END-IF                                                               
063800     .                                                                    
064000 IMS-STATUSKONTROLL SECTION.                                              
064100                                                                          
064200     SET STATUS-IX TO 1                                                   
064300     SEARCH GODK-STATUS                                                   
064400       AT END CALL FELLOG                                                 
064500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
064600     END-SEARCH                                                           
064700     .                                                                    
