000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                W9011200.                                     
000300 AUTHOR.                    URBAN ZACKRISSON.                             
000400 DATE-WRITTEN.              OKT  -84.                                     
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.   TP-PROGRAM                                               
000800*                ARTIKELINFORMATION                                       
000900                                                                          
001000*    INDATA.                                                              
001100*        TRANSAKTION: W9T112                                              
001200*        MID:         W9I11201                                            
001300*    UTDATA.                                                              
001400*        MOD:         W9O11201                                            
001500                                                                          
001600 ENVIRONMENT DIVISION.                                                    
001700                                                                          
001800 DATA DIVISION.                                                           
001900     EJECT                                                                
002000 WORKING-STORAGE SECTION.                                                 
002100                                                                          
002200*    -COPY WY2000W9                                                       
002300 77  IDPGM                       PIC X(8)   VALUE 'W9011200'.             
002400 77      SPAR-FLLSRDEL           PIC X      VALUE SPACE.                  
002500 77      IDARTNR-WS              PIC X(9)   VALUE SPACE.                  
002600 77      IDDISTR-WS              PIC X(4)   VALUE SPACE.                  
002700 77      JA                      PIC X(1)   VALUE 'J'.                    
002800 77      NEJ                     PIC X(1)   VALUE 'N'.                    
002900 77      SW-VISAS                PIC X(1)   VALUE 'J'.                    
003000 77      MAX-MOD-LENGD           PIC S9(4)  VALUE +594  COMP SYNC.        
003100 77      MAX-ANT-BILD-RADER      PIC S9(9)  VALUE +28   COMP SYNC.        
003200 77      INDX                    PIC S9(9)  VALUE +0    COMP SYNC.        
003300 77      IX                      PIC S9(9)  VALUE +1    COMP SYNC.        
003400 77      W-KDERS                 PIC S9(3)  VALUE +0    COMP-3.           
003500 77      DAGENS-VECKA            PIC S9(4)  VALUE +0    COMP-3.           
003600 77      VECKOR-TILL-PUBLICERING PIC S9(6)  VALUE ZERO.                   
003700 77      INLEVERANS-VECKA        PIC S9(4)  VALUE +0    COMP-3.           
003800 77      WS-FLGEMART             PIC X(1)   VALUE 'N'.                    
003900 77      WS-KDPRODSL             PIC S9(3)  VALUE ZERO  COMP-3.           
004000 77      WS-PRARTBTO-EXP         PIC S9(7)V9(2) VALUE ZERO COMP-3.        
004100 77      FL-FINGERAD-KDERS       PIC X(1)   VALUE 'N'.                    
004200                                                                          
004900 01  FELTEXT.                                                             
005000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005200     SKIP3                                                                
005300 01      W-TIFINLV               PIC S9(5).                               
005400 01      FILLER  REDEFINES W-TIFINLV.                                     
005500   03    W-TIFINLV-AAR           PIC 9(2).                                
005600   03    W-TIFINLV-VECKA         PIC 9(2).                                
005700   03    W-TIFINLV-DAG           PIC 9(1).                                
005800     SKIP3                                                                
005900 01      SPRAK-KODER             PIC X(15)                                
006000         VALUE  'D  E  F  GB S  '.                                        
006100     SKIP3                                                                
006200 01      FILLER REDEFINES SPRAK-KODER.                                    
006300   03    IDSKYLT                 PIC X(3)   OCCURS 5.                     
006400     SKIP3                                                                
006500 01      BLADDRINGS-FALT.                                                 
006600   03      LAESTA-KAT-SEG        PIC 9(3)   VALUE ZERO.                   
006700     EJECT                                                                
007100                                                                          
007200                                                                          
007300 01  TEST-IDARTNR                PIC 9(9)  COMP-3.                        
007400*01  FILLER    -COPY WWBYT02  -RED  TEST-IDARTNR.                         
007500*01  FILLER    -COPY WWBYT19  -RED  TEST-IDARTNR.                         
007600     EJECT                                                                
007700                                                                          
007800 01    NYCKLAR-TILL-DLI.                                                  
007900                                                                          
008000   03    W-IDARTNR-X.                                                     
008100     05    W-IDARTNR             PIC S9(9)  VALUE ZERO  COMP-3.           
008200   03    W-IDARTNR-BYT-X.                                                 
008300     05    W-IDARTNR-BYT         PIC S9(9)  VALUE ZERO  COMP-3.           
008400   03    W-KDNOTTYP-X.                                                    
008500     05    W-KDNOTTYP            PIC S9(1)  VALUE ZERO  COMP-3.           
008600   03    W-IDSKYLT-X.                                                     
008700     05    W-IDSKYLT             PIC X(3)   VALUE SPACE.                  
008800   03    W-WDC101KY-X.                                                    
008900     05    W-IDARTNR-P           PIC S9(9)  VALUE ZERO  COMP-3.           
009000     05    W-IDMARKBO            PIC X(1)   VALUE SPACE.                  
009100*   NYCKLAR TILL KUNDREGISTRET ***********                                
009200                                                                          
009300     03  W-IDGMT-MAX-X.                                                   
009400         05  W-IDDISTR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
009500         05  W-IDKUNDNR-MAX      PIC S9(7)                                
009600                                      VALUE 9999999  COMP-3.              
009700                                                                          
009800     03  W-IDGMT-MIN-X.                                                   
009900         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
010000         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
010100                                                                          
010200*   NYCKLAR TILL BETALNINGSREGISTRET ***********                          
010300   03    W-WDB101KY-X.                                                    
010400     05  W-WDB1-IDPARTNR          PIC X(9)    VALUE SPACE.                
010500     05  W-WDB1-IDFTG             PIC 9(2)    VALUE ZERO.                 
010600                                                                          
010700     SKIP3                                                                
010800 01    DYNAMISKA-SUBPROGRAM.                                              
010900   03    FILLER                  PIC X(16)  VALUE 'SUBPROGRAM'.           
011000   03    WDATKONV                PIC X(8)   VALUE 'WDATKONV'.             
011100   03    W400ARTU                PIC X(8)   VALUE 'W400ARTU'.             
011200   03    FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
011300   03    CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
011400   03    W005INIT                PIC X(8)   VALUE 'W005INIT'.             
011500     EJECT                                                                
011600*                   ****    PARAMETRAR TILL W005INIT                      
011700*01  -COPY WMSGINIT                                                       
011800     EJECT                                                                
011900 01    MEDDELANDEN.                                                       
012000                                                                          
012100   03    W-FEL-1.                                                         
012200     05    FILLER        PIC X(26)   VALUE                                
012300                             'ARTIKELNUMMER EJ NUMERISKT'.                
012400     05    FILLER        PIC X(26)   VALUE                                
012500                             'PARTNUMBER NOT NUMERIC    '.                
012600   03    FILLER REDEFINES W-FEL-1.                                        
012700     05    FEL-1         PIC X(26) OCCURS 2.                              
012800   03    W-FEL-6.                                                         
012900     05    FILLER        PIC X(19)   VALUE                                
013000                             'ARTIKELN HAR UTGÅTT'.                       
013100     05    FILLER        PIC X(19)   VALUE                                
013200                             'NOT STORED ANYMORE '.                       
013300   03    FILLER REDEFINES W-FEL-6.                                        
013400     05    FEL-6         PIC X(19) OCCURS 2.                              
013500   03    W-FEL-7.                                                         
013600     05    FILLER        PIC X(36)   VALUE                                
013700                         'ARTIKELN FINNS EJ I ARTIKELREGISTRET'.          
013800     05    FILLER        PIC X(36)   VALUE                                
013900                             'THIS PART IS NOT IN THE DATABASE'.          
014000   03    FILLER REDEFINES W-FEL-7.                                        
014100     05    FEL-7         PIC X(36) OCCURS 2.                              
014200   03    W-FEL-8.                                                         
014300     05    FILLER        PIC X(18)   VALUE                                
014400                             'FELAKTIGT DISTRIKT'.                        
014500     05    FILLER        PIC X(18)   VALUE                                
014600                             'WRONG DISTRICT    '.                        
014700   03    FILLER REDEFINES W-FEL-8.                                        
014800     05    FEL-8         PIC X(18) OCCURS 2.                              
014900   03    W-FEL-9.                                                         
015000     05    FILLER        PIC X(33)   VALUE                                
015100                         'ARTIKELN EJ TILLGÄNGLIG FÖR ORDER'.             
015200     05    FILLER        PIC X(33)   VALUE                                
015300                             'NOT AVAILABLE FOR ORDERS'.                  
015400   03    FILLER REDEFINES W-FEL-9.                                        
015500     05    FEL-9         PIC X(33) OCCURS 2.                              
015600   03    W-MED-1.                                                         
015700     05    FILLER        PIC X(4)   VALUE                                 
015800                             'FLER'.                                      
015900     05    FILLER        PIC X(4)   VALUE                                 
016000                             'MORE'.                                      
016100   03    FILLER REDEFINES W-MED-1.                                        
016200     05    MED-1         PIC X(4) OCCURS 2.                               
016300                                                                          
016400   03    W-MED-3.                                                         
016500     05    FILLER        PIC X(50)  VALUE                                 
016600           'ARTIKELN ÄR ERSATT, SE 9102                       '.          
016700     05    FILLER        PIC X(50)  VALUE                                 
016800           'THIS PART IS SUPERSEDED, SEE 9102                 '.          
016900   03    FILLER REDEFINES W-MED-3.                                        
017000     05    MED-3         PIC X(50) OCCURS 2.                              
017100                                                                          
017200   03    W-MED-3-GEMART.                                                  
017300     05    FILLER        PIC X(50)  VALUE                                 
017400           'ARTIKELN ÄR ERSATT, SE 9102       GEMENSAM PV/LV  '.          
017500     05    FILLER        PIC X(50)  VALUE                                 
017600           'THIS PART IS SUPERSEDED, SEE 9102   COMMON CP/TP  '.          
017700   03    FILLER REDEFINES W-MED-3-GEMART.                                 
017800     05    MED-3-GEMART  PIC X(50) OCCURS 2.                              
017900                                                                          
018000                                                                          
018100   03    W-MED-4.                                                         
018200     05    FILLER        PIC X(50)   VALUE                                
018300           'ARTIKELN ÄR ERSATT                                '.          
018400     05    FILLER        PIC X(50)   VALUE                                
018500           'THIS PART IS SUPERSEDED                           '.          
018600   03    FILLER REDEFINES W-MED-4.                                        
018700     05    MED-4         PIC X(50) OCCURS 2.                              
018800                                                                          
018900   03    W-MED-4-GEMART.                                                  
019000     05    FILLER        PIC X(50)   VALUE                                
019100           'ARTIKELN ÄR ERSATT              GEMENSAM PV/LV    '.          
019200     05    FILLER        PIC X(50)   VALUE                                
019300           'THIS PART IS SUPERSEDED           COMMON CP/TP    '.          
019400   03    FILLER REDEFINES W-MED-4-GEMART.                                 
019500     05    MED-4-GEMART  PIC X(50) OCCURS 2.                              
019600                                                                          
019700                                                                          
019800   03    W-MED-5.                                                         
019900     05    FILLER        PIC X(50)   VALUE                                
020000           'ARTIKELN ÄR TILLFÄLLIGT ERSATT                    '.          
020100     05    FILLER        PIC X(50)   VALUE                                
020200           'THIS PART IS TEMPORARILY SUPERSEDED               '.          
020300   03    FILLER REDEFINES W-MED-5.                                        
020400     05    MED-5         PIC X(50) OCCURS 2.                              
020500                                                                          
020600   03    W-MED-5-GEMART.                                                  
020700     05    FILLER        PIC X(50)   VALUE                                
020800           'ARTIKELN ÄR TILLFÄLLIGT ERSATT      GEMENSAM PV/LV'.          
020900     05    FILLER        PIC X(50)   VALUE                                
021000           'THIS PART IS TEMPORARILY SUPERSEDED   COMMON CP/TP'.          
021100   03    FILLER REDEFINES W-MED-5-GEMART.                                 
021200     05    MED-5-GEMART  PIC X(50) OCCURS 2.                              
021300                                                                          
021400                                                                          
021500   03    W-MED-6.                                                         
021600     05    FILLER        PIC X(50)   VALUE                                
021700           'ARTIKELN ÄR TILLFÄLLIGT ERSATT, SE 9102           '.          
021800     05    FILLER        PIC X(50)   VALUE                                
021900           'THIS PART IS TEMPORARILY SUPERSEDED, SEE 9102     '.          
022000   03    FILLER REDEFINES W-MED-6.                                        
022100     05    MED-6         PIC X(50) OCCURS 2.                              
022200                                                                          
022300   03    W-MED-6-GEMART.                                                  
022400     05    FILLER        PIC X(50)   VALUE                                
022500           'ARTIKELN ÄR TILLF ERSATT, SE 9102   GEMENSAM PV/LV'.          
022600     05    FILLER        PIC X(50)   VALUE                                
022700           'THIS PART IS TEMP SUPERSEDED SEE 9102 COMMON CP/TP'.          
022800   03    FILLER REDEFINES W-MED-6-GEMART.                                 
022900     05    MED-6-GEMART  PIC X(50) OCCURS 2.                              
023000                                                                          
023100                                                                          
023200   03    W-MED-7.                                                         
023300     05    FILLER        PIC X(50)   VALUE                                
023400           'GEMENSAM PV/LV                                    '.          
023500     05    FILLER        PIC X(50)   VALUE                                
023600           'COMMON CP/TP                                      '.          
023700   03    FILLER REDEFINES W-MED-7.                                        
023800     05    MED-7         PIC X(50) OCCURS 2.                              
023900     EJECT                                                                
024000                                                                          
024100 01      FILLER          PIC X(16)   VALUE 'WDATAREA'.                    
024200*01      RTDATAREA  -COPY WDATAREA.                                       
024300     EJECT                                                                
024400******************************************************************        
024500*                                                                         
024600*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
024700*                                                                         
024800 01      FILLER          PIC X(16)   VALUE 'MFS-WS'.                      
024900     SKIP3                                                                
025000*01      MID -COPY W9I11201 -PRE MID-.                                    
025100     SKIP3                                                                
025200*01      -COPY WMSGAREA                                                   
025300     SKIP3                                                                
025400*  03    MOD -COPY W9O11201 -PRE MOD- -RED MSG-AREA.                      
025500     EJECT                                                                
025600*01  -COPY WMFSAREA.                                                      
025700     EJECT                                                                
025800******************************************************************        
025900*                                                                         
026000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
026100*                                                                         
026200 01  IMS-WS.                                                              
026300   03    FILLER          PIC X(16)   VALUE 'IMS-WS'.                      
026400     SKIP3                                                                
026500*****                    **** STATUS-KOD FRÅN IMS                         
026600   03    STATUS-WS       PIC XX.                                          
026700         88  SEGMENT-FINNS       VALUE '  '.                              
026800         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
026900         88  SEGMENT-FINNS-REDAN VALUE 'II'.                              
027000     SKIP3                                                                
027100   03    GODK-STATUSKODER.                                                
027200     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027300     SKIP3                                                                
027400 01      SSA1            PIC X(64).                                       
027500 01      SSA2            PIC X(64).                                       
027600 01      SSA3            PIC X(64).                                       
027700     EJECT                                                                
027800*                            IMS FUNKTIONSKODER                           
027900*01      -COPY W0003                                                      
028000     EJECT                                                                
028100*                            DLI INPUT-OUTPUT AREA                        
028200 01  DLI-IO-AREA.                                                         
028300     03  IO-AREA         PIC X(900)  VALUE SPACE.                         
028400     SKIP3                                                                
028500*    03  WLARTC01 -COPY WDK601               -RED IO-AREA.                
028600     EJECT                                                                
028700*    03  WLARTC11 -COPY WDK611               -RED IO-AREA.                
028800     EJECT                                                                
028900*    03  WLARTC25 -COPY WDK625               -RED IO-AREA.                
029000     EJECT                                                                
029100*    03  WLBENA01 -COPY WDD301 -PRE BEN-     -RED IO-AREA.                
029200     EJECT                                                                
029300*    03  WLBENA11 -COPY WDD311 -PRE BEN-     -RED IO-AREA.                
029400     EJECT                                                                
029500*    03  WLKATN01 -COPY WDN601 -PRE MAST-    -RED IO-AREA.                
029600     EJECT                                                                
029700*    03  WLKATN11 -COPY WDN611 -PRE MAST-    -RED IO-AREA.                
029800     EJECT                                                                
029900*    03  WLERSA01 -COPY WDD701 -PRE ERSATT-  -RED IO-AREA.                
030000     EJECT                                                                
030100*    03  WLERSA11 -COPY WDD702 -PRE TILLK-   -RED IO-AREA.                
030200     EJECT                                                                
030300*    03  WLPRIA01 -COPY WDC101 -PRE PRIA-  -RED IO-AREA.                  
030400     EJECT                                                                
030500 01  DLI-IO-AREA2.                                                        
030600**   KUNDREGISTER                                                         
030700*    03  WLGMTA01 -COPY WDB201 -PRE GMTA-                                 
030800     EJECT                                                                
030900 01  DLI-IO-AREA3.                                                        
031000     03  IO-AREA3        PIC X(600)  VALUE SPACE.                         
031100     SKIP3                                                                
031200*    03  WLBETC01 -COPY WDB101 -PRE WDB1-    -RED IO-AREA3.               
031300     EJECT                                                                
031400 01  DLI-IO-AREA4.                                                        
031500     03  WDK611   -COPY WDK611 -PRE OBJ-                                  
031600     EJECT                                                                
031700 LINKAGE SECTION.                                                         
031800*01  -COPY W0009     -PRE MSG-                                            
031900     EJECT                                                                
032000*01  -COPY W0008     -PRE USEA-                                           
032100         05  FILLER           PIC X.                                      
032200     EJECT                                                                
032300*01  -COPY W0008     -PRE ARTC-                                           
032400         05  FILLER           PIC X.                                      
032500                                                                          
032600*01  -COPY W0008     -PRE BEN-                                            
032700         05  FILLER           PIC X.                                      
032800     EJECT                                                                
032900*01  -COPY W0008     -PRE MAST-                                           
033000         05  FILLER           PIC X.                                      
033100                                                                          
033200*01  -COPY W0008     -PRE ERSA-                                           
033300         05  FILLER           PIC X.                                      
033400     EJECT                                                                
033500*01  -COPY W0008     -PRE PRIA-                                           
033600         05  FILLER           PIC X.                                      
033700     EJECT                                                                
033800*01  -COPY W0008     -PRE GMTA-                                           
033900     05  FILLER                  PIC X.                                   
034000     EJECT                                                                
034100*01    -COPY W0008   -PRE WDB1-                                           
034200     05  FILLER                  PIC X.                                   
034300     EJECT                                                                
034400 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
034500                                  ARTC-PCB BEN-PCB                        
034600                          MAST-PCB ERSA-PCB PRIA-PCB                      
034700                          GMTA-PCB WDB1-PCB.                              
034800 MAIN SECTION.                                                            
034900     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
035000                                   ARTC-PCB BEN-PCB                       
035100                          MAST-PCB ERSA-PCB PRIA-PCB                      
035200                          GMTA-PCB WDB1-PCB.                              
035300                                                                          
035400     PERFORM IMS-GET-MSG                                                  
035500                                                                          
035600     IF SEGMENT-FINNS                                                     
035700        PERFORM A-SPARA-NYCKLAR-OCH-INIT                                  
035800                                                                          
035900       IF IDARTNR-WS NOT NUMERIC                                          
036000          MOVE FEL-1 (INDX) TO MOD-TEMFSFEL                               
036100       ELSE                                                               
036200          IF IDDISTR-WS NOT NUMERIC                                       
036300             MOVE FEL-8 (INDX) TO MOD-TEMFSFEL                            
036400          ELSE                                                            
036500           IF IDDISTR-WS > ZERO                                           
036600            MOVE IDARTNR-WS TO W-IDARTNR                                  
036700                               W-IDARTNR-P                                
036800                               TEST-IDARTNR                               
036900            MOVE IDDISTR-WS TO W-IDDISTR-MIN                              
037000                               W-IDDISTR-MAX                              
037100                                                                          
037200            PERFORM C-KOLLA-VILLKOR-RED-BILD                              
037300            IF SW-VISAS = NEJ                                             
037400               PERFORM D-RENSA-FALT                                       
037500               MOVE FEL-7 (INDX) TO MOD-TEMFSFEL                          
037600            ELSE                                                          
037700               IF W-KDERS = +29                                           
037800                  PERFORM D-RENSA-FALT                                    
037900                  MOVE FEL-6 (INDX) TO MOD-TEMFSFEL                       
038000               ELSE                                                       
038100                  EVALUATE TRUE                                           
038200                     WHEN W-KDERS < 21                                    
038300                          MOVE MFS-RENSA-FAELT TO MOD-TIERSDAT            
038400                                      MOD-IDARTNR-TILLK                   
038500                  END-EVALUATE                                            
038600               END-IF                                                     
038700            END-IF                                                        
038800           ELSE                                                           
038900             MOVE FEL-8 (INDX) TO MOD-TEMFSFEL                            
039000           END-IF                                                         
039100          END-IF                                                          
039200       END-IF                                                             
039300     END-IF                                                               
039400     PERFORM IMS-INSERT-MSG                                               
039500                                                                          
039600     MOVE ZERO TO RETURN-CODE                                             
039700     GOBACK                                                               
039800     .                                                                    
039900     EJECT                                                                
040000 A-SPARA-NYCKLAR-OCH-INIT SECTION.                                        
040100                                                                          
040200     IF MSG-DUBBLA-TRANSKODER                                             
040300       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W9I11201                 
040400       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
040500       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
040600       MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                  
040700       MOVE ZERO                          TO MID-BLADDRING-ANT            
040800     ELSE                                                                 
040900       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W9I11201                 
041000       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
041100       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
041200       MOVE ' '                           TO MFS-KDTRTYP                  
041300     END-IF                                                               
041400     MOVE ALL '+' TO MSGI-WMSGINIT                                        
041500     MOVE '001'             TO MSGI-KDCALL                                
041600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
041700     MOVE '9112'            TO MSGI-IDTRANS                               
041800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
041900     IF MFS-IDTRANS = '9112'                                              
042000     OR (MID-IDARTNR-IN NUMERIC                                           
042100     AND MID-IDARTNR-IN > ZERO)                                           
042200         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
042300     END-IF                                                               
042400     IF MFS-IDTRANS = '9112'                                              
042500     OR (MID-IDDISTR-IN NUMERIC                                           
042600     AND MID-IDDISTR-IN > ZERO)                                           
042700         MOVE MID-IDDISTR-IN TO MSGI-IDDISTR                              
042800     END-IF                                                               
042900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
043000     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
043100     MOVE MSGI-IDDISTR TO IDDISTR-WS                                      
043200     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
043300     INSPECT IDDISTR-WS REPLACING ALL SPACE BY ZERO                       
043400     IF MID-IDARTNR-IN = ALL '+'                                          
043500       CONTINUE                                                           
043600     ELSE                                                                 
043700       IF MID-IDDISTR-IN = ALL '+'                                        
043800          CONTINUE                                                        
043900       ELSE                                                               
044000          MOVE ZERO TO MID-BLADDRING-ANT                                  
044100          MOVE ' ' TO MFS-KDTRTYP                                         
044200       END-IF                                                             
044300     END-IF                                                               
044400     MOVE LOW-VALUE  TO MSG-AREA                                          
044500     MOVE 'W9O11201' TO MFS-IDMOD                                         
044600     MOVE '9112' TO MOD-IDTRANS                                           
044700     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
044800     MOVE IDDISTR-WS TO MOD-IDDISTR-UT                                    
044900     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
045000     INSPECT MOD-IDDISTR-UT REPLACING LEADING ZERO BY SPACE               
045100     MOVE MAX-MOD-LENGD TO MSG-KVLL                                       
045200                                                                          
045300     IF SWEDISH-TEXT                                                      
045400       MOVE +1 TO INDX                                                    
045500     ELSE                                                                 
045600       MOVE +2 TO INDX                                                    
045700     END-IF                                                               
045800     IF MFS-IDTRANS NOT = '9112'                                          
045900       MOVE ' ' TO MFS-KDTRTYP                                            
046000     END-IF                                                               
046100     IF (MID-BLADDRING-ANT NOT NUMERIC)                                   
046200     OR (MFS-IDTRANS NOT = '9112')                                        
046300       MOVE ZERO TO MID-BLADDRING-ANT                                     
046400     END-IF                                                               
046500     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
046600                             MOD-IDDISTR-IN                               
046700                             MOD-TEMFSFEL                                 
046800                             MOD-TEMFSINF                                 
046900                                                                          
047000     MOVE JA  TO SW-VISAS                                                 
047100     MOVE ZERO TO W-KDERS                                                 
047200                  DAGENS-VECKA                                            
047300                  INLEVERANS-VECKA                                        
047400     .                                                                    
047500     EJECT                                                                
047600 C-KOLLA-VILLKOR-RED-BILD SECTION.                                        
047700                                                                          
047800     PERFORM IMS-GET-ARTIKEL-ROT                                          
047900                                                                          
048000     IF SEGMENT-SAKNAS                                                    
048100       MOVE FEL-7 (INDX)       TO MOD-TEMFSFEL                            
048200     ELSE                                                                 
048300       IF ART-FLIART = JA                                                 
048400         IF INDX = +1                                                     
048500           MOVE JA           TO MOD-KDIART                                
048600         ELSE                                                             
048700           MOVE 'Y'          TO MOD-KDIART                                
048800         END-IF                                                           
048900       ELSE                                                               
049000         MOVE SPACE TO MOD-KDIART                                         
049100       END-IF                                                             
049200       MOVE '-'            TO MOD-STRECK                                  
049300       MOVE ART-REKSIFFR   TO MOD-REKSIFFR                                
049400       MOVE ART-TIFINLV    TO MOD-TIFINLV                                 
049500                              W-TIFINLV                                   
049600       MOVE ART-IDFKNGRP   TO MOD-IDFKNGRP                                
049700       MOVE ART-KDPRODSL   TO MOD-KDPRODSL                                
049800                              WS-KDPRODSL                                 
049900       MOVE ART-KDSORT     TO MOD-KDSORT                                  
050000                                                                          
050100       PERFORM CA-DATUM-BERAKNING                                         
050200                                                                          
050300       IF VECKOR-TILL-PUBLICERING > +2                                    
050400         MOVE FEL-9 (INDX) TO MOD-TEMFSFEL                                
050500       END-IF                                                             
050600       MOVE ART-KDERS-UTG TO W-KDERS                                      
050700                                                                          
050800       PERFORM IMS-GNP-ARTC11                                             
050900       IF SEGMENT-FINNS                                                   
051000         MOVE CLAG-FLLSRDEL TO SPAR-FLLSRDEL                              
051100         IF CLAG-KDUART = 'M'                                             
051200           MOVE NEJ TO SW-VISAS                                           
051300         ELSE                                                             
051400           IF BYT02-RENOV                                                 
051500            IF BYT19-BYTES                                                
051600             ADD +6000           TO TEST-IDARTNR                          
051700            END-IF                                                        
051800            IF BYT19-RADIO                                                
051900             ADD +1000           TO TEST-IDARTNR                          
052000            END-IF                                                        
052100             MOVE TEST-IDARTNR   TO W-IDARTNR-BYT                         
052200             PERFORM IMS-GU-ARTC11                                        
052300             IF SEGMENT-FINNS                                             
052400               MOVE OBJ-CLAG-KVPOINT TO MOD-KVPOINT                       
052500             ELSE                                                         
052600               MOVE CLAG-KVPOINT     TO MOD-KVPOINT                       
052700             END-IF                                                       
052800           ELSE                                                           
052900             MOVE CLAG-KVPOINT       TO MOD-KVPOINT                       
053000           END-IF                                                         
053100           MOVE CLAG-IDBERED     TO MOD-IDBERED                           
053200           MOVE CLAG-IDKAT(1)    TO MOD-IDKAT(1)                          
053300           MOVE CLAG-IDKAT(2)    TO MOD-IDKAT(2)                          
053400           MOVE CLAG-IDKAT(3)    TO MOD-IDKAT(3)                          
053500           MOVE CLAG-KDUART      TO MOD-KDUART                            
053600           MOVE CLAG-KVQPACK-0   TO MOD-KVQPACK-0                         
053700           MOVE CLAG-IDSTATNR(3) TO MOD-IDSTATNR                          
053800           MOVE CLAG-VKART      TO MOD-VKART                              
053900           MOVE CLAG-VLARTNTO   TO MOD-VLARTNTO                           
054000           MOVE CLAG-KDFARLIG   TO MOD-KDFARLIG                           
054100           MOVE CLAG-KDBPSR     TO MOD-KDBPSR                             
054200           MOVE CLAG-KDAGE      TO MOD-KDAGE                              
054300           MOVE CLAG-KDARTURS   TO MOD-KDARTURS                           
054400           IF CLAG-FLGEMART = JA                                          
054500              MOVE JA TO WS-FLGEMART                                      
054600              MOVE MED-7 (INDX) TO MOD-TEMFSINF                           
054700           ELSE                                                           
054800              MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                        
054900           END-IF                                                         
055000                                                                          
055100           MOVE CLAG-KDSRA      TO MOD-KDSRA                              
055200           IF INDX = +1                                                   
055300             MOVE CLAG-FLLSRDEL TO MOD-FLLSRDEL                           
055400           ELSE                                                           
055500             IF CLAG-FLLSRDEL = 'J'                                       
055600               MOVE 'Y'         TO MOD-FLLSRDEL                           
055700             ELSE                                                         
055800               MOVE CLAG-FLLSRDEL TO MOD-FLLSRDEL                         
055900             END-IF                                                       
056000           END-IF                                                         
056100           MOVE CLAG-KDERS      TO W-KDERS                                
056200          END-IF                                                          
056300       ELSE                                                               
056400         MOVE MFS-RENSA-FAELT   TO MOD-IDBERED                            
056500                                   MOD-IDKAT(1)                           
056600                                   MOD-IDKAT(2)                           
056700                                   MOD-IDKAT(3)                           
056800                                   MOD-KVQPACK-0                          
056900                                   MOD-KDARTURS                           
057000                                   MOD-VKART                              
057100                                   MOD-VLARTNTO                           
057200                                   MOD-KDFARLIG                           
057300                                   MOD-KDBPSR                             
057400                                   MOD-KDAGE                              
057500                                   MOD-KDSRA                              
057600       END-IF                                                             
057700                                                                          
057800       IF W-KDERS = +52                                                   
057900         MOVE NEJ TO SW-VISAS                                             
058000       ELSE                                                               
058100         EVALUATE TRUE                                                    
058200         WHEN W-KDERS < 21                                                
058300           IF SPAR-FLLSRDEL = 'N'                                         
058400             MOVE NEJ   TO SW-VISAS                                       
058500           ELSE                                                           
058600             MOVE +0    TO MOD-KDERS                                      
058700             MOVE JA    TO FL-FINGERAD-KDERS                              
058800           END-IF                                                         
058900          WHEN OTHER                                                      
059000           MOVE W-KDERS TO MOD-KDERS                                      
059100         END-EVALUATE                                                     
059200       END-IF                                                             
059300                                                                          
059400       PERFORM IMS-GET-ARTIKEL-ROT                                        
059500                                                                          
059600       MOVE +3 TO W-KDNOTTYP                                              
059700       PERFORM IMS-GET-NOTERING                                           
059800       IF SEGMENT-FINNS                                                   
059900         MOVE NOT-TEARTNOT      TO MOD-TEARTNOT-3                         
060000       ELSE                                                               
060100         MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-3                           
060200       END-IF                                                             
060300                                                                          
060400       MOVE +7 TO W-KDNOTTYP                                              
060500       PERFORM IMS-GET-NOTERING                                           
060600       IF SEGMENT-FINNS                                                   
060700         MOVE NOT-TEARTNOT      TO MOD-TEARTNOT-7                         
060800       ELSE                                                               
060900         MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-7                           
061000       END-IF                                                             
061100                                                                          
061200       IF SW-VISAS = JA                                                   
061300                                                                          
061400          PERFORM IMS-GET-GMTA                                            
061500          IF SEGMENT-FINNS                                                
061600             MOVE GMTA-GMT-IDPARTNR        TO W-WDB1-IDPARTNR             
061700             MOVE 57                       TO W-WDB1-IDFTG                
061800             PERFORM IMS-GET-BETC01                                       
061900             IF SEGMENT-FINNS                                             
062000                MOVE WDB1-BET-IDMARKBO   TO W-IDMARKBO                    
062100                PERFORM CB-LAS-FLYTTA-PRIA                                
062200             ELSE                                                         
062300                MOVE ZERO                  TO MOD-KDARTKAM                
062400                MOVE ZERO                  TO MOD-KDARTRAB                
062500                MOVE ZERO                  TO MOD-PRARTBTO-EXP            
062600                MOVE SPACE                 TO MOD-KDVALISO                
062700             END-IF                                                       
062800          ELSE                                                            
062900             MOVE ZERO                     TO MOD-KDARTKAM                
063000             MOVE ZERO                     TO MOD-KDARTRAB                
063100             MOVE ZERO                     TO MOD-PRARTBTO-EXP            
063200             MOVE SPACE                    TO MOD-KDVALISO                
063300          END-IF                                                          
063400                                                                          
063500         PERFORM CC-HAEMTA-UPPG-WDD3                                      
063600                                                                          
063700         PERFORM CD-LAS-KATALOGBET                                        
063800                                                                          
063900         PERFORM IMS-GET-ERSATT                                           
064000         IF SEGMENT-SAKNAS                                                
064100           MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-TILLK                      
064200         ELSE                                                             
064300           IF FL-FINGERAD-KDERS = JA                                      
064400             CONTINUE                                                     
064500           ELSE                                                           
064600             IF ERSATT-KVKORT > +1                                        
064700               IF W-KDERS = +27 OR +28                                    
064800                 IF WS-FLGEMART = JA                                      
064900                    MOVE MED-6-GEMART (INDX) TO MOD-TEMFSINF              
065000                 ELSE                                                     
065100                    MOVE MED-6 (INDX) TO MOD-TEMFSINF                     
065200                 END-IF                                                   
065300               ELSE                                                       
065400                 IF WS-FLGEMART = JA                                      
065500                    MOVE MED-3-GEMART (INDX) TO MOD-TEMFSINF              
065600                 ELSE                                                     
065700                    MOVE MED-3 (INDX) TO MOD-TEMFSINF                     
065800                 END-IF                                                   
065900               END-IF                                                     
066000               MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-TILLK                  
066100             ELSE                                                         
066200               IF W-KDERS = +27 OR +28                                    
066300                 IF WS-FLGEMART = JA                                      
066400                    MOVE MED-5-GEMART (INDX) TO MOD-TEMFSINF              
066500                 ELSE                                                     
066600                    MOVE MED-5 (INDX) TO MOD-TEMFSINF                     
066700                 END-IF                                                   
066800               ELSE                                                       
066900                 IF WS-FLGEMART = JA                                      
067000                    MOVE MED-4-GEMART (INDX) TO MOD-TEMFSINF              
067100                 ELSE                                                     
067200                    MOVE MED-4 (INDX) TO MOD-TEMFSINF                     
067300                  END-IF                                                  
067400               END-IF                                                     
067500               PERFORM IMS-GET-TILLKOMMANDE                               
067600               IF (SEGMENT-FINNS AND                                      
067700                     TILLK-IDARTNR-TILLK NUMERIC)                         
067800                 MOVE TILLK-IDARTNR-TILLK TO                              
067900                                        MOD-IDARTNR-TILLK                 
068000                                        W-IDARTNR                         
068100                 PERFORM IMS-GET-ARTIKEL-ROT                              
068200                 IF SEGMENT-FINNS                                         
068300                   MOVE '-'              TO MOD-STRECK2                   
068400                   MOVE ART-REKSIFFR TO MOD-REKSIFFR-TILLK                
068500                 END-IF                                                   
068600               ELSE                                                       
068700                 MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-TILLK                
068800                                         MOD-STRECK2                      
068900                                         MOD-REKSIFFR-TILLK               
069000               END-IF                                                     
069100             END-IF                                                       
069200           END-IF                                                         
069300         END-IF                                                           
069400       END-IF                                                             
069500     END-IF                                                               
069600     .                                                                    
069700     EJECT                                                                
069800 CA-DATUM-BERAKNING SECTION.                                              
069900                                                                          
070000     MOVE ART-TIERSDAT    TO MOD-TIERSDAT                                 
070100                                                                          
070200     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
070300                                                                          
070400     CALL WDATKONV USING DAT-KDDATFORM                                    
070500                         DAT-I-TIDATUM                                    
070600                         DAT-O-TIDATUM                                    
070700                         DAT-KDSVAR                                       
070800                                                                          
070900     IF DAT-KDSVAR-OK                                                     
071000       MOVE DAT-TIAA      TO TMP1-YY                                      
071100       MOVE W-TIFINLV-AAR TO TMP2-YY                                      
071200       PERFORM WY2000P9                                                   
071300                                                                          
071400       MULTIPLY TMP1-YY BY 52 GIVING DAGENS-VECKA                         
071500       ADD DAT-TIVV TO DAGENS-VECKA                                       
071600                                                                          
071700       MULTIPLY TMP2-YY BY 52 GIVING INLEVERANS-VECKA                     
071800       ADD W-TIFINLV-VECKA TO INLEVERANS-VECKA                            
071900                                                                          
072000       SUBTRACT DAGENS-VECKA FROM INLEVERANS-VECKA                        
072100                GIVING VECKOR-TILL-PUBLICERING                            
072200     ELSE                                                                 
072300       CALL FELLOG                                                        
072400     END-IF                                                               
072500     .                                                                    
072600     EJECT                                                                
072700 CB-LAS-FLYTTA-PRIA SECTION.                                              
072800                                                                          
072900     PERFORM IMS-GET-PRIA                                                 
073000     IF SEGMENT-FINNS                                                     
073100        MOVE PRIA-ART-KDARTKAM       TO MOD-KDARTKAM                      
              IF WDB1-BET-FLARTRAB = 'J'                                        
                MOVE PRIA-ART-KDARTRAB-ALT   TO MOD-KDARTRAB                    
              ELSE                                                              
                MOVE PRIA-ART-KDARTRAB       TO MOD-KDARTRAB                    
              END-IF                                                            
073300        MOVE PRIA-ART-PRARTBTO-MARK  TO MOD-PRARTBTO-EXP                  
073500        MOVE 'SEK'                   TO MOD-KDVALISO                      
073600     ELSE                                                                 
073700        MOVE ZERO                    TO MOD-KDARTKAM                      
073800        MOVE ZERO                    TO MOD-KDARTRAB                      
073900        MOVE ZERO                    TO MOD-PRARTBTO-EXP                  
074000        MOVE SPACE                   TO MOD-KDVALISO                      
074100     END-IF                                                               
074200                                                                          
074300     .                                                                    
074400     EJECT                                                                
074500 CC-HAEMTA-UPPG-WDD3 SECTION.                                             
074600                                                                          
074700     PERFORM IMS-GU-BEN-SEQ                                               
074800                                                                          
074900     PERFORM UNTIL                                                        
075000      NOT ( IX < 6 )                                                      
075100       MOVE IDSKYLT (IX) TO W-IDSKYLT                                     
075200                                                                          
075300       PERFORM IMS-GNP-BEN-SEQ                                            
075400                                                                          
075500       IF IDSKYLT (IX) = 'D  '                                            
075600         MOVE BEN-TEXT-BEART TO MOD-BEART-TYS                             
075700                                                                          
075800       ELSE                                                               
075900         EVALUATE TRUE                                                    
076000         WHEN IDSKYLT (IX) = 'E  '                                        
076100           MOVE BEN-TEXT-BEART TO MOD-BEART-SPA                           
076200                                                                          
076300         WHEN IDSKYLT (IX) = 'F  '                                        
076400           MOVE BEN-TEXT-BEART TO MOD-BEART-FRA                           
076500                                                                          
076600         WHEN IDSKYLT (IX) = 'GB '                                        
076700           MOVE BEN-TEXT-BEART TO MOD-BEART-ENG                           
076800                                                                          
076900         WHEN IDSKYLT (IX) = 'S  '                                        
077000           MOVE BEN-TEXT-BEART TO MOD-BEART-SVE                           
077100         END-EVALUATE                                                     
077200       END-IF                                                             
077300       ADD +1 TO IX                                                       
077400     END-PERFORM                                                          
077500     .                                                                    
077600     EJECT                                                                
077700 CD-LAS-KATALOGBET SECTION.                                               
077800                                                                          
077900     MOVE +0 TO LAESTA-KAT-SEG                                            
078000     SET MOD-IX TO +1                                                     
078100                                                                          
078200     PERFORM IMS-GU-MASTER-ROT                                            
078300     PERFORM UNTIL                                                        
078400      (NOT ( MID-BLADDRING-ANT > LAESTA-KAT-SEG ))                        
078500      OR SEGMENT-SAKNAS                                                   
078600       PERFORM IMS-GET-MASTER-KATINFO                                     
078700       ADD +1 TO LAESTA-KAT-SEG                                           
078800     END-PERFORM                                                          
078900     IF SEGMENT-FINNS                                                     
079000       PERFORM IMS-GET-MASTER-KATINFO                                     
079100       PERFORM UNTIL                                                      
079200        NOT ( MOD-IX NOT > MAX-ANT-BILD-RADER + 1 )                       
079300         IF SEGMENT-FINNS                                                 
079400           IF MOD-IX > MAX-ANT-BILD-RADER                                 
079500             MOVE MED-1 (INDX) TO MOD-MORE                                
079600             MOVE LAESTA-KAT-SEG TO MOD-BLADDRING-ANT                     
079700           ELSE                                                           
079800             MOVE MAST-KAT-BEEMBLEM TO                                    
079900                  MOD-BEEMBLEM (MOD-IX)                                   
080000             ADD +1 TO LAESTA-KAT-SEG                                     
080100             PERFORM IMS-GET-MASTER-KATINFO                               
080200           END-IF                                                         
080300         ELSE                                                             
080400           IF MOD-IX < 29                                                 
080500              MOVE MFS-RENSA-FAELT TO MOD-BEEMBLEM (MOD-IX)               
080600           END-IF                                                         
080700         END-IF                                                           
080800         SET MOD-IX UP BY +1                                              
080900       END-PERFORM                                                        
081000     ELSE                                                                 
081100       PERFORM UNTIL                                                      
081200        NOT ( MOD-IX < +29 )                                              
081300         MOVE MFS-RENSA-FAELT TO MOD-BEEMBLEM (MOD-IX)                    
081400         SET MOD-IX UP BY +1                                              
081500       END-PERFORM                                                        
081600     END-IF                                                               
081700     .                                                                    
081800     EJECT                                                                
081900 D-RENSA-FALT SECTION.                                                    
082000                                                                          
082100     MOVE MFS-RENSA-FAELT TO MOD-KVQPACK-0                                
082200                             MOD-PRARTBTO-EXP                             
082300                             MOD-KDVALISO                                 
082400                             MOD-KDPRODSL                                 
082500                             MOD-IDFKNGRP                                 
082600                             MOD-TIFINLV                                  
082700                             MOD-FLLSRDEL                                 
082800                             MOD-KDUART                                   
082900                             MOD-KDIART                                   
083000                                                                          
083100                             MOD-KDSORT                                   
083200                             MOD-KDSRA                                    
083300                             MOD-KDARTURS                                 
083400                             MOD-IDBERED                                  
083500                             MOD-KDFARLIG                                 
083600                             MOD-KDAGE                                    
083700                             MOD-VKART                                    
083800                             MOD-KDBPSR                                   
083900                                                                          
084000                             MOD-VLARTNTO                                 
084100                             MOD-IDARTNR-TILLK                            
084200                                                                          
084300     IF (SW-VISAS = NEJ) OR (W-KDERS NOT = +29)                           
084400       MOVE MFS-RENSA-FAELT TO MOD-MORE                                   
084500                               MOD-TIERSDAT                               
084600                               MOD-KDERS                                  
084700                               MOD-BEART-SVE                              
084800                               MOD-BEART-ENG                              
084900                               MOD-BEART-TYS                              
085000                               MOD-BEART-FRA                              
085100                               MOD-BEART-SPA                              
085200                                                                          
085300       SET MOD-IX TO +1                                                   
085400       PERFORM UNTIL                                                      
085500        ( MOD-IX > +28 )                                                  
085600         MOVE MFS-RENSA-FAELT TO MOD-BEEMBLEM (MOD-IX)                    
085700         SET MOD-IX UP BY +1                                              
085800       END-PERFORM                                                        
085900       MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-3                             
086000                               MOD-TEARTNOT-7                             
086100       MOVE SPACE           TO MOD-STRECK                                 
086200                               MOD-REKSIFFR                               
086300     END-IF                                                               
086400     .                                                                    
086500     EJECT                                                                
086600                                                                          
086700*E-SPARA-SKARMBILD SECTION.                                               
086800*                                                                         
086900*    MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                             
087000*                              MOD-KVQPACK-0                              
087100*                              MOD-BEART-SVE                              
087200*                              MOD-BEART-ENG                              
087300*                              MOD-BEART-TYS                              
087400*                              MOD-BEART-FRA                              
087500*                              MOD-BEART-SPA                              
087600*                              MOD-PRARTBTO-EXP                           
087700*                              MOD-KDVALISO                               
087800*                              MOD-KDPRODSL                               
087900*                              MOD-IDFKNGRP                               
088000*                              MOD-TIFINLV                                
088100*                              MOD-FLLSRDEL                               
088200*                              MOD-KDUART                                 
088300*                              MOD-KDIART                                 
088400*                                                                         
088500*                                                                         
088600*    MOVE MFS-ROER-EJ-FAELT TO MOD-KDSORT                                 
088700*                              MOD-KDSRA                                  
088800*                              MOD-KDARTURS                               
088900*                              MOD-IDBERED                                
089000*                              MOD-KDFARLIG                               
089100*                              MOD-KDBPSR                                 
089200*                              MOD-VKART                                  
089300*                              MOD-KDAGE                                  
089400*                                                                         
089500*    SET MOD-IX TO +1                                                     
089600*    PERFORM UNTIL                                                        
089700*     ( MOD-IX > +28 )                                                    
089800*      MOVE MFS-ROER-EJ-FAELT TO MOD-BEEMBLEM (MOD-IX)                    
089900*      SET MOD-IX UP BY +1                                                
090000*    END-PERFORM                                                          
090100*    MOVE MFS-ROER-EJ-FAELT TO MOD-MORE                                   
090200*                              MOD-VLARTNTO                               
090300*                              MOD-KDERS                                  
090400*                              MOD-TIERSDAT                               
090500*                              MOD-IDARTNR-TILLK                          
090600*    .                                                                    
090700     EJECT                                                                
093000* IMS SEKTIONER                                                           
093100     SKIP3                                                                
093200 IMS-GET-MSG SECTION.                                                     
093300                                                                          
093400     MOVE '  QC' TO GODK-STATUSKODER                                      
093500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
093600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
093700     PERFORM IMS-STATUSKONTROLL                                           
093800     .                                                                    
093900     SKIP3                                                                
094000 IMS-INSERT-MSG SECTION.                                                  
094100                                                                          
094200     IF ENGLISH-TEXT                                                      
094300       MOVE 'N' TO MFS-KDHUVOMR                                           
094400     END-IF                                                               
094500     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
094600     MOVE SPACE TO GODK-STATUSKODER                                       
094700     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
094800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
094900     PERFORM IMS-STATUSKONTROLL                                           
095000     .                                                                    
095100     EJECT                                                                
095200 IMS-GET-ARTIKEL-ROT SECTION.                                             
095300                                                                          
095400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
095500            DELIMITED BY SIZE INTO SSA1                                   
095600     MOVE '  GE' TO GODK-STATUSKODER                                      
095700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
095800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
095900     PERFORM IMS-STATUSKONTROLL                                           
096000     .                                                                    
096100     SKIP3                                                                
096200 IMS-GNP-ARTC11 SECTION.                                                  
096300                                                                          
096400     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
096500     MOVE '  GE' TO GODK-STATUSKODER                                      
096600     CALL CBLTDLI USING                                                   
096700                    GNP ARTC-PCB DLI-IO-AREA SSA1                         
096800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
096900     PERFORM IMS-STATUSKONTROLL                                           
097000     .                                                                    
097100     SKIP3                                                                
097200 IMS-GU-ARTC11 SECTION.                                                   
097300                                                                          
097400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-BYT-X ')'                     
097500            DELIMITED BY SIZE INTO SSA1                                   
097600     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA2                                 
097700     MOVE '  GE' TO GODK-STATUSKODER                                      
097800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA4 SSA1 SSA2                
097900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
098000     PERFORM IMS-STATUSKONTROLL                                           
098100     .                                                                    
098200     EJECT                                                                
098300 IMS-GET-NOTERING SECTION.                                                
098400                                                                          
098500     MOVE 'WLARTC11(KDSEGKEY =1)' TO SSA1                                 
098600     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
098700            DELIMITED BY SIZE INTO SSA2                                   
098800     MOVE '  GE' TO GODK-STATUSKODER                                      
098900     CALL CBLTDLI USING                                                   
099000                    GNP ARTC-PCB DLI-IO-AREA SSA1 SSA2                    
099100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
099200     PERFORM IMS-STATUSKONTROLL                                           
099300     .                                                                    
099400     EJECT                                                                
099500 IMS-GU-BEN-SEQ SECTION.                                                  
099600                                                                          
099700     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
099800            DELIMITED BY SIZE INTO SSA1                                   
099900     MOVE '  ' TO GODK-STATUSKODER                                        
100000     CALL CBLTDLI USING GU BEN-PCB DLI-IO-AREA SSA1                       
100100     MOVE BEN-STATUS-CODE TO STATUS-WS                                    
100200     PERFORM IMS-STATUSKONTROLL                                           
100300     .                                                                    
100400     SKIP2                                                                
100500 IMS-GNP-BEN-SEQ SECTION.                                                 
100600                                                                          
100700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
100800            DELIMITED BY SIZE INTO SSA1                                   
100900     MOVE '  ' TO GODK-STATUSKODER                                        
101000     CALL CBLTDLI USING GNP BEN-PCB DLI-IO-AREA SSA1                      
101100     MOVE BEN-STATUS-CODE TO STATUS-WS                                    
101200     PERFORM IMS-STATUSKONTROLL                                           
101300     .                                                                    
101400     EJECT                                                                
101500 IMS-GU-MASTER-ROT SECTION.                                               
101600                                                                          
101700     STRING 'WLKATN01(IDARTNR  =' W-IDARTNR-X ')'                         
101800            DELIMITED BY SIZE INTO SSA1                                   
101900     MOVE '  GE' TO GODK-STATUSKODER                                      
102000     CALL CBLTDLI USING GU MAST-PCB DLI-IO-AREA SSA1                      
102100     MOVE MAST-STATUS-CODE TO STATUS-WS                                   
102200     PERFORM IMS-STATUSKONTROLL                                           
102300     .                                                                    
102400     SKIP3                                                                
102500 IMS-GET-MASTER-KATINFO SECTION.                                          
102600                                                                          
102700     MOVE 'WLKATN11 ' TO SSA1                                             
102800     MOVE '  GE' TO GODK-STATUSKODER                                      
102900     CALL CBLTDLI USING GNP MAST-PCB DLI-IO-AREA SSA1                     
103000     MOVE MAST-STATUS-CODE TO STATUS-WS                                   
103100     PERFORM IMS-STATUSKONTROLL                                           
103200     .                                                                    
103300     EJECT                                                                
103400 IMS-GET-ERSATT SECTION.                                                  
103500                                                                          
103600     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
103700            DELIMITED BY SIZE INTO SSA1                                   
103800     MOVE '  GE' TO GODK-STATUSKODER                                      
103900     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA SSA1                      
104000     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
104100     PERFORM IMS-STATUSKONTROLL                                           
104200     .                                                                    
104300     SKIP3                                                                
104400 IMS-GET-TILLKOMMANDE SECTION.                                            
104500                                                                          
104600     MOVE 'WLERSA11 ' TO SSA1                                             
104700     MOVE '  GE' TO GODK-STATUSKODER                                      
104800     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
104900     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
105000     PERFORM IMS-STATUSKONTROLL                                           
105100     .                                                                    
105200 IMS-GET-PRIA SECTION.                                                    
105300     STRING 'WLPRIA01(WDC101KY =' W-WDC101KY-X ')'                        
105400          DELIMITED BY SIZE INTO SSA1                                     
105500     MOVE '  GE' TO GODK-STATUSKODER                                      
105600     CALL CBLTDLI USING GU PRIA-PCB DLI-IO-AREA SSA1                      
105700     MOVE PRIA-STATUS-CODE TO STATUS-WS                                   
105800     PERFORM IMS-STATUSKONTROLL                                           
105900     .                                                                    
106000     EJECT                                                                
106100 IMS-GET-GMTA SECTION.                                                    
106200     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-MIN-X                           
106300                   '&IDGMT   <=' W-IDGMT-MAX-X ')'                        
106400          DELIMITED BY SIZE INTO SSA1                                     
106500     MOVE '  GE' TO GODK-STATUSKODER                                      
106600     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA2 SSA1                     
106700     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
106800     PERFORM IMS-STATUSKONTROLL                                           
106900     .                                                                    
107000     SKIP3                                                                
107100 IMS-GET-BETC01 SECTION.                                                  
107200     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
107300          DELIMITED BY SIZE INTO SSA1                                     
107400     MOVE '  GE' TO GODK-STATUSKODER                                      
107500     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA3 SSA1                     
107600     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
107700     PERFORM IMS-STATUSKONTROLL                                           
107800     .                                                                    
107900     SKIP3                                                                
108000 IMS-STATUSKONTROLL SECTION.                                              
108100     SET STATUS-IX TO 1                                                   
108200     SEARCH GODK-STATUS                                                   
108300       AT END                                                             
108400         CALL FELLOG                                                      
108500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
108600         CONTINUE                                                         
108700     END-SEARCH                                                           
108800     .                                                                    
108900     EJECT                                                                
109000*    -COPY WY2000P9                                                       
