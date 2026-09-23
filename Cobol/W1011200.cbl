000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1011200.                                                
000300*AUTHOR.         BODIL LINDAHL.                                           
000400*DATE-WRITTEN.   APRIL 1985.                                              
000500*    FUNKTION.                                                            
000600*                BYTE SVENSK BENÄMNING.                                   
000700*                BYTE HOMONYMKOD.                                         
000800     SKIP2                                                                
000900*    INDATA.                                                              
001000*        TRANSAKTION: W1T112                                              
001100*                     W1T112U                                             
001200*        MID:         W1I11201                                            
001300*                                                                         
001400*    UTDATA.                                                              
001500*        MOD:         W1O11201                                            
001600*                                                                         
001700*    ÄNDRINGAR.                                                           
001800*      2000-10-30  NYTT SPRÅK (TURKISKA) TILLKOMMER /C.E.                 
001900*      2003-01-09  UPPDAT AV BENA-BEN-FLAENDR VID FÖRÄNDRING /CE          
002000*      2005-06-15  9 nya språk tillkommer. eTracker 2053029.              
002100*                  Förändrad visning av översättningarna krävs för        
002200*                  att få plats med all info på skärmen.                  
002300*                  MFS formaten ändras därför oxå.                        
002400*      2005-10-18  BENA-BEN-FLAENDR skall bara sättas till JA för         
002500*                  produktslagen 11 till 29 vid förändring.               
002600*                                                                         
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP3                                                                
003000 DATA DIVISION.                                                           
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(8)    VALUE 'W1011200'.            
003600 77  JA                          PIC X(1)    VALUE 'J'.                   
003700 77  NEJ                         PIC X(1)    VALUE 'N'.                   
003800 77  REGISTRERAD                 PIC X(1)    VALUE '®'.                   
003900 77  SVENSKA                     PIC X(1)    VALUE 'S'.                   
004000 77  WS-FLRSBEART                PIC X(1)    VALUE SPACE.                 
004100 77  WS-NYTT-NUMMER              PIC S9(7)   VALUE +0   COMP-3.           
004200 77  WS-BEART                    PIC X(25)   VALUE SPACE.                 
004300 77  MAX-RAD                     PIC S9(3)   VALUE +11  COMP-3.           
004400 77  MAX-HOM-PLUS-1              PIC S9(3)   VALUE +3   COMP-3.           
004500 77  MAX-IDSKYLT                 PIC S9(3)   VALUE +26  COMP-3.           
004600 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
004700 77  RAD-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
004800 77  IDSKYLT-IX                  PIC S9(9)   VALUE +0   COMP SYNC.        
004900                                                                          
005000 77  WS-FLAENDR-A                PIC X(1)    VALUE 'N'.                   
005100 77  WS-IDBENNR-A                PIC S9(7)   VALUE ZERO COMP-3.           
005200                                                                          
005300 77  WS-FLAENDR-B                PIC X(1)    VALUE 'N'.                   
005400 77  WS-IDBENNR-B                PIC S9(7)   VALUE ZERO COMP-3.           
005500                                                                          
005600 77  NEVIS-SW                 PIC X       VALUE 'N'.                      
005700     88 NEVIS-ARTIKEL   VALUE 'J'.                                        
005800     88 ANDRA-ARTIKLAR  VALUE 'N'.                                        
005900     SKIP3                                                                
006000                                                                          
006100*01  -COPY WWPRODSL                                                       
006200*      --- VALID IDDC CODES                                               
006300*                                                                         
006400*01    -COPY WWDC99                                                       
006500       EJECT                                                              
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
006800   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
006900   03  W005INIT                  PIC X(8)    VALUE 'W005INIT'.            
007000   03  W006KOM                   PIC X(8)    VALUE 'W006KOM '.            
007100                                                                          
007200 01  WS-IDARTNR                             PIC X(9).                     
007300 01  IDARTNR-WS REDEFINES WS-IDARTNR        PIC 9(9).                     
007400     SKIP2                                                                
007500 01  WS-KDHOMONYM                           PIC X.                        
007600 01  KDHOMONYM-WS REDEFINES WS-KDHOMONYM    PIC 9.                        
007700     SKIP2                                                                
007800 01  INPUT-RETT                        PIC X    VALUE 'J'.                
007900     SKIP2                                                                
008000 01  SUBPROGRAM.                                                          
008100     03 WREVERSE                       PIC X(8) VALUE 'WREVERSE'.         
008200     EJECT                                                                
008300     SKIP3                                                                
008400                                                                          
008500 01  MESSAGE-CODES.                                                       
008600     03  INF-UPDATE-DONE          PIC X(3)    VALUE '101'.                
008700     03  ERR-WRONG-KEY            PIC X(3)    VALUE '401'.                
008800     03  ERR-PART-NUM-NOT-NUMERIC PIC X(3)    VALUE '020'.                
008900     03  ERR-PART-NUM-NOT-FOUND   PIC X(3)    VALUE '769'.                
009000     EJECT                                                                
009100                                                                          
009200 01  IDSKYLT-TABELL.                                                      
009300     03 FILLER   VALUE 'CZ '        PIC X(3).                             
009400     03 FILLER   VALUE 'D  '        PIC X(3).                             
009500     03 FILLER   VALUE 'DK '        PIC X(3).                             
009600     03 FILLER   VALUE 'E  '        PIC X(3).                             
009700     03 FILLER   VALUE 'F  '        PIC X(3).                             
009800     03 FILLER   VALUE 'GB '        PIC X(3).                             
009900     03 FILLER   VALUE 'GR '        PIC X(3).                             
010000     03 FILLER   VALUE 'H  '        PIC X(3).                             
010100     03 FILLER   VALUE 'I  '        PIC X(3).                             
010200     03 FILLER   VALUE 'IR '        PIC X(3).                             
010300     03 FILLER   VALUE 'J  '        PIC X(3).                             
010400     03 FILLER   VALUE 'KOR'        PIC X(3).                             
010500     03 FILLER   VALUE 'MAL'        PIC X(3).                             
010600     03 FILLER   VALUE 'NL '        PIC X(3).                             
010700     03 FILLER   VALUE 'P  '        PIC X(3).                             
010800     03 FILLER   VALUE 'PL '        PIC X(3).                             
010900     03 FILLER   VALUE 'RC '        PIC X(3).                             
011000     03 FILLER   VALUE 'RCN'        PIC X(3).                             
011100     03 FILLER   VALUE 'RO '        PIC X(3).                             
011200     03 FILLER   VALUE 'RUS'        PIC X(3).                             
011300     03 FILLER   VALUE 'S  '        PIC X(3).                             
011400     03 FILLER   VALUE 'SF '        PIC X(3).                             
011500     03 FILLER   VALUE 'T  '        PIC X(3).                             
011600     03 FILLER   VALUE 'TR '        PIC X(3).                             
011700     03 FILLER   VALUE 'USA'        PIC X(3).                             
011800     03 FILLER   VALUE 'YU '        PIC X(3).                             
011900 01  IDSKYLT-TAB REDEFINES IDSKYLT-TABELL.                                
012000     03  FILLER OCCURS 26.                                                
012100      05  WS-IDSKYLT                PIC X(3).                             
012200     SKIP3                                                                
012300 01  DAGENS-DATUM                   PIC 9(6)   VALUE ZERO.                
012400     SKIP2                                                                
012500 01  WS-TEHOMONYM.                                                        
012600     03  BM-RS-NAMN                 PIC X(7).                             
012700     03  FILLER                     PIC X(53).                            
012800     EJECT                                                                
012900*01  -COPY WREVAREA                                                       
013000     EJECT                                                                
013100*                   ****    PARAMETRAR TILL W005INIT                      
013200*01  -COPY WMSGINIT                                                       
013300     EJECT                                                                
013400 01  NYCKLAR-TILL-DLI.                                                    
013500     03  W-IDARTNR-X.                                                     
013600         05  W-IDARTNR            PIC S9(9) COMP-3 VALUE ZERO.            
013700     03  W-IDBENNR-X.                                                     
013800         05  W-IDBENNR            PIC S9(7) COMP-3 VALUE ZERO.            
013900     03  W-IDSKYLT-X.                                                     
014000         05  W-IDSKYLT            PIC X(3)  VALUE SPACE.                  
014100     03  W-BEART-X.                                                       
014200         05  W-BEART              PIC X(25) VALUE SPACE.                  
014300     03  W-1207-KEY-X.                                                    
014400         05  FILLER               PIC X(4)  VALUE '1207'.                 
014500         05  FILLER               PIC X(26) VALUE LOW-VALUE.              
014600     EJECT                                                                
014700 01  MEDDELANDE.                                                          
014800*                                                                         
014900     03  W-FEL-1.                                                         
015000         05  FILLER              PIC X(35)  VALUE                         
015100            'ARTIKELNUMMER EJ NUMERISKT'.                                 
015200         05  FILLER              PIC X(35)  VALUE                         
015300            'PART NUMBER NOT NUMERIC   '.                                 
015400     03  FILLER REDEFINES W-FEL-1.                                        
015500         05  FEL-1               PIC X(35) OCCURS 2.                      
015600                                                                          
015700     03  W-FEL-2.                                                         
015800         05  FILLER              PIC X(35) VALUE                          
015900            'BENÄMNING SAKNAS PÅ BENREG'.                                 
016000         05  FILLER              PIC X(35) VALUE                          
016100            'DESCRIPTION IS MISSING    '.                                 
016200     03  FILLER REDEFINES W-FEL-2.                                        
016300         05  FEL-2               PIC X(35) OCCURS 2.                      
016400                                                                          
016500     03  W-FEL-3.                                                         
016600         05  FILLER             PIC X(35) VALUE                           
016700            'ARTIKELNUMMER SAKNAS               '.                        
016800         05  FILLER             PIC X(35) VALUE                           
016900            'THIS PART IS NOT IN THE DATABASE'.                           
017000     03  FILLER REDEFINES W-FEL-3.                                        
017100         05  FEL-3              PIC X(35) OCCURS 2.                       
017200                                                                          
017300     03  W-FEL-4.                                                         
017400         05  FILLER             PIC X(35)  VALUE                          
017500            'SÖKT HOMONYMKOD SAKNAS'.                                     
017600         05  FILLER             PIC X(35)  VALUE                          
017700            'HOM.CODE IS MISSING   '.                                     
017800     03  FILLER REDEFINES W-FEL-4.                                        
017900         05  FEL-4              PIC X(35)  OCCURS 2.                      
018000                                                                          
018100     03  W-FEL-5.                                                         
018200         05  FILLER             PIC X(35)  VALUE                          
018300            'FRI TEXT FINNS - KORRIGERA'.                                 
018400         05  FILLER             PIC X(35)  VALUE                          
018500            'HOM.CODE EXISTS  VERIFY   '.                                 
018600     03  FILLER REDEFINES W-FEL-5.                                        
018700         05  FEL-5             PIC X(35)   OCCURS 2.                      
018800                                                                          
018900     03  W-FEL-6.                                                         
019000         05  FILLER            PIC X(36)   VALUE                          
019100            'SÖKT BENÄMNING OCH HOMONYMKOD SAKNAS'.                       
019200         05  FILLER            PIC X(36)   VALUE                          
019300            'HOM.CODE IS MISSING                 '.                       
019400     03  FILLER REDEFINES W-FEL-6.                                        
019500         05  FEL-6             PIC X(36) OCCURS 2.                        
019600                                                                          
019700     03  W-MED-1.                                                         
019800         05   FILLER                  PIC X(36) VALUE                     
019900              'UPPDATERING GJORD'.                                        
020000         05   FILLER                  PIC X(36) VALUE                     
020100              'UPDATED          '.                                        
020200     03  FILLER REDEFINES W-MED-1.                                        
020300         05   MED-1                   PIC X(36) OCCURS 2.                 
020400                                                                          
020500     03  W-MED-2.                                                         
020600         05  FILLER                   PIC X(40) VALUE                     
020700            'EJ NAMNLEX BENÄMNING                  '.                     
020800         05  FILLER                   PIC X(40) VALUE                     
020900            'THIS DESCR.IS NOT A NAMNLEX DESCRIPTION'.                    
021000     03  FILLER REDEFINES W-MED-2.                                        
021100         05  MED-2                    PIC X(40) OCCURS 2.                 
021200                                                                          
021300     03  W-MED-3.                                                         
021400         05  FILLER                   PIC X(36) VALUE                     
021500            'UPPLYSTA FÄLT FEL           '.                               
021600         05  FILLER                   PIC X(36) VALUE                     
021700            'HIGH LIGHTED FIELD INCORRECT'.                               
021800     03  FILLER REDEFINES W-MED-3.                                        
021900         05  MED-3                    PIC X(36) OCCURS 2.                 
022000                                                                          
022100     03  W-MED-4.                                                         
022200         05  FILLER                   PIC X(36) VALUE                     
022300            'UPPDATERING EJ TILLÅTEN     '.                               
022400         05  FILLER                   PIC X(36) VALUE                     
022500            'UPDATE NOT ALLOWED          '.                               
022600     03  FILLER REDEFINES W-MED-4.                                        
022700         05  MED-4                    PIC X(36) OCCURS 2.                 
022800                                                                          
022900     EJECT                                                                
023000*                        ****    MFS OCH SKÄRMHANTERING                   
023100 01  FILLER              PIC X(16)   VALUE 'MFS-WS'.                      
023200     SKIP2                                                                
023300*01  MID -COPY W1I11201                                                   
023400     EJECT                                                                
023500*01  -COPY WMSGAREA                                                       
023600     EJECT                                                                
023700*    03  MOD -COPY W1O11201  -RED MSG-AREA.                               
023800     EJECT                                                                
023900*01  -COPY WMFSAREA.                                                      
024000     EJECT                                                                
024100******************************************************************        
024200*****                                                                     
024300*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024400*****                                                                     
024500 01  IMS-WS.                                                              
024600     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
024700     SKIP3                                                                
024800*****                    **** STATUS-KOD FRÅN IMS                         
024900     03  STATUS-WS               PIC X(2).                                
025000         88  SEGMENT-FINNS                   VALUE '  '.                  
025100         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
025200     SKIP3                                                                
025300     03  GODK-STATUSKODER.                                                
025400         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
025500     SKIP3                                                                
025600 01  SSA1                        PIC X(128).                              
025700 01  SSA2                        PIC X(128).                              
025800     EJECT                                                                
025900*                            IMS FUNKTIONSKODER                           
026000*01  -COPY W0003                                                          
026100     EJECT                                                                
026200*                            DLI INPUT-OUTPUT AREA                        
026300 01  DLI-IO-AREA.                                                         
026400     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
026500     SKIP3                                                                
026600*    03  WLBENA  -COPY WDD301   -PRE BENA-  -RED IO-AREA.                 
026700     EJECT                                                                
026800*    03  WLBENA  -COPY WDD311   -PRE BENA-  -RED IO-AREA.                 
026900     EJECT                                                                
027000*    03  WLBENA  -COPY WDD312   -PRE BENA-  -RED IO-AREA.                 
027100*    03  WLBENA  -COPY WDD313   -PRE BENA-  -RED IO-AREA.                 
027200     EJECT                                                                
027300*    03  WLXXAI  -COPY WDGX1208 -PRE XXAI-  -RED IO-AREA.                 
027400     EJECT                                                                
027500*                            DLI INPUT-OUTPUT AREA-2                      
027600 01  DLI-IO-AREA-2.                                                       
027700     03  IO-AREA-2               PIC X(200)  VALUE SPACE.                 
027800     SKIP3                                                                
027900*    03  WLARTC  -COPY WDK601               -RED IO-AREA-2.               
028000     EJECT                                                                
028100*    --- AREA FÖR KOMMUNIKATION MED DISPATCHER                            
028200*                                                                         
028300 01  FILLER                 PIC X(16)   VALUE 'KOM-DISP-IO-AREA'.         
028400     SKIP3                                                                
028500 01  KOM-IO-AREA.                                                         
028600*    03  -COPY WMSGKOM                                                    
028700     EJECT                                                                
028800 LINKAGE SECTION.                                                         
028900     SKIP2                                                                
029000*01  -COPY W0009     -PRE MSG-                                            
029100     EJECT                                                                
029200*01  -COPY W0009     -PRE ALT-                                            
029300     EJECT                                                                
029400*01  -COPY W0008     -PRE USEA-                                           
029500         05  FILLER              PIC X.                                   
029600     EJECT                                                                
029700*01  -COPY W0008     -PRE BENA-                                           
029800         05  FILLER              PIC X.                                   
029900     EJECT                                                                
030000*01  -COPY W0008     -PRE BENB-                                           
030100         05  FILLER              PIC X.                                   
030200     EJECT                                                                
030300*01  -COPY W0008     -PRE BENC-                                           
030400         05  FILLER              PIC X.                                   
030500     EJECT                                                                
030600*01  -COPY W0008     -PRE XXAI-                                           
030700         05  FILLER              PIC X.                                   
030800     EJECT                                                                
030900*01  -COPY W0008     -PRE ARTC-                                           
031000         05  FILLER              PIC X.                                   
031100     EJECT                                                                
031200 PROCEDURE DIVISION USING MSG-PCB ALT-PCB USEA-PCB                        
031300                                  BENA-PCB BENB-PCB BENC-PCB              
031400                                  XXAI-PCB ARTC-PCB.                      
031500 MAIN SECTION.                                                            
031600     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB                       
031700                                   BENA-PCB BENB-PCB BENC-PCB             
031800                                   XXAI-PCB ARTC-PCB.                     
031900     PERFORM IMS-GET-MSG                                                  
032000     IF SEGMENT-FINNS                                                     
032100       PERFORM A-INIT-SPARA-INPUT                                         
032200       IF MFS-UPD-X                                                       
032300         PERFORM IMS-GN-MSG-KOM                                           
032400       END-IF                                                             
032500       IF WS-IDARTNR NUMERIC                                              
032600         IF MFS-UPDATE OR MFS-UPD-X                                       
032700           PERFORM E-ROER-EJ-FAELT                                        
032800           PERFORM D-KOLLA-SKAERMEN                                       
032900           IF INPUT-RETT = JA                                             
033000             PERFORM B-UPPDATERA                                          
033100           END-IF                                                         
033200         ELSE                                                             
033300           PERFORM C-LAS-BASEN                                            
033400         END-IF                                                           
033500       ELSE                                                               
033600         MOVE FEL-1(IX)     TO MOD-TEMFSFEL                               
033700         MOVE ERR-PART-NUM-NOT-NUMERIC                                    
033800                            TO MSG-KOM-IDMFSMED                           
033900       END-IF                                                             
034000       MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                  
034100       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
034200       IF MFS-UPD-X                                                       
034300         IF MSG-KOM-IDMFSMED = SPACE                                      
034400           MOVE INF-UPDATE-DONE TO MSG-KOM-IDMFSMED                       
034500         END-IF                                                           
034600         MOVE SPACE             TO MSG-KOM-KDSVAR                         
034700         PERFORM IMS-INSERT-MSG-KOM                                       
034800       ELSE                                                               
034900         COMPUTE MSG-KVLL = 4 + ( LENGTH OF MOD-W1O11201 )                
035000         PERFORM IMS-INSERT-MSG                                           
035100       END-IF                                                             
035200     END-IF                                                               
035300     MOVE ZERO TO RETURN-CODE                                             
035400     GOBACK                                                               
035500     .                                                                    
035600     EJECT                                                                
035700 A-INIT-SPARA-INPUT SECTION.                                              
035800     SKIP2                                                                
035900     IF MSG-DUBBLA-TRANSKODER                                             
036000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I11201                 
036100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
036200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
036300     ELSE                                                                 
036400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W1I11201                  
036500       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
036600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
036700     END-IF                                                               
036800     IF MFS-IDTRANS = '1112'                                              
036900       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
037000     ELSE                                                                 
037100       MOVE SPACE TO MFS-KDTRTYP                                          
037200     END-IF                                                               
037300                                                                          
037400     MOVE ALL '+' TO MSGI-WMSGINIT                                        
037500     MOVE '001'             TO MSGI-KDCALL                                
037600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
037700     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
037800     MOVE '1112'            TO MSGI-IDTRANS                               
037900     IF MFS-IDTRANS = '1112'                                              
038000     OR (MID-IDARTNR-IN NUMERIC                                           
038100     AND MID-IDARTNR-IN > ZERO)                                           
038200         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
038300     END-IF                                                               
038400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
038500     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
038600     MOVE MSGI-IDDC    TO WS-IDDC                                         
038700     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
038800                                                                          
038900     IF MID-IDARTNR-IN = ALL '+' OR SPACE                                 
039000        CONTINUE                                                          
039100     ELSE                                                                 
039200       IF MSG-KDTRANS-1 NOT = 'W1T112X'                                   
039300         MOVE SPACE TO MFS-KDTRTYP                                        
039400       END-IF                                                             
039500     END-IF                                                               
039600                                                                          
039700     IF MSGI-IDLAND-SPR = 'SE'                                            
039800       MOVE +1 TO IX                                                      
039900       MOVE 'W1O11201' TO MFS-IDMOD                                       
040000     ELSE                                                                 
040100       MOVE +2 TO IX                                                      
040200       MOVE 'W1O112N1' TO MFS-IDMOD                                       
040300     END-IF                                                               
040400     MOVE LOW-VALUE TO MOD-W1O11201                                       
040500     MOVE '1112' TO MOD-IDTRANS                                           
040600                                                                          
040700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
040800                             MOD-TEMFSINF                                 
040900                             MOD-IDARTNR-IN                               
041000                             MOD-BEART-NY                                 
041100                             MOD-KDHOMONYM-NY                             
041200                             MOD-FLRSBEART                                
041300     .                                                                    
041400     EJECT                                                                
041500 B-UPPDATERA SECTION.                                                     
041600     SKIP2                                                                
041700     ACCEPT DAGENS-DATUM FROM DATE                                        
041800                                                                          
041900     MOVE IDARTNR-WS TO W-IDARTNR                                         
042000*    --- LÄSER ARTIKELNS BENA01 MED BENC-PCB                              
042100     PERFORM IMS-GET-BENA01-BSEQ                                          
042200     IF SEGMENT-FINNS                                                     
042300       IF WS-BEART = SPACE                                                
042400         PERFORM BA-UPPDATERA-KDHOMONYM                                   
042500       ELSE                                                               
042600         EVALUATE TRUE                                                    
042700         WHEN WS-KDHOMONYM = SPACE                                        
042800            PERFORM BB-UPPDATERA-BEART                                    
042900         WHEN OTHER                                                       
043000            PERFORM BC-UPPDATERA-KDHOM-BEART                              
043100         END-EVALUATE                                                     
043200       END-IF                                                             
043300                                                                          
043400*      --- KOLLA WS-FLAENDR FÖR UPPDATERING AV BENA-BEN-FLAENDR           
043500*      --- Denna sätts enbart om Switchen NEVIS-SW har värdet             
043600*      --- för NEVIS-ARTIKEL (D.v.s.=JA)                                  
043700*                                                                         
043800       IF WS-FLAENDR-A = JA                                               
043900         MOVE WS-IDBENNR-A TO W-IDBENNR                                   
044000         PERFORM IMS-GHU-BENA01                                           
044100         MOVE JA           TO BENA-BEN-FLAENDR                            
044200         PERFORM IMS-REPL-BENA01                                          
044300       END-IF                                                             
044400       IF WS-FLAENDR-B = JA                                               
044500         MOVE WS-IDBENNR-B TO W-IDBENNR                                   
044600         PERFORM IMS-GHU-BENA01                                           
044700         MOVE JA           TO BENA-BEN-FLAENDR                            
044800         PERFORM IMS-REPL-BENA01                                          
044900       END-IF                                                             
045000     ELSE                                                                 
045100       MOVE FEL-3(IX)      TO MOD-TEMFSFEL                                
045200       MOVE ERR-PART-NUM-NOT-FOUND                                        
045300                           TO MSG-KOM-IDMFSMED                            
045400     END-IF                                                               
045500     .                                                                    
045600     EJECT                                                                
045700 BA-UPPDATERA-KDHOMONYM SECTION.                                          
045800     SKIP2                                                                
045900*    -- WDD301 ÄR LÄST FÖR ARTIKELN                                       
046000     MOVE BENA-BEN-IDBENNR TO WS-IDBENNR-A                                
046100                                                                          
046200*    -- LÄS DEN SVENSKA TEXTEN                                            
046300     MOVE SVENSKA          TO W-IDSKYLT                                   
046400     PERFORM IMS-GU-BENA11-BSEQ                                           
046500                                                                          
046600     MOVE BENA-TEXT-BEART TO W-BEART                                      
046700     PERFORM IMS-GET-BENA01-ASEQ                                          
046800*    -- LÄS FRAM DEN NYA WDD301 MED DEN NYA HOMONYMKODEN                  
046900     PERFORM UNTIL SEGMENT-SAKNAS                                         
047000                OR BENA-BEN-KDHOMONYM = KDHOMONYM-WS                      
047100       PERFORM IMS-GET-BENA01-ASEQ                                        
047200     END-PERFORM                                                          
047300     IF SEGMENT-FINNS                                                     
047400*      -- SPARA DET NYA BENÄMNINGSNUMRET                                  
047500       MOVE BENA-BEN-IDBENNR TO W-IDBENNR  WS-IDBENNR-B                   
047600                                                                          
047700*      -- LÄS NU DET GAMLA BENÄMNINGSNUMRET OCH TAG BORT ARTIKELN         
047800       PERFORM IMS-GET-BENA01-BSEQ                                        
047900       PERFORM IMS-GET-BENA12-BSEQ                                        
048000       PERFORM IMS-DLET-BENA12-BSEQ                                       
048100       IF NEVIS-ARTIKEL                                                   
048200         MOVE JA TO WS-FLAENDR-A                                          
048300       END-IF                                                             
048400                                                                          
048500*      -- LÄS NU DET NYA BENÄMNINGSNUMRET OCH SKRIV UT PÅ SKÄRM           
048600       PERFORM IMS-GET-BENA01                                             
048700       PERFORM IMS-GET-BENA11                                             
048800       PERFORM UNTIL NOT SEGMENT-FINNS                                    
048900         PERFORM S01-FLYTTA-BEART                                         
049000         PERFORM IMS-GET-BENA11                                           
049100       END-PERFORM                                                        
049200       PERFORM S02-FLYTTA-HOM                                             
049300                                                                          
049400*      -- FLYTTA ARTIKELN TILL DET NYA BENÄMNINGSNUMRET                   
049500       MOVE IDARTNR-WS TO BENA-ART-IDARTNR                                
049600       MOVE NEJ        TO BENA-ART-FLFELHOMO                              
049700       MOVE WS-KDHOMONYM TO MOD-KDHOMONYM                                 
049800       PERFORM IMS-ISRT-BENA12                                            
049900       IF NEVIS-ARTIKEL                                                   
050000         MOVE JA TO WS-FLAENDR-B                                          
050100       END-IF                                                             
050200       MOVE MED-1(IX)          TO MOD-TEMFSINF                            
050300     ELSE                                                                 
050400       MOVE FEL-4(IX)          TO MOD-TEMFSFEL                            
050500       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDHOMONYM-ATTR                      
050600     END-IF                                                               
050700     .                                                                    
050800     EJECT                                                                
050900 BB-UPPDATERA-BEART SECTION.                                              
051000     SKIP2                                                                
051100*    -- WDD301 ÄR LÄST FÖR ARTIKELN                                       
051200     MOVE BENA-BEN-IDBENNR TO WS-IDBENNR-A                                
051300                                                                          
051400*    -- LÄS FRAM DEN NYA WDD301 MED DEN NYA BENÄMNINGEN                   
051500     MOVE WS-BEART TO W-BEART                                             
051600     MOVE SVENSKA TO W-IDSKYLT                                            
051700     PERFORM IMS-GET-BENA01-ASEQ                                          
051800     PERFORM UNTIL SEGMENT-SAKNAS OR BENA-BEN-KDHOMONYM = 0               
051900       PERFORM IMS-GET-BENA01-ASEQ                                        
052000     END-PERFORM                                                          
052100     IF SEGMENT-FINNS                                                     
052200*      -- SPARA NYA BENÄMNINGSNUMRET                                      
052300       MOVE BENA-BEN-IDBENNR TO W-IDBENNR                                 
052400                                WS-IDBENNR-B                              
052500       IF WS-FLRSBEART = JA                                               
052600         PERFORM BBA-UTAN-HOM                                             
052700       ELSE                                                               
052800         PERFORM IMS-GET-BENA13-ASEQ                                      
052900         IF SEGMENT-FINNS                                                 
053000           MOVE BENA-HOM-TEHOMONYM TO WS-TEHOMONYM                        
053100           IF BM-RS-NAMN = 'BM-NAMN' OR 'RS-NAMN'                         
053200             PERFORM BBA-UTAN-HOM                                         
053300           ELSE                                                           
053400             PERFORM BBB-MED-HOM                                          
053500           END-IF                                                         
053600         ELSE                                                             
053700           PERFORM BBA-UTAN-HOM                                           
053800         END-IF                                                           
053900       END-IF                                                             
054000     ELSE                                                                 
054100       IF WS-FLRSBEART = JA                                               
054200         PERFORM BBE-REG-RSUNIK                                           
054300       ELSE                                                               
054400         MOVE FEL-2(IX)          TO MOD-TEMFSFEL                          
054500         MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-NY-ATTR                     
054600       END-IF                                                             
054700     END-IF                                                               
054800     .                                                                    
054900     EJECT                                                                
055000 BBA-UTAN-HOM SECTION.                                                    
055100     SKIP2                                                                
055200*    -- TAG BORT ARTIKELN FRÅN DEN GAMLA BENÄMNINGEN                      
055300     PERFORM IMS-GET-BENA01-BSEQ                                          
055400     PERFORM IMS-GET-BENA12-BSEQ                                          
055500     PERFORM IMS-DLET-BENA12-BSEQ                                         
055600     IF NEVIS-ARTIKEL                                                     
055700       MOVE JA TO WS-FLAENDR-A                                            
055800     END-IF                                                               
055900                                                                          
056000*    -- LÄS DEN NYA BENÄMNINGEN                                           
056100     PERFORM IMS-GET-BENA01                                               
056200     MOVE BENA-BEN-KDHOMONYM TO MOD-KDHOMONYM                             
056300     PERFORM IMS-GET-BENA11                                               
056400     PERFORM UNTIL SEGMENT-SAKNAS                                         
056500       PERFORM S01-FLYTTA-BEART                                           
056600       PERFORM IMS-GET-BENA11                                             
056700     END-PERFORM                                                          
056800     PERFORM S02-FLYTTA-HOM                                               
056900*    -- LÄGG TILL ARTIKELN FÖR DEN NYA BENÄMNINGEN                        
057000     MOVE IDARTNR-WS TO BENA-ART-IDARTNR                                  
057100     MOVE NEJ        TO BENA-ART-FLFELHOMO                                
057200     PERFORM IMS-ISRT-BENA12                                              
057300     IF NEVIS-ARTIKEL                                                     
057400       MOVE JA TO WS-FLAENDR-B                                            
057500     END-IF                                                               
057600     MOVE MED-1(IX) TO MOD-TEMFSINF                                       
057700     .                                                                    
057800     EJECT                                                                
057900 BBB-MED-HOM SECTION.                                                     
058000     SKIP2                                                                
058100*    -- TAG BORT ARTIKELN FRÅN DEN GAMLA BENÄMNINGEN                      
058200     PERFORM IMS-GET-BENA01-BSEQ                                          
058300     PERFORM IMS-GET-BENA12-BSEQ                                          
058400     PERFORM IMS-DLET-BENA12-BSEQ                                         
058500     IF NEVIS-ARTIKEL                                                     
058600       MOVE JA TO WS-FLAENDR-A                                            
058700     END-IF                                                               
058800                                                                          
058900*    -- LÄS DEN NYA BENÄMNINGEN                                           
059000     PERFORM IMS-GET-BENA01                                               
059100     PERFORM IMS-GET-BENA11                                               
059200     PERFORM UNTIL SEGMENT-SAKNAS                                         
059300       PERFORM S01-FLYTTA-BEART                                           
059400       PERFORM IMS-GET-BENA11                                             
059500     END-PERFORM                                                          
059600     PERFORM S02-FLYTTA-HOM                                               
059700*    -- LÄGG TILL ARTIKELN FÖR DEN NYA BENÄMNINGEN                        
059800     MOVE IDARTNR-WS TO BENA-ART-IDARTNR                                  
059900     MOVE JA TO BENA-ART-FLFELHOMO                                        
060000     MOVE SPACE TO MOD-KDHOMONYM                                          
060100     PERFORM IMS-ISRT-BENA12                                              
060200     IF NEVIS-ARTIKEL                                                     
060300       MOVE JA TO WS-FLAENDR-B                                            
060400     END-IF                                                               
060500     MOVE MED-1(IX)     TO MOD-TEMFSINF                                   
060600     MOVE FEL-5(IX)     TO MOD-TEMFSFEL                                   
060700     .                                                                    
060800     EJECT                                                                
060900 BBE-REG-RSUNIK SECTION.                                                  
061000     SKIP2                                                                
061100     PERFORM IMS-GET-XXAI01                                               
061200     PERFORM IMS-GET-XXAI11                                               
061300*    MOVE XXAI-1208-IDBENNR TO WS-NYTT-NUMMER                             
061400*    ADD +1 TO WS-NYTT-NUMMER                                             
061500     ADD +1 TO XXAI-1208-IDBENNR                                          
061600     IF XXAI-1208-IDBENNR  > XXAI-1208-IDBENNR-MAX                        
061700       MOVE +1 TO XXAI-1208-IDBENNR                                       
061800     END-IF                                                               
061900     MOVE XXAI-1208-IDBENNR TO W-IDBENNR                                  
062000                               XXAI-1208-IDBENNR                          
062100     PERFORM IMS-REPL-XXAI11                                              
062200                                                                          
062300     MOVE XXAI-1208-IDBENNR TO BENA-BEN-IDBENNR                           
062400     MOVE ZERO              TO BENA-BEN-KDHOMONYM                         
062500                               MOD-KDHOMONYM                              
062600     MOVE ZERO              TO BENA-BEN-TIUPPDAT-STOP                     
062700     MOVE +0                TO BENA-BEN-KDBENSTAT                         
062800     IF NEVIS-ARTIKEL                                                     
062900       MOVE JA              TO BENA-BEN-FLAENDR                           
063000     END-IF                                                               
063100     PERFORM IMS-ISRT-BENA01                                              
063200                                                                          
063300     MOVE +1 TO IDSKYLT-IX                                                
063400*      --- ALLA GODK WDD3-SPRÅK SKALL HA ETT 11-SEGMENT                   
063500     PERFORM UNTIL IDSKYLT-IX > MAX-IDSKYLT                               
063600       MOVE WS-IDSKYLT(IDSKYLT-IX) TO BENA-TEXT-IDSKYLT                   
063700       MOVE SPACE                  TO BENA-TEXT-BEARTEXT                  
063800       MOVE DAGENS-DATUM           TO BENA-TEXT-TIUPPDAT                  
063900       IF WS-IDSKYLT(IDSKYLT-IX) = 'S  '  OR                              
064000          (KDPRODSL-BIMA                                                  
064100                        AND                                               
064200           WS-IDSKYLT(IDSKYLT-IX) = 'GB ')                                
064300         MOVE JA                  TO BENA-TEXT-FLOVERSATT                 
064400         MOVE WS-BEART            TO BENA-TEXT-BEARTEXT                   
064500       ELSE                                                               
064600         MOVE NEJ                 TO BENA-TEXT-FLOVERSATT                 
064700       END-IF                                                             
064800       PERFORM IMS-ISRT-BENA11                                            
064900       PERFORM S01-FLYTTA-BEART                                           
065000       ADD +1 TO IDSKYLT-IX                                               
065100     END-PERFORM                                                          
065200                                                                          
065300     MOVE SPACE TO REV-TETEXT                                             
065400     MOVE WS-BEART TO REV-TETEXT                                          
065500     CALL WREVERSE USING REV-TETEXT                                       
065600                                                                          
065700     IF KDPRODSL-BIMA                                                     
065800       MOVE REV-TETEXT   TO BENA-TEXT-BEARTEXT                            
065900     ELSE                                                                 
066000       MOVE SPACE        TO BENA-TEXT-BEARTEXT                            
066100     END-IF                                                               
066200                                                                          
066300     MOVE ' BG'          TO BENA-TEXT-IDSKYLT                             
066400     MOVE DAGENS-DATUM   TO BENA-TEXT-TIUPPDAT                            
066500     MOVE NEJ            TO BENA-TEXT-FLOVERSATT                          
066600     PERFORM IMS-ISRT-BENA11                                              
066700                                                                          
066800     MOVE '  S'          TO BENA-TEXT-IDSKYLT                             
066900     MOVE REV-TETEXT     TO BENA-TEXT-BEARTEXT                            
067000     MOVE DAGENS-DATUM   TO BENA-TEXT-TIUPPDAT                            
067100     MOVE JA             TO BENA-TEXT-FLOVERSATT                          
067200     PERFORM IMS-ISRT-BENA11                                              
067300                                                                          
067400     PERFORM IMS-GET-BENA01-BSEQ                                          
067500     PERFORM IMS-GET-BENA12-BSEQ                                          
067600     PERFORM IMS-DLET-BENA12-BSEQ                                         
067700                                                                          
067800     MOVE IDARTNR-WS TO BENA-ART-IDARTNR                                  
067900     MOVE NEJ TO        BENA-ART-FLFELHOMO                                
068000     PERFORM IMS-ISRT-BENA12                                              
068100     MOVE MED-1(IX) TO MOD-TEMFSINF                                       
068200     .                                                                    
068300     EJECT                                                                
068400 BC-UPPDATERA-KDHOM-BEART SECTION.                                        
068500     SKIP2                                                                
068600*    -- WDD301 ÄR LÄST FÖR ARTIKELN                                       
068700     MOVE BENA-BEN-IDBENNR TO WS-IDBENNR-A                                
068800                                                                          
068900     MOVE WS-BEART TO W-BEART                                             
069000     MOVE SVENSKA TO W-IDSKYLT                                            
069100*    -- LÄS DEN NYA BENÄMNINGENS ROT                                      
069200     PERFORM IMS-GET-BENA01-ASEQ                                          
069300     IF SEGMENT-FINNS                                                     
069400       PERFORM UNTIL SEGMENT-SAKNAS                                       
069500                  OR BENA-BEN-KDHOMONYM = KDHOMONYM-WS                    
069600         PERFORM IMS-GET-BENA01-ASEQ                                      
069700       END-PERFORM                                                        
069800       IF SEGMENT-FINNS                                                   
069900*        -- SPARA NYA BENÄMNINGSNUMRET                                    
070000         MOVE BENA-BEN-IDBENNR TO W-IDBENNR                               
070100                                  WS-IDBENNR-B                            
070200*        -- TAG FÖRST BORT ARTIKELN FRÅN GAMLA BENÄMNINGSNUMRET           
070300         PERFORM IMS-GET-BENA01-BSEQ                                      
070400         PERFORM IMS-GET-BENA12-BSEQ                                      
070500         PERFORM IMS-DLET-BENA12-BSEQ                                     
070600                                                                          
070700*        -- LÄS DEN NYA BENÄMNINGEN                                       
070800         PERFORM IMS-GET-BENA01                                           
070900         PERFORM IMS-GET-BENA11                                           
071000         PERFORM UNTIL SEGMENT-SAKNAS                                     
071100           PERFORM S01-FLYTTA-BEART                                       
071200           PERFORM IMS-GET-BENA11                                         
071300         END-PERFORM                                                      
071400         PERFORM S02-FLYTTA-HOM                                           
071500                                                                          
071600*        -- LÄGG TILL ARTIKELN FÖR DEN NYA BENÄMNINGEN                    
071700         MOVE IDARTNR-WS TO BENA-ART-IDARTNR                              
071800         MOVE NEJ        TO BENA-ART-FLFELHOMO                            
071900         MOVE WS-KDHOMONYM TO MOD-KDHOMONYM                               
072000         PERFORM IMS-ISRT-BENA12                                          
072100         IF NEVIS-ARTIKEL                                                 
072200           MOVE JA TO WS-FLAENDR-B                                        
072300         END-IF                                                           
072400         MOVE MED-1(IX) TO MOD-TEMFSINF                                   
072500       ELSE                                                               
072600         MOVE FEL-4(IX) TO MOD-TEMFSFEL                                   
072700         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDHOMONYM-ATTR                    
072800       END-IF                                                             
072900     ELSE                                                                 
073000       MOVE FEL-6(IX) TO MOD-TEMFSFEL                                     
073100       MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-NY-ATTR                       
073200                                  MOD-KDHOMONYM-ATTR                      
073300     END-IF                                                               
073400     .                                                                    
073500     EJECT                                                                
073600 C-LAS-BASEN SECTION.                                                     
073700     SKIP2                                                                
073800     MOVE IDARTNR-WS TO W-IDARTNR                                         
073900     PERFORM IMS-GET-BENA01-BSEQ                                          
074000     IF SEGMENT-FINNS                                                     
074100       MOVE BENA-BEN-IDBENNR TO W-IDBENNR                                 
074200       IF BENA-BEN-KDHOMONYM = ZERO                                       
074300         MOVE SPACE TO MOD-KDHOMONYM                                      
074400       ELSE                                                               
074500         MOVE BENA-BEN-KDHOMONYM TO MOD-KDHOMONYM                         
074600       END-IF                                                             
074700       IF BENA-BEN-KDBENSTAT = 1 OR 2                                     
074800         MOVE MED-2(IX) TO MOD-TEMFSFEL                                   
074900       END-IF                                                             
075000       PERFORM IMS-GET-BENA12-BSEQ                                        
075100       IF BENA-ART-FLFELHOMO = JA                                         
075200         MOVE FEL-5(IX) TO MOD-TEMFSFEL                                   
075300       ELSE                                                               
075400         EVALUATE TRUE                                                    
075500             WHEN BENA-ART-FLFELHOMO = NEJ AND MOD-KDHOMONYM =            
075600            SPACE                                                         
075700           MOVE ZERO TO MOD-KDHOMONYM                                     
075800         END-EVALUATE                                                     
075900       END-IF                                                             
076000       PERFORM IMS-GET-BENA01                                             
076100                                                                          
076200       MOVE +1 TO RAD-IX                                                  
076300       PERFORM UNTIL  RAD-IX > MAX-IDSKYLT                                
076400         MOVE WS-IDSKYLT(RAD-IX) TO W-IDSKYLT                             
076500         PERFORM IMS-GNP-BENA11                                           
076600         IF SEGMENT-FINNS                                                 
076700           PERFORM S01-FLYTTA-BEART                                       
076800         ELSE                                                             
076900           EVALUATE TRUE                                                  
077000*     *      LATIN-1  SPRÅK                                               
077100             WHEN BENA-TEXT-IDSKYLT = 'D  '                               
077200               MOVE MFS-RENSA-FAELT TO MOD-BEART(1)                       
077300             WHEN BENA-TEXT-IDSKYLT = 'DK '                               
077400               MOVE MFS-RENSA-FAELT TO MOD-BEART(2)                       
077500             WHEN BENA-TEXT-IDSKYLT = 'E  '                               
077600               MOVE MFS-RENSA-FAELT TO MOD-BEART(3)                       
077700             WHEN BENA-TEXT-IDSKYLT = 'F  '                               
077800               MOVE MFS-RENSA-FAELT TO MOD-BEART(4)                       
077900             WHEN BENA-TEXT-IDSKYLT = 'GB '                               
078000               MOVE MFS-RENSA-FAELT TO MOD-BEART(5)                       
078100             WHEN BENA-TEXT-IDSKYLT = 'I  '                               
078200               MOVE MFS-RENSA-FAELT TO MOD-BEART (6)                      
078300             WHEN BENA-TEXT-IDSKYLT = 'MAL'                               
078400               MOVE MFS-RENSA-FAELT TO MOD-BEART (7)                      
078500             WHEN BENA-TEXT-IDSKYLT = 'NL '                               
078600               MOVE MFS-RENSA-FAELT TO MOD-BEART (8)                      
078700             WHEN BENA-TEXT-IDSKYLT = 'P  '                               
078800               MOVE MFS-RENSA-FAELT TO MOD-BEART (9)                      
078900             WHEN BENA-TEXT-IDSKYLT = 'S  '                               
079000               MOVE MFS-RENSA-FAELT TO MOD-BEART-S                        
079100             WHEN BENA-TEXT-IDSKYLT = 'SF '                               
079200               MOVE MFS-RENSA-FAELT TO MOD-BEART (10)                     
079300             WHEN BENA-TEXT-IDSKYLT = 'USA'                               
079400               MOVE MFS-RENSA-FAELT TO MOD-BEART (11)                     
079500*     *   UTF8-SPRÅK (EJ VISNINGSBARA PÅ 3270-SKÄRM)                      
079600             WHEN BENA-TEXT-IDSKYLT = 'CZ '                               
079700                 MOVE MFS-RENSA-FAELT TO MOD-BEART-CZ-FINNS               
079800             WHEN BENA-TEXT-IDSKYLT = 'GR '                               
079900                 MOVE MFS-RENSA-FAELT TO MOD-BEART-GR-FINNS               
080000             WHEN BENA-TEXT-IDSKYLT = 'H  '                               
080100                 MOVE MFS-RENSA-FAELT TO MOD-BEART-H-FINNS                
080200             WHEN BENA-TEXT-IDSKYLT = 'IR '                               
080300                 MOVE MFS-RENSA-FAELT TO MOD-BEART-IR-FINNS               
080400             WHEN BENA-TEXT-IDSKYLT = 'J  '                               
080500                 MOVE MFS-RENSA-FAELT TO MOD-BEART-J-FINNS                
080600             WHEN BENA-TEXT-IDSKYLT = 'KOR'                               
080700                 MOVE MFS-RENSA-FAELT TO MOD-BEART-KOR-FINNS              
080800             WHEN BENA-TEXT-IDSKYLT = 'PL '                               
080900                 MOVE MFS-RENSA-FAELT TO MOD-BEART-PL-FINNS               
081000             WHEN BENA-TEXT-IDSKYLT = 'RC '                               
081100                 MOVE MFS-RENSA-FAELT TO MOD-BEART-RC-FINNS               
081200             WHEN BENA-TEXT-IDSKYLT = 'RCN'                               
081300                 MOVE MFS-RENSA-FAELT TO MOD-BEART-RCN-FINNS              
081400             WHEN BENA-TEXT-IDSKYLT = 'RO '                               
081500                 MOVE MFS-RENSA-FAELT TO MOD-BEART-RO-FINNS               
081600             WHEN BENA-TEXT-IDSKYLT = 'RUS'                               
081700                 MOVE MFS-RENSA-FAELT TO MOD-BEART-RUS-FINNS              
081800             WHEN BENA-TEXT-IDSKYLT = 'T  '                               
081900                 MOVE MFS-RENSA-FAELT TO MOD-BEART-T-FINNS                
082000             WHEN BENA-TEXT-IDSKYLT = 'TR '                               
082100                 MOVE MFS-RENSA-FAELT TO MOD-BEART-TR-FINNS               
082200             WHEN BENA-TEXT-IDSKYLT = 'YU '                               
082300                 MOVE MFS-RENSA-FAELT TO MOD-BEART-YU-FINNS               
082400             WHEN OTHER CONTINUE                                          
082500           END-EVALUATE                                                   
082600         END-IF                                                           
082700         ADD +1 TO RAD-IX                                                 
082800       END-PERFORM                                                        
082900                                                                          
083000       PERFORM IMS-GET-BENA01                                             
083100                                                                          
083200       MOVE +1 TO RAD-IX                                                  
083300       PERFORM UNTIL RAD-IX >= MAX-HOM-PLUS-1                             
083400         PERFORM IMS-GET-BENA13                                           
083500         IF SEGMENT-FINNS                                                 
083600           MOVE BENA-HOM-TEHOMONYM TO MOD-TEHOMONYM(RAD-IX)               
083700         ELSE                                                             
083800           MOVE MFS-RENSA-FAELT TO MOD-TEHOMONYM(RAD-IX)                  
083900         END-IF                                                           
084000         ADD +1 TO RAD-IX                                                 
084100       END-PERFORM                                                        
084200     ELSE                                                                 
084300       MOVE FEL-3(IX) TO MOD-TEMFSFEL                                     
084400     END-IF                                                               
084500     .                                                                    
084600     EJECT                                                                
084700 D-KOLLA-SKAERMEN SECTION.                                                
084800                                                                          
084900     MOVE JA TO INPUT-RETT                                                
085000                                                                          
085100     MOVE IDARTNR-WS TO W-IDARTNR                                         
085200     PERFORM IMS-GET-ARTC01                                               
085300     IF SEGMENT-FINNS                                                     
085400        MOVE ART-KDPRODSL    TO TEST-KDPRODSL                             
085500        IF KDPRODSL-VOLVO-BIMA                                            
085600           IF KDPRODSL-VOLVO-ALL                                          
085700              SET NEVIS-ARTIKEL TO TRUE                                   
085800           END-IF                                                         
085900           IF CDC OR SDC                                                  
086000              CONTINUE                                                    
086100           ELSE                                                           
086200              MOVE NEJ TO INPUT-RETT                                      
086300              MOVE MED-4(IX) TO MOD-TEMFSINF                              
086400           END-IF                                                         
086500        END-IF                                                            
086600     END-IF                                                               
086700                                                                          
086800     IF MID-FLRSBEART = JA OR NEJ                                         
086900       MOVE MID-FLRSBEART TO WS-FLRSBEART                                 
087000       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLRSBEART-ATTR                    
087100     ELSE                                                                 
087200       EVALUATE TRUE                                                      
087300       WHEN MID-FLRSBEART = ALL '+' OR SPACE                              
087400         MOVE NEJ TO WS-FLRSBEART                                         
087500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLRSBEART-ATTR                  
087600        WHEN OTHER                                                        
087700         MOVE MFS-ALFA-FAELT-FEL TO MOD-FLRSBEART-ATTR                    
087800         MOVE NEJ                TO INPUT-RETT                            
087900         MOVE MED-3(IX)          TO MOD-TEMFSFEL                          
088000       END-EVALUATE                                                       
088100     END-IF                                                               
088200     INSPECT MID-BEART-NY REPLACING ALL '<' BY SPACE                      
088300     INSPECT MID-BEART-NY REPLACING ALL '>' BY SPACE                      
088400     IF MID-BEART-NY = ALL '+' OR SPACE                                   
088500       IF MID-KDHOMONYM-NY = ALL '+' OR SPACE                             
088600         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDHOMONYM-ATTR                    
088700         MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-NY-ATTR                     
088800         MOVE NEJ                TO INPUT-RETT                            
088900         MOVE MED-3(IX)          TO MOD-TEMFSFEL                          
089000       ELSE                                                               
089100         IF MID-KDHOMONYM-NY NUMERIC                                      
089200           MOVE MID-KDHOMONYM-NY    TO WS-KDHOMONYM                       
089300           MOVE SPACE               TO WS-BEART                           
089400           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDHOMONYM-ATTR                 
089500         ELSE                                                             
089600           MOVE MFS-NUM-FAELT-FEL   TO MOD-KDHOMONYM-ATTR                 
089700           MOVE NEJ                 TO INPUT-RETT                         
089800           MOVE MED-3(IX)           TO MOD-TEMFSFEL                       
089900         END-IF                                                           
090000       END-IF                                                             
090100     ELSE                                                                 
090200       MOVE MID-BEART-NY TO WS-BEART                                      
090300       IF MID-KDHOMONYM-NY = ALL '+' OR SPACE                             
090400         MOVE SPACE TO WS-KDHOMONYM                                       
090500       ELSE                                                               
090600         IF MID-KDHOMONYM-NY NUMERIC                                      
090700           MOVE MID-KDHOMONYM-NY    TO WS-KDHOMONYM                       
090800           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDHOMONYM-ATTR                 
090900         ELSE                                                             
091000           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDHOMONYM-ATTR                  
091100           MOVE NEJ                TO INPUT-RETT                          
091200           MOVE MED-3(IX)          TO MOD-TEMFSFEL                        
091300         END-IF                                                           
091400       END-IF                                                             
091500     END-IF                                                               
091600     .                                                                    
091700     EJECT                                                                
091800 E-ROER-EJ-FAELT SECTION.                                                 
091900     SKIP2                                                                
092000     MOVE MFS-ROER-EJ-FAELT TO MOD-BEART-S                                
092100                                                                          
092200     MOVE +1 TO RAD-IX                                                    
092300     PERFORM UNTIL RAD-IX > MAX-RAD                                       
092400       MOVE MFS-ROER-EJ-FAELT TO MOD-BEART(RAD-IX)                        
092500       ADD +1 TO RAD-IX                                                   
092600     END-PERFORM                                                          
092700     MOVE MFS-ROER-EJ-FAELT TO MOD-KDHOMONYM                              
092800                               MOD-BEART-NY                               
092900                               MOD-KDHOMONYM-NY                           
093000                               MOD-FLRSBEART                              
093100                               MOD-BEART-CZ-FINNS                         
093200                               MOD-BEART-GR-FINNS                         
093300                               MOD-BEART-H-FINNS                          
093400                               MOD-BEART-IR-FINNS                         
093500                               MOD-BEART-J-FINNS                          
093600                               MOD-BEART-KOR-FINNS                        
093700                               MOD-BEART-PL-FINNS                         
093800                               MOD-BEART-RC-FINNS                         
093900                               MOD-BEART-RCN-FINNS                        
094000                               MOD-BEART-RO-FINNS                         
094100                               MOD-BEART-RUS-FINNS                        
094200                               MOD-BEART-T-FINNS                          
094300                               MOD-BEART-TR-FINNS                         
094400                               MOD-BEART-YU-FINNS                         
094500     .                                                                    
094600     EJECT                                                                
094700 S01-FLYTTA-BEART SECTION.                                                
094800     SKIP2                                                                
094900     EVALUATE TRUE                                                        
095000                                                                          
095100*      LATIN-1  SPRÅK                                                     
095200       WHEN BENA-TEXT-IDSKYLT = 'D  '                                     
095300         MOVE BENA-TEXT-BEART TO MOD-BEART(1)                             
095400       WHEN BENA-TEXT-IDSKYLT = 'DK '                                     
095500         MOVE BENA-TEXT-BEART TO MOD-BEART(2)                             
095600       WHEN BENA-TEXT-IDSKYLT = 'E  '                                     
095700         MOVE BENA-TEXT-BEART TO MOD-BEART(3)                             
095800       WHEN BENA-TEXT-IDSKYLT = 'F  '                                     
095900         MOVE BENA-TEXT-BEART TO MOD-BEART(4)                             
096000       WHEN BENA-TEXT-IDSKYLT = 'GB '                                     
096100         MOVE BENA-TEXT-BEART TO MOD-BEART(5)                             
096200       WHEN BENA-TEXT-IDSKYLT = 'I  '                                     
096300         MOVE BENA-TEXT-BEART TO MOD-BEART (6)                            
096400       WHEN BENA-TEXT-IDSKYLT = 'MAL'                                     
096500         MOVE BENA-TEXT-BEART TO MOD-BEART (7)                            
096600       WHEN BENA-TEXT-IDSKYLT = 'NL '                                     
096700         MOVE BENA-TEXT-BEART TO MOD-BEART (8)                            
096800       WHEN BENA-TEXT-IDSKYLT = 'P  '                                     
096900         MOVE BENA-TEXT-BEART TO MOD-BEART (9)                            
097000       WHEN BENA-TEXT-IDSKYLT = 'S  '                                     
097100         MOVE BENA-TEXT-BEART TO MOD-BEART-S                              
097200       WHEN BENA-TEXT-IDSKYLT = 'SF '                                     
097300         MOVE BENA-TEXT-BEART TO MOD-BEART (10)                           
097400       WHEN BENA-TEXT-IDSKYLT = 'USA'                                     
097500         MOVE BENA-TEXT-BEART TO MOD-BEART (11)                           
097600                                                                          
097700*   UTF8-SPRÅK                                                            
097800       WHEN BENA-TEXT-IDSKYLT = 'CZ '                                     
097900         IF BENA-TEXT-BEART = SPACE                                       
098000           MOVE SPACE         TO MOD-BEART-CZ-FINNS                       
098100         ELSE                                                             
098200           MOVE REGISTRERAD   TO MOD-BEART-CZ-FINNS                       
098300         END-IF                                                           
098400       WHEN BENA-TEXT-IDSKYLT = 'GR '                                     
098500         IF BENA-TEXT-BEART = SPACE                                       
098600           MOVE SPACE         TO MOD-BEART-GR-FINNS                       
098700         ELSE                                                             
098800           MOVE REGISTRERAD   TO MOD-BEART-GR-FINNS                       
098900         END-IF                                                           
099000       WHEN BENA-TEXT-IDSKYLT = 'H  '                                     
099100         IF BENA-TEXT-BEART = SPACE                                       
099200           MOVE SPACE         TO MOD-BEART-H-FINNS                        
099300         ELSE                                                             
099400           MOVE REGISTRERAD   TO MOD-BEART-H-FINNS                        
099500         END-IF                                                           
099600       WHEN BENA-TEXT-IDSKYLT = 'IR '                                     
099700         IF BENA-TEXT-BEART = SPACE                                       
099800           MOVE SPACE         TO MOD-BEART-IR-FINNS                       
099900         ELSE                                                             
100000           MOVE REGISTRERAD   TO MOD-BEART-IR-FINNS                       
100100         END-IF                                                           
100200       WHEN BENA-TEXT-IDSKYLT = 'J  '                                     
100300         IF BENA-TEXT-BEART = SPACE                                       
100400           MOVE SPACE         TO MOD-BEART-J-FINNS                        
100500         ELSE                                                             
100600           MOVE REGISTRERAD   TO MOD-BEART-J-FINNS                        
100700         END-IF                                                           
100800       WHEN BENA-TEXT-IDSKYLT = 'KOR'                                     
100900         IF BENA-TEXT-BEART = SPACE                                       
101000           MOVE SPACE         TO MOD-BEART-KOR-FINNS                      
101100         ELSE                                                             
101200           MOVE REGISTRERAD   TO MOD-BEART-KOR-FINNS                      
101300         END-IF                                                           
101400       WHEN BENA-TEXT-IDSKYLT = 'PL '                                     
101500         IF BENA-TEXT-BEART = SPACE                                       
101600           MOVE SPACE         TO MOD-BEART-PL-FINNS                       
101700         ELSE                                                             
101800           MOVE REGISTRERAD   TO MOD-BEART-PL-FINNS                       
101900         END-IF                                                           
102000       WHEN BENA-TEXT-IDSKYLT = 'RC '                                     
102100         IF BENA-TEXT-BEART = SPACE                                       
102200           MOVE SPACE         TO MOD-BEART-RC-FINNS                       
102300         ELSE                                                             
102400           MOVE REGISTRERAD   TO MOD-BEART-RC-FINNS                       
102500         END-IF                                                           
102600       WHEN BENA-TEXT-IDSKYLT = 'RCN'                                     
102700         IF BENA-TEXT-BEART = SPACE                                       
102800           MOVE SPACE         TO MOD-BEART-RCN-FINNS                      
102900         ELSE                                                             
103000           MOVE REGISTRERAD   TO MOD-BEART-RCN-FINNS                      
103100         END-IF                                                           
103200       WHEN BENA-TEXT-IDSKYLT = 'RO '                                     
103300         IF BENA-TEXT-BEART = SPACE                                       
103400           MOVE SPACE         TO MOD-BEART-RO-FINNS                       
103500         ELSE                                                             
103600           MOVE REGISTRERAD   TO MOD-BEART-RO-FINNS                       
103700         END-IF                                                           
103800       WHEN BENA-TEXT-IDSKYLT = 'RUS'                                     
103900         IF BENA-TEXT-BEART = SPACE                                       
104000           MOVE SPACE         TO MOD-BEART-RUS-FINNS                      
104100         ELSE                                                             
104200           MOVE REGISTRERAD   TO MOD-BEART-RUS-FINNS                      
104300         END-IF                                                           
104400       WHEN BENA-TEXT-IDSKYLT = 'T  '                                     
104500         IF BENA-TEXT-BEART = SPACE                                       
104600           MOVE SPACE         TO MOD-BEART-T-FINNS                        
104700         ELSE                                                             
104800           MOVE REGISTRERAD   TO MOD-BEART-T-FINNS                        
104900         END-IF                                                           
105000       WHEN BENA-TEXT-IDSKYLT = 'TR '                                     
105100         IF BENA-TEXT-BEART = SPACE                                       
105200           MOVE SPACE         TO MOD-BEART-TR-FINNS                       
105300         ELSE                                                             
105400           MOVE REGISTRERAD   TO MOD-BEART-TR-FINNS                       
105500         END-IF                                                           
105600       WHEN BENA-TEXT-IDSKYLT = 'YU '                                     
105700         IF BENA-TEXT-BEART = SPACE                                       
105800           MOVE SPACE         TO MOD-BEART-YU-FINNS                       
105900         ELSE                                                             
106000           MOVE REGISTRERAD   TO MOD-BEART-YU-FINNS                       
106100         END-IF                                                           
106200                                                                          
106300       WHEN OTHER CONTINUE                                                
106400     END-EVALUATE                                                         
106500     .                                                                    
106600     EJECT                                                                
106700 S02-FLYTTA-HOM SECTION.                                                  
106800     SKIP2                                                                
106900     PERFORM IMS-GET-BENA01                                               
107000     MOVE +1 TO RAD-IX                                                    
107100     PERFORM UNTIL RAD-IX >= MAX-HOM-PLUS-1                               
107200       PERFORM IMS-GET-BENA13                                             
107300       IF SEGMENT-FINNS                                                   
107400         MOVE BENA-HOM-TEHOMONYM TO MOD-TEHOMONYM(RAD-IX)                 
107500       ELSE                                                               
107600         MOVE MFS-RENSA-FAELT TO MOD-TEHOMONYM(RAD-IX)                    
107700       END-IF                                                             
107800       ADD +1 TO RAD-IX                                                   
107900     END-PERFORM                                                          
108000     .                                                                    
108100     EJECT                                                                
108200* IMS SEKTIONER                                                           
108300     SKIP3                                                                
108400 IMS-GET-MSG SECTION.                                                     
108500     SKIP2                                                                
108600     MOVE '  QC' TO GODK-STATUSKODER                                      
108700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
108800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
108900     PERFORM IMS-STATUS-KONTROLL                                          
109000     .                                                                    
109100     SKIP3                                                                
109200 IMS-GN-MSG-KOM SECTION.                                                  
109300     MOVE '  QD' TO GODK-STATUSKODER                                      
109400     CALL CBLTDLI USING GN MSG-PCB KOM-IO-AREA                            
109500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
109600     PERFORM IMS-STATUS-KONTROLL                                          
109700     .                                                                    
109800     EJECT                                                                
109900 IMS-INSERT-MSG SECTION.                                                  
110000     SKIP2                                                                
110100     IF MSGI-IDLAND-SPR = 'SE'                                            
110200       MOVE '0' TO MFS-KDHUVOMR                                           
110300     END-IF                                                               
110400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
110500     MOVE SPACE TO GODK-STATUSKODER                                       
110600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
110700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
110800     PERFORM IMS-STATUS-KONTROLL                                          
110900     .                                                                    
111000     EJECT                                                                
111100 IMS-INSERT-MSG-KOM SECTION.                                              
111200                                                                          
111300     MOVE SPACE TO GODK-STATUSKODER                                       
111400     CALL CBLTDLI USING ISRT ALT-PCB KOM-IO-AREA                          
111500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
111600     PERFORM IMS-STATUS-KONTROLL                                          
111700     .                                                                    
111800     EJECT                                                                
111900 IMS-GET-XXAI01 SECTION.                                                  
112000     STRING 'WLXXAI01(WDGXKEY  =' W-1207-KEY-X ')'                        
112100             DELIMITED BY SIZE INTO SSA1                                  
112200     MOVE '  GE' TO GODK-STATUSKODER                                      
112300     CALL CBLTDLI USING GU XXAI-PCB DLI-IO-AREA SSA1                      
112400     MOVE XXAI-STATUS-CODE TO STATUS-WS                                   
112500     PERFORM IMS-STATUS-KONTROLL                                          
112600     .                                                                    
112700     SKIP3                                                                
112800 IMS-GET-XXAI11 SECTION.                                                  
112900     MOVE 'WLXXAI11 ' TO SSA1                                             
113000     MOVE '  ' TO GODK-STATUSKODER                                        
113100     CALL CBLTDLI USING GHNP XXAI-PCB DLI-IO-AREA SSA1                    
113200     MOVE XXAI-STATUS-CODE TO STATUS-WS                                   
113300     PERFORM IMS-STATUS-KONTROLL                                          
113400     .                                                                    
113500     SKIP3                                                                
113600 IMS-REPL-XXAI11 SECTION.                                                 
113700     MOVE '  ' TO GODK-STATUSKODER                                        
113800     CALL CBLTDLI USING REPL XXAI-PCB DLI-IO-AREA                         
113900     MOVE XXAI-STATUS-CODE TO STATUS-WS                                   
114000     PERFORM IMS-STATUS-KONTROLL                                          
114100     .                                                                    
114200     EJECT                                                                
114300 IMS-GET-BENA01-BSEQ SECTION.                                             
114400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
114500             DELIMITED BY SIZE INTO SSA1                                  
114600     MOVE '  GE' TO GODK-STATUSKODER                                      
114700     CALL CBLTDLI USING GN BENC-PCB DLI-IO-AREA SSA1                      
114800     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
114900     PERFORM IMS-STATUS-KONTROLL                                          
115000     .                                                                    
115100     SKIP3                                                                
115200 IMS-GU-BENA11-BSEQ SECTION.                                              
115300     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
115400             DELIMITED BY SIZE INTO SSA1                                  
115500     MOVE '  GE' TO GODK-STATUSKODER                                      
115600     CALL CBLTDLI USING GU BENC-PCB DLI-IO-AREA SSA1                      
115700     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
115800     PERFORM IMS-STATUS-KONTROLL                                          
115900     .                                                                    
116000     SKIP3                                                                
116100 IMS-GET-BENA12-BSEQ SECTION.                                             
116200     STRING 'WLBENA12(IDARTNR  =' W-IDARTNR-X ')'                         
116300             DELIMITED BY SIZE INTO SSA1                                  
116400     MOVE '  GE' TO GODK-STATUSKODER                                      
116500     CALL CBLTDLI USING GHU BENC-PCB DLI-IO-AREA SSA1                     
116600     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
116700     PERFORM IMS-STATUS-KONTROLL                                          
116800     .                                                                    
116900     EJECT                                                                
117000 IMS-DLET-BENA12-BSEQ SECTION.                                            
117100     MOVE '  ' TO GODK-STATUSKODER                                        
117200     CALL CBLTDLI USING DLET BENC-PCB DLI-IO-AREA                         
117300     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
117400     PERFORM IMS-STATUS-KONTROLL                                          
117500     .                                                                    
117600     SKIP3                                                                
117700 IMS-GET-BENA01-ASEQ SECTION.                                             
117800     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
117900             W-BEART-X ')'                                                
118000             DELIMITED BY SIZE INTO SSA1                                  
118100     MOVE '  GE' TO GODK-STATUSKODER                                      
118200     CALL CBLTDLI USING GN BENB-PCB DLI-IO-AREA SSA1                      
118300     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
118400     PERFORM IMS-STATUS-KONTROLL                                          
118500     .                                                                    
118600     SKIP3                                                                
118700 IMS-GET-BENA13-ASEQ SECTION.                                             
118800     MOVE 'WLBENA13 ' TO SSA1                                             
118900     MOVE '  GE' TO GODK-STATUSKODER                                      
119000     CALL CBLTDLI USING GNP BENB-PCB DLI-IO-AREA                          
119100     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
119200     PERFORM IMS-STATUS-KONTROLL                                          
119300     .                                                                    
119400     SKIP3                                                                
119500 IMS-GET-BENA01 SECTION.                                                  
119600     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
119700             DELIMITED BY SIZE INTO SSA1                                  
119800     MOVE '  GE' TO GODK-STATUSKODER                                      
119900     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
120000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
120100     PERFORM IMS-STATUS-KONTROLL                                          
120200     .                                                                    
120300     EJECT                                                                
120400 IMS-GHU-BENA01 SECTION.                                                  
120500     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
120600             DELIMITED BY SIZE INTO SSA1                                  
120700     MOVE '  GE' TO GODK-STATUSKODER                                      
120800     CALL CBLTDLI USING GHU BENA-PCB DLI-IO-AREA SSA1                     
120900     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
121000     PERFORM IMS-STATUS-KONTROLL                                          
121100     .                                                                    
121200     EJECT                                                                
121300 IMS-REPL-BENA01 SECTION.                                                 
121400     MOVE '  ' TO GODK-STATUSKODER                                        
121500     CALL CBLTDLI USING REPL BENA-PCB DLI-IO-AREA                         
121600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
121700     PERFORM IMS-STATUS-KONTROLL                                          
121800     .                                                                    
121900     EJECT                                                                
122000 IMS-GET-BENA11 SECTION.                                                  
122100     MOVE 'WLBENA11 ' TO SSA1                                             
122200     MOVE '  GE' TO GODK-STATUSKODER                                      
122300     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
122400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
122500     PERFORM IMS-STATUS-KONTROLL                                          
122600     .                                                                    
122700     SKIP3                                                                
122800 IMS-GNP-BENA11 SECTION.                                                  
122900     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
123000             DELIMITED BY SIZE INTO SSA1                                  
123100     MOVE '  GE' TO GODK-STATUSKODER                                      
123200     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
123300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
123400     PERFORM IMS-STATUS-KONTROLL                                          
123500     .                                                                    
123600     SKIP3                                                                
123700 IMS-ISRT-BENA01 SECTION.                                                 
123800     MOVE 'WLBENA01 ' TO SSA1                                             
123900     MOVE '  ' TO GODK-STATUSKODER                                        
124000     CALL CBLTDLI USING ISRT BENA-PCB DLI-IO-AREA SSA1                    
124100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
124200     PERFORM IMS-STATUS-KONTROLL                                          
124300     .                                                                    
124400     EJECT                                                                
124500 IMS-ISRT-BENA11 SECTION.                                                 
124600     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
124700              DELIMITED BY SIZE INTO SSA1                                 
124800     MOVE 'WLBENA11 ' TO SSA2                                             
124900     MOVE '  ' TO GODK-STATUSKODER                                        
125000     CALL CBLTDLI USING ISRT BENA-PCB DLI-IO-AREA SSA1 SSA2               
125100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
125200     PERFORM IMS-STATUS-KONTROLL                                          
125300     .                                                                    
125400     SKIP3                                                                
125500 IMS-ISRT-BENA12 SECTION.                                                 
125600     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X  ')'                        
125700              DELIMITED BY SIZE INTO SSA1                                 
125800     MOVE 'WLBENA12  ' TO SSA2                                            
125900     MOVE '  ' TO GODK-STATUSKODER                                        
126000     CALL CBLTDLI USING ISRT BENA-PCB DLI-IO-AREA SSA1 SSA2               
126100     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
126200     PERFORM IMS-STATUS-KONTROLL                                          
126300     .                                                                    
126400     SKIP3                                                                
126500 IMS-GET-BENA13 SECTION.                                                  
126600     MOVE 'WLBENA13 ' TO SSA1                                             
126700     MOVE '  GE' TO GODK-STATUSKODER                                      
126800     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
126900     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
127000     PERFORM IMS-STATUS-KONTROLL                                          
127100     .                                                                    
127200     EJECT                                                                
127300 IMS-STATUS-KONTROLL SECTION.                                             
127400     SET STATUS-IX TO 1                                                   
127500     SEARCH GODK-STATUS                                                   
127600       AT END                                                             
127700         CALL FELLOG                                                      
127800     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
127900       CONTINUE                                                           
128000     END-SEARCH                                                           
128100     .                                                                    
128200     SKIP3                                                                
128300 IMS-GET-ARTC01 SECTION.                                                  
128400     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
128500            DELIMITED BY SIZE INTO SSA1                                   
128600     MOVE '  GE' TO GODK-STATUSKODER                                      
128700     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-2 SSA1                    
128800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
128900     PERFORM IMS-STATUS-KONTROLL                                          
129000     .                                                                    
