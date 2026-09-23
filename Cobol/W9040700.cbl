000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9040700.                                                
000300*AUTHOR.         BODIL LINDAHL.                                           
000400*DATE-WRITTEN.   APRIL 1985.                                              
000500*    FUNKTION.                                                            
000600*                BYTE SVENSK BENÄMNING.                                   
000700*                BYTE HOMONYMKOD.                                         
000800     SKIP2                                                                
000900*    INDATA.                                                              
001000*        TRANSAKTION: W90407T                                             
001100*                     W90407U                                             
001200*        MID:         W90407I1                                            
001300*    UTDATA.                                                              
001400*        MOD:         W90407O1                                            
001500*                                                                         
001600*    ÄNDRINGAR.                                                           
001700*      2000-10-30  NYTT SPRÅK (TURKISKA) TILLKOMMER /C.E.                 
001800*      2003-01-09  UPPDAT AV BENA-BEN-FLAENDR VID FÖRÄNDRING /CE          
001900*                                                                         
002000*      2005-06-16  ANPASSA UPPDATERINGEN AV NY BENÄMNING                  
002100*                  FÖR DE 9 NYA SPRÅKEN.                                  
002200*                  (Ändring ej begärd av kund, utan är nödvändigt         
002300*                   för att alla WDD311-segment skall läggas upp          
002400*                   vid nyupplägg av benämning )                          
002500*                  Observera att om SPIE vill ändra till att få           
002600*                  info om översättningar i MOD-en, bör koden från        
002700*                  1112 användas.[väsentligt förändrad] )                 
002800*                                                                         
002900*      2005-10-18  BENA-BEN-FLAENDR skall bara sättas till JA för         
003000*                  produktslagen 11 till 29 vid förändring.               
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP3                                                                
003400 DATA DIVISION.                                                           
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(8)    VALUE 'W9040700'.            
004000 77  JA                          PIC X(1)    VALUE 'J'.                   
004100 77  NEJ                         PIC X(1)    VALUE 'N'.                   
004200 77  SVENSKA                     PIC X(1)    VALUE 'S'.                   
004300 77  WS-FLRSBEART                PIC X(1)    VALUE SPACE.                 
004400 77  WS-NYTT-NUMMER              PIC S9(7)   VALUE +0   COMP-3.           
004500 77  WS-BEART                    PIC X(25)   VALUE SPACE.                 
004600 77  MAX-RAD                     PIC S9(3)   VALUE +16  COMP-3.           
004700 77  MAX-HOM-PLUS-1              PIC S9(3)   VALUE +3   COMP-3.           
004800 77  MAX-IDSKYLT                 PIC S9(3)   VALUE +26  COMP-3.           
004900 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
005000 77  RAD-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
005100 77  IDSKYLT-IX                  PIC S9(9)   VALUE +0   COMP SYNC.        
005200                                                                          
005300 77  WS-FLAENDR-A                PIC X(1)    VALUE 'N'.                   
005400 77  WS-IDBENNR-A                PIC S9(7)   VALUE ZERO COMP-3.           
005500                                                                          
005600 77  WS-FLAENDR-B                PIC X(1)    VALUE 'N'.                   
005700 77  WS-IDBENNR-B                PIC S9(7)   VALUE ZERO COMP-3.           
005800                                                                          
005900 77  NEVIS-SW                 PIC X       VALUE 'N'.                      
006000     88 NEVIS-ARTIKEL   VALUE 'J'.                                        
006100     88 ANDRA-ARTIKLAR  VALUE 'N'.                                        
006200     SKIP3                                                                
006300                                                                          
006400*                                                                         
006500*01    -COPY WWPRODSL                                                     
006600*      --- VALID IDDC CODES                                               
006700*                                                                         
006800*01    -COPY WWDC99                                                       
006900       EJECT                                                              
007000 01  DYNAMISKA-SUBPROGRAM.                                                
007100   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
007200   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
007300   03  W005INIT                  PIC X(8)    VALUE 'W005INIT'.            
007400                                                                          
007500 01  WS-IDARTNR                             PIC X(9).                     
007600 01  IDARTNR-WS REDEFINES WS-IDARTNR        PIC 9(9).                     
007700     SKIP2                                                                
007800 01  WS-KDHOMONYM                           PIC X.                        
007900 01  KDHOMONYM-WS REDEFINES WS-KDHOMONYM    PIC 9.                        
008000     SKIP2                                                                
008100 01  INPUT-RETT                        PIC X    VALUE 'J'.                
008200     SKIP2                                                                
008300 01  SUBPROGRAM.                                                          
008400     03 WREVERSE                       PIC X(8) VALUE 'WREVERSE'.         
008500     EJECT                                                                
008600                                                                          
008700 01  TABELL                            PIC X(48)                          
008800*    -- Om SPIE skall ha visning av alla översättningar, kopiera          
008900*    -- lösningen i  W1011200.  (Denna tabell tas då bort)                
009000     VALUE 'D  E  F  GB I  J  KORMALNL P  RC RUSSF T  TR USA'.            
009100                                                                          
009200                                                                          
009300 01  TAB REDEFINES TABELL.                                                
009400     03  FILLER OCCURS 16.                                                
009500      05  NYCKEL-IDSKYLT            PIC X(3).                             
009600     SKIP3                                                                
009700 01  IDSKYLT-TABELL.                                                      
009800     03 FILLER   VALUE 'CZ '        PIC X(3).                             
009900     03 FILLER   VALUE 'D  '        PIC X(3).                             
010000     03 FILLER   VALUE 'DK '        PIC X(3).                             
010100     03 FILLER   VALUE 'E  '        PIC X(3).                             
010200     03 FILLER   VALUE 'F  '        PIC X(3).                             
010300     03 FILLER   VALUE 'GB '        PIC X(3).                             
010400     03 FILLER   VALUE 'GR '        PIC X(3).                             
010500     03 FILLER   VALUE 'H  '        PIC X(3).                             
010600     03 FILLER   VALUE 'I  '        PIC X(3).                             
010700     03 FILLER   VALUE 'IR '        PIC X(3).                             
010800     03 FILLER   VALUE 'J  '        PIC X(3).                             
010900     03 FILLER   VALUE 'KOR'        PIC X(3).                             
011000     03 FILLER   VALUE 'MAL'        PIC X(3).                             
011100     03 FILLER   VALUE 'ML '        PIC X(3).                             
011200     03 FILLER   VALUE 'P  '        PIC X(3).                             
011300     03 FILLER   VALUE 'PL '        PIC X(3).                             
011400     03 FILLER   VALUE 'RC '        PIC X(3).                             
011500     03 FILLER   VALUE 'RCN'        PIC X(3).                             
011600     03 FILLER   VALUE 'RO '        PIC X(3).                             
011700     03 FILLER   VALUE 'RUS'        PIC X(3).                             
011800     03 FILLER   VALUE 'S  '        PIC X(3).                             
011900     03 FILLER   VALUE 'SF '        PIC X(3).                             
012000     03 FILLER   VALUE 'T  '        PIC X(3).                             
012100     03 FILLER   VALUE 'TR '        PIC X(3).                             
012200     03 FILLER   VALUE 'USA'        PIC X(3).                             
012300     03 FILLER   VALUE 'YU '        PIC X(3).                             
012400 01  IDSKYLT-TAB REDEFINES IDSKYLT-TABELL.                                
012500     03  FILLER OCCURS 26.                                                
012600      05  WS-IDSKYLT                PIC X(3).                             
012700     SKIP3                                                                
012800 01  DAGENS-DATUM                   PIC 9(6)   VALUE ZERO.                
012900     SKIP2                                                                
013000 01  WS-TEHOMONYM.                                                        
013100     03  BM-RS-NAMN                 PIC X(7).                             
013200     03  FILLER                     PIC X(53).                            
013300     EJECT                                                                
013400*01  -COPY WREVAREA                                                       
013500     EJECT                                                                
013600*                   ****    PARAMETRAR TILL W005INIT                      
013700*01  -COPY WMSGINIT                                                       
013800     EJECT                                                                
013900 01  NYCKLAR-TILL-DLI.                                                    
014000     03  W-IDARTNR-X.                                                     
014100         05  W-IDARTNR            PIC S9(9) COMP-3 VALUE ZERO.            
014200     03  W-IDBENNR-X.                                                     
014300         05  W-IDBENNR            PIC S9(7) COMP-3 VALUE ZERO.            
014400     03  W-IDSKYLT-X.                                                     
014500         05  W-IDSKYLT            PIC X(3)  VALUE SPACE.                  
014600     03  W-BEART-X.                                                       
014700         05  W-BEART              PIC X(25) VALUE SPACE.                  
014800     03  W-1207-KEY-X.                                                    
014900         05  FILLER               PIC X(4)  VALUE '1207'.                 
015000         05  FILLER               PIC X(26) VALUE LOW-VALUE.              
015100     EJECT                                                                
015200 01  MEDDELANDE.                                                          
015300*                                                                         
015400     03  W-FEL-1.                                                         
015500         05  FILLER              PIC X(35)  VALUE                         
015600            'ARTIKELNUMMER EJ NUMERISKT'.                                 
015700         05  FILLER              PIC X(35)  VALUE                         
015800            'PART NUMBER NOT NUMERIC   '.                                 
015900     03  FILLER REDEFINES W-FEL-1.                                        
016000         05  FEL-1               PIC X(35) OCCURS 2.                      
016100                                                                          
016200     03  W-FEL-2.                                                         
016300         05  FILLER              PIC X(35) VALUE                          
016400            'BENÄMNING SAKNAS PÅ BENREG'.                                 
016500         05  FILLER              PIC X(35) VALUE                          
016600            'DESCRIPTION IS MISSING    '.                                 
016700     03  FILLER REDEFINES W-FEL-2.                                        
016800         05  FEL-2               PIC X(35) OCCURS 2.                      
016900                                                                          
017000     03  W-FEL-3.                                                         
017100         05  FILLER             PIC X(35) VALUE                           
017200            'ARTIKELNUMMER SAKNAS               '.                        
017300         05  FILLER             PIC X(35) VALUE                           
017400            'THIS PART IS NOT IN THE DATABASE'.                           
017500     03  FILLER REDEFINES W-FEL-3.                                        
017600         05  FEL-3              PIC X(35) OCCURS 2.                       
017700                                                                          
017800     03  W-FEL-4.                                                         
017900         05  FILLER             PIC X(35)  VALUE                          
018000            'SÖKT HOMONYMKOD SAKNAS'.                                     
018100         05  FILLER             PIC X(35)  VALUE                          
018200            'HOM.CODE IS MISSING   '.                                     
018300     03  FILLER REDEFINES W-FEL-4.                                        
018400         05  FEL-4              PIC X(35)  OCCURS 2.                      
018500                                                                          
018600     03  W-FEL-5.                                                         
018700         05  FILLER             PIC X(35)  VALUE                          
018800            'FRI TEXT FINNS - KORRIGERA'.                                 
018900         05  FILLER             PIC X(35)  VALUE                          
019000            'HOM.CODE EXISTS  VERIFY   '.                                 
019100     03  FILLER REDEFINES W-FEL-5.                                        
019200         05  FEL-5             PIC X(35)   OCCURS 2.                      
019300                                                                          
019400     03  W-FEL-6.                                                         
019500         05  FILLER            PIC X(36)   VALUE                          
019600            'SÖKT BENÄMNING OCH HOMONYMKOD SAKNAS'.                       
019700         05  FILLER            PIC X(36)   VALUE                          
019800            'HOM.CODE IS MISSING                 '.                       
019900     03  FILLER REDEFINES W-FEL-6.                                        
020000         05  FEL-6             PIC X(36) OCCURS 2.                        
020100                                                                          
020200     03  W-MED-1.                                                         
020300         05   FILLER                  PIC X(36) VALUE                     
020400              'UPPDATERING GJORD'.                                        
020500         05   FILLER                  PIC X(36) VALUE                     
020600              'UPDATED          '.                                        
020700     03  FILLER REDEFINES W-MED-1.                                        
020800         05   MED-1                   PIC X(36) OCCURS 2.                 
020900                                                                          
021000     03  W-MED-2.                                                         
021100         05  FILLER                   PIC X(40) VALUE                     
021200            'EJ NAMNLEX BENÄMNING                  '.                     
021300         05  FILLER                   PIC X(40) VALUE                     
021400            'THIS DESCR.IS NOT A NAMNLEX DESCRIPTION'.                    
021500     03  FILLER REDEFINES W-MED-2.                                        
021600         05  MED-2                    PIC X(40) OCCURS 2.                 
021700                                                                          
021800     03  W-MED-3.                                                         
021900         05  FILLER                   PIC X(36) VALUE                     
022000            'UPPLYSTA FÄLT FEL           '.                               
022100         05  FILLER                   PIC X(36) VALUE                     
022200            'HIGH LIGHTED FIELD INCORRECT'.                               
022300     03  FILLER REDEFINES W-MED-3.                                        
022400         05  MED-3                    PIC X(36) OCCURS 2.                 
022500                                                                          
022600     03  W-MED-4.                                                         
022700         05  FILLER                   PIC X(36) VALUE                     
022800            'UPPDATERING EJ TILLÅTEN     '.                               
022900         05  FILLER                   PIC X(36) VALUE                     
023000            'UPDATE NOT ALLOWED          '.                               
023100     03  FILLER REDEFINES W-MED-4.                                        
023200         05  MED-4                    PIC X(36) OCCURS 2.                 
023300                                                                          
023400     03  W-MED-5.                                                         
023500         05  FILLER                   PIC X(25) VALUE                     
023600            'ÖVERSÄTTNING FINNS       '.                                  
023700         05  FILLER                   PIC X(25) VALUE                     
023800            'TRANSLATION EXIST        '.                                  
023900     03  FILLER REDEFINES W-MED-5.                                        
024000         05  MED-5                    PIC X(25) OCCURS 2.                 
024100     EJECT                                                                
024200*                        ****    MFS OCH SKÄRMHANTERING                   
024300 01  FILLER              PIC X(16)   VALUE 'MFS-WS'.                      
024400     SKIP2                                                                
024500*01  MID -COPY W90407I1                                                   
024600     EJECT                                                                
024700*01  -COPY WMSGAREA                                                       
024800     EJECT                                                                
024900*    03  MOD -COPY W90407O1  -RED MSG-AREA.                               
025000     EJECT                                                                
025100*01  -COPY WMFSAREA.                                                      
025200     EJECT                                                                
025300******************************************************************        
025400*****                                                                     
025500*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025600*****                                                                     
025700 01  IMS-WS.                                                              
025800     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
025900     SKIP3                                                                
026000*****                    **** STATUS-KOD FRÅN IMS                         
026100     03  STATUS-WS               PIC X(2).                                
026200         88  SEGMENT-FINNS                   VALUE '  '.                  
026300         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
026400     SKIP3                                                                
026500     03  GODK-STATUSKODER.                                                
026600         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
026700     SKIP3                                                                
026800 01  SSA1                        PIC X(128).                              
026900 01  SSA2                        PIC X(128).                              
027000     EJECT                                                                
027100*                            IMS FUNKTIONSKODER                           
027200*01  -COPY W0003                                                          
027300     EJECT                                                                
027400*                            DLI INPUT-OUTPUT AREA                        
027500 01  DLI-IO-AREA.                                                         
027600     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
027700     SKIP3                                                                
027800*    03  WLBENA  -COPY WDD301   -PRE BENA-  -RED IO-AREA.                 
027900     EJECT                                                                
028000*    03  WLBENA  -COPY WDD311   -PRE BENA-  -RED IO-AREA.                 
028100     EJECT                                                                
028200*    03  WLBENA  -COPY WDD312   -PRE BENA-  -RED IO-AREA.                 
028300*    03  WLBENA  -COPY WDD313   -PRE BENA-  -RED IO-AREA.                 
028400     EJECT                                                                
028500*    03  WLXXAI  -COPY WDGX1208 -PRE XXAI-  -RED IO-AREA.                 
028600     EJECT                                                                
028700*                            DLI INPUT-OUTPUT AREA-2                      
028800 01  DLI-IO-AREA-2.                                                       
028900     03  IO-AREA-2               PIC X(200)  VALUE SPACE.                 
029000     SKIP3                                                                
029100*    03  WLARTC  -COPY WDK601               -RED IO-AREA-2.               
029200     EJECT                                                                
029300 LINKAGE SECTION.                                                         
029400     SKIP2                                                                
029500*01  -COPY W0009     -PRE MSG-                                            
029600     EJECT                                                                
029700*01  -COPY W0008     -PRE USEA-                                           
029800         05  FILLER              PIC X.                                   
029900     EJECT                                                                
030000*01  -COPY W0008     -PRE BENA-                                           
030100         05  FILLER              PIC X.                                   
030200     EJECT                                                                
030300*01  -COPY W0008     -PRE BENB-                                           
030400         05  FILLER              PIC X.                                   
030500     EJECT                                                                
030600*01  -COPY W0008     -PRE BENC-                                           
030700         05  FILLER              PIC X.                                   
030800     EJECT                                                                
030900*01  -COPY W0008     -PRE XXAI-                                           
031000         05  FILLER              PIC X.                                   
031100     EJECT                                                                
031200*01  -COPY W0008     -PRE ARTC-                                           
031300         05  FILLER              PIC X.                                   
031400     EJECT                                                                
031500 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
031600                                  BENA-PCB BENB-PCB BENC-PCB              
031700                                  XXAI-PCB ARTC-PCB.                      
031800 MAIN SECTION.                                                            
031900     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
032000                                   BENA-PCB BENB-PCB BENC-PCB             
032100                                   XXAI-PCB ARTC-PCB.                     
032200     PERFORM IMS-GET-MSG                                                  
032300     IF SEGMENT-FINNS                                                     
032400       PERFORM A-INIT-SPARA-INPUT                                         
032500       IF WS-IDARTNR NUMERIC                                              
032600         IF MFS-UPDATE                                                    
032700*          PERFORM E-ROER-EJ-FAELT                                        
032800           PERFORM D-KOLLA-SKAERMEN                                       
032900           IF INPUT-RETT = JA                                             
033000             PERFORM B-UPPDATERA                                          
033100           END-IF                                                         
033200         ELSE                                                             
033300           PERFORM C-LAS-BASEN                                            
033400         END-IF                                                           
033500       ELSE                                                               
033600         MOVE FEL-1(IX) TO MOD-TEMFSFEL                                   
033700       END-IF                                                             
033800*      MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                  
033900*      INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
034000       COMPUTE MSG-KVLL = 4 + ( LENGTH OF MOD-W90407O1 )                  
034100       PERFORM IMS-INSERT-MSG                                             
034200     END-IF                                                               
034300     MOVE ZERO TO RETURN-CODE                                             
034400     GOBACK                                                               
034500     .                                                                    
034600     EJECT                                                                
034700 A-INIT-SPARA-INPUT SECTION.                                              
034800     SKIP2                                                                
034900     IF MSG-DUBBLA-TRANSKODER                                             
035000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90407I1-CTX             
035100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
035200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
035300     ELSE                                                                 
035400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W90407I1-CTX              
035500       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
035600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
035700     END-IF                                                               
035800     IF MFS-IDTRANS = '9407'                                              
035900       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
036000     ELSE                                                                 
036100       MOVE SPACE TO MFS-KDTRTYP                                          
036200     END-IF                                                               
036300                                                                          
036400     MOVE ALL '+' TO MSGI-WMSGINIT                                        
036500     MOVE '001'             TO MSGI-KDCALL                                
036600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
036700                               MSGI-IDLTERM-USER                          
036800     MOVE '9407'            TO MSGI-IDTRANS                               
036900     IF MFS-IDTRANS = '9407'                                              
037000     OR (MID-IDARTNR-IN NUMERIC                                           
037100     AND MID-IDARTNR-IN > ZERO)                                           
037200         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
037300     END-IF                                                               
037400     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
037500     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
037600     MOVE MSGI-IDDC    TO WS-IDDC                                         
037700     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
037800                                                                          
037900     IF MID-IDARTNR-IN = ALL '+' OR SPACE                                 
038000        CONTINUE                                                          
038100     ELSE                                                                 
038200       MOVE SPACE TO MFS-KDTRTYP                                          
038300     END-IF                                                               
038400     IF MSGI-IDLAND-SPR = 'SE'                                            
038500       MOVE +1 TO IX                                                      
038600     ELSE                                                                 
038700       MOVE +2 TO IX                                                      
038800     END-IF                                                               
038900     MOVE LOW-VALUE TO MOD-W90407O1                                       
039000     MOVE 'W90407O1' TO MFS-IDMOD                                         
039100     MOVE '9407' TO MOD-IDTRANS                                           
039200                                                                          
039300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
039400                             MOD-TEMFSINF                                 
039500*                            MOD-IDARTNR-IN                               
039600*                            MOD-BEART-NY                                 
039700*                            MOD-KDHOMONYM-NY                             
039800*                            MOD-FLRSBEART                                
039900     .                                                                    
040000     EJECT                                                                
040100 B-UPPDATERA SECTION.                                                     
040200     SKIP2                                                                
040300     ACCEPT DAGENS-DATUM FROM DATE                                        
040400                                                                          
040500     MOVE IDARTNR-WS TO W-IDARTNR                                         
040600*    --- LÄSER ARTIKELNS BENA01 MED BENC-PCB                              
040700     PERFORM IMS-GET-BENA01-BSEQ                                          
040800     IF SEGMENT-FINNS                                                     
040900       IF WS-BEART = SPACE                                                
041000         PERFORM BA-UPPDATERA-KDHOMONYM                                   
041100       ELSE                                                               
041200         EVALUATE TRUE                                                    
041300         WHEN WS-KDHOMONYM = SPACE                                        
041400            PERFORM BB-UPPDATERA-BEART                                    
041500         WHEN OTHER                                                       
041600            PERFORM BC-UPPDATERA-KDHOM-BEART                              
041700         END-EVALUATE                                                     
041800       END-IF                                                             
041900                                                                          
042000*      --- KOLLA WS-FLAENDR FÖR UPPDATERING AV BENA-BEN-FLAENDR           
042100*      --- Denna sätts enbart om Switchen NEVIS-SW har värdet             
042200*      --- för NEVIS-ARTIKEL (D.v.s.=JA)                                  
042300       IF WS-FLAENDR-A = JA                                               
042400         MOVE WS-IDBENNR-A TO W-IDBENNR                                   
042500         PERFORM IMS-GHU-BENA01                                           
042600         MOVE JA           TO BENA-BEN-FLAENDR                            
042700         PERFORM IMS-REPL-BENA01                                          
042800       END-IF                                                             
042900       IF WS-FLAENDR-B = JA                                               
043000         MOVE WS-IDBENNR-B TO W-IDBENNR                                   
043100         PERFORM IMS-GHU-BENA01                                           
043200         MOVE JA           TO BENA-BEN-FLAENDR                            
043300         PERFORM IMS-REPL-BENA01                                          
043400       END-IF                                                             
043500     ELSE                                                                 
043600       MOVE FEL-3(IX) TO MOD-TEMFSFEL                                     
043700     END-IF                                                               
043800     .                                                                    
043900     EJECT                                                                
044000 BA-UPPDATERA-KDHOMONYM SECTION.                                          
044100     SKIP2                                                                
044200*    -- WDD301 ÄR LÄST FÖR ARTIKELN                                       
044300     MOVE BENA-BEN-IDBENNR TO WS-IDBENNR-A                                
044400                                                                          
044500*    -- LÄS DEN SVENSKA TEXTEN                                            
044600     MOVE SVENSKA          TO W-IDSKYLT                                   
044700     PERFORM IMS-GU-BENA11-BSEQ                                           
044800                                                                          
044900     MOVE BENA-TEXT-BEART TO W-BEART                                      
045000     PERFORM IMS-GET-BENA01-ASEQ                                          
045100*    -- LÄS FRAM DEN NYA WDD301 MED DEN NYA HOMONYMKODEN                  
045200     PERFORM UNTIL SEGMENT-SAKNAS                                         
045300                OR BENA-BEN-KDHOMONYM = KDHOMONYM-WS                      
045400       PERFORM IMS-GET-BENA01-ASEQ                                        
045500     END-PERFORM                                                          
045600     IF SEGMENT-FINNS                                                     
045700*      -- SPARA DET NYA BENÄMNINGSNUMRET                                  
045800       MOVE BENA-BEN-IDBENNR TO W-IDBENNR  WS-IDBENNR-B                   
045900                                                                          
046000*      -- LÄS NU DET GAMLA BENÄMNINGSNUMRET OCH TAG BORT ARTIKELN         
046100       PERFORM IMS-GET-BENA01-BSEQ                                        
046200       PERFORM IMS-GET-BENA12-BSEQ                                        
046300       PERFORM IMS-DLET-BENA12-BSEQ                                       
046400       IF NEVIS-ARTIKEL                                                   
046500         MOVE JA TO WS-FLAENDR-A                                          
046600       END-IF                                                             
046700                                                                          
046800*      -- LÄS NU DET NYA BENÄMNINGSNUMRET OCH SKRIV UT PÅ SKÄRM           
046900*      -- Läs bara HOMONYM-TEXT då SPIE inte vill ha benämn-text.         
047000*      -- ÄT 05:7 **-märker även onödig IMS-läsning av BENA11.            
047100**     PERFORM IMS-GET-BENA01                                             
047200**     PERFORM IMS-GET-BENA11                                             
047300**     PERFORM UNTIL NOT SEGMENT-FINNS                                    
047400*        PERFORM S01-FLYTTA-BEART                                         
047500**       PERFORM IMS-GET-BENA11                                           
047600**     END-PERFORM                                                        
047700       PERFORM S02-FLYTTA-HOMONYMTEXT                                     
047800                                                                          
047900*      -- FLYTTA ARTIKELN TILL DET NYA BENÄMNINGSNUMRET                   
048000       MOVE IDARTNR-WS TO BENA-ART-IDARTNR                                
048100       MOVE NEJ        TO BENA-ART-FLFELHOMO                              
048200       MOVE WS-KDHOMONYM TO MOD-KDHOMONYM                                 
048300       PERFORM IMS-ISRT-BENA12                                            
048400       IF NEVIS-ARTIKEL                                                   
048500         MOVE JA TO WS-FLAENDR-B                                          
048600       END-IF                                                             
048700       MOVE MED-1(IX) TO MOD-TEMFSINF                                     
048800     ELSE                                                                 
048900       MOVE FEL-4(IX) TO MOD-TEMFSFEL                                     
049000       MOVE MFS-ALFA-FAELT-FEL TO MOD-KDHOMONYM-ATTR                      
049100     END-IF                                                               
049200     .                                                                    
049300     EJECT                                                                
049400 BB-UPPDATERA-BEART SECTION.                                              
049500     SKIP2                                                                
049600*    -- WDD301 ÄR LÄST FÖR ARTIKELN                                       
049700     MOVE BENA-BEN-IDBENNR TO WS-IDBENNR-A                                
049800                                                                          
049900*    -- LÄS FRAM DEN NYA WDD301 MED DEN NYA BENÄMNINGEN                   
050000     MOVE WS-BEART TO W-BEART                                             
050100     MOVE SVENSKA TO W-IDSKYLT                                            
050200     PERFORM IMS-GET-BENA01-ASEQ                                          
050300     PERFORM UNTIL SEGMENT-SAKNAS OR BENA-BEN-KDHOMONYM = 0               
050400       PERFORM IMS-GET-BENA01-ASEQ                                        
050500     END-PERFORM                                                          
050600     IF SEGMENT-FINNS                                                     
050700*      -- SPARA NYA BENÄMNINGSNUMRET                                      
050800       MOVE BENA-BEN-IDBENNR TO W-IDBENNR                                 
050900                                WS-IDBENNR-B                              
051000**     **-märker denna kod, då SPIE inte kan ange RS-unik                 
051100**     IF WS-FLRSBEART = JA                                               
051200**       PERFORM BBA-UTAN-HOM                                             
051300**     ELSE                                                               
051400         PERFORM IMS-GET-BENA13-ASEQ                                      
051500         IF SEGMENT-FINNS                                                 
051600           MOVE BENA-HOM-TEHOMONYM TO WS-TEHOMONYM                        
051700           IF BM-RS-NAMN = 'BM-NAMN' OR 'RS-NAMN'                         
051800             PERFORM BBA-UTAN-HOM                                         
051900           ELSE                                                           
052000             PERFORM BBB-MED-HOM                                          
052100           END-IF                                                         
052200         ELSE                                                             
052300           PERFORM BBA-UTAN-HOM                                           
052400         END-IF                                                           
052500**     END-IF                                                             
052600     ELSE                                                                 
052700**     **-märker denna kod, då SPIE inte kan ange RS-unik                 
052800**     IF WS-FLRSBEART = JA                                               
052900**       PERFORM BBE-REG-RSUNIK                                           
053000**     ELSE                                                               
053100         MOVE FEL-2(IX) TO MOD-TEMFSFEL                                   
053200         MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-NY-ATTR                     
053300**     END-IF                                                             
053400     END-IF                                                               
053500     .                                                                    
053600     EJECT                                                                
053700 BBA-UTAN-HOM SECTION.                                                    
053800     SKIP2                                                                
053900*    -- TAG BORT ARTIKELN FRÅN DEN GAMLA BENÄMNINGEN                      
054000     PERFORM IMS-GET-BENA01-BSEQ                                          
054100     PERFORM IMS-GET-BENA12-BSEQ                                          
054200     PERFORM IMS-DLET-BENA12-BSEQ                                         
054300     IF NEVIS-ARTIKEL                                                     
054400       MOVE JA TO WS-FLAENDR-A                                            
054500     END-IF                                                               
054600                                                                          
054700*    -- LÄS DEN NYA BENÄMNINGEN                                           
054800     PERFORM IMS-GET-BENA01                                               
054900     MOVE BENA-BEN-KDHOMONYM TO MOD-KDHOMONYM                             
055000                                                                          
055100*      -- Läs bara HOMONYM-TEXT då SPIE inte vill ha benämn-text.         
055200*      -- ÄT 05:7 **-märker även onödig IMS-läsning av BENA11.            
055300**   PERFORM IMS-GET-BENA11                                               
055400**   PERFORM UNTIL SEGMENT-SAKNAS                                         
055500*      PERFORM S01-FLYTTA-BEART                                           
055600**     PERFORM IMS-GET-BENA11                                             
055700**   END-PERFORM                                                          
055800                                                                          
055900     PERFORM S02-FLYTTA-HOMONYMTEXT                                       
056000                                                                          
056100*    -- LÄGG TILL ARTIKELN FÖR DEN NYA BENÄMNINGEN                        
056200     MOVE IDARTNR-WS TO BENA-ART-IDARTNR                                  
056300     MOVE NEJ        TO BENA-ART-FLFELHOMO                                
056400     PERFORM IMS-ISRT-BENA12                                              
056500     IF NEVIS-ARTIKEL                                                     
056600       MOVE JA TO WS-FLAENDR-B                                            
056700     END-IF                                                               
056800     MOVE MED-1(IX) TO MOD-TEMFSINF                                       
056900     .                                                                    
057000     EJECT                                                                
057100 BBB-MED-HOM SECTION.                                                     
057200     SKIP2                                                                
057300*    -- TAG BORT ARTIKELN FRÅN DEN GAMLA BENÄMNINGEN                      
057400     PERFORM IMS-GET-BENA01-BSEQ                                          
057500     PERFORM IMS-GET-BENA12-BSEQ                                          
057600     PERFORM IMS-DLET-BENA12-BSEQ                                         
057700     MOVE JA TO WS-FLAENDR-A                                              
057800                                                                          
057900*    -- LÄS DEN NYA BENÄMNINGEN                                           
058000*      -- Läs bara HOMONYM-TEXT då SPIE inte vill ha benämn-text.         
058100*      -- ÄT 05:7 **-märker även onödig IMS-läsning av BENA11.            
058200**   PERFORM IMS-GET-BENA01                                               
058300**   PERFORM IMS-GET-BENA11                                               
058400**   PERFORM UNTIL SEGMENT-SAKNAS                                         
058500*      PERFORM S01-FLYTTA-BEART                                           
058600**     PERFORM IMS-GET-BENA11                                             
058700**   END-PERFORM                                                          
058800                                                                          
058900     PERFORM S02-FLYTTA-HOMONYMTEXT                                       
059000                                                                          
059100*    -- LÄGG TILL ARTIKELN FÖR DEN NYA BENÄMNINGEN                        
059200     MOVE IDARTNR-WS TO BENA-ART-IDARTNR                                  
059300     MOVE JA TO BENA-ART-FLFELHOMO                                        
059400     MOVE SPACE TO MOD-KDHOMONYM                                          
059500     PERFORM IMS-ISRT-BENA12                                              
059600     MOVE JA TO WS-FLAENDR-B                                              
059700     MOVE MED-1(IX) TO MOD-TEMFSINF                                       
059800     MOVE FEL-5(IX) TO MOD-TEMFSFEL                                       
059900     .                                                                    
060000     EJECT                                                                
060100*BBE-REG-RSUNIK SECTION.                                                  
060200**                                                                        
060300**   **-märker denna kod, då SPIE inte kan ange RS-unikt upplägg          
060400**      P.g.a. att FLRSBEART inte finns med i MIDDEN. (se BB- )           
060500**                                                                        
060600**   PERFORM IMS-GET-XXAI01                                               
060700**   PERFORM IMS-GET-XXAI11                                               
060800**   MOVE XXAI-1208-IDBENNR TO WS-NYTT-NUMMER                             
060900**   ADD +1 TO WS-NYTT-NUMMER                                             
061000**   IF WS-NYTT-NUMMER < XXAI-1208-IDBENNR-MAX                            
061100**     MOVE WS-NYTT-NUMMER TO W-IDBENNR                                   
061200**                            XXAI-1208-IDBENNR                           
061300**     PERFORM IMS-REPL-XXAI11                                            
061400**                                                                        
061500**     MOVE WS-NYTT-NUMMER TO BENA-BEN-IDBENNR                            
061600**     MOVE ZERO           TO BENA-BEN-KDHOMONYM                          
061700**                            MOD-KDHOMONYM                               
061800**     MOVE ZERO           TO BENA-BEN-TIUPPDAT-STOP                      
061900**     MOVE +2             TO BENA-BEN-KDBENSTAT                          
062000**     IF NEVIS-ARTIKEL                                                   
062100**       MOVE JA           TO BENA-BEN-FLAENDR                            
062200**     END-IF                                                             
062300**     PERFORM IMS-ISRT-BENA01                                            
062400**                                                                        
062500**     MOVE +1 TO IDSKYLT-IX                                              
062600*      --- Alla godk WDD3-språk skall ha ett eget 11-segment              
062700**     PERFORM UNTIL IDSKYLT-IX > MAX-IDSKYLT                             
062800**       MOVE WS-IDSKYLT(IDSKYLT-IX) TO BENA-TEXT-IDSKYLT                 
062900**       MOVE SPACE                  TO BENA-TEXT-BEARTEXT                
063000**       MOVE DAGENS-DATUM           TO BENA-TEXT-TIUPPDAT                
063100**       IF WS-IDSKYLT(IDSKYLT-IX) = 'S  '                                
063200**         MOVE JA                  TO BENA-TEXT-FLOVERSATT               
063300**         MOVE WS-BEART            TO BENA-TEXT-BEARTEXT                 
063400**       ELSE                                                             
063500**         MOVE NEJ                 TO BENA-TEXT-FLOVERSATT               
063600**       END-IF                                                           
063700**       PERFORM IMS-ISRT-BENA11                                          
063800*        PERFORM S01-FLYTTA-BEART                                         
063900**       ADD +1 TO IDSKYLT-IX                                             
064000**     END-PERFORM                                                        
064100**     MOVE SPACE TO REV-TETEXT                                           
064200**     MOVE WS-BEART TO REV-TETEXT                                        
064300**     CALL WREVERSE USING REV-TETEXT                                     
064400**                                                                        
064500**     MOVE ' BG'          TO BENA-TEXT-IDSKYLT                           
064600**     MOVE SPACE          TO BENA-TEXT-BEARTEXT                          
064700**     MOVE DAGENS-DATUM   TO BENA-TEXT-TIUPPDAT                          
064800**     MOVE NEJ            TO BENA-TEXT-FLOVERSATT                        
064900**     PERFORM IMS-ISRT-BENA11                                            
065000**                                                                        
065100**     MOVE '  S'          TO BENA-TEXT-IDSKYLT                           
065200**     MOVE REV-TETEXT     TO BENA-TEXT-BEARTEXT                          
065300**     MOVE DAGENS-DATUM   TO BENA-TEXT-TIUPPDAT                          
065400**     MOVE JA             TO BENA-TEXT-FLOVERSATT                        
065500**     PERFORM IMS-ISRT-BENA11                                            
065600**                                                                        
065700**     PERFORM IMS-GET-BENA01-BSEQ                                        
065800**     PERFORM IMS-GET-BENA12-BSEQ                                        
065900**     PERFORM IMS-DLET-BENA12-BSEQ                                       
066000**                                                                        
066100**     MOVE IDARTNR-WS TO BENA-ART-IDARTNR                                
066200**     MOVE NEJ TO        BENA-ART-FLFELHOMO                              
066300**     PERFORM IMS-ISRT-BENA12                                            
066400**     MOVE MED-1(IX) TO MOD-TEMFSINF                                     
066500**   END-IF                                                               
066600**   .                                                                    
066700     EJECT                                                                
066800 BC-UPPDATERA-KDHOM-BEART SECTION.                                        
066900     SKIP2                                                                
067000*    -- WDD301 ÄR LÄST FÖR ARTIKELN                                       
067100     MOVE BENA-BEN-IDBENNR TO WS-IDBENNR-A                                
067200                                                                          
067300     MOVE WS-BEART TO W-BEART                                             
067400     MOVE SVENSKA TO W-IDSKYLT                                            
067500*    -- LÄS DEN NYA BENÄMNINGENS ROT                                      
067600     PERFORM IMS-GET-BENA01-ASEQ                                          
067700     IF SEGMENT-FINNS                                                     
067800       PERFORM UNTIL SEGMENT-SAKNAS                                       
067900                  OR BENA-BEN-KDHOMONYM = KDHOMONYM-WS                    
068000         PERFORM IMS-GET-BENA01-ASEQ                                      
068100       END-PERFORM                                                        
068200       IF SEGMENT-FINNS                                                   
068300*        -- SPARA NYA BENÄMNINGSNUMRET                                    
068400         MOVE BENA-BEN-IDBENNR TO W-IDBENNR                               
068500                                  WS-IDBENNR-B                            
068600*        -- TAG FÖRST BORT ARTIKELN FRÅN GAMLA BENÄMNINGSNUMRET           
068700         PERFORM IMS-GET-BENA01-BSEQ                                      
068800         PERFORM IMS-GET-BENA12-BSEQ                                      
068900         PERFORM IMS-DLET-BENA12-BSEQ                                     
069000                                                                          
069100*        -- LÄS DEN NYA BENÄMNINGEN                                       
069200*      -- Läs bara HOMONYM-TEXT då SPIE inte vill ha benämn-text.         
069300*      -- ÄT 05:7 **-märker även onödig IMS-läsning av BENA11.            
069400**       PERFORM IMS-GET-BENA01                                           
069500**       PERFORM IMS-GET-BENA11                                           
069600**       PERFORM UNTIL SEGMENT-SAKNAS                                     
069700*          PERFORM S01-FLYTTA-BEART                                       
069800**         PERFORM IMS-GET-BENA11                                         
069900**       END-PERFORM                                                      
070000                                                                          
070100         PERFORM S02-FLYTTA-HOMONYMTEXT                                   
070200                                                                          
070300*        -- LÄGG TILL ARTIKELN FÖR DEN NYA BENÄMNINGEN                    
070400         MOVE IDARTNR-WS TO BENA-ART-IDARTNR                              
070500         MOVE NEJ        TO BENA-ART-FLFELHOMO                            
070600         MOVE WS-KDHOMONYM TO MOD-KDHOMONYM                               
070700         PERFORM IMS-ISRT-BENA12                                          
070800         IF NEVIS-ARTIKEL                                                 
070900           MOVE JA TO WS-FLAENDR-B                                        
071000         END-IF                                                           
071100         MOVE MED-1(IX) TO MOD-TEMFSINF                                   
071200       ELSE                                                               
071300         MOVE FEL-4(IX) TO MOD-TEMFSFEL                                   
071400         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDHOMONYM-ATTR                    
071500       END-IF                                                             
071600     ELSE                                                                 
071700       MOVE FEL-6(IX) TO MOD-TEMFSFEL                                     
071800       MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-NY-ATTR                       
071900                                  MOD-KDHOMONYM-ATTR                      
072000     END-IF                                                               
072100     .                                                                    
072200     EJECT                                                                
072300 C-LAS-BASEN SECTION.                                                     
072400     SKIP2                                                                
072500     MOVE IDARTNR-WS TO W-IDARTNR                                         
072600     PERFORM IMS-GET-BENA01-BSEQ                                          
072700     IF SEGMENT-FINNS                                                     
072800       MOVE BENA-BEN-IDBENNR TO W-IDBENNR                                 
072900       IF BENA-BEN-KDHOMONYM = ZERO                                       
073000         MOVE SPACE TO MOD-KDHOMONYM                                      
073100       ELSE                                                               
073200         MOVE BENA-BEN-KDHOMONYM TO MOD-KDHOMONYM                         
073300       END-IF                                                             
073400       IF BENA-BEN-KDBENSTAT = 1 OR 2                                     
073500         MOVE MED-2(IX) TO MOD-TEMFSFEL                                   
073600       END-IF                                                             
073700       PERFORM IMS-GET-BENA12-BSEQ                                        
073800       IF BENA-ART-FLFELHOMO = JA                                         
073900         MOVE FEL-5(IX) TO MOD-TEMFSFEL                                   
074000       ELSE                                                               
074100         EVALUATE TRUE                                                    
074200             WHEN BENA-ART-FLFELHOMO = NEJ AND MOD-KDHOMONYM =            
074300            SPACE                                                         
074400           MOVE ZERO TO MOD-KDHOMONYM                                     
074500         END-EVALUATE                                                     
074600         CONTINUE                                                         
074700       END-IF                                                             
074800       PERFORM IMS-GET-BENA01                                             
074900                                                                          
075000       MOVE +1 TO RAD-IX                                                  
075100       PERFORM UNTIL  RAD-IX > MAX-RAD                                    
075200         MOVE NYCKEL-IDSKYLT(RAD-IX) TO W-IDSKYLT                         
075300         PERFORM IMS-GNP-BENA11                                           
075400*        IF SEGMENT-FINNS                                                 
075500*          IF ( BENA-TEXT-IDSKYLT =                                       
075600*            'J  ' OR 'KOR' OR 'RC ' OR 'RUS' OR 'T  ' OR 'TR ' )         
075700*          AND ( BENA-TEXT-BEART NOT = SPACE )                            
075800*            MOVE MED-5(IX)      TO MOD-BEART(RAD-IX)                     
075900*          ELSE                                                           
076000*            MOVE BENA-TEXT-BEART TO MOD-BEART(RAD-IX)                    
076100*          END-IF                                                         
076200*        ELSE                                                             
076300*          MOVE MFS-RENSA-FAELT TO MOD-BEART(RAD-IX)                      
076400*        END-IF                                                           
076500         ADD +1 TO RAD-IX                                                 
076600       END-PERFORM                                                        
076700                                                                          
076800       MOVE SVENSKA TO W-IDSKYLT                                          
076900       PERFORM IMS-GU-BENA11                                              
077000*      MOVE BENA-TEXT-BEART TO MOD-BEART-S                                
077100                                                                          
077200       PERFORM IMS-GET-BENA01                                             
077300                                                                          
077400       MOVE +1 TO RAD-IX                                                  
077500       PERFORM UNTIL RAD-IX >= MAX-HOM-PLUS-1                             
077600         PERFORM IMS-GET-BENA13                                           
077700         IF SEGMENT-FINNS                                                 
077800           MOVE BENA-HOM-TEHOMONYM TO MOD-TEHOMONYM(RAD-IX)               
077900         ELSE                                                             
078000           MOVE MFS-RENSA-FAELT TO MOD-TEHOMONYM(RAD-IX)                  
078100         END-IF                                                           
078200         ADD +1 TO RAD-IX                                                 
078300       END-PERFORM                                                        
078400     ELSE                                                                 
078500       MOVE FEL-3(IX) TO MOD-TEMFSFEL                                     
078600     END-IF                                                               
078700     .                                                                    
078800     EJECT                                                                
078900 D-KOLLA-SKAERMEN SECTION.                                                
079000                                                                          
079100     MOVE JA TO INPUT-RETT                                                
079200                                                                          
079300     MOVE IDARTNR-WS TO W-IDARTNR                                         
079400     PERFORM IMS-GET-ARTC01                                               
079500     IF SEGMENT-FINNS                                                     
079600        MOVE ART-KDPRODSL TO TEST-KDPRODSL                                
079700        IF KDPRODSL-VOLVO-BIMA                                            
079800           IF KDPRODSL-VOLVO-ALL                                          
079900              SET NEVIS-ARTIKEL TO TRUE                                   
080000           END-IF                                                         
080100           IF CDC OR SDC                                                  
080200              CONTINUE                                                    
080300           ELSE                                                           
080400              MOVE NEJ TO INPUT-RETT                                      
080500              MOVE MED-4(IX) TO MOD-TEMFSINF                              
080600           END-IF                                                         
080700        END-IF                                                            
080800     END-IF                                                               
080900                                                                          
081000*    IF MID-FLRSBEART = JA OR NEJ                                         
081100*      MOVE MID-FLRSBEART TO WS-FLRSBEART                                 
081200*      MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLRSBEART-ATTR                    
081300*    ELSE                                                                 
081400*      EVALUATE TRUE                                                      
081500*      WHEN MID-FLRSBEART = ALL '+' OR SPACE                              
081600*        MOVE NEJ TO WS-FLRSBEART                                         
081700*        MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLRSBEART-ATTR                  
081800*       WHEN OTHER                                                        
081900*        MOVE MFS-ALFA-FAELT-FEL TO MOD-FLRSBEART-ATTR                    
082000*        MOVE NEJ TO INPUT-RETT                                           
082100*        MOVE MED-3(IX) TO MOD-TEMFSFEL                                   
082200*      END-EVALUATE                                                       
082300*    END-IF                                                               
082400     INSPECT MID-BEART-NY REPLACING ALL '<' BY SPACE                      
082500     INSPECT MID-BEART-NY REPLACING ALL '>' BY SPACE                      
082600     IF MID-BEART-NY = ALL '+' OR SPACE                                   
082700       IF MID-KDHOMONYM-NY = ALL '+' OR SPACE                             
082800         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDHOMONYM-ATTR                    
082900         MOVE MFS-ALFA-FAELT-FEL TO MOD-BEART-NY-ATTR                     
083000         MOVE NEJ TO INPUT-RETT                                           
083100         MOVE MED-3(IX) TO MOD-TEMFSFEL                                   
083200       ELSE                                                               
083300         IF MID-KDHOMONYM-NY NUMERIC                                      
083400           MOVE MID-KDHOMONYM-NY TO WS-KDHOMONYM                          
083500           MOVE SPACE TO WS-BEART                                         
083600           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDHOMONYM-ATTR                 
083700         ELSE                                                             
083800           MOVE MFS-NUM-FAELT-FEL TO MOD-KDHOMONYM-ATTR                   
083900           MOVE NEJ TO INPUT-RETT                                         
084000           MOVE MED-3(IX) TO MOD-TEMFSFEL                                 
084100         END-IF                                                           
084200       END-IF                                                             
084300     ELSE                                                                 
084400       MOVE MID-BEART-NY TO WS-BEART                                      
084500       IF MID-KDHOMONYM-NY = ALL '+' OR SPACE                             
084600         MOVE SPACE TO WS-KDHOMONYM                                       
084700       ELSE                                                               
084800         IF MID-KDHOMONYM-NY NUMERIC                                      
084900           MOVE MID-KDHOMONYM-NY TO WS-KDHOMONYM                          
085000           MOVE MFS-NUM-FAELT-RAETT TO MOD-KDHOMONYM-ATTR                 
085100         ELSE                                                             
085200           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDHOMONYM-ATTR                  
085300           MOVE NEJ TO INPUT-RETT                                         
085400           MOVE MED-3(IX) TO MOD-TEMFSFEL                                 
085500         END-IF                                                           
085600       END-IF                                                             
085700     END-IF                                                               
085800     .                                                                    
085900     EJECT                                                                
086000*E-ROER-EJ-FAELT SECTION.                                                 
086100     SKIP2                                                                
086200*    MOVE MFS-ROER-EJ-FAELT TO MOD-BEART-S                                
086300                                                                          
086400*    MOVE +1 TO RAD-IX                                                    
086500*    PERFORM UNTIL RAD-IX > MAX-RAD                                       
086600*      MOVE MFS-ROER-EJ-FAELT TO MOD-BEART(RAD-IX)                        
086700*      ADD +1 TO RAD-IX                                                   
086800*    END-PERFORM                                                          
086900     MOVE MFS-ROER-EJ-FAELT TO MOD-KDHOMONYM                              
087000*                              MOD-BEART-NY                               
087100*                              MOD-KDHOMONYM-NY                           
087200*                              MOD-FLRSBEART                              
087300*    .                                                                    
087400     EJECT                                                                
087500*S01-FLYTTA-BEART SECTION.                                                
087600*    SKIP2                                                                
087700*    EVALUATE TRUE                                                        
087800*                                                                         
087900*      WHEN BENA-TEXT-IDSKYLT = 'D  '                                     
088000*        MOVE BENA-TEXT-BEART TO MOD-BEART(1)                             
088100*                                                                         
088200*      WHEN BENA-TEXT-IDSKYLT = 'E  '                                     
088300*        MOVE BENA-TEXT-BEART TO MOD-BEART(2)                             
088400*                                                                         
088500*      WHEN BENA-TEXT-IDSKYLT = 'F  '                                     
088600*        MOVE BENA-TEXT-BEART TO MOD-BEART(3)                             
088700*                                                                         
088800*      WHEN BENA-TEXT-IDSKYLT = 'GB '                                     
088900*        MOVE BENA-TEXT-BEART TO MOD-BEART(4)                             
089000*                                                                         
089100*      WHEN BENA-TEXT-IDSKYLT = 'I  '                                     
089200*        MOVE BENA-TEXT-BEART TO MOD-BEART (5)                            
089300*                                                                         
089400*      WHEN BENA-TEXT-IDSKYLT = 'J  '                                     
089500*        IF BENA-TEXT-BEART NOT = SPACE                                   
089600*          MOVE MED-5(IX)     TO MOD-BEART (6)                            
089700*        END-IF                                                           
089800*                                                                         
089900*      WHEN BENA-TEXT-IDSKYLT = 'KOR'                                     
090000*        IF BENA-TEXT-BEART NOT = SPACE                                   
090100*          MOVE MED-5(IX)     TO MOD-BEART (7)                            
090200*        END-IF                                                           
090300*                                                                         
090400*      WHEN BENA-TEXT-IDSKYLT = 'MAL'                                     
090500*        MOVE BENA-TEXT-BEART TO MOD-BEART (8)                            
090600*                                                                         
090700*      WHEN BENA-TEXT-IDSKYLT = 'NL '                                     
090800*        MOVE BENA-TEXT-BEART TO MOD-BEART (9)                            
090900*                                                                         
091000*      WHEN BENA-TEXT-IDSKYLT = 'P  '                                     
091100*        MOVE BENA-TEXT-BEART TO MOD-BEART (10)                           
091200*                                                                         
091300*      WHEN BENA-TEXT-IDSKYLT = 'RC '                                     
091400*        IF BENA-TEXT-BEART NOT = SPACE                                   
091500*          MOVE MED-5(IX)     TO MOD-BEART (11)                           
091600*        END-IF                                                           
091700*                                                                         
091800*      WHEN BENA-TEXT-IDSKYLT = 'RUS'                                     
091900*        IF BENA-TEXT-BEART NOT = SPACE                                   
092000*          MOVE MED-5(IX)     TO MOD-BEART (12)                           
092100*        END-IF                                                           
092200*                                                                         
092300*      WHEN BENA-TEXT-IDSKYLT = 'S  '                                     
092400*        MOVE BENA-TEXT-BEART TO MOD-BEART-S                              
092500*                                                                         
092600*      WHEN BENA-TEXT-IDSKYLT = 'SF '                                     
092700*        MOVE BENA-TEXT-BEART TO MOD-BEART (13)                           
092800*                                                                         
092900*      WHEN BENA-TEXT-IDSKYLT = 'T  '                                     
093000*        IF BENA-TEXT-BEART NOT = SPACE                                   
093100*          MOVE MED-5(IX)     TO MOD-BEART (14)                           
093200*        END-IF                                                           
093300*                                                                         
093400*      WHEN BENA-TEXT-IDSKYLT = 'TR '                                     
093500*        IF BENA-TEXT-BEART NOT = SPACE                                   
093600*          MOVE MED-5(IX)     TO MOD-BEART (15)                           
093700*        END-IF                                                           
093800*                                                                         
093900*      WHEN BENA-TEXT-IDSKYLT = 'USA'                                     
094000*        MOVE BENA-TEXT-BEART TO MOD-BEART (16)                           
094100*                                                                         
094200*      WHEN OTHER CONTINUE                                                
094300*    END-EVALUATE                                                         
094400     .                                                                    
094500     EJECT                                                                
094600 S02-FLYTTA-HOMONYMTEXT SECTION.                                          
094700     SKIP2                                                                
094800     PERFORM IMS-GET-BENA01                                               
094900     MOVE +1 TO RAD-IX                                                    
095000     PERFORM UNTIL RAD-IX >= MAX-HOM-PLUS-1                               
095100       PERFORM IMS-GET-BENA13                                             
095200       IF SEGMENT-FINNS                                                   
095300         MOVE BENA-HOM-TEHOMONYM TO MOD-TEHOMONYM(RAD-IX)                 
095400       ELSE                                                               
095500         MOVE MFS-RENSA-FAELT TO MOD-TEHOMONYM(RAD-IX)                    
095600       END-IF                                                             
095700       ADD +1 TO RAD-IX                                                   
095800     END-PERFORM                                                          
095900     .                                                                    
096000     EJECT                                                                
096100* IMS SEKTIONER                                                           
096200     SKIP3                                                                
096300 IMS-GET-MSG SECTION.                                                     
096400     SKIP2                                                                
096500     MOVE '  QC' TO GODK-STATUSKODER                                      
096600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
096700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
096800     PERFORM IMS-STATUS-KONTROLL                                          
096900     .                                                                    
097000     SKIP3                                                                
097100 IMS-INSERT-MSG SECTION.                                                  
097200     SKIP2                                                                
097300*    IF MSGI-IDLAND-SPR = 'SE'                                            
097400*      MOVE '0' TO MFS-KDHUVOMR                                           
097500*    END-IF                                                               
097600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
097700     MOVE SPACE TO GODK-STATUSKODER                                       
097800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
097900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
098000     PERFORM IMS-STATUS-KONTROLL                                          
098100     .                                                                    
098200     EJECT                                                                
098300*IMS-GET-XXAI01 SECTION.                                                  
098400*    STRING 'WLXXAI01(WDGXKEY  =' W-1207-KEY-X ')'                        
098500*            DELIMITED BY SIZE INTO SSA1                                  
098600*    MOVE '  GE' TO GODK-STATUSKODER                                      
098700*    CALL CBLTDLI USING GU XXAI-PCB DLI-IO-AREA SSA1                      
098800*    MOVE XXAI-STATUS-CODE TO STATUS-WS                                   
098900*    PERFORM IMS-STATUS-KONTROLL                                          
099000*    .                                                                    
099100     SKIP3                                                                
099200*IMS-GET-XXAI11 SECTION.                                                  
099300*    MOVE 'WLXXAI11 ' TO SSA1                                             
099400*    MOVE '  ' TO GODK-STATUSKODER                                        
099500*    CALL CBLTDLI USING GHNP XXAI-PCB DLI-IO-AREA SSA1                    
099600*    MOVE XXAI-STATUS-CODE TO STATUS-WS                                   
099700*    PERFORM IMS-STATUS-KONTROLL                                          
099800*    .                                                                    
099900     SKIP3                                                                
100000*IMS-REPL-XXAI11 SECTION.                                                 
100100*    MOVE '  ' TO GODK-STATUSKODER                                        
100200*    CALL CBLTDLI USING REPL XXAI-PCB DLI-IO-AREA                         
100300*    MOVE XXAI-STATUS-CODE TO STATUS-WS                                   
100400*    PERFORM IMS-STATUS-KONTROLL                                          
100500*    .                                                                    
100600     EJECT                                                                
100700 IMS-GET-BENA01-BSEQ SECTION.                                             
100800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
100900             DELIMITED BY SIZE INTO SSA1                                  
101000     MOVE '  GE' TO GODK-STATUSKODER                                      
101100     CALL CBLTDLI USING GN BENC-PCB DLI-IO-AREA SSA1                      
101200     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
101300     PERFORM IMS-STATUS-KONTROLL                                          
101400     .                                                                    
101500     SKIP3                                                                
101600 IMS-GU-BENA11-BSEQ SECTION.                                              
101700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
101800             DELIMITED BY SIZE INTO SSA1                                  
101900     MOVE '  GE' TO GODK-STATUSKODER                                      
102000     CALL CBLTDLI USING GU BENC-PCB DLI-IO-AREA SSA1                      
102100     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
102200     PERFORM IMS-STATUS-KONTROLL                                          
102300     .                                                                    
102400     SKIP3                                                                
102500 IMS-GET-BENA12-BSEQ SECTION.                                             
102600     STRING 'WLBENA12(IDARTNR  =' W-IDARTNR-X ')'                         
102700             DELIMITED BY SIZE INTO SSA1                                  
102800     MOVE '  GE' TO GODK-STATUSKODER                                      
102900     CALL CBLTDLI USING GHU BENC-PCB DLI-IO-AREA SSA1                     
103000     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
103100     PERFORM IMS-STATUS-KONTROLL                                          
103200     .                                                                    
103300     EJECT                                                                
103400 IMS-DLET-BENA12-BSEQ SECTION.                                            
103500     MOVE '  ' TO GODK-STATUSKODER                                        
103600     CALL CBLTDLI USING DLET BENC-PCB DLI-IO-AREA                         
103700     MOVE BENC-STATUS-CODE TO STATUS-WS                                   
103800     PERFORM IMS-STATUS-KONTROLL                                          
103900     .                                                                    
104000     SKIP3                                                                
104100 IMS-GET-BENA01-ASEQ SECTION.                                             
104200     STRING 'WLBENA01(WDD3ASEQ =' W-IDSKYLT-X                             
104300             W-BEART-X ')'                                                
104400             DELIMITED BY SIZE INTO SSA1                                  
104500     MOVE '  GE' TO GODK-STATUSKODER                                      
104600     CALL CBLTDLI USING GN BENB-PCB DLI-IO-AREA SSA1                      
104700     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
104800     PERFORM IMS-STATUS-KONTROLL                                          
104900     .                                                                    
105000     SKIP3                                                                
105100 IMS-GET-BENA13-ASEQ SECTION.                                             
105200     MOVE 'WLBENA13 ' TO SSA1                                             
105300     MOVE '  GE' TO GODK-STATUSKODER                                      
105400     CALL CBLTDLI USING GNP BENB-PCB DLI-IO-AREA                          
105500     MOVE BENB-STATUS-CODE TO STATUS-WS                                   
105600     PERFORM IMS-STATUS-KONTROLL                                          
105700     .                                                                    
105800     SKIP3                                                                
105900 IMS-GET-BENA01 SECTION.                                                  
106000     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
106100             DELIMITED BY SIZE INTO SSA1                                  
106200     MOVE '  GE' TO GODK-STATUSKODER                                      
106300     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
106400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
106500     PERFORM IMS-STATUS-KONTROLL                                          
106600     .                                                                    
106700     EJECT                                                                
106800 IMS-GHU-BENA01 SECTION.                                                  
106900     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
107000             DELIMITED BY SIZE INTO SSA1                                  
107100     MOVE '  GE' TO GODK-STATUSKODER                                      
107200     CALL CBLTDLI USING GHU BENA-PCB DLI-IO-AREA SSA1                     
107300     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
107400     PERFORM IMS-STATUS-KONTROLL                                          
107500     .                                                                    
107600     EJECT                                                                
107700 IMS-REPL-BENA01 SECTION.                                                 
107800     MOVE '  ' TO GODK-STATUSKODER                                        
107900     CALL CBLTDLI USING REPL BENA-PCB DLI-IO-AREA                         
108000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
108100     PERFORM IMS-STATUS-KONTROLL                                          
108200     .                                                                    
108300     EJECT                                                                
108400*IMS-GET-BENA11 SECTION.                                                  
108500*    MOVE 'WLBENA11 ' TO SSA1                                             
108600*    MOVE '  GE' TO GODK-STATUSKODER                                      
108700*    CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
108800*    MOVE BENA-STATUS-CODE TO STATUS-WS                                   
108900*    PERFORM IMS-STATUS-KONTROLL                                          
109000*    .                                                                    
109100     SKIP3                                                                
109200 IMS-GU-BENA11 SECTION.                                                   
109300     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
109400             DELIMITED BY SIZE INTO SSA1                                  
109500     MOVE '  GE' TO GODK-STATUSKODER                                      
109600     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1                      
109700     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
109800     PERFORM IMS-STATUS-KONTROLL                                          
109900     .                                                                    
110000     SKIP3                                                                
110100 IMS-GNP-BENA11 SECTION.                                                  
110200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
110300             DELIMITED BY SIZE INTO SSA1                                  
110400     MOVE '  GE' TO GODK-STATUSKODER                                      
110500     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
110600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
110700     PERFORM IMS-STATUS-KONTROLL                                          
110800     .                                                                    
110900     SKIP3                                                                
111000*IMS-ISRT-BENA01 SECTION.                                                 
111100*    MOVE 'WLBENA01 ' TO SSA1                                             
111200*    MOVE '  ' TO GODK-STATUSKODER                                        
111300*    CALL CBLTDLI USING ISRT BENA-PCB DLI-IO-AREA SSA1                    
111400*    MOVE BENA-STATUS-CODE TO STATUS-WS                                   
111500*    PERFORM IMS-STATUS-KONTROLL                                          
111600*    .                                                                    
111700     EJECT                                                                
111800*IMS-ISRT-BENA11 SECTION.                                                 
111900*    STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X ')'                         
112000*             DELIMITED BY SIZE INTO SSA1                                 
112100*    MOVE 'WLBENA11 ' TO SSA2                                             
112200*    MOVE '  ' TO GODK-STATUSKODER                                        
112300*    CALL CBLTDLI USING ISRT BENA-PCB DLI-IO-AREA SSA1 SSA2               
112400*    MOVE BENA-STATUS-CODE TO STATUS-WS                                   
112500*    PERFORM IMS-STATUS-KONTROLL                                          
112600*    .                                                                    
112700     SKIP3                                                                
112800 IMS-ISRT-BENA12 SECTION.                                                 
112900     STRING 'WLBENA01(IDBENNR  =' W-IDBENNR-X  ')'                        
113000              DELIMITED BY SIZE INTO SSA1                                 
113100     MOVE 'WLBENA12  ' TO SSA2                                            
113200     MOVE '  ' TO GODK-STATUSKODER                                        
113300     CALL CBLTDLI USING ISRT BENA-PCB DLI-IO-AREA SSA1 SSA2               
113400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
113500     PERFORM IMS-STATUS-KONTROLL                                          
113600     .                                                                    
113700     SKIP3                                                                
113800 IMS-GET-BENA13 SECTION.                                                  
113900     MOVE 'WLBENA13 ' TO SSA1                                             
114000     MOVE '  GE' TO GODK-STATUSKODER                                      
114100     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA SSA1                     
114200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
114300     PERFORM IMS-STATUS-KONTROLL                                          
114400     .                                                                    
114500     EJECT                                                                
114600 IMS-STATUS-KONTROLL SECTION.                                             
114700     SET STATUS-IX TO 1                                                   
114800     SEARCH GODK-STATUS                                                   
114900       AT END                                                             
115000         CALL FELLOG                                                      
115100     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
115200       CONTINUE                                                           
115300     END-SEARCH                                                           
115400     .                                                                    
115500     SKIP3                                                                
115600 IMS-GET-ARTC01 SECTION.                                                  
115700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
115800            DELIMITED BY SIZE INTO SSA1                                   
115900     MOVE '  GE' TO GODK-STATUSKODER                                      
116000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-2 SSA1                    
116100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
116200     PERFORM IMS-STATUS-KONTROLL                                          
116300     .                                                                    
