000100 ID DIVISION.                                                             
000200 PROGRAM-ID. W5010500.                                                    
000300 AUTHOR. RICHARD THÖRNGREN.                                               
000400 DATE-WRITTEN. SEPTEMBER 78.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION.                                                            
000800*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
000900*     THIS PROGRAM ALSO HAS A WEB-LDC VERSION CALLED WL0170               
001000*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!             
001100*                TP-PROGRAM FÖR EKONOMI (INVENTERINGSJUSTERINGAR)         
001200*                HÄMTAS FRÅN INVENTERINGSHISTORIKREGISTRET (WDH7)         
001300*                                                                         
001400*    OBS - FIX-LÖSNING FÖR LDC                                 !!!        
001500*    OBS - MÅSTE GÖRAS MER GENERELL OM LDC-BEGREPPET UTVECKLAS !!!        
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W5T105                                              
001900*        MID:         W5I10501                                            
002000*    UTDATA.                                                              
002100*        MOD:         W5O10501                                            
002200*    SUBPROGRAM.                                                          
002300*        FELLOG                                                           
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP3                                                                
002700 DATA DIVISION.                                                           
002800     EJECT                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77      IDPGM           PIC X(8)    VALUE 'W5010500'.                    
003300 77      JA              PIC X       VALUE 'J'.                           
003400 77      NEJ             PIC X       VALUE 'N'.                           
003500                                                                          
003600 77      WS-IDDC         PIC X(2)    VALUE SPACE.                         
003700 77      WS-CDC-11       PIC X(2)    VALUE '11'.                          
003800                                                                          
003900 77      CDC-FINNS       PIC X       VALUE 'N'.                           
004000 77      SDC-NDC-FINNS   PIC X       VALUE 'N'.                           
004100 77      NDC-FINNS       PIC X       VALUE 'N'.                           
004200 77      HAEMTA-INFO     PIC X       VALUE 'J'.                           
004300 77      IDARTNR-WS      PIC X(9)    VALUE SPACE.                         
004400 77      MAX-INDX        PIC S9(2)   VALUE +12   COMP SYNC.               
004500 77      MAX-RAD         PIC S9(2)   VALUE +2    COMP SYNC.               
004600 77      MAX-RAD1        PIC S9(2)   VALUE +2    COMP SYNC.               
004700 77      INDX            PIC S9(2)   VALUE +0    COMP SYNC.               
004800 77      RAD             PIC S9(1)   VALUE +0    COMP SYNC.               
004900 77      RAD1            PIC S9(1)   VALUE +0    COMP SYNC.               
005000 77      SPRAK-IX        PIC S9(9)   VALUE +0    COMP SYNC.               
005100 77      MAX-MOD-LAENGD  PIC S9(4)   VALUE +889  COMP SYNC.               
005200 77      W-TIJUSTDA1     PIC S9(7)   VALUE +9999999 COMP-3.               
005300 77      W-TIJUSTDA2     PIC S9(7)   VALUE +9999999 COMP-3.               
005400 77      ANTAL-CDC       PIC S9(9)   VALUE +0    COMP-3.                  
005500 77      ANTAL-NDC       PIC S9(9)   VALUE +0    COMP-3.                  
005600 77      ANTAL-SDC-NDC   PIC S9(9)   VALUE +0    COMP-3.                  
005700 01     W-KVJUSTYP-TOT.                                                   
005800        03 W-KVJUSTYP    PIC X.                                           
005900        03 W-FLAUTLSJ    PIC X.                                           
006000                                                                          
006100*   --- DC DEFINITION FOR FRANCE.                                         
006200*       DC 22 IS A VALID INPUT TO THIS SCREEN. SO WE NEED A CONST.        
006300*                                                                         
006400 77    WDB6-A-SW         PIC X       VALUE 'J'.                           
006500       88  WDB6-A-FINNS              VALUE 'J'.                           
006600       88  WDB6-A-SAKNAS             VALUE 'N'.                           
006700                                                                          
006800 77    WDB6-B-SW         PIC X       VALUE 'J'.                           
006900       88  WDB6-B-FINNS              VALUE 'J'.                           
007000       88  WDB6-B-SAKNAS             VALUE 'N'.                           
007100                                                                          
007200 77    WDB6-C-SW         PIC X       VALUE 'J'.                           
007300       88  WDB6-C-FINNS              VALUE 'J'.                           
007400       88  WDB6-C-SAKNAS             VALUE 'N'.                           
007500                                                                          
007600 77    NYCKLAR-SW        PIC X       VALUE 'J'.                           
007700       88  NYCKLAR-OK                VALUE 'J'.                           
007800       88  NYCKLAR-FEL               VALUE 'N'.                           
007900                                                                          
008000 77    W-IDTRANS         PIC X(4)    VALUE SPACE.                         
008100       88  EGEN-MID                  VALUE '5105'.                        
008200       88  GODK-MID                  VALUE '5101' '5102'                  
008300                                           '5103' '5104'                  
008400                                           '5106' '5107'                  
008500                                           '5108' '5109'.                 
008600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008700 01  GENERELLA-SUBPROGRAM.                                                
008800     03   WDATKONV                   PIC X(8)  VALUE 'WDATKONV'.          
008900     03   WMEDKONV                   PIC X(8)  VALUE 'WMEDKONV'.          
009000     03   CBLTDLI                    PIC X(8)  VALUE 'CBLTDLI '.          
009100     03   FELLOG                     PIC X(8)  VALUE 'FELLOG  '.          
009200     03  W005INIT                    PIC X(8)  VALUE 'W005INIT'.          
009300     SKIP3                                                                
009400*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009500*01 -COPY WMSGINIT                                                        
009600     SKIP3                                                                
009700*   --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                               
009800*01 -COPY WMEDAREA                                                        
009900 01  MESSAGE-CODES.                                                       
010000     03  INF-FIRST-PAGE              PIC X(3)  VALUE '006'.               
010100     03  ERR-PART-MISSING            PIC X(3)  VALUE '017'.               
010200     03  ERR-PART-DELETED            PIC X(3)  VALUE '018'.               
010300     03  INF-LAST-PAGE               PIC X(3)  VALUE '106'.               
010400     03  INF-MORE-INFO-EXISTS        PIC X(3)  VALUE '169'.               
010500     03  ERR-WRONG-KEY               PIC X(3)  VALUE '401'.               
010600 01      NYCKLAR-TILL-DLI.                                                
010700                                                                          
010800   03    W-IDARTNR-X.                                                     
010900     05  W-IDARTNR       PIC S9(9)   VALUE ZERO  COMP-3.                  
011000                                                                          
011100   03    W-KDSEGKEY-X.                                                    
011200     05  W-KDSEGKEY      PIC X       VALUE '1'.                           
011300                                                                          
011400   03    W-IDDC-X.                                                        
011500     05  W-IDDC          PIC X(2)    VALUE SPACE.                         
011600                                                                          
011700   03    W-IDDC-CDC-X.                                                    
011800     05  W-IDDC-CDC      PIC X(2)    VALUE '11'.                          
011900                                                                          
012000   03    W-IDDC-ALL-X.                                                    
012100     05  W-IDDC-ALL      PIC X(2)    VALUE '6Z'.                          
012200                                                                          
012300   03    W-IDDC-NDC-X.                                                    
012400     05  W-IDDC-NDC      PIC X(2)    VALUE SPACE.                         
012500                                                                          
012600   03    W-IDDC-CDC-MIN-X.                                                
012700     05  W-IDDC-CDC-MIN  PIC X(2)    VALUE SPACE.                         
012800                                                                          
012900   03    W-IDDC-CDC-MAX-X.                                                
013000     05  W-IDDC-CDC-MAX  PIC X(2)    VALUE SPACE.                         
013100                                                                          
013200   03    W-DAREGDAT-X.                                                    
013300     05  W-DAREGDAT      PIC 9(8)   VALUE 99999999.                       
013400                                                                          
013500   03  W-IDDC-B6-X.                                                       
013600     05  W-IDDC-B6       PIC X(2).                                        
013700                                                                          
013800     EJECT                                                                
013900******************************************************************        
014000*                                                                         
014100*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
014200*                                                                         
014300 01      FILLER          PIC X(16)   VALUE 'MFS-WS          '.            
014400     SKIP3                                                                
014500*01      MID -COPY W5I10501 -PRE MID-.                                    
014600     EJECT                                                                
014700*01      -COPY WMSGAREA                                                   
014800     EJECT                                                                
014900*  03    MOD -COPY W5O10501 -PRE MOD- -RED MSG-AREA.                      
015000     EJECT                                                                
015100*01      -COPY WMFSAREA                                                   
015200     EJECT                                                                
015300 01  FILLER              PIC X(16)  VALUE 'WDATAREA'.                     
015400*01      -COPY WDATAREA                                                   
015500     EJECT                                                                
015600******************************************************************        
015700*****                                                                     
015800*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015900*****                                                                     
016000 01  IMS-WS.                                                              
016100   03    FILLER          PIC X(16)   VALUE '     IMS-WS     '.            
016200     SKIP3                                                                
016300*****                    **** STATUS-KOD FRÅN IMS                         
016400   03    STATUS-WS       PIC XX.                                          
016500         88  SEGMENT-FINNS       VALUE '  '.                              
016600         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
016700         88  SEGMENT-FINNS-REDAN VALUE 'II'.                              
016800     SKIP3                                                                
016900   03    GODK-STATUSKODER.                                                
017000     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017100     SKIP3                                                                
017200 01      SSA1            PIC X(128).                                      
017300 01      SSA2            PIC X(128).                                      
017400 01      SSA3            PIC X(128).                                      
017500     EJECT                                                                
017600*                            IMS FUNKTIONSKODER                           
017700*01      -COPY W0003                                                      
017800     EJECT                                                                
017900*                            DLI INPUT-OUTPUT AREA                        
018000 01      DLI-IO-AREA     PIC X(200)  VALUE SPACE.                         
018100     SKIP3                                                                
018200*01      WLARTC01 -COPY WDK601              -RED DLI-IO-AREA.             
018300     EJECT                                                                
018400 01      DLI-IO-AREA2    PIC X(200)  VALUE SPACE.                         
018500     SKIP3                                                                
018600*01      WLINVC01 -COPY WDH701              -RED DLI-IO-AREA2.            
018700     EJECT                                                                
018800*01      WLINVC11 -COPY WDH711              -RED DLI-IO-AREA2.            
018900     EJECT                                                                
019000 01  FILLER               PIC X(16)   VALUE 'WDB601-A AREA'.              
019100 01   DLI-IO-AREA-B601-A.                                                 
019200*     03  -COPY WDB601 -PRE A-                                            
019300     EJECT                                                                
019400 01  FILLER               PIC X(16)   VALUE 'WDB601-B AREA'.              
019500 01   DLI-IO-AREA-B601-B.                                                 
019600*     03  -COPY WDB601 -PRE B-                                            
019700     EJECT                                                                
019800 01  FILLER               PIC X(16)   VALUE 'WDB601-C AREA'.              
019900 01   DLI-IO-AREA-B601-C.                                                 
020000*     03  -COPY WDB601 -PRE C-                                            
020100                                                                          
020200     EJECT                                                                
020300 LINKAGE SECTION.                                                         
020400*01  -COPY W0009     -PRE MSG-                                            
020500                                                                          
020600*01  -COPY W0008     -PRE USEA-.                                          
020700         05  FILLER           PIC X.                                      
020800                                                                          
020900*01  -COPY W0008     -PRE AC-                                             
021000         05  FILLER           PIC X.                                      
021100     EJECT                                                                
021200*01  -COPY W0008     -PRE INVC-                                           
021300         05  FILLER           PIC X.                                      
021400     EJECT                                                                
021500*01  -COPY W0008     -PRE WDB6-                                           
021600     05  FILLER               PIC X.                                      
021700                                                                          
021800     EJECT                                                                
021900 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
022000                                  AC-PCB INVC-PCB WDB6-PCB.               
022100 MAIN SECTION.                                                            
022200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
022300                                  AC-PCB INVC-PCB WDB6-PCB.               
022400                                                                          
022500     PERFORM IMS-GET-MSG                                                  
022600     IF SEGMENT-FINNS                                                     
022700       PERFORM A-INIT                                                     
022800       PERFORM B-KOLLA-NYCKEL                                             
022900       IF NYCKLAR-OK                                                      
023000         IF MFS-FIRST                                                     
023100           PERFORM C-FOERSTA-SIDA                                         
023200         ELSE                                                             
023300           IF MFS-ENTER                                                   
023400             PERFORM D-NAESTA-SIDA                                        
023500           ELSE                                                           
023600             PERFORM E-SAMMA-SIDA                                         
023700           END-IF                                                         
023800         END-IF                                                           
023900         PERFORM F-LAS-INVENTERING                                        
024000       END-IF                                                             
024100       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
024200       PERFORM IMS-INSERT-MSG                                             
024300     END-IF                                                               
024400     MOVE ZERO TO RETURN-CODE                                             
024500     GOBACK                                                               
024600     .                                                                    
024700     EJECT                                                                
024800 A-INIT SECTION.                                                          
024900     IF MSG-DUBBLA-TRANSKODER                                             
025000       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W5I10501                 
025100       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
025200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
025300     ELSE                                                                 
025400       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W5I10501                  
025500       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
025600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
025700     END-IF                                                               
025800                                                                          
025900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
026000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
026100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
026200                                                                          
026300     MOVE LOW-VALUE TO MSG-AREA                                           
026400     MOVE 'W5O105N1' TO MFS-IDMOD                                         
026500     MOVE '5105' TO MOD-IDTRANS                                           
026600     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
026700                                                                          
026800     IF NOT EGEN-MID                                                      
026900       MOVE SPACE TO MFS-KDTRTYP                                          
027000       MOVE '7' TO MFS-IDPFK                                              
027100     END-IF                                                               
027200                                                                          
027300     .                                                                    
027400     EJECT                                                                
027500 B-KOLLA-NYCKEL SECTION.                                                  
027600     MOVE JA TO NYCKLAR-SW                                                
027700                                                                          
027800     MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR-IN                             
027900                                                                          
028000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
028100     MOVE '001'             TO MSGI-KDCALL                                
028200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
028300     MOVE '5105'            TO MSGI-IDTRANS                               
028400     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
028500                                                                          
028600     IF MFS-IDTRANS = '5105' OR (MID-IDARTNR-IN NUMERIC                   
028700          AND MID-IDARTNR-IN > ZERO)                                      
028800       MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                                
028900     END-IF                                                               
029000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
029100                                                                          
029200     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
029300       MOVE +2 TO SPRAK-IX                                                
029400       MOVE 'GB ' TO MED-IDSKYLT                                          
029500     ELSE                                                                 
029600       MOVE +1 TO SPRAK-IX                                                
029700       MOVE 'S  ' TO MED-IDSKYLT                                          
029800     END-IF                                                               
029900                                                                          
030000     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
030100     INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                   
030200                                                                          
030300     IF MID-IDARTNR-IN = ALL '+'                                          
030400       CONTINUE                                                           
030500     ELSE                                                                 
030600       MOVE '7'            TO MFS-IDPFK                                   
030700       MOVE SPACE          TO MFS-KDTRTYP                                 
030800     END-IF                                                               
030900     IF IDARTNR-WS NOT NUMERIC                                            
031000       MOVE NEJ TO NYCKLAR-SW                                             
031100     END-IF                                                               
031200                                                                          
031300     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
031400                                                                          
031500     IF MID-IDDC-IN = ALL '+'                                             
031600       MOVE MID-IDDC-UT   TO WS-IDDC                                      
031700                             W-IDDC-B6                                    
031800       PERFORM IMS-GU-WDB601-A                                            
031900       IF A-DCS-KDDC NOT = SPACE AND NOT A-DCS-DDC                        
032000         INSPECT WS-IDDC REPLACING LEADING SPACE BY ZERO                  
032100       ELSE                                                               
032200         MOVE MSGI-IDDC   TO WS-IDDC                                      
032300         MOVE '7'         TO MFS-IDPFK                                    
032400         MOVE SPACE       TO MFS-KDTRTYP                                  
032500       END-IF                                                             
032600     ELSE                                                                 
032700       MOVE MID-IDDC-IN TO WS-IDDC                                        
032800       MOVE '7'         TO MFS-IDPFK                                      
032900       MOVE SPACE       TO MFS-KDTRTYP                                    
033000     END-IF                                                               
033100                                                                          
033200     IF WS-IDDC NOT = A-DCS-IDDC                                          
033300        MOVE WS-IDDC TO W-IDDC-B6                                         
033400        PERFORM IMS-GU-WDB601-A                                           
033500     END-IF                                                               
033600                                                                          
033700     IF A-DCS-KDDC = SPACE OR A-DCS-DDC                                   
033800       IF NOT EGEN-MID                                                    
033900         MOVE ZERO TO WS-IDDC                                             
034000       ELSE                                                               
034100         MOVE NEJ TO NYCKLAR-SW                                           
034200       END-IF                                                             
034300     END-IF                                                               
034400                                                                          
034500     MOVE WS-IDDC   TO W-IDDC-B6                                          
034600     MOVE JA        TO WDB6-A-SW                                          
034700     PERFORM IMS-GU-WDB601-A                                              
034800     IF SEGMENT-SAKNAS                                                    
034900        MOVE NEJ    TO WDB6-A-SW                                          
035000     END-IF                                                               
035100                                                                          
035200     MOVE MSGI-IDDC TO W-IDDC-B6                                          
035300     MOVE JA        TO WDB6-B-SW                                          
035400     PERFORM IMS-GU-WDB601-B                                              
035500     IF SEGMENT-SAKNAS                                                    
035600        MOVE NEJ    TO WDB6-B-SW                                          
035700     END-IF                                                               
035800                                                                          
035900     IF WDB6-B-FINNS                                                      
036000       CONTINUE                                                           
036100     ELSE                                                                 
036200       MOVE NEJ TO NYCKLAR-SW                                             
036300     END-IF                                                               
036400                                                                          
036500     IF EGEN-MID OR NYCKLAR-OK                                            
036600       MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                  
036700       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
036800       MOVE WS-IDDC    TO MOD-IDDC-UT                                     
036900       INSPECT MOD-IDDC-UT REPLACING LEADING ZERO BY SPACE                
037000     ELSE                                                                 
037100       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                             
037200       MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                                
037300     END-IF                                                               
037400                                                                          
037500     IF NYCKLAR-FEL                                                       
037600       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
037700       CALL WMEDKONV USING MED-WMEDAREA                                   
037800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
037900       PERFORM MFS-RENSA-FAELT-UT                                         
038000     END-IF                                                               
038100     .                                                                    
038200     EJECT                                                                
038300 C-FOERSTA-SIDA SECTION.                                                  
038400     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
038500     CALL WMEDKONV USING MED-WMEDAREA                                     
038600     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
038700     .                                                                    
038800     EJECT                                                                
038900 D-NAESTA-SIDA SECTION.                                                   
039000     MOVE MID-TIJUSTDA1-NEXT  TO W-TIJUSTDA1                              
039100     MOVE MID-TIJUSTDA2-NEXT  TO W-TIJUSTDA2                              
039200     .                                                                    
039300     EJECT                                                                
039400 E-SAMMA-SIDA SECTION.                                                    
039500     MOVE MID-TIJUSTDA1-ENTER TO W-TIJUSTDA1                              
039600     MOVE MID-TIJUSTDA2-ENTER TO W-TIJUSTDA2                              
039700     .                                                                    
039800     EJECT                                                                
039900 F-LAS-INVENTERING SECTION.                                               
040000     SKIP2                                                                
040100     PERFORM FA-KOLLA-IDDC                                                
040200                                                                          
040300     IF  HAEMTA-INFO   = JA                                               
040400       MOVE IDARTNR-WS TO W-IDARTNR                                       
040500       PERFORM IMS-GET-CDC-ARTIKEL                                        
040600       IF SEGMENT-FINNS                                                   
040700         IF ART-KDERS-UTG > +0                                            
040800           MOVE ERR-PART-DELETED TO MED-IDMFSFEL                          
040900           CALL WMEDKONV USING MED-WMEDAREA                               
041000           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
041100         END-IF                                                           
041200                                                                          
041300                                                                          
041400         PERFORM IMS-GU-ROT-INVHIST                                       
041500         IF SEGMENT-FINNS                                                 
041600           IF(WDB6-B-FINNS    AND                                         
041700             (B-DCS-CDC       OR                                          
041800              B-DCS-CDC-TR    OR                                          
041900             (B-DCS-SDC AND B-DCS-IDLANDX2 NOT = 'CN') OR                 
042000              B-DCS-JAPAN OR                                              
042010              B-DCS-AUSTRALIA))                                           
042100             PERFORM FB-CDC                                               
042200             PERFORM FC-SDC-NDC-PF                                        
042300           END-IF                                                         
042400                                                                          
042500           IF WDB6-B-FINNS    AND                                         
042600             (B-DCS-NDC-NA OR                                             
042700              B-DCS-LAND-NON-VCC-OWNED)                                   
042800             PERFORM FD-NDC                                               
042900           END-IF                                                         
043000         ELSE                                                             
043100           MOVE ERR-PART-MISSING TO MED-IDMFSFEL                          
043200           CALL WMEDKONV USING MED-WMEDAREA                               
043300           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
043400           PERFORM MFS-RENSA-FAELT-UT                                     
043500           MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                           
043600         END-IF                                                           
043700       ELSE                                                               
043800         MOVE ERR-PART-MISSING TO MED-IDMFSFEL                            
043900         CALL WMEDKONV USING MED-WMEDAREA                                 
044000         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
044100         PERFORM MFS-RENSA-FAELT-UT                                       
044200         MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                             
044300       END-IF                                                             
044400     END-IF                                                               
044500     .                                                                    
044600     EJECT                                                                
044700 FA-KOLLA-IDDC SECTION.                                                   
044800     SKIP2                                                                
044900     MOVE JA TO HAEMTA-INFO                                               
045000     IF (WDB6-A-FINNS    AND                                              
045100       (A-DCS-CDC       OR                                                
045200        A-DCS-CDC-TR    OR                                                
045310       (A-DCS-SDC AND A-DCS-IDLANDX2 NOT = 'CN') OR                       
045400        A-DCS-JAPAN OR                                                    
045410        A-DCS-AUSTRALIA))                                                 
045500                                                                          
045600       IF (WDB6-B-FINNS    AND                                            
045700         (B-DCS-CDC       OR                                              
045800          B-DCS-CDC-TR    OR                                              
045910         (B-DCS-SDC AND B-DCS-IDLANDX2 NOT = 'CN') OR                     
045920          B-DCS-JAPAN OR                                                  
045930          B-DCS-AUSTRALIA))                                               
046100                                                                          
046200         IF WDB6-A-FINNS  AND                                             
046300           (A-DCS-CDC     OR                                              
046400            A-DCS-CDC-TR)                                                 
046500           MOVE ZERO     TO W-IDDC                                        
046600         ELSE                                                             
046700           MOVE WS-IDDC  TO W-IDDC                                        
046800         END-IF                                                           
046900       ELSE                                                               
047000         MOVE NEJ TO HAEMTA-INFO                                          
047100         MOVE ERR-WRONG-KEY    TO MED-IDMFSINF                            
047200         CALL WMEDKONV USING MED-WMEDAREA                                 
047300         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
047400       END-IF                                                             
047500     ELSE                                                                 
047600       IF WDB6-B-FINNS    AND                                             
047620          (B-DCS-LAND-NON-VCC-OWNED OR                                    
047700           B-DCS-NDC-NA)                                                  
047800*        LÄS BARA NDC                                                     
047900         CONTINUE                                                         
048000       ELSE                                                               
048100         MOVE NEJ TO HAEMTA-INFO                                          
048200         MOVE ERR-WRONG-KEY    TO MED-IDMFSINF                            
048300         CALL WMEDKONV USING MED-WMEDAREA                                 
048400         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
048500       END-IF                                                             
048600     END-IF                                                               
048700     .                                                                    
048800     EJECT                                                                
048900 FB-CDC SECTION.                                                          
049000     MOVE NEJ TO CDC-FINNS                                                
049100     MOVE +1 TO INDX                                                      
049200     MOVE +1 TO RAD                                                       
049300     IF (W-TIJUSTDA1 = +9999999) AND                                      
049400        (W-TIJUSTDA2 NOT = +9999999)                                      
049500       MOVE 'GE' TO STATUS-WS                                             
049600       MOVE MFS-RENSA-FAELT TO MOD-SUINVJUST-C1                           
049700     ELSE                                                                 
049800       MOVE +0 TO ANTAL-CDC                                               
049900       MOVE W-TIJUSTDA1 TO W-DAREGDAT                                     
050000       IF W-TIJUSTDA1 NOT = ZERO                                          
050100         IF W-TIJUSTDA1 < 500000                                          
050200           MOVE 20 TO W-DAREGDAT (1:2)                                    
050300         ELSE                                                             
050400           IF W-TIJUSTDA1 < 999999                                        
050500             MOVE 19 TO W-DAREGDAT (1:2)                                  
050600           ELSE                                                           
050700             MOVE 99999999 TO W-DAREGDAT                                  
050800           END-IF                                                         
050900         END-IF                                                           
051000       END-IF                                                             
051100       PERFORM IMS-GET-INVENTERING-CDC                                    
051200     END-IF                                                               
051300     IF SEGMENT-FINNS                                                     
051400       MOVE INVH-DAREGDAT-CLO(3:6) TO MOD-TIJUSTDA1-ENTER                 
051500     ELSE                                                                 
051600       MOVE +9999999               TO MOD-TIJUSTDA1-ENTER                 
051700     END-IF                                                               
051800     PERFORM UNTIL RAD > 2                                                
051900       IF SEGMENT-FINNS                                                   
052000         MOVE JA TO CDC-FINNS                                             
052100         PERFORM S01-KONV-DATUM                                           
052200         IF DAT-KDSVAR-OK                                                 
052300           MOVE DAT-TIAAVVD        TO MOD-TIJUSTDA (INDX RAD)             
052400         ELSE                                                             
052500           MOVE ZERO               TO MOD-TIJUSTDA (INDX RAD)             
052600         END-IF                                                           
052700                                                                          
052800         MOVE INVH-KDJUSTYP TO W-KVJUSTYP                                 
052900         IF INVH-FLAUTLSJ = 'J'                                           
053000           MOVE 'A'             TO W-FLAUTLSJ                             
053100         ELSE                                                             
053200           MOVE SPACE           TO W-FLAUTLSJ                             
053300         END-IF                                                           
053400         MOVE W-KVJUSTYP-TOT    TO MOD-KDJUSTYP (INDX RAD)                
053500         MOVE INVH-KVJUSTKV TO MOD-KVJUSTKV (INDX RAD)                    
053600         IF INVH-KDJUSTYP NOT = 6                                         
053700           COMPUTE ANTAL-CDC = ANTAL-CDC + INVH-KVJUSTKV                  
053800         END-IF                                                           
053900         PERFORM IMS-GET-INVENTERING-CDC                                  
054000       ELSE                                                               
054100         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
054200       END-IF                                                             
054300       ADD 1 TO INDX                                                      
054400       IF INDX > 12                                                       
054500         ADD +1 TO RAD                                                    
054600         MOVE +1 TO INDX                                                  
054700       END-IF                                                             
054800     END-PERFORM                                                          
054900     IF CDC-FINNS = JA                                                    
055000       MOVE ANTAL-CDC TO MOD-SUINVJUST-C1                                 
055100     END-IF                                                               
055200     IF SEGMENT-FINNS                                                     
055300       MOVE INVH-DAREGDAT-CLO(3:6) TO MOD-TIJUSTDA1-NEXT                  
055400       IF NOT MFS-FIRST                                                   
055500         MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                        
055600         CALL WMEDKONV USING MED-WMEDAREA                                 
055700         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
055800       END-IF                                                             
055900     ELSE                                                                 
056000       MOVE +9999999               TO MOD-TIJUSTDA1-NEXT                  
056100       IF NOT MFS-FIRST                                                   
056200         MOVE INF-LAST-PAGE TO MED-IDMFSINF                               
056300         CALL WMEDKONV USING MED-WMEDAREA                                 
056400         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
056500       END-IF                                                             
056600     END-IF                                                               
056700     .                                                                    
056800     EJECT                                                                
056900 FC-SDC-NDC-PF SECTION.                                                   
057000                                                                          
057100     IF WS-IDDC NOT = C-DCS-IDDC                                          
057200        MOVE WS-IDDC TO W-IDDC-B6                                         
057300        PERFORM IMS-GU-WDB601-C                                           
057400     END-IF                                                               
057500                                                                          
057600     IF (W-TIJUSTDA2 = +9999999) AND                                      
057700        (W-TIJUSTDA1 NOT = +9999999)                                      
057800       MOVE 'GE' TO STATUS-WS                                             
057900       MOVE MFS-RENSA-FAELT TO MOD-SUINVJUST-C2                           
058000     ELSE                                                                 
058100       MOVE +0             TO ANTAL-SDC-NDC                               
058200       MOVE W-TIJUSTDA2    TO W-DAREGDAT                                  
058300       IF W-TIJUSTDA2 NOT = ZERO                                          
058400         IF W-TIJUSTDA2 < 500000                                          
058500           MOVE 20         TO W-DAREGDAT (1:2)                            
058600         ELSE                                                             
058700           IF W-TIJUSTDA2 < 999999                                        
058800             MOVE 19       TO W-DAREGDAT (1:2)                            
058900           ELSE                                                           
059000             MOVE 99999999 TO W-DAREGDAT                                  
059100           END-IF                                                         
059200         END-IF                                                           
059300       END-IF                                                             
059400       IF WS-IDDC = ZERO OR C-DCS-CDC                                     
059500         PERFORM IMS-GET-INVENT-ALL-DCFIRST                               
059600         IF SEGMENT-FINNS                                                 
059700           IF INVH-IDDC = WS-CDC-11                                       
059800             PERFORM UNTIL SEGMENT-SAKNAS OR                              
059900                           INVH-IDDC NOT = WS-CDC-11                      
060000               PERFORM IMS-GET-INVENT-ALL-DCNEXT                          
060100             END-PERFORM                                                  
060200           END-IF                                                         
060300         END-IF                                                           
060400       ELSE                                                               
060500         MOVE WS-IDDC TO W-IDDC                                           
060600         PERFORM IMS-GET-INVENTERING-DCFIRST                              
060700       END-IF                                                             
060800     END-IF                                                               
060900     IF SEGMENT-FINNS                                                     
061000       MOVE INVH-DAREGDAT-CLO(3:6) TO MOD-TIJUSTDA2-ENTER                 
061100       MOVE NEJ TO SDC-NDC-FINNS                                          
061200       MOVE +1 TO INDX                                                    
061300       MOVE +1 TO RAD1                                                    
061400       MOVE +0 TO ANTAL-SDC-NDC                                           
061500       MOVE W-TIJUSTDA2 TO W-DAREGDAT                                     
061600       IF W-TIJUSTDA2 NOT = ZERO                                          
061700         IF W-TIJUSTDA2 < 500000                                          
061800           MOVE 20     TO W-DAREGDAT (1:2)                                
061900         ELSE                                                             
062000           IF W-TIJUSTDA2 < 999999                                        
062100             MOVE 19   TO W-DAREGDAT (1:2)                                
062200           ELSE                                                           
062300             MOVE 99999999 TO W-DAREGDAT                                  
062400           END-IF                                                         
062500         END-IF                                                           
062600       END-IF                                                             
062700       PERFORM UNTIL RAD1 > 2                                             
062800         IF SEGMENT-FINNS                                                 
062900           MOVE INVH-IDDC         TO W-IDDC-B6                            
063000           MOVE JA                TO WDB6-C-SW                            
063100           PERFORM IMS-GU-WDB601-C                                        
063200           IF SEGMENT-SAKNAS                                              
063300              MOVE NEJ            TO WDB6-C-SW                            
063400           END-IF                                                         
063500                                                                          
063600           IF (WDB6-C-FINNS    AND                                        
063700             (C-DCS-CDC       OR                                          
063800              C-DCS-CDC-TR    OR                                          
063910             (C-DCS-SDC AND C-DCS-IDLANDX2 NOT = 'CN') OR                 
064000              C-DCS-JAPAN OR                                              
064010              C-DCS-AUSTRALIA))                                           
064100             MOVE JA              TO SDC-NDC-FINNS                        
064200             PERFORM S01-KONV-DATUM                                       
064300             IF DAT-KDSVAR-OK                                             
064400               MOVE DAT-TIAAVVD   TO MOD-TIJUSTDA-SDC(INDX RAD1)          
064500             ELSE                                                         
064600               MOVE ZERO          TO MOD-TIJUSTDA-SDC(INDX RAD1)          
064700             END-IF                                                       
064800             MOVE INVH-KDJUSTYP   TO W-KVJUSTYP                           
064900             IF INVH-FLAUTLSJ = 'J'                                       
065000               MOVE 'A'           TO W-FLAUTLSJ                           
065100             ELSE                                                         
065200               MOVE SPACE         TO W-FLAUTLSJ                           
065300             END-IF                                                       
065400             MOVE W-KVJUSTYP-TOT  TO MOD-KDJUSTYP-SDC(INDX RAD1)          
065500             MOVE INVH-KVJUSTKV   TO MOD-KVJUSTKV-SDC(INDX RAD1)          
065600             MOVE INVH-IDDC       TO MOD-IDDC (INDX RAD1)                 
065700                                                                          
065800             IF INVH-KDJUSTYP NOT = 6                                     
065900               COMPUTE ANTAL-SDC-NDC =                                    
066000                       ANTAL-SDC-NDC + INVH-KVJUSTKV                      
066100             END-IF                                                       
066200           ELSE                                                           
066300             ADD -1 TO INDX                                               
066400           END-IF                                                         
066500                                                                          
066600           IF WS-IDDC = ZERO OR  WS-CDC-11                                
066700             PERFORM IMS-GET-INVENT-ALL-DCNEXT                            
066800             IF SEGMENT-FINNS                                             
066900               IF INVH-IDDC = WS-CDC-11                                   
067000                 PERFORM UNTIL SEGMENT-SAKNAS OR                          
067100                               INVH-IDDC NOT = WS-CDC-11                  
067200                   PERFORM IMS-GET-INVENT-ALL-DCNEXT                      
067300                 END-PERFORM                                              
067400               END-IF                                                     
067500             END-IF                                                       
067600           ELSE                                                           
067700             PERFORM IMS-GET-INVENTERING-DCNEXT                           
067800           END-IF                                                         
067900                                                                          
068000         ELSE                                                             
068100           PERFORM MFS-RENSA-RAD1-FAELT-UT                                
068200         END-IF                                                           
068300         ADD +1 TO INDX                                                   
068400         IF INDX > 12                                                     
068500           ADD +1 TO RAD1                                                 
068600           MOVE +1 TO INDX                                                
068700         END-IF                                                           
068800       END-PERFORM                                                        
068900                                                                          
069000       IF SDC-NDC-FINNS = JA                                              
069100         MOVE ANTAL-SDC-NDC          TO MOD-SUINVJUST-C2                  
069200       END-IF                                                             
069300       IF SEGMENT-FINNS                                                   
069400         MOVE INVH-DAREGDAT-CLO(3:6) TO MOD-TIJUSTDA2-NEXT                
069500         IF NOT MFS-FIRST                                                 
069600           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
069700           CALL WMEDKONV USING MED-WMEDAREA                               
069800           MOVE MED-MFSINF TO MOD-TEMFSINF                                
069900         END-IF                                                           
070000       ELSE                                                               
070100         MOVE +9999999               TO MOD-TIJUSTDA2-NEXT                
070200         IF NOT MFS-FIRST                                                 
070300           MOVE INF-LAST-PAGE TO MED-IDMFSINF                             
070400           CALL WMEDKONV USING MED-WMEDAREA                               
070500           MOVE MED-MFSINF TO MOD-TEMFSINF                                
070600         END-IF                                                           
070700       END-IF                                                             
070800     ELSE                                                                 
070900       MOVE +9999999           TO MOD-TIJUSTDA2-ENTER                     
071000       IF CDC-FINNS = NEJ                                                 
071100         MOVE ERR-PART-MISSING TO MED-IDMFSFEL                            
071200         CALL WMEDKONV USING MED-WMEDAREA                                 
071300         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
071400         PERFORM MFS-RENSA-FAELT-UT                                       
071500         MOVE MFS-RENSA-FAELT  TO MOD-TEMFSINF                            
071600       END-IF                                                             
071700     END-IF                                                               
071800     .                                                                    
071900     EJECT                                                                
072000 FD-NDC SECTION.                                                          
072100                                                                          
072200     MOVE NEJ TO NDC-FINNS                                                
072300     MOVE +1 TO INDX                                                      
072400     MOVE +1 TO RAD                                                       
072500     MOVE WS-IDDC TO W-IDDC-NDC                                           
072600     IF (W-TIJUSTDA2 = +9999999) AND                                      
072700        (W-TIJUSTDA1 NOT = +9999999)                                      
072800       MOVE 'GE' TO STATUS-WS                                             
072900       MOVE MFS-RENSA-FAELT TO MOD-SUINVJUST-C2                           
073000     ELSE                                                                 
073100       MOVE +0 TO ANTAL-NDC                                               
073200       MOVE W-TIJUSTDA2 TO W-DAREGDAT                                     
073300       IF W-TIJUSTDA2 NOT = ZERO                                          
073400         IF W-TIJUSTDA2 < 500000                                          
073500           MOVE 20      TO W-DAREGDAT (1:2)                               
073600         ELSE                                                             
073700           IF W-TIJUSTDA2 < 999999                                        
073800             MOVE 19    TO W-DAREGDAT (1:2)                               
073900           ELSE                                                           
074000             MOVE 99999999 TO W-DAREGDAT                                  
074100           END-IF                                                         
074200         END-IF                                                           
074300       END-IF                                                             
074400       PERFORM IMS-GET-INVENTERING-NDC                                    
074500     END-IF                                                               
074600     IF SEGMENT-FINNS                                                     
074700       MOVE INVH-DAREGDAT-CLO(3:6) TO MOD-TIJUSTDA2-ENTER                 
074800     ELSE                                                                 
074900       MOVE +9999999               TO MOD-TIJUSTDA2-ENTER                 
075000     END-IF                                                               
075100     IF SEGMENT-FINNS                                                     
075200       PERFORM UNTIL RAD > 2                                              
075300         IF SEGMENT-FINNS                                                 
075400           MOVE JA TO NDC-FINNS                                           
075500           PERFORM S01-KONV-DATUM                                         
075600           IF DAT-KDSVAR-OK                                               
075700             MOVE DAT-TIAAVVD    TO                                       
075800             MOD-TIJUSTDA-SDC (INDX RAD)                                  
075900           ELSE                                                           
076000             MOVE ZERO TO                                                 
076100             MOD-TIJUSTDA-SDC (INDX RAD)                                  
076200           END-IF                                                         
076300                                                                          
076400           MOVE INVH-KDJUSTYP TO W-KVJUSTYP                               
076500                                                                          
076600           IF INVH-FLAUTLSJ = 'J'                                         
076700             MOVE 'A'        TO W-FLAUTLSJ                                
076800           ELSE                                                           
076900             MOVE SPACE      TO W-FLAUTLSJ                                
077000           END-IF                                                         
077100                                                                          
077200           MOVE W-KVJUSTYP-TOT    TO                                      
077300                MOD-KDJUSTYP-SDC (INDX RAD)                               
077400           MOVE INVH-KVJUSTKV TO                                          
077500                MOD-KVJUSTKV-SDC (INDX RAD)                               
077600           MOVE INVH-IDDC         TO                                      
077700                MOD-IDDC (INDX RAD)                                       
077800           IF INVH-KDJUSTYP NOT = 6                                       
077900             COMPUTE ANTAL-NDC = ANTAL-NDC + INVH-KVJUSTKV                
078000           END-IF                                                         
078100           PERFORM IMS-GET-INVENTERING-NDC                                
078200         ELSE                                                             
078300           PERFORM MFS-RENSA-RAD-FAELT-UT                                 
078400         END-IF                                                           
078500         ADD 1 TO INDX                                                    
078600         IF INDX > 12                                                     
078700           ADD +1 TO RAD                                                  
078800           MOVE +1 TO INDX                                                
078900         END-IF                                                           
079000       END-PERFORM                                                        
079100       IF NDC-FINNS = JA                                                  
079200         MOVE ANTAL-NDC TO MOD-SUINVJUST-C2                               
079300       END-IF                                                             
079400       IF SEGMENT-FINNS                                                   
079500         MOVE INVH-DAREGDAT-CLO(3:6) TO MOD-TIJUSTDA1-NEXT                
079600         IF NOT MFS-FIRST                                                 
079700           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
079800           CALL WMEDKONV USING MED-WMEDAREA                               
079900           MOVE MED-MFSINF TO MOD-TEMFSINF                                
080000         END-IF                                                           
080100       ELSE                                                               
080200         MOVE +9999999               TO MOD-TIJUSTDA1-NEXT                
080300         IF NOT MFS-FIRST                                                 
080400           MOVE INF-LAST-PAGE TO MED-IDMFSINF                             
080500           CALL WMEDKONV USING MED-WMEDAREA                               
080600           MOVE MED-MFSINF TO MOD-TEMFSINF                                
080700         END-IF                                                           
080800       END-IF                                                             
080900     ELSE                                                                 
081000       IF NDC-FINNS = NEJ                                                 
081100         MOVE ERR-PART-MISSING TO MED-IDMFSFEL                            
081200         CALL WMEDKONV USING MED-WMEDAREA                                 
081300         MOVE MED-MFSFEL       TO MOD-TEMFSFEL                            
081400         PERFORM MFS-RENSA-FAELT-UT                                       
081500         MOVE MFS-RENSA-FAELT  TO MOD-TEMFSINF                            
081600       END-IF                                                             
081700     END-IF                                                               
081800     .                                                                    
081900     EJECT                                                                
082000 S01-KONV-DATUM SECTION.                                                  
082100                                                                          
082200     MOVE INVH-DAREGDAT-CLO(3:6) TO DAT-I-TIDATUM                         
082300     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
082400                                                                          
082500     CALL WDATKONV USING       DAT-KDDATFORM                              
082600                               DAT-I-TIDATUM                              
082700                               DAT-O-TIDATUM                              
082800                               DAT-KDSVAR                                 
082900     .                                                                    
083000     EJECT                                                                
083100 MFS-RENSA-FAELT-UT SECTION.                                              
083200     MOVE MFS-RENSA-FAELT TO MOD-TIJUSTDA1-ENTER                          
083300                             MOD-TIJUSTDA2-ENTER                          
083400                             MOD-TIJUSTDA1-NEXT                           
083500                             MOD-TIJUSTDA2-NEXT                           
083600                             MOD-SUINVJUST-C1                             
083700                             MOD-SUINVJUST-C2                             
083800     MOVE +1 TO RAD                                                       
083900     PERFORM UNTIL RAD > MAX-RAD                                          
084000       MOVE +1 TO INDX                                                    
084100       PERFORM UNTIL INDX > MAX-INDX                                      
084200         PERFORM MFS-RENSA-RAD-FAELT-UT                                   
084300         ADD +1 TO INDX                                                   
084400       END-PERFORM                                                        
084500       ADD +1 TO RAD                                                      
084600     END-PERFORM                                                          
084700                                                                          
084800     MOVE +1 TO RAD1                                                      
084900                                                                          
085000     PERFORM UNTIL RAD1 > MAX-RAD1                                        
085100       MOVE +1 TO INDX                                                    
085200       PERFORM UNTIL INDX > MAX-INDX                                      
085300         PERFORM MFS-RENSA-RAD1-FAELT-UT                                  
085400         ADD +1 TO INDX                                                   
085500       END-PERFORM                                                        
085600       ADD +1 TO RAD1                                                     
085700     END-PERFORM                                                          
085800     .                                                                    
085900     SKIP3                                                                
086000 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
086100     MOVE MFS-RENSA-FAELT TO MOD-TIJUSTDA     (INDX RAD)                  
086200                             MOD-KDJUSTYP     (INDX RAD)                  
086300                             MOD-KVJUSTKV     (INDX RAD)                  
086400                                                                          
086500     .                                                                    
086600     SKIP2                                                                
086700 MFS-RENSA-RAD1-FAELT-UT SECTION.                                         
086800     MOVE MFS-RENSA-FAELT TO MOD-TIJUSTDA-SDC (INDX RAD1)                 
086900                             MOD-KDJUSTYP-SDC (INDX RAD1)                 
087000                             MOD-KVJUSTKV-SDC (INDX RAD1)                 
087100                             MOD-IDDC         (INDX RAD1)                 
087200     .                                                                    
087300* IMS SEKTIONER                                                           
087400     SKIP3                                                                
087500 IMS-GET-MSG SECTION.                                                     
087600                                                                          
087700     MOVE '  QC' TO GODK-STATUSKODER                                      
087800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
087900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
088000     PERFORM IMS-STATUSKONTROLL                                           
088100     .                                                                    
088200                                                                          
088300     SKIP2                                                                
088400 IMS-INSERT-MSG SECTION.                                                  
088500                                                                          
088600     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
088700       MOVE '0' TO MFS-KDHUVOMR                                           
088800     END-IF                                                               
088900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
089000     MOVE SPACE TO GODK-STATUSKODER                                       
089100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
089200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
089300     PERFORM IMS-STATUSKONTROLL                                           
089400     .                                                                    
089500                                                                          
089600     SKIP2                                                                
089700 IMS-GET-CDC-ARTIKEL SECTION.                                             
089800                                                                          
089900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
090000            DELIMITED BY SIZE INTO SSA1                                   
090100     MOVE '  GE' TO GODK-STATUSKODER                                      
090200     CALL CBLTDLI USING GU AC-PCB DLI-IO-AREA SSA1                        
090300     MOVE AC-STATUS-CODE TO STATUS-WS                                     
090400     PERFORM IMS-STATUSKONTROLL                                           
090500     .                                                                    
090600     SKIP2                                                                
090700 IMS-GU-ROT-INVHIST SECTION.                                              
090800                                                                          
090900     STRING 'WLINVC01(IDARTNR  =' W-IDARTNR-X ')'                         
091000            DELIMITED BY SIZE INTO SSA1                                   
091100     MOVE '  GE' TO GODK-STATUSKODER                                      
091200     CALL CBLTDLI USING GU INVC-PCB DLI-IO-AREA2 SSA1                     
091300     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
091400     PERFORM IMS-STATUSKONTROLL                                           
091500     .                                                                    
091600     EJECT                                                                
091700 IMS-GET-INVENTERING-CDC SECTION.                                         
091800                                                                          
091900     STRING 'WLINVC11(DAREGDAT<=' W-DAREGDAT-X                            
092000                    '&IDDC     =' W-IDDC-CDC-X ')'                        
092100            DELIMITED BY SIZE INTO SSA1                                   
092200     MOVE '  GE' TO GODK-STATUSKODER                                      
092300     CALL CBLTDLI USING GNP INVC-PCB DLI-IO-AREA2 SSA1                    
092400     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
092500     PERFORM IMS-STATUSKONTROLL                                           
092600     .                                                                    
092700     SKIP2                                                                
092800 IMS-GET-INVENTERING-DCFIRST SECTION.                                     
092900                                                                          
093000     STRING 'WLINVC11*F(DAREGDAT<=' W-DAREGDAT-X                          
093100                    '&IDDC     =' W-IDDC-X ')'                            
093200            DELIMITED BY SIZE INTO SSA1                                   
093300     MOVE '  GE' TO GODK-STATUSKODER                                      
093400     CALL CBLTDLI USING GNP INVC-PCB DLI-IO-AREA2 SSA1                    
093500     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
093600     PERFORM IMS-STATUSKONTROLL                                           
093700     .                                                                    
093800     SKIP2                                                                
093900 IMS-GET-INVENTERING-DCNEXT  SECTION.                                     
094000                                                                          
094100     STRING 'WLINVC11(DAREGDAT<=' W-DAREGDAT-X                            
094200                    '&IDDC     =' W-IDDC-X ')'                            
094300            DELIMITED BY SIZE INTO SSA1                                   
094400     MOVE '  GE' TO GODK-STATUSKODER                                      
094500     CALL CBLTDLI USING GNP INVC-PCB DLI-IO-AREA2 SSA1                    
094600     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
094700     PERFORM IMS-STATUSKONTROLL                                           
094800     .                                                                    
094900     SKIP2                                                                
095000 IMS-GET-INVENTERING-NDC SECTION.                                         
095100                                                                          
095200     STRING 'WLINVC11(DAREGDAT<=' W-DAREGDAT-X                            
095300                    '&IDDC     =' W-IDDC-NDC-X ')'                        
095400            DELIMITED BY SIZE INTO SSA1                                   
095500     MOVE '  GE' TO GODK-STATUSKODER                                      
095600     CALL CBLTDLI USING GNP INVC-PCB DLI-IO-AREA2 SSA1                    
095700     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
095800     PERFORM IMS-STATUSKONTROLL                                           
095900     .                                                                    
096000     EJECT                                                                
096100 IMS-GET-INVENT-ALL-DCFIRST SECTION.                                      
096200                                                                          
096300     STRING 'WLINVC11*F(DAREGDAT<=' W-DAREGDAT-X ')'                      
096400            DELIMITED BY SIZE INTO SSA1                                   
096500     MOVE '  GE' TO GODK-STATUSKODER                                      
096600     CALL CBLTDLI USING GNP INVC-PCB DLI-IO-AREA2 SSA1                    
096700     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
096800     PERFORM IMS-STATUSKONTROLL                                           
096900     .                                                                    
097000     EJECT                                                                
097100 IMS-GET-INVENT-ALL-DCNEXT SECTION.                                       
097200                                                                          
097300     STRING 'WLINVC11(DAREGDAT<=' W-DAREGDAT-X ')'                        
097400            DELIMITED BY SIZE INTO SSA1                                   
097500     MOVE '  GE' TO GODK-STATUSKODER                                      
097600     CALL CBLTDLI USING GNP INVC-PCB DLI-IO-AREA2 SSA1                    
097700     MOVE INVC-STATUS-CODE TO STATUS-WS                                   
097800     PERFORM IMS-STATUSKONTROLL                                           
097900     .                                                                    
098000     EJECT                                                                
098100 IMS-GU-WDB601-A  SECTION.                                                
098200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
098300          DELIMITED BY SIZE INTO SSA1                                     
098400     MOVE '  GE' TO GODK-STATUSKODER                                      
098500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-A SSA1               
098600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
098700     PERFORM IMS-STATUSKONTROLL                                           
098800     IF SEGMENT-SAKNAS                                                    
098900        MOVE SPACE TO A-DCS-KDDC                                          
099000     END-IF                                                               
099100     .                                                                    
099200     EJECT                                                                
099300 IMS-GU-WDB601-B  SECTION.                                                
099400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
099500          DELIMITED BY SIZE INTO SSA1                                     
099600     MOVE '  GE' TO GODK-STATUSKODER                                      
099700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-B SSA1               
099800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
099900     PERFORM IMS-STATUSKONTROLL                                           
100000     .                                                                    
100100     EJECT                                                                
100200 IMS-GU-WDB601-C  SECTION.                                                
100300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
100400          DELIMITED BY SIZE INTO SSA1                                     
100500     MOVE '  GE' TO GODK-STATUSKODER                                      
100600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-C SSA1               
100700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
100800     PERFORM IMS-STATUSKONTROLL                                           
100900     IF SEGMENT-SAKNAS                                                    
101000        MOVE SPACE TO C-DCS-KDDC                                          
101100     END-IF                                                               
101200     .                                                                    
101300     EJECT                                                                
101400 IMS-STATUSKONTROLL SECTION.                                              
101500                                                                          
101600     SET STATUS-IX TO 1                                                   
101700     SEARCH GODK-STATUS                                                   
101800       AT END                                                             
101900         CALL FELLOG                                                      
102000     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
102100       CONTINUE                                                           
102200     END-SEARCH                                                           
102300     .                                                                    
