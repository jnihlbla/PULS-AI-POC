000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4013100.                                                
000400 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000500 DATE-WRITTEN.   90/08/03.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET REDIGERAR ARTIKELFRÅGA VOR FÖR VOR-GRUPPEN.           
001100*        BLÄDDRING ÄR MÖJLIG PÅ VAGNTILLHÖRIGHET OCH FLER                 
001200*        SPRÅK FÖR ARTIKEL-BENÄMNING.                                     
001300*                                                                         
001400*        DATABASER SOM BEARBETAS.                                         
001500*                                                                         
001600*        PROGRAMMET ÄR EN FRÅGE-MPP                                       
001700*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001800*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001900*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
002000*        PROGRAMMET LÄSER      WLINLJ (WDM5)                              
002100*        PROGRAMMET LÄSER      WLARTM (WDK9)                              
002200*        PROGRAMMET LÄSER      WLKATN (WDN6)                              
002300*        PROGRAMMET LÄSER      WLERSA (WDD7)                              
002400*        PROGRAMMET LÄSER      WLINLB (WDD9)                              
002500*                                                                         
002600*                                                                         
002700*    INDATA.                                                              
002800*        TRANSAKTION: W4T131                                              
002900*        MID:         W4I13101                                            
003000*                                                                         
003100*    UTDATA.                                                              
003200*        MOD:         W4O13101                                            
003300*                                                                         
003400*    E-TRACKER: 7450319  2008-HÖST  VOHF                                  
003410*    E-TRACKER: 10254592 2015 DECOMISSION VOHF                            
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900 WORKING-STORAGE SECTION.                                                 
004000*    -COPY WY2000W1                                                       
004100     SKIP3                                                                
004200 77  IDPGM                       PIC X(08)   VALUE 'W4013100'.            
004300                                                                          
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600                                                                          
004700*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004800 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004900 77  MAX-INDX                    PIC S9(4)  VALUE +10   COMP SYNC.        
005000                                                                          
005100 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +700  COMP SYNC.        
005200                                                                          
005300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005400 77  BEHORIG                     PIC X       VALUE SPACE.                 
005500                                                                          
005600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005700     88  NYCKLAR-OK                          VALUE 'J'.                   
005800     88  NYCKLAR-FEL                         VALUE 'N'.                   
005900     88  NYCKLEL-EJ-NUMERISK                 VALUE 'A'.                   
006000                                                                          
006100 77  ALLT-SW                     PIC X       VALUE 'J'.                   
006200     88  ALLT-OK                             VALUE 'J'.                   
006300                                                                          
006400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006500     88  EGEN-MID                            VALUE '4131'.                
006600     88  GODK-MID                            VALUE '4131' '4132'          
006700                                                   '4133' '4134'          
006800                                                   '4135' '4136'          
006900                                                   '4137' '4138'          
007000                                                   '4139' '1101'          
007100                                                   '1102' '1103'          
007200                                                   '1213' '1221'          
007300                                                   '2101' '2102'          
007400                                                   '2103' '2106'          
007500                                                   '2344'                 
007600                                                   '3103' '3151'          
007700                                                   '2226' '4101'          
007800                                                   '4102' '4103'          
007900                                                   '4104' '5107'.         
008000 01  ARTIKEL-REKSIFFRA.                                                   
008100     03  ARTNR                   PIC X(9)    VALUE SPACE.                 
008200     03  STRECK                  PIC X(1)    VALUE '-'.                   
008300     03  REKSIFFRA               PIC X(1)    VALUE SPACE.                 
008400                                                                          
008500 77  W-SDC-KVLS                  PIC S9(7) COMP-3 VALUE ZERO.             
008600 77  W-SDC-KVEFRS                PIC S9(7) COMP-3 VALUE ZERO.             
008700 77  W-SDC-KVAKS-SDC             PIC S9(7) COMP-3 VALUE ZERO.             
008800 77  W-SDC-KVAKS-PAV             PIC S9(7) COMP-3 VALUE ZERO.             
008900 77  W-SDC-KVUTRS                PIC S9(7) COMP-3 VALUE ZERO.             
009000 77  W-SDC-KVOKS                 PIC S9(7) COMP-3 VALUE ZERO.             
009100 01  FILLER                      PIC  X(10) VALUE 'HHHHHHHHHH'.           
009200 77  W-SPAR-KDERS                PIC  9(2) VALUE ZERO.                    
009300 77  W-ANT-ERSA                  PIC S9(1) COMP-3 VALUE ZERO.             
009400 77  W-SPAR-IDLEVNR              PIC X(5)    VALUE SPACE.                 
009500 77  W-SPAR-TILEVBSK             PIC S9(7) COMP-3 VALUE ZERO.             
009600     EJECT                                                                
009700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009800 01  GENERELLA-SUBPROGRAM.                                                
009900     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
010400     EJECT                                                                
010500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
010600*01 -COPY WMSGINIT                                                        
010700     EJECT                                                                
010800*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
010900*   -COPY WDATAREA                                                        
011000     EJECT                                                                
011100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
011200*   -COPY WMEDAREA                                                        
011300     SKIP3                                                                
011400                                                                          
011500     SKIP3                                                                
011600 01  MESSAGE-CODES.                                                       
011700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011800     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
011900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
012000     EJECT                                                                
012100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
012200*                                                                         
012300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
012400     SKIP3                                                                
012500*01  MID -COPY W4I13101    -PRE MID-                                      
012600     EJECT                                                                
012700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012800     SKIP3                                                                
012900*01  -COPY WMSGAREA                                                       
013000     EJECT                                                                
013100     03  MOD REDEFINES MSG-AREA.                                          
013200*      05  -COPY W4O13101    -PRE MOD-                                    
013300     EJECT                                                                
013400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013500     SKIP3                                                                
013600*01  -COPY WMFSAREA                                                       
013700     EJECT                                                                
013800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013900*                                                                         
014000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014100     SKIP3                                                                
014200 01  NYCKLAR-TILL-DLI.                                                    
014300     03  W-IDARTNR-X.                                                     
014400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014500                                                                          
014600     03  W-WDD901KY-X.                                                    
014610         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
014611         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
014620                                                                          
014700     03  W-IDLEVNR-X.                                                     
014800         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
014900                                                                          
015000     03  W-IDSKYLT-X.                                                     
015100         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
015200                                                                          
015300     03  W-KDNOTTYP-X.                                                    
015400         05  W-KDNOTTYP          PIC S9(1)   VALUE ZERO COMP-3.           
015500                                                                          
015600     03  W-WDN611KY-X.                                                    
015700         05  W-IDFORDON          PIC S9(3)   VALUE ZERO COMP-3.           
015800         05  W-TIOMBRYT-1        PIC S9(7)   VALUE ZERO COMP-3.           
015900                                                                          
016000*    --- STATUS-KOD FRÅN IMS                                              
016100 01  STATUS-WS                   PIC XX.                                  
016200     88  SEGMENT-FINNS                       VALUE '  '.                  
016300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016500     88  BASEN-SLUT                          VALUE 'GB'.                  
016600     SKIP2                                                                
016700 01  GODK-STATUSKODER.                                                    
016800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016900     SKIP3                                                                
017000 01  SSA1                        PIC X(64).                               
017100 01  SSA2                        PIC X(64).                               
017200 01  SSA3                        PIC X(64).                               
017300     EJECT                                                                
017400*    --- IMS FUNKTIONSKODER                                               
017500*01  -COPY W0003                                                          
017600     EJECT                                                                
017700*    ---  DLI INPUT-OUTPUT AREA                                           
017800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
017900     SKIP3                                                                
018000 01  DLI-IO-AREA.                                                         
018100     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
018200     SKIP3                                                                
018300     03  WLBENA11 REDEFINES IO-AREA.                                      
018400*        05  -COPY WDD311                                                 
018500     EJECT                                                                
018600     03  WLARTC01 REDEFINES IO-AREA.                                      
018700*        05  -COPY WDK601                                                 
018800     EJECT                                                                
018900     03  WLARTC25 REDEFINES IO-AREA.                                      
019000*        05  -COPY WDK625                                                 
019100     EJECT                                                                
019200     03  WLARTC11 REDEFINES IO-AREA.                                      
019300*        05  -COPY WDK611                                                 
019400     EJECT                                                                
019500     03  WLARTS11 REDEFINES IO-AREA.                                      
019600*        05  -COPY WDK711                                                 
019700     EJECT                                                                
019800     03  WLERSA11 REDEFINES IO-AREA.                                      
019900*        05  -COPY WDD702   -PRE ERS-                                     
020000     EJECT                                                                
020100     03  WLINLB02 REDEFINES IO-AREA.                                      
020200*        05  -COPY WDD902   -PRE LEV-                                     
020300     EJECT                                                                
020400     03  WLINLB24 REDEFINES IO-AREA.                                      
020500*        05  -COPY WDD924   -PRE INLB-                                    
020600     EJECT                                                                
020700************************************************************              
020800**** HÄR SKALL LÄGGAS TILL COPY-TEXT FÖR WDM5 **************              
020900************************************************************              
021000     03  WLARTM01 REDEFINES IO-AREA.                                      
021100*        05  -COPY WDK901   -PRE ARTM-                                    
021200     EJECT                                                                
021300     03  WLKATN01 REDEFINES IO-AREA.                                      
021400*        05  -COPY WDN601                                                 
021500     EJECT                                                                
021600     03  WLKATN11 REDEFINES IO-AREA.                                      
021700*        05  -COPY WDN611                                                 
021800     EJECT                                                                
021900 LINKAGE SECTION.                                                         
022000                                                                          
022100*01  -COPY W0009      -PRE MSG-                                           
022200     EJECT                                                                
022300*01  -COPY W0008      -PRE USEA-                                          
022400     05  FILLER                  PIC X.                                   
022500     EJECT                                                                
022600*01  -COPY W0008      -PRE BENA-                                          
022700     05  FILLER                  PIC X.                                   
022800     EJECT                                                                
022900*01  -COPY W0008      -PRE ARTC-                                          
023000     05  FILLER                  PIC X.                                   
023100     EJECT                                                                
023200*01  -COPY W0008      -PRE ARTS-                                          
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023500************************************************************              
023600**** HÄR SKALL LÄGGAS TILL PCB  WDM5 ***********************              
023700************************************************************              
023800*01  -COPY W0008      -PRE ARTM-                                          
023900     05  FILLER                  PIC X.                                   
024000     EJECT                                                                
024100*01  -COPY W0008      -PRE KATN-                                          
024200     05  FILLER                  PIC X.                                   
024300     EJECT                                                                
024400*01  -COPY W0008      -PRE ERSA-                                          
024500     05  FILLER                  PIC X.                                   
024600     EJECT                                                                
024700*01  -COPY W0008      -PRE INLB-                                          
024800     05  FILLER                  PIC X.                                   
024900     EJECT                                                                
025000 PROCEDURE DIVISION  USING MSG-PCB  USEA-PCB BENA-PCB ARTC-PCB            
025100                           ARTS-PCB ARTM-PCB KATN-PCB ERSA-PCB            
025200                           INLB-PCB.                                      
025300     ENTRY 'DLITCBL' USING MSG-PCB  USEA-PCB BENA-PCB ARTC-PCB            
025400                           ARTS-PCB ARTM-PCB KATN-PCB ERSA-PCB            
025500                           INLB-PCB.                                      
025600                                                                          
025700     PERFORM IMS-GET-MSG                                                  
025800     IF SEGMENT-FINNS                                                     
025900       PERFORM A-INIT                                                     
026000       PERFORM B-KOLLA-NYCKLAR                                            
026100       IF NYCKLAR-OK                                                      
026200         PERFORM IMS-GU-ARTC01                                            
026300         IF SEGMENT-FINNS                                                 
026400           IF MFS-FIRST                                                   
026500             PERFORM C-FOERSTA-SIDA                                       
026600           ELSE                                                           
026700             IF MFS-NEXT                                                  
026800               PERFORM D-NAESTA-SIDA                                      
026900             ELSE                                                         
027000               PERFORM E-SAMMA-SIDA                                       
027100             END-IF                                                       
027200           END-IF                                                         
027300           IF ALLT-OK                                                     
027400             PERFORM F-LAES-VISA-INFO                                     
027500           END-IF                                                         
027600         ELSE                                                             
027700           MOVE '017' TO MED-IDMFSFEL                                     
027800           CALL WMEDKONV USING MED-WMEDAREA                               
027900           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
028000           PERFORM MFS-RENSA-FAELT-UT                                     
028100           MOVE MFS-RENSA-FAELT TO MOD-TEMFSINF                           
028200         END-IF                                                           
028300       END-IF                                                             
028400       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
028500       PERFORM IMS-INSERT-MSG                                             
028600     END-IF                                                               
028700                                                                          
028800     MOVE ZERO TO RETURN-CODE                                             
028900     GOBACK                                                               
029000     .                                                                    
029100     EJECT                                                                
029200 A-INIT SECTION.                                                          
029300     IF MSG-DUBBLA-TRANSKODER                                             
029400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I13101                 
029500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
029600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
029700     ELSE                                                                 
029800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I13101                  
029900       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
030000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
030100     END-IF                                                               
030200                                                                          
030300     MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                             
030400     MOVE MSG-IDPFK TO MFS-IDPFK                                          
030500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
030600                                                                          
030700     MOVE LOW-VALUE TO MSG-AREA                                           
030800     MOVE 'W4O131N1' TO MFS-IDMOD                                         
030900     MOVE '4131' TO MOD-IDTRANS                                           
031000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
031100                                                                          
031200     IF NOT EGEN-MID                                                      
031300       MOVE SPACE TO MFS-KDTRTYP                                          
031400       MOVE '7' TO MFS-IDPFK                                              
031500     END-IF                                                               
031600     .                                                                    
031700     EJECT                                                                
031800 B-KOLLA-NYCKLAR SECTION.                                                 
031900                                                                          
032000     MOVE JA TO NYCKLAR-SW                                                
032100                                                                          
032200     MOVE ALL '+'           TO MSGI-WMSGINIT                              
032300     MOVE '001'             TO MSGI-KDCALL                                
032400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
032500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
032600     MOVE '4131'            TO MSGI-IDTRANS                               
032700                                                                          
032800     IF EGEN-MID                                                          
032900        MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                               
033000     END-IF                                                               
033100                                                                          
033200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
033300                                                                          
033400     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
033500                                                                          
033600     PERFORM BA-KOLLA-ARTNR                                               
033700                                                                          
033800     IF NYCKLAR-FEL                                                       
033900       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
034000       CALL WMEDKONV USING MED-WMEDAREA                                   
034100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
034200       PERFORM MFS-RENSA-FAELT-UT                                         
034300     END-IF                                                               
034400                                                                          
034500     IF NYCKLEL-EJ-NUMERISK                                               
034600       MOVE '001'         TO MED-IDMFSFEL                                 
034700       CALL WMEDKONV USING MED-WMEDAREA                                   
034800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
034900       PERFORM MFS-RENSA-FAELT-UT                                         
035000       MOVE NEJ TO NYCKLAR-SW                                             
035100     END-IF                                                               
035200                                                                          
035300     IF ALLT-OK                                                           
035400       CONTINUE                                                           
035500     ELSE                                                                 
035600       MOVE '405' TO MED-IDMFSFEL                                         
035700       CALL WMEDKONV USING MED-WMEDAREA                                   
035800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
035900       PERFORM MFS-RENSA-FAELT-IN                                         
036000       PERFORM MFS-RENSA-FAELT-UT                                         
036100       MOVE NEJ TO NYCKLAR-SW                                             
036200     END-IF                                                               
036300                                                                          
036400     .                                                                    
036500     EJECT                                                                
036600                                                                          
036700 BA-KOLLA-ARTNR SECTION.                                                  
036800*    -- KONTROLL AV IDARTNR                                               
036900     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
037000                                                                          
037100     IF MID-IDARTNR-IN     NOT = ALL '+'                                  
037200       MOVE '7'         TO MFS-IDPFK                                      
037300       MOVE SPACE       TO MFS-KDTRTYP                                    
037400                           MID-FLAGGA                                     
037500     END-IF                                                               
037600                                                                          
037700     IF MSGI-IDARTNR NUMERIC                                              
037800       MOVE MSGI-IDARTNR TO W-IDARTNR                                     
037900     ELSE                                                                 
038000       MOVE 'A' TO NYCKLAR-SW                                             
038100     END-IF                                                               
038200                                                                          
038300     .                                                                    
038400     EJECT                                                                
038500                                                                          
038600 C-FOERSTA-SIDA SECTION.                                                  
038700                                                                          
038800     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
038900     CALL WMEDKONV USING MED-WMEDAREA                                     
039000     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
039100                                                                          
039200*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
039300     MOVE ZERO  TO MOD-IDARTNR-ENTER                                      
039400     MOVE ZERO  TO MOD-IDFORDON-ENTER                                     
039500     MOVE ZERO  TO MOD-TIOMBRYT-ENTER                                     
039600     MOVE ZERO  TO MOD-IDARTNR-NEXT                                       
039700     MOVE ZERO  TO MOD-IDFORDON-NEXT                                      
039800     MOVE ZERO  TO MOD-TIOMBRYT-NEXT                                      
039900     MOVE ZERO  TO MOD-FLAGGA                                             
040000     MOVE JA TO ALLT-SW                                                   
040100     .                                                                    
040200     EJECT                                                                
040300 D-NAESTA-SIDA SECTION.                                                   
040400                                                                          
040500     MOVE MID-IDARTNR-NEXT TO W-IDARTNR                                   
040600     MOVE MID-IDFORDON-NEXT TO W-IDFORDON                                 
040700     MOVE MID-TIOMBRYT-NEXT TO W-TIOMBRYT-1                               
040800     MOVE JA TO ALLT-SW                                                   
040900     .                                                                    
041000     SKIP3                                                                
041100 E-SAMMA-SIDA SECTION.                                                    
041200                                                                          
041300     MOVE MID-IDARTNR-ENTER TO W-IDARTNR                                  
041400     MOVE MID-IDFORDON-ENTER TO W-IDFORDON                                
041500     MOVE MID-TIOMBRYT-ENTER TO W-TIOMBRYT-1                              
041600     MOVE JA TO ALLT-SW                                                   
041700     .                                                                    
041800     EJECT                                                                
041900 F-LAES-VISA-INFO SECTION.                                                
042000                                                                          
042100     PERFORM FA-REDIGERA-WDK6-ART                                         
042200     PERFORM FB-REDIGERA-WDD3-BENA                                        
042300     PERFORM FC-REDIGERA-WDK6-ARTC                                        
042400     PERFORM FD-REDIGERA-WDM5-INLJ                                        
042500     PERFORM FE-REDIGERA-WDK9-ARTM                                        
042600     PERFORM FF-REDIGERA-WDN6-KATN                                        
042700     PERFORM FG-REDIGERA-WDK7-ARTS                                        
042800     PERFORM FH-REDIGERA-WDD7-ERSA                                        
042900     PERFORM FI-REDIGERA-WDD9-INLA                                        
043000     .                                                                    
043100     EJECT                                                                
043200 FA-REDIGERA-WDK6-ART SECTION.                                            
043300                                                                          
043400     MOVE ART-IDLEVNR        TO W-SPAR-IDLEVNR                            
043500     MOVE ART-IDARTNR        TO ARTNR                                     
043600                                MOD-IDARTNR-ENTER                         
043700                                MOD-IDARTNR-NEXT                          
043800     MOVE ART-REKSIFFR       TO REKSIFFRA                                 
043900     MOVE ARTIKEL-REKSIFFRA  TO MOD-IDARTNR-UT                            
044000     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
044100     MOVE ART-KDERS-UTG      TO MOD-KDERS                                 
044200                                W-SPAR-KDERS                              
044300     MOVE ART-IDLEVNR        TO MOD-IDLEVNR                               
044400     MOVE ART-IDFKNGRP       TO MOD-IDFKNGRP                              
044500     .                                                                    
044600     EJECT                                                                
044700 FB-REDIGERA-WDD3-BENA SECTION.                                           
044800     IF MID-FLAGGA = SPACE                                                
044900       MOVE '1'                 TO MOD-FLAGGA                             
045000       MOVE 'S'                 TO W-IDSKYLT                              
045100       PERFORM IMS-GU-BENA11-SEQ                                          
045200       IF SEGMENT-FINNS                                                   
045300         MOVE TEXT-BEART       TO MOD-BEART-1                             
045400       ELSE                                                               
045500         MOVE SPACE            TO MOD-BEART-1                             
045600       END-IF                                                             
045700       MOVE 'GB'                TO W-IDSKYLT                              
045800       PERFORM IMS-GU-BENA11-SEQ                                          
045900       IF SEGMENT-FINNS                                                   
046000         MOVE TEXT-BEART       TO MOD-BEART-2                             
046100       ELSE                                                               
046200         MOVE SPACE            TO MOD-BEART-2                             
046300       END-IF                                                             
046400       MOVE 'D'                 TO W-IDSKYLT                              
046500       PERFORM IMS-GU-BENA11-SEQ                                          
046600       IF SEGMENT-FINNS                                                   
046700         MOVE TEXT-BEART       TO MOD-BEART-3                             
046800       ELSE                                                               
046900         MOVE SPACE            TO MOD-BEART-3                             
047000       END-IF                                                             
047100     ELSE                                                                 
047200       MOVE SPACE               TO MOD-FLAGGA                             
047300       MOVE 'F'                 TO W-IDSKYLT                              
047400       PERFORM IMS-GU-BENA11-SEQ                                          
047500       IF SEGMENT-FINNS                                                   
047600         MOVE TEXT-BEART       TO MOD-BEART-1                             
047700       ELSE                                                               
047800         MOVE SPACE            TO MOD-BEART-1                             
047900       END-IF                                                             
048000       MOVE 'E'                TO W-IDSKYLT                               
048100       PERFORM IMS-GU-BENA11-SEQ                                          
048200       IF SEGMENT-FINNS                                                   
048300         MOVE TEXT-BEART       TO MOD-BEART-2                             
048400       ELSE                                                               
048500         MOVE SPACE            TO MOD-BEART-2                             
048600       END-IF                                                             
048700       MOVE SPACE              TO MOD-BEART-3                             
048800     END-IF                                                               
048900                                                                          
049000     .                                                                    
049100     EJECT                                                                
049200 FC-REDIGERA-WDK6-ARTC SECTION.                                           
049300                                                                          
049400     PERFORM FCA-REDIGERA-WDK6-ARTC11                                     
049500     PERFORM FCB-REDIGERA-WDK6-ARTC25                                     
049600     .                                                                    
049700     EJECT                                                                
049800 FCA-REDIGERA-WDK6-ARTC11 SECTION.                                        
049900                                                                          
050000     PERFORM IMS-GNP-ARTC11                                               
050100     IF SEGMENT-FINNS                                                     
050200         MOVE CLAG-KDERS       TO W-SPAR-KDERS                            
050300         MOVE CLAG-IDBERED     TO MOD-IDBERED                             
050400                                                                          
050500         MOVE CLAG-IDANSK       TO MOD-IDANSK                             
050600                                                                          
050700         MOVE CLAG-KDARTURS       TO MOD-KDARTURS                         
050800         MOVE CLAG-KDFARLIG     TO MOD-KDFARLIG                           
050900         IF CLAG-KDFARLIG = +4                                            
051000           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFARLIG-ATTR                
051100                                         MOD-KDFARLIG-TXT-ATTR            
051200           IF SWEDISH-TEXT                                                
051300             MOVE 'FARLIGT GODS' TO MOD-KDFARLIG-TXT                      
051400           ELSE                                                           
051500             MOVE 'DANG. GOODS' TO MOD-KDFARLIG-TXT                       
051600           END-IF                                                         
051700         END-IF                                                           
051800         IF CLAG-KDFARLIG = +7                                            
051900           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDFARLIG-ATTR                
052000                                         MOD-KDFARLIG-TXT-ATTR            
052100           IF SWEDISH-TEXT                                                
052200             MOVE 'BEDÖMNING FG' TO MOD-KDFARLIG-TXT                      
052300           ELSE                                                           
052400             MOVE 'JUDGEMENT DG' TO MOD-KDFARLIG-TXT                      
052500           END-IF                                                         
052600         END-IF                                                           
052700         MOVE CLAG-VKART        TO MOD-VKART                              
052800         MOVE CLAG-VLARTNTO     TO MOD-VLARTNTO                           
052900         MOVE CLAG-KVLS         TO MOD-KVLS-1                             
053000         MOVE CLAG-KVRESS       TO MOD-KVRESS-1                           
053100         MOVE CLAG-KVEFRS       TO MOD-KVEFRS-1                           
053200         MOVE CLAG-KVAKS-CDC    TO MOD-KVAKS-1                            
053300         MOVE CLAG-KVAKS-PAV    TO MOD-KVAKS-PAV-1                        
053400         MOVE CLAG-KVUTRS       TO MOD-KVUTRS-1                           
053500         MOVE CLAG-KVSPANT      TO MOD-KVSPANT-1                          
053600         MOVE CLAG-TIDISPIN     TO MOD-TIDISPIN-1                         
053700         MOVE CLAG-KDERS        TO MOD-KDERS                              
053800         MOVE CLAG-ADLAGOMR     TO MOD-ADLAGOMR                           
053900         IF CLAG-ADLAGOMR-SVS   > ZERO OR                                 
054000            CLAG-ADLAGOMR-CD(1) > 0                                       
054100           MOVE JA              TO MOD-FLERPL                             
054200         ELSE                                                             
054300           MOVE NEJ             TO MOD-FLERPL                             
054400         END-IF                                                           
054500         MOVE CLAG-ADGANG       TO MOD-ADGANG                             
054600         MOVE CLAG-ADPLATS      TO MOD-ADPLATS                            
054700         MOVE CLAG-PRARTBTO-EXP TO MOD-PRARTBTO-EXP                       
054800                                                                          
054900         PERFORM FCAA-FIXA-SPARRKOD                                       
055000     END-IF                                                               
055100     .                                                                    
055200     EJECT                                                                
055300 FCAA-FIXA-SPARRKOD        SECTION.                                       
055400                                                                          
055500     MOVE ZERO TO MOD-SPARRKOD                                            
055600                                                                          
055700     IF CLAG-KDUART = 'M'                                                 
055800       MOVE 6 TO MOD-SPARRKOD                                             
055900     ELSE                                                                 
056000       IF CLAG-KDUART = 'S'                                               
056100         MOVE 4 TO MOD-SPARRKOD                                           
056200       ELSE                                                               
056300         IF CLAG-FLLSRDEL = 'N'                                           
056400           MOVE 3 TO MOD-SPARRKOD                                         
056500         ELSE                                                             
056600           IF CLAG-KDLEVSP = 20 OR 21                                     
056700             MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-SPARRKOD-ATTR              
056800             MOVE 2 TO MOD-SPARRKOD                                       
056900           END-IF                                                         
057000         END-IF                                                           
057100       END-IF                                                             
057200     END-IF                                                               
057300                                                                          
057400     .                                                                    
057500     EJECT                                                                
057600 FCB-REDIGERA-WDK6-ARTC25 SECTION.                                        
057700                                                                          
057800     MOVE +1 TO INDX                                                      
057900     PERFORM UNTIL INDX > 3                                               
058000       IF INDX = 1                                                        
058100         MOVE +1                TO W-KDNOTTYP                             
058200         PERFORM IMS-GNP-ARTC25                                           
058300         IF SEGMENT-FINNS                                                 
058400           MOVE NOT-TEARTNOT    TO MOD-TEARTNOT-1                         
058500         ELSE                                                             
058600           MOVE SPACE            TO MOD-TEARTNOT-1                        
058700         END-IF                                                           
058800       ELSE                                                               
058900         IF INDX = 2                                                      
059000           MOVE +2              TO W-KDNOTTYP                             
059100           PERFORM IMS-GNP-ARTC25                                         
059200           IF SEGMENT-FINNS                                               
059300             MOVE NOT-TEARTNOT   TO MOD-TEARTNOT-2                        
059400           ELSE                                                           
059500             MOVE SPACE          TO MOD-TEARTNOT-2                        
059600           END-IF                                                         
059700         ELSE                                                             
059800           IF INDX = 3                                                    
059900             MOVE +6            TO W-KDNOTTYP                             
060000             PERFORM IMS-GNP-ARTC25                                       
060100           END-IF                                                         
060200         END-IF                                                           
060300       END-IF                                                             
060400       ADD +1 TO INDX                                                     
060500     END-PERFORM                                                          
060600     .                                                                    
060700     EJECT                                                                
060800 FD-REDIGERA-WDM5-INLJ SECTION.                                           
060900     CONTINUE                                                             
061000     .                                                                    
061100     EJECT                                                                
061200 FE-REDIGERA-WDK9-ARTM SECTION.                                           
061300                                                                          
061400     MOVE ZERO TO MOD-KVOKS-1                                             
061500     MOVE ZERO TO MOD-KVOKS-2                                             
061600     MOVE ZERO TO MOD-KVOKS-VOR                                           
061700     PERFORM IMS-GU-ARTM01                                                
061800     IF SEGMENT-FINNS                                                     
061900        COMPUTE MOD-KVOKS-1 =                                             
062000                ARTM-ART-KVOKS-DAG + ARTM-ART-KVOKS-BULK +                
062100                ARTM-ART-KVOKS-VOR                                        
062200        MOVE ARTM-ART-KVOKS-VOR TO MOD-KVOKS-VOR                          
062300     END-IF                                                               
062400     .                                                                    
062500     EJECT                                                                
062600 FF-REDIGERA-WDN6-KATN SECTION.                                           
062700                                                                          
062800     MOVE +1 TO INDX                                                      
062900     PERFORM IMS-GU-KATN01                                                
063000     IF SEGMENT-FINNS                                                     
063100       PERFORM UNTIL SEGMENT-SAKNAS OR                                    
063200                     BASEN-SLUT     OR                                    
063300                     INDX  > MAX-INDX                                     
063400         PERFORM IMS-GNP-KATN11                                           
063500         IF SEGMENT-FINNS                                                 
063600           IF INDX = 1                                                    
063700             MOVE KAT-IDFORDON TO MOD-IDFORDON-ENTER                      
063800             MOVE KAT-TIOMBRYT-9KOMPL TO MOD-TIOMBRYT-ENTER               
063900             MOVE KAT-IDFORDON TO MOD-IDFORDON-NEXT                       
064000             MOVE KAT-TIOMBRYT-9KOMPL TO MOD-TIOMBRYT-NEXT                
064100           END-IF                                                         
064200           MOVE KAT-BEEMBLEM TO MOD-BEEMBLEM(INDX)                        
064300         END-IF                                                           
064400         ADD +1 TO INDX                                                   
064500       END-PERFORM                                                        
064600       IF INDX > MAX-INDX                                                 
064700         PERFORM IMS-GNP-KATN11                                           
064800         IF SEGMENT-FINNS                                                 
064900           MOVE KAT-IDFORDON TO   MOD-IDFORDON-NEXT                       
065000           MOVE KAT-TIOMBRYT-9KOMPL TO MOD-TIOMBRYT-NEXT                  
065100           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
065200           CALL WMEDKONV USING MED-WMEDAREA                               
065300           MOVE MED-MFSINF TO MOD-TEMFSINF                                
065400******** ELSE                                                             
065500*          MOVE ZERO       TO     MOD-IDFORDON-NEXT                       
065600*          MOVE ZERO       TO     MOD-TIOMBRYT-NEXT                       
065700         END-IF                                                           
065800*      ELSE                                                               
065900*        MOVE ZERO         TO     MOD-IDFORDON-NEXT                       
066000******** MOVE ZERO         TO     MOD-TIOMBRYT-NEXT                       
066100       END-IF                                                             
066200     END-IF                                                               
066300     .                                                                    
066400     EJECT                                                                
066500 FG-REDIGERA-WDK7-ARTS SECTION.                                           
066600                                                                          
066700     MOVE ZERO                     TO W-SDC-KVLS                          
066800                                      W-SDC-KVEFRS                        
066900                                      W-SDC-KVAKS-SDC                     
067000                                      W-SDC-KVAKS-PAV                     
067100                                      W-SDC-KVUTRS                        
067200                                      W-SDC-KVOKS                         
067300     PERFORM IMS-GU-ARTS01                                                
067400     IF SEGMENT-FINNS                                                     
067500        PERFORM IMS-GNP-ARTS11                                            
067600        PERFORM UNTIL SEGMENT-SAKNAS                                      
067700          ADD SLAG-KVLS            TO W-SDC-KVLS                          
067800          ADD SLAG-KVEFRS          TO W-SDC-KVEFRS                        
067900          ADD SLAG-KVAKS-SDC       TO W-SDC-KVAKS-SDC                     
068000          ADD SLAG-KVAKS-PAV       TO W-SDC-KVAKS-PAV                     
068100          ADD SLAG-KVUTRS          TO W-SDC-KVUTRS                        
068200          ADD SLAG-KVOKS-DAG       TO W-SDC-KVOKS                         
068300          ADD SLAG-KVOKS-BULK      TO W-SDC-KVOKS                         
068400          PERFORM IMS-GNP-ARTS11                                          
068500        END-PERFORM                                                       
068600     END-IF                                                               
068700                                                                          
068800     MOVE W-SDC-KVLS                TO MOD-KVLS-2                         
068900     MOVE W-SDC-KVEFRS              TO MOD-KVEFRS-2                       
069000     MOVE W-SDC-KVAKS-SDC           TO MOD-KVAKS-2                        
069100     MOVE W-SDC-KVAKS-PAV           TO MOD-KVAKS-PAV-2                    
069200     MOVE W-SDC-KVUTRS              TO MOD-KVUTRS-2                       
069300     MOVE W-SDC-KVOKS               TO MOD-KVOKS-2                        
069400     .                                                                    
069500     EJECT                                                                
069600 FH-REDIGERA-WDD7-ERSA SECTION.                                           
069700                                                                          
069800     MOVE +1                 TO W-ANT-ERSA                                
069900     PERFORM IMS-GU-ERSA01                                                
070000     IF SEGMENT-FINNS                                                     
070100        PERFORM IMS-GNP-ERSA11                                            
070200        PERFORM UNTIL SEGMENT-SAKNAS OR W-ANT-ERSA > 3                    
070300           IF ERS-FLTEXT       = JA                                       
070400              CONTINUE                                                    
070500           ELSE                                                           
070600             EVALUATE TRUE                                                
070700               WHEN W-ANT-ERSA = 1                                        
070800                  MOVE ERS-IDARTNR-TILLK TO MOD-IDARTNR-ERS-1             
070900                                                                          
071000               WHEN W-ANT-ERSA = 2                                        
071100                  MOVE ERS-IDARTNR-TILLK TO MOD-IDARTNR-ERS-2             
071200                                                                          
071300               WHEN W-ANT-ERSA = 3                                        
071400                  MOVE ERS-IDARTNR-TILLK TO MOD-IDARTNR-ERS-3             
071500                                                                          
071600             END-EVALUATE                                                 
071700             ADD +1 TO W-ANT-ERSA                                         
071800           END-IF                                                         
071900           PERFORM IMS-GNP-ERSA11                                         
072000        END-PERFORM                                                       
072100     END-IF                                                               
072200                                                                          
072300     .                                                                    
072400     EJECT                                                                
072500 FI-REDIGERA-WDD9-INLA SECTION.                                           
072600                                                                          
072700     MOVE 999999    TO W-SPAR-TILEVBSK                                    
072800                                                                          
072810     MOVE W-IDARTNR TO W-IDARTNR-D9                                       
072820     MOVE MSGI-IDDC TO W-IDDC-D9                                          
072900     PERFORM IMS-GU-INLB01                                                
073000     IF SEGMENT-FINNS                                                     
073100       PERFORM IMS-GNP-INLB11                                             
073200       PERFORM UNTIL SEGMENT-SAKNAS                                       
073300         MOVE LEV-IDLEVNR TO W-IDLEVNR                                    
073400         PERFORM IMS-GNP-INLB24                                           
073500         IF SEGMENT-FINNS                                                 
073600            MOVE INLB-LEV-TILEVBSK-INL      TO TMP1-YYMMDD                
073700            MOVE W-SPAR-TILEVBSK            TO TMP2-YYMMDD                
073800            PERFORM WY2000P1                                              
073900            IF TMP1-YYMMDD < TMP2-YYMMDD AND                              
074000               INLB-LEV-FLSENLEV         =  NEJ                           
074100                MOVE INLB-LEV-TILEVBSK-INL   TO W-SPAR-TILEVBSK           
074200                MOVE INLB-LEV-KVAVIS-BSKKVAR TO MOD-KVAVIS                
074300            END-IF                                                        
074400         END-IF                                                           
074500         PERFORM IMS-GNP-INLB11                                           
074600       END-PERFORM                                                        
074700     ELSE                                                                 
074800       MOVE MFS-RENSA-FAELT     TO MOD-KVAVIS                             
074900                                   MOD-TILEVBSK                           
075000     END-IF                                                               
075100                                                                          
075200     IF W-SPAR-TILEVBSK < 999999                                          
075300         MOVE W-SPAR-TILEVBSK   TO DAT-I-TIDATUM                          
075400         MOVE 'AAMMDD'          TO DAT-KDDATFORM                          
075500         CALL WDATKONV USING DAT-KDDATFORM,                               
075600                             DAT-I-TIDATUM,                               
075700                             DAT-O-TIDATUM,                               
075800                             DAT-KDSVAR                                   
075900         IF DAT-KDSVAR-OK                                                 
076000            MOVE DAT-TIAAVVD TO MOD-TILEVBSK                              
076100         END-IF                                                           
076200     END-IF                                                               
076300     .                                                                    
076400     EJECT                                                                
076500 MFS-RENSA-FAELT-UT SECTION.                                              
076600                                                                          
076700*    --- ALLA UTDATA-FÄLT                                                 
076800*    --- INKL. BLÄDDRINGSNYCKLAR                                          
076900     MOVE MFS-RENSA-FAELT TO MOD-BEART-1                                  
077000                             MOD-BEART-2                                  
077100                             MOD-BEART-3                                  
077200                             MOD-KVLS-1                                   
077300                             MOD-KVLS-2                                   
077400                             MOD-ADLAGOMR                                 
077500                             MOD-ADGANG                                   
077600                             MOD-ADPLATS                                  
077700                             MOD-FLERPL                                   
077800                             MOD-PRARTBTO-EXP                             
077900                             MOD-KVRESS-1                                 
078000                             MOD-KVRESS-2                                 
078100                             MOD-IDLEVNR                                  
078200                             MOD-VKART                                    
078300                             MOD-KVOKS-1                                  
078400                             MOD-KVOKS-2                                  
078500                             MOD-IDANSK                                   
078600                             MOD-VLARTNTO                                 
078700                             MOD-KVEFRS-1                                 
078800                             MOD-KVEFRS-2                                 
078900                             MOD-IDBERED                                  
079000                             MOD-KDFARLIG                                 
079100                             MOD-KDFARLIG-TXT                             
079200                             MOD-KVAKS-1                                  
079300                             MOD-KVAKS-2                                  
079400                             MOD-IDFKNGRP                                 
079500                             MOD-KDERS                                    
079600                             MOD-KVAKS-PAV-1                              
079700                             MOD-KVAKS-PAV-2                              
079800                             MOD-SPARRKOD                                 
079900                             MOD-KVUTRS-1                                 
080000                             MOD-KVUTRS-2                                 
080100                             MOD-KVSPANT-1                                
080200                             MOD-KVSPANT-2                                
080300                             MOD-KDARTURS                                 
080400                             MOD-TIDISPIN-1                               
080500                             MOD-TIDISPIN-2                               
080600                             MOD-TEARTNOT-1                               
080700                             MOD-TEARTNOT-2                               
080800                             MOD-TEMFSINF                                 
080900                             MOD-IDARTNR-ENTER                            
081000                             MOD-IDARTNR-NEXT                             
081100                             MOD-IDFORDON-ENTER                           
081200                             MOD-IDFORDON-NEXT                            
081300                             MOD-TIOMBRYT-ENTER                           
081400                             MOD-TIOMBRYT-NEXT                            
081500                             MOD-FLAGGA                                   
081600                             MOD-IDARTNR-ERS-1                            
081700                             MOD-IDARTNR-ERS-2                            
081800                             MOD-IDARTNR-ERS-3                            
081900                             MOD-TILEVBSK                                 
082000                             MOD-KVAVIS                                   
082100     MOVE +1 TO INDX                                                      
082200     PERFORM UNTIL INDX NOT < MAX-INDX                                    
082300       MOVE MFS-RENSA-FAELT TO                                            
082400                             MOD-BEEMBLEM(INDX)                           
082500       ADD +1 TO INDX                                                     
082600     END-PERFORM                                                          
082700     .                                                                    
082800     SKIP2                                                                
082900 MFS-RENSA-FAELT-IN SECTION.                                              
083000                                                                          
083100*    --- ALLA INDATA-FÄLT                                                 
083200     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
083300                             MOD-IDARTNR-UT                               
083400     .                                                                    
083500     EJECT                                                                
083600* --- IMS SEKTIONER ---                                                   
083700     SKIP3                                                                
083800 IMS-GET-MSG SECTION.                                                     
083900                                                                          
084000     MOVE '  QC' TO GODK-STATUSKODER                                      
084100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
084200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
084300     PERFORM IMS-STATUSKONTROLL                                           
084400     .                                                                    
084500     SKIP3                                                                
084600 IMS-INSERT-MSG SECTION.                                                  
084700                                                                          
084800     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
084900       MOVE '0' TO MFS-KDHUVOMR                                           
085000     END-IF                                                               
085100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
085200     MOVE SPACE TO GODK-STATUSKODER                                       
085300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
085400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
085500     PERFORM IMS-STATUSKONTROLL                                           
085600     .                                                                    
085700     EJECT                                                                
085800 IMS-GU-BENA11-SEQ SECTION.                                               
085900                                                                          
086000     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
086100          DELIMITED BY SIZE INTO SSA1                                     
086200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
086300          DELIMITED BY SIZE INTO SSA2                                     
086400     MOVE '  GE' TO GODK-STATUSKODER                                      
086500     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
086600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
086700     PERFORM IMS-STATUSKONTROLL                                           
086800     .                                                                    
086900     EJECT                                                                
087000                                                                          
087100 IMS-GU-ARTC01 SECTION.                                                   
087200                                                                          
087300     STRING 'WLARTC01(IDARTNR = ' W-IDARTNR-X ')'                         
087400          DELIMITED BY SIZE INTO SSA1                                     
087500     MOVE '  GE' TO GODK-STATUSKODER                                      
087600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
087700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
087800     PERFORM IMS-STATUSKONTROLL                                           
087900     .                                                                    
088000                                                                          
088100                                                                          
088200 IMS-GNP-ARTC11 SECTION.                                                  
088300                                                                          
088400     MOVE 'WLARTC11' TO SSA1                                              
088500     MOVE '  GE' TO GODK-STATUSKODER                                      
088600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
088700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
088800     PERFORM IMS-STATUSKONTROLL                                           
088900     .                                                                    
089000                                                                          
089100 IMS-GNP-ARTC25 SECTION.                                                  
089200                                                                          
089300     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
089400          DELIMITED BY SIZE INTO SSA1                                     
089500     MOVE '  GE' TO GODK-STATUSKODER                                      
089600     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1                     
089700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
089800     PERFORM IMS-STATUSKONTROLL                                           
089900     .                                                                    
090000                                                                          
090100 IMS-GU-ARTS01 SECTION.                                                   
090200                                                                          
090300     STRING 'WLARTS01(IDARTNR = ' W-IDARTNR-X ')'                         
090400          DELIMITED BY SIZE INTO SSA1                                     
090500     MOVE '  GE' TO GODK-STATUSKODER                                      
090600     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA SSA1                      
090700     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
090800     PERFORM IMS-STATUSKONTROLL                                           
090900     .                                                                    
091000                                                                          
091100                                                                          
091200 IMS-GNP-ARTS11 SECTION.                                                  
091300                                                                          
091400     MOVE 'WLARTS11' TO SSA1                                              
091500     MOVE '  GE' TO GODK-STATUSKODER                                      
091600     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-AREA SSA1                     
091700     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
091800     PERFORM IMS-STATUSKONTROLL                                           
091900     .                                                                    
092000                                                                          
092100                                                                          
092200 IMS-GU-ERSA01 SECTION.                                                   
092300                                                                          
092400     STRING 'WLERSA01(IDARTNR = ' W-IDARTNR-X ')'                         
092500          DELIMITED BY SIZE INTO SSA1                                     
092600     MOVE '  GE' TO GODK-STATUSKODER                                      
092700     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA SSA1                      
092800     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
092900     PERFORM IMS-STATUSKONTROLL                                           
093000     .                                                                    
093100                                                                          
093200                                                                          
093300 IMS-GNP-ERSA11 SECTION.                                                  
093400                                                                          
093500     MOVE 'WLERSA11' TO SSA1                                              
093600     MOVE '  GE' TO GODK-STATUSKODER                                      
093700     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
093800     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
093900     PERFORM IMS-STATUSKONTROLL                                           
094000     .                                                                    
094100                                                                          
094200 IMS-GU-ARTM01 SECTION.                                                   
094300                                                                          
094400     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
094500          DELIMITED BY SIZE INTO SSA1                                     
094600     MOVE '  GE' TO GODK-STATUSKODER                                      
094700     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA SSA1                      
094800     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
094900     PERFORM IMS-STATUSKONTROLL                                           
095000     .                                                                    
095100     EJECT                                                                
095200 IMS-GU-KATN01 SECTION.                                                   
095300                                                                          
095400     STRING 'WLKATN01(IDARTNR  =' W-IDARTNR-X ')'                         
095500          DELIMITED BY SIZE INTO SSA1                                     
095600     MOVE '  GE' TO GODK-STATUSKODER                                      
095700     CALL CBLTDLI USING GU KATN-PCB DLI-IO-AREA SSA1                      
095800     MOVE KATN-STATUS-CODE TO STATUS-WS                                   
095900     PERFORM IMS-STATUSKONTROLL                                           
096000     .                                                                    
096100     EJECT                                                                
096200 IMS-GNP-KATN11 SECTION.                                                  
096300                                                                          
096400     STRING 'WLKATN11(WDN611KY=>' W-WDN611KY-X ')'                        
096500          DELIMITED BY SIZE INTO SSA1                                     
096600     MOVE '  GBGE' TO GODK-STATUSKODER                                    
096700     CALL CBLTDLI USING GNP KATN-PCB DLI-IO-AREA SSA1                     
096800     MOVE KATN-STATUS-CODE TO STATUS-WS                                   
096900     PERFORM IMS-STATUSKONTROLL                                           
097000     .                                                                    
097100     EJECT                                                                
097200 IMS-GU-INLB01 SECTION.                                                   
097300                                                                          
097400     STRING 'WLINLB01(WDD901KY= ' W-WDD901KY-X ')'                        
097500          DELIMITED BY SIZE INTO SSA1                                     
097600     MOVE '  GE' TO GODK-STATUSKODER                                      
097700     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1                      
097800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
097900     PERFORM IMS-STATUSKONTROLL                                           
098000     .                                                                    
098100                                                                          
098200 IMS-GNP-INLB11 SECTION.                                                  
098300                                                                          
098400     STRING 'WLINLB01(WDD901KY= ' W-WDD901KY-X ')'                        
098500          DELIMITED BY SIZE INTO SSA1                                     
098600     MOVE 'WLINLB11' TO SSA2                                              
098700     MOVE '  GE' TO GODK-STATUSKODER                                      
098800     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1 SSA2                
098900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
099000     PERFORM IMS-STATUSKONTROLL                                           
099100     .                                                                    
099200                                                                          
099300 IMS-GNP-INLB24 SECTION.                                                  
099400                                                                          
099500     STRING 'WLINLB11(IDLEVNR = ' W-IDLEVNR-X ')'                         
099600          DELIMITED BY SIZE INTO SSA1                                     
099700     MOVE 'WLINLB24' TO SSA2                                              
099800     MOVE '  GE' TO GODK-STATUSKODER                                      
099900     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1 SSA2                
100000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
100100     PERFORM IMS-STATUSKONTROLL                                           
100200     .                                                                    
100300                                                                          
100400 IMS-STATUSKONTROLL SECTION.                                              
100500                                                                          
100600     SET STATUS-IX TO 1                                                   
100700     SEARCH GODK-STATUS                                                   
100800       AT END CALL FELLOG                                                 
100900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
101000     END-SEARCH                                                           
101100     .                                                                    
101200     EJECT                                                                
101300*    -COPY WY2000P1                                                       
